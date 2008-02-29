{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
Author: Sven Lorenz / Borg@Sven-of-Nine.de
}

/////////////////////////////////////////////////////////////////////////////////////
/// Klassen, um verschieden Kompressionen durchführen zu können
///
///
/// BASE64
///
///
///
///
///
/////////////////////////////////////////////////////////////////////////////////////
unit class_compression;

interface
uses unit_typedefs,
     class_bitshifter,     //Für einfach Bithandling
     unit_strings;         //Zum Stringeinlesen

//Hilfsrecord für kleinere LZW-Bibliotheken
type lzwstruct = record
     pre  : signed32;
     post : char;
end;

//Die Verfahrenskonstanten
const
     COMPRESSION_UNKNOWN= 0;
     COMPRESSION_BASE64 = 1;
     COMPRESSION_HEX    = 2;
     COMPRESSION_RLE    = 3;
     COMPRESSION_LZW    = 4;
     COMPRESSION_DIPH   = 5;

type TCompressor = class (tobject)
     private
           u32method : unsigned32;
           bits      : TBitshifter;
     protected
           //Die Base64-Funktionen
           function base64tostring(sInput:Longstring):Longstring;
           function stringtobase64(sInput:Longstring):Longstring;

           //Die Hex-Funktionen
           function hextostring(sInput:Longstring):Longstring;
           function stringtohex(sInput:Longstring):Longstring;

           //LZW-Kompression
           function stringtolzw(sInput:Longstring):Longstring;
           function lzwtostring(sInput:Longstring):Longstring;
           //Hilfsfunktionen für LZW
           function lzwrecreate(index:unsigned32):longstring;
           function lzwfind(data:lzwstruct):signed32;
           function lzwadd(data:lzwstruct):signed32;
           procedure lzwaddbits();
           procedure lzwpushdata(u32Data:unsigned32);
           function lzwpopdata():unsigned32;
           procedure lzwinit();

           //Hilfsfunktionen
           function chartohex (cInput:Char):Longstring;
           function hextochar (sInput:Longstring):Char;
     public
           constructor create();

           //Komprimieren
           function deflate(sInput : Longstring):longstring;
           function deflate_file(sFilename : Longstring):longstring;
           //Dekomprimieren
           function inflate(sInput : Longstring):longstring;
           function inflate_file(sFilename : Longstring):longstring;

           //Kompressionmethode wählen
           property method : unsigned32 read u32method write u32method;
end;


implementation
uses sysutils;             //Für IntToHex

var
   //Ein Hilfsarray für Base64-Kodierung
   base64array : array[0..63] of char =(
                                        'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
                                        'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
                                        '0','1','2','3','4','5','6','7','8','9','+','/'
                                        );


constructor TCompressor.create();
begin
     Self.Method:=COMPRESSION_BASE64;

     Self.Bits:=TBitshifter.Create();
     Self.Bits.Init();
end;

/////////////////////////////////////////////////////////////////////////////////////
// Die abstrakten Aufrufe
function TCompressor.deflate(sInput : Longstring):longstring;
begin
     case (Self.Method) of
          COMPRESSION_BASE64 : result:=Self.stringtobase64(sInput);
          COMPRESSION_HEX    : result:=Self.stringtohex(sInput);
          COMPRESSION_LZW    : result:=Self.stringtolzw(sInput);
     end;
end;

function TCompressor.inflate(sInput : Longstring):longstring;
begin
     case (Self.Method) of
          COMPRESSION_BASE64 : result:=Self.base64tostring(sInput);
          COMPRESSION_HEX    : result:=Self.hextostring(sInput);
          COMPRESSION_LZW    : result:=Self.lzwtostring(sInput);
     end;
end;

function TCompressor.inflate_file(sFilename : Longstring):longstring;
begin
     result:=Self.inflate(string_read(sFilename));
end;

function TCompressor.deflate_file(sFilename : Longstring):longstring;
begin
     result:=Self.Deflate(string_read(sFilename));
end;


/////////////////////////////////////////////////////////////////////////////////////
///
/// Hilfsfunktionen
///
/////////////////////////////////////////////////////////////////////////////////////

function TCompressor.chartohex(cInput:Char):Longstring;
begin
     result:=IntToHex(Ord(cInput),2);
end;

//Aus einem zweiteiligen Hexstring einen Char machen
// 0x041 = 'A';
function TCompressor.hextochar(sInput:Longstring):Char;
var
   u8Low : byte;
   u8High: byte;
begin
     u8Low :=byte(sInput[2]);
     u8High:=byte(sInput[1]);

     //Auf den passenden Bereich skalieren
     case (u8Low) of
          48..57    : dec(u8Low,48);
          65..70    : dec(u8Low,55);
          97..102   : dec(u8Low,86);
     end;
     case (u8High) of
          48..57    : dec(u8High,48);
          65..70    : dec(u8High,55);
          97..102   : dec(u8High,86);
     end;

     //Fertig
     result:=chr(u8High shl 4 or u8Low);     
end;

/////////////////////////////////////////////////////////////////////////////////////
///
/// Ab hier kommen die einzelnen Kompressionsfunktion
///
/////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////
// Base64 (eigentlich keine Kompression)
/////////////////////////////////////////////////////////////////////////////////////
//Aus einem Base64-Kodierten String wieder den Urpsrung machen
function TCompressor.base64tostring(sInput:Longstring):Longstring;

//Aus einem ASCII-Wert seinen Index im Base64-Array machen
function base64(cInput : Char):byte;
begin
     result:=Ord(cInput);
     case (result) of
          //Zahlen
          Ord('0')..Ord('9')  : inc (result,4 );
          //Großes Alphabet
          Ord('A')..Ord('Z')  : dec (result,65);
          //Kleines Alphabet
          Ord('a')..Ord('z')  : dec (result,71);
          //Plus / Minus
          Ord('+')            : result:=62;
          Ord('/')            : result:=63;
     else
         result:=0;
     end;
end;

var
   u32Index  : unsigned32;
   u32Size   : unsigned32;
   u32Buff   : unsigned32;
begin
     result:='';

     u32Size:=unsigned32(length(sInput));

     u32Index:=1;
     while (u32Index < u32Size) do
           begin
                //Vier mal Sechs Bits reinschieben
                u32Buff:=base64(sInput[u32Index + 0]) shl 18 or
                         base64(sInput[u32Index + 1]) shl 12 or
                         base64(sInput[u32Index + 2]) shl  6 or
                         base64(sInput[u32Index + 3]);

                //Und drei mal acht raus
                result:=result + chr( u32Buff shr 16 and 255);
                result:=result + chr( u32Buff shr  8 and 255);
                result:=result + chr( u32Buff shr  0 and 255);

                inc(u32Index,4);
           end;

     //Die Endnullen rausnehmen
     //Sieht zwar im Nachhinein etwas komisch aus,
     //aber braucht dafür keine IFs in der Dekodierschleife
     while (sInput[u32Size]='=') do
           begin
                delete(result,Length(result),1);
                dec(u32Size);
           end;
end;

//Aus jeweils drei Bytes des Datenstromes werden 24Bit und damit 4*6 Bit
//gebildet. Diese 4 Zeichen holen wir uns aus dem Base64Array und fügen sie
//dem Ergebnis zu. Ist die Gesamtzahl der Bytes nicht durch drei teilbar,
//füllen wir nullen auf
function TCompressor.stringtobase64(sInput:Longstring):Longstring;
var
   u32Mod  : unsigned32;
   u32Index: unsigned32;
   u32buff : unsigned32;
   u32Size : unsigned32;
   sTail   : Longstring;
begin
     sTail:='';
     result:='';
     u32Size:=unsigned32(Length(sInput));

     //Auf die richtige Anzahl Bytes bringen
     u32Mod :=3 - (u32Size mod 3);
     if (u32Mod=3) then u32Mod:=0;

     u32Index:=1;
     while (u32Index <= u32Mod) do
           begin
                //String padden
                sInput:=sInput + #00;
                sTail :=sTail  + '=';
                inc(u32Index);
           end;

     //Nun gehen wir einfach den ganzen String durch
     u32Index:=1;
     while (u32Index < u32Size) do
           begin
                //Zeichen in den Bitbuffer schieben
                u32Buff:=ord(sInput[u32Index]) shl 16  or ord(sInput[u32Index+1]) shl 8  or ord(sInput[u32Index+2]) shl 0;

                //Unsere Zeichen (mit sechs Bit rausholen)
                result:=result + base64array[ (u32Buff shr 18) and 63];
                result:=result + base64array[ (u32Buff shr 12) and 63];
                result:=result + base64array[ (u32Buff shr  6) and 63];
                result:=result + base64array[ (u32Buff       ) and 63];

                inc(u32Index,3);
           end;

     //Und den Schwanz anhängen
     result:=copy(result,1,unsigned32(length(result))-u32Mod)+ sTail;
end;


/////////////////////////////////////////////////////////////////////////////////////
// Hex (eigentlich keine Kompression)
/////////////////////////////////////////////////////////////////////////////////////
function TCompressor.hextostring(sInput:Longstring):Longstring;
var
   u32Index : unsigned32;
   u32Size  : unsigned32;
begin
     result:='';

     //Auf zweier anpassen
     u32Size:=(unsigned32(Length(sInput)) div 2) shl 1;

     if (u32Size > 0) then
        begin
             u32Index:=1;
             while (u32Index <= u32Size) do
                 begin
                      result:=result + Self.HexToChar(sInput[u32Index]+sInput[u32Index+1]);
                      
                      inc(u32Index,2);
                 end;
        end;
end;

function TCompressor.stringtohex(sInput:Longstring):Longstring;
var
   u32Index : unsigned32;
   u32Size  : unsigned32;
begin
     result:='';

     u32Size:=unsigned32(Length(sInput));

     if (u32Size > 0) then
        begin
             for u32Index:=1 to u32Size do
                 begin
                      result:=result + Self.CharToHex(sInput[u32Index]);
                 end;
        end;
end;


/////////////////////////////////////////////////////////////////////////////////////
// LZW
/////////////////////////////////////////////////////////////////////////////////////

//LZW
const
     LZW_EMPTY    = -255;
     LZW_NOT_FOUND= -100;
     LZW_MIN_BITS = 1;
     LZW_MAX_BITS = 16;
var
   //Array mit Dictionary
   aLZWBibo    : array of lzwstruct;
   //Bibliothekswechsel bis zum nächsten Bitwechsel
   u32LZWLimit : unsigned32;
   //Anzahl der aktuellen Streambits
   u32LZWBits  : unsigned32;

//Die Bibliothek mit alle ASCII-Zeichen füllen (Ohne Vorgänger);
procedure TCompressor.lzwinit();
var
   u32Index : unsigned32;
   sTemp    : LZWStruct;
begin
     //Alles freigeben
     SetLength(aLZWBibo,0);

     //Aktuelle Streamgröße setzem
     u32LZWBits :=LZW_MIN_BITS;
     //Der Puffer enthält maximal soviele Einträge
     u32LZWLimit:=0;
     Self.LZWAddBits();


     sTemp.pre :=LZW_EMPTY;
     for u32Index:=0 to 255 do
         begin
              sTemp.Post:=char(u32Index);
              Self.lzwadd(sTemp);
         end;
end;

//Den Ausgangsdatenstrom die übergeben Zahl zufügen
//Die Anzahl der gespeicherten Bits ist variabel und
//hängt von der aktuellen Größe des Dictionaries ab
procedure TCompressor.lzwpushdata(u32Data:unsigned32);
var
   u32Index : unsigned32;
begin
     for u32Index:=1 to u32LZWBits do
         begin
              Self.bits.pushbit( byte(u32Data) );
              u32Data:=u32Data shr 1;
         end;
end;

//Aus dem Stack die Bits lesen
//Da wir hier umgekehrt aus dem Puffer lesen, müssen wir ein paar
//Shift-Verrenkungen machen, um den Datenstrom richtig zu lesen
function TCompressor.lzwpopdata():unsigned32;
var
   u32Index : unsigned32;
   u32Temp  : unsigned32;
begin
     result:=0;
     for u32Index:=1 to u32LZWBits do
         begin
              u32Temp:=Self.Bits.findnextbit() shl u32LZWBits;
              result:=(result  or u32Temp) shr 1;
         end;
     result:=result;
end;

//Aus der Bibliothek wieder einen Eintrag restaurieren.
//Dabei gehen wir rekursiv die Einträge durch, bis
//Post=LZW-EMPTY ist
function TCompressor.lzwrecreate(index:unsigned32):longstring;
begin
     result:='';
     if (aLZWBibo[index].pre<>LZW_EMPTY) then
        begin
             result:=LZWRecreate(aLZWBibo[index].pre) + aLZWBibo[index].post;
        end
     else
        begin
             result:=aLZWBibo[index].post;
        end;
end;

//Prüfen, ob es den Eintrag schon gibt
function TCompressor.lzwfind(data:lzwstruct):signed32;
var
   u32Pos : unsigned32;
   u32Size: unsigned32;
   sTemp  : lzwstruct;
begin
     result:=LZW_NOT_FOUND;

     u32Size:=Length(aLZWBibo);
     if (u32Size > 0) then
        begin
             for u32Pos:=u32Size -1 downto 0 do
                 begin
                      //Bibo-Eintrag holen
                      sTemp:=aLZWBibo[u32Pos];

                      //Ist das Prefix identisch ?
                      if (sTemp.pre=data.pre) then
                         begin
                              //Auch das Zeichen identisch ?
                              if (sTemp.post=data.post) then
                                 begin
                                      result:=u32Pos;
                                      break;
                                 end;
                         end;
                 end;
        end;
end;

//Einen neuen Eintrag der Bibliothek zufügen
function TCompressor.lzwadd(data:lzwstruct):signed32;
var
   u32Pos : unsigned32;
begin
     u32Pos:=Length(aLZWBibo);

     SetLength(aLZWBibo,u32Pos+1);

     aLZWBibo[u32Pos]:=data;

     result:=signed32(u32Pos);
end;


//Wenn die aktuelle Dic-Größe nicht mehr aussreicht,
//wir die Bitbreite erhöht. Ist die maximale Breite
//erreicht, wir das Dict. neu initialisiert
procedure TCompressor.LZWaddbits();
begin
     //Verfügbare Bits um eins erhöhen
     inc(u32LZWBits);

     //Limit anpassen
     u32LZWLimit:=1 shl u32LZWBits;
end;

//Einen String LZW komprimieren
function TCompressor.stringtolzw(sInput:Longstring):Longstring;
var
   u32Index : unsigned32;
   s32Found : signed32;
   sData    : LZWStruct;
begin
     Self.Bits.init();

     result:='';

     //Die Bibliothek initialisieren
     self.lzwinit();

     //Wir fangen mit einem Leeren Pre-Index an
     sData.Pre:=LZW_EMPTY;
     //Alle Zeichen in der Kette verarebiten
     for u32Index:=1 to Length(sInput) do
         begin
              //Zeichen holen
              sData.Post:=sInput[u32Index];

              //Gibt es den Eintrag schon ?
              // Ein Eintrag besteht immer aus seinem Verweis auf einen
              // Vorgänger in der Bibliothek (oder EMPTY wenn es keinen Vorgänger gibt)
              // und dem aktuell zu kodierenden Zeichen
              s32found:=lzwfind(sData);

              //Wenn wir einen Eintrag gefunden haben., merken wir uns diesen als
              //Vorgänger.
              if (s32found <> LZW_NOT_FOUND) then
                 begin
                      //Speichern
                      sData.Pre :=s32Found;
                 end
              else
                 begin
                      //Nichts gefunden, dann fügen wir den Vorgänger und das
                      //Zeichen der Bibliothek zu
                      //Nein, zum Puffer zufügen
                      Self.lzwadd(sData);

                      //Und geben den Vorgänger aus
                      Self.LZWPushData(unsigned32(sData.pre));

                      //Aktuelles Zeichen wird nun der Vorgänger
                      sData.pre :=ord(sData.Post);
                 end;
         end;

     //Letztes Zeichen nicht vergessen
     if (sData.pre <> LZW_EMPTY) then
        begin
             Self.LZWPushData(unsigned32(sData.pre));
        end;

     result:=Self.bits.hex;
end;


function TCompressor.lzwtostring(sInput:Longstring):Longstring;
var
   u32Index : unsigned32;
   u32Last  : unsigned32;
   u32Next  : unsigned32;
   u32Count : unsigned32;
   sData    : LZWStruct;
   sTemp    : Longstring;
begin
     //Bibliothek initialisieren
     result:='';
     Self.LZWInit();

     //Daten auf den Stack schieben
     Self.Bits.Init();
     Self.bits.hex:=sInput;

     //Da die Kompression auf den Stack pushed, wir aber den "untersten Wert"
     //des Stacks benötigen, benutzen wir den direkten Zugriff ohne zu poppen
     Self.Bits.resetfind();

     u32Count:=0;

     //Erstes Zeichen
     u32Last:=Self.LZWPopData();
     result:=result + Self.LZWRecreate(u32Last);

     //Und nun solange, bis nichts mehr kommt
     repeat
           inc(u32Count);
           //Nächsten Wert holen
           u32Next:=Self.lzwpopdata();

           if (not Self.bits.error) then
              begin
                   //Gibt es diesen Eintrag schon ?
                   if (u32next < unsigned32(length(aLZWBibo))) then
                      begin
                           //Eintrag holen
                           sTemp:=Self.LZWRecreate(u32Next);

                           //Neuen Eintrag zusammenbauen
                           sData.pre :=u32Last;
                           sData.post:=sTemp[1];
                           lzwadd(sData);
                      end
                   else
                      begin
                           //Eintrag holen
                           sTemp:=Self.LZWRecreate(u32Last);

                           //Neuen Eintrag zusammenbauen
                           sData.pre :=u32Last;
                           sData.post:=sTemp[1];
                           lzwadd(sData);
                      end;
                   //Eintrag holen
                   result:=result + Self.LZWRecreate(u32Next);

                   u32Last:=u32Next;
              end;
     until (Self.Bits.Error);
end;


end.
