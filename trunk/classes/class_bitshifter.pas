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

unit class_bitshifter;
/////////////////////////////////////////////////////////////////////////////////////
///
/// Die Klasse Bitshifter erlaubt es, einzelne Bits auf einen Stack zu schieben
/// und das Ergebnis der Vergänge als Base64String zurückzubekommen
/// Klingt zwar sehr exotisch, ist aber zur Textkompression sehr brauchbar
///
/////////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,sysutils;

type TBitshifter = class(TObject)
     private
            //Der Puffer für alle Bytes
            aBuffer      : array of Byte;

            //Größe des Puffers
            u32Buffer    : unsigned32;

            //Das aktuelle Byte
            u8CurrentByte : Byte;
            //Anzahl der schon belegten Bits im aktuellen Byte
            u8CurrentBits : Byte;

            //Position des Bitscanners
            u32OldestBit  : unsigned32;

            //Fehlerflag
            bError        : Boolean;
     protected
           procedure savebyte(u8Data:Byte);
           function  loadbyte():Byte;
           //Inhalt des Stacks auslesen
           function  hextobyte(sInput:Longstring):Byte;
           function  gethex   ():longstring;
           procedure sethex   (sData : Longstring);
     public
           constructor create();

           procedure init();

           //Die Stackfunktionen
           procedure pushbit (u8Bit:Byte);
           procedure pushbyte(u8Byte:Byte);

           //Den Stack einfach umdrehen
           procedure revert  ();

           function popbit   ():Byte;
           function popbyte  ():Byte;

           //Den Stack von hinten nach vorne durchlesen
           //also vom ältesten Wert zu neusten Bitweise vordringen
           function readbit(u32Index : unsigned32):byte;
           procedure resetfind();
           function findoldestbit():Byte;
           function findnextbit():Byte;

           //Den Stack auslesen / setzen
           property hex    : longstring read gethex write sethex;

           //Größe des Puffers
           property size   : unsigned32 read u32Buffer write u32Buffer;

           //Stackfehler
           property error  : boolean read bError write bError;
end;

implementation
/////////////////////////////////////////////////////////////////////////////////////
//Konstruktor
constructor TBitshifter.create();
begin
     Self.Init;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Alles initialisieren
procedure TBitshifter.init();
begin
     Self.u8CurrentBits:=0;
     Self.u8CurrentByte:=0;
     SetLength(aBuffer,0);
     Self.u32Buffer:=Length(aBuffer);
     Self.bError:=FALSE;
end;

//Aus einem zweiteiligen Hexstring einen Char machen
// 0x041 = 'A';
function TBitshifter.hextobyte(sInput:Longstring):Byte;
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
     result:=u8High shl 4 or u8Low;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Den aktuellen Puffer als Hex ausgeben
function  TBitshifter.gethex():longstring;
var
   u32Index : unsigned32;
begin
     u32Index:=Length(Self.aBuffer);

     result:='';
     while (u32Index > 0) do
           begin
                result:=result + IntToHex(Self.aBuffer[u32Index-1],2);
                dec (u32Index);
           end;

     //Wenn das currentbyte nicht leer ist, auch dieses anfügen
     //Und den Rest zum vollen Byte mit Nullen füllen
     if (Self.u8CurrentBits<>0) then
        begin
             result:=IntToHex(Self.u8CurrentByte shl (8 - Self.u8CurrentBits),2) + result;
        end;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Den aktuellen Puffer aus einem Hexstring setzen
procedure TBitshifter.sethex(sData : Longstring);
var
   u32Size  : unsigned32;
begin
     //Alles resetten
     Self.init();

     //Auf zweier anpassen
     u32Size:=(unsigned32(Length(sData)) div 2) shl 1;

     //Und die Hexwerte auf den Stack schieben
     while (u32Size > 0) do
           begin
                Self.PushByte(Self.HexToByte(sData[u32Size-1]+sData[u32Size]));
                dec(u32Size,2);
           end;
end;


/////////////////////////////////////////////////////////////////////////////////////
//Ein Byte auf den Buffer legen
procedure TBitshifter.savebyte(u8Data:Byte);
var
   u32Size : unsigned32;
begin
     u32Size:=Length(Self.aBuffer);

     SetLength(Self.aBuffer,u32Size+1);

     Self.aBuffer[u32Size]:=u8Data;

     Self.u32Buffer:=u32Size+1;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Ein Byte vom Buffer holen
function TBitshifter.loadbyte():Byte;
var
   u32Size : unsigned32;
begin
     if (u32Buffer>0) then
        begin
             u32Size:=Length(Self.aBuffer);

             //Index eins runter, da wir null basiert sind
             dec (u32Size);

             result:=Self.aBuffer[u32Size];

             SetLength(Self.aBuffer,u32Size);

             Self.u32Buffer:=u32Size;
        end
     else
        begin
             result:=0;
             Self.error:=TRUE;
        end; 
end;

/////////////////////////////////////////////////////////////////////////////////////
//Ein Bit lesen
function TBitshifter.popbit ():Byte;
begin
     //Hat das aktulle Byte noch bits ?
     if (Self.u8CurrentBits>0) then
        begin
             //Dann diese liefern
             result:=Self.u8CurrentByte and 1;

             //Und das Byte anpassen
             Self.u8CurrentByte:=Self.u8CurrentByte shr 1;

             dec (Self.u8CurrentBits);
        end
     else
        begin
             //Ansonsten holen wir uns ein Byte vom Stack
             Self.u8CurrentByte:=Self.LoadByte();
             Self.u8CurrentBits:=8;
             //So, nun rufen wir uns einfach selbst nochmal auf
             result:=Self.PopBit();
        end; 

end;

/////////////////////////////////////////////////////////////////////////////////////
//Ein kpl. Byte holen
function TBitshifter.popbyte():Byte;
begin
     result:=           Self.popbit shl 0;
     result:=result  or Self.popbit shl 1;
     result:=result  or Self.popbit shl 2;
     result:=result  or Self.popbit shl 3;
     result:=result  or Self.popbit shl 4;
     result:=result  or Self.popbit shl 5;
     result:=result  or Self.popbit shl 6;
     result:=result  or Self.popbit shl 7;
end;


/////////////////////////////////////////////////////////////////////////////////////
//Ein Bit in das aktuelle Byte schieben. Ist das Byte voll,
//schieben wir es auf den Puffer und fangen ein neues an
procedure TBitshifter.pushbit(u8Bit:Byte);
begin
     //Das übergebene Bit dem aktuellen Byte zufügen
     Self.u8CurrentByte:=(Self.u8CurrentByte shl 1) or (u8Bit and 1);

     //Den Bitcounter erhöhen
     inc(Self.u8CurrentBits);

     //Byte voll ?
     if (Self.u8CurrentBits > 7) then
        begin
             //Byte ablegen
             Self.savebyte(Self.u8CurrentByte);

             //Alles zurücksetzen
             Self.u8CurrentByte:=0;
             Self.u8CurrentBits:=0;
        end;
end;


/////////////////////////////////////////////////////////////////////////////////////
//Ein kpl. Byte ablegen
procedure TBitshifter.pushbyte(u8byte:Byte);
begin
     //Einfach auf die harte Tour.
     Self.PushBit(u8Byte shr 7);
     Self.PushBit(u8Byte shr 6);
     Self.PushBit(u8Byte shr 5);
     Self.PushBit(u8Byte shr 4);
     Self.PushBit(u8Byte shr 3);
     Self.PushBit(u8Byte shr 2);
     Self.PushBit(u8Byte shr 1);
     Self.PushBit(u8Byte shr 0);
end;


//Ein Bit mit dem Index u32Index aus dem Stack holen
//Index = 0 ist dabie der unterste EIntrag im Stack
function TBitshifter.readbit(u32Index : unsigned32):byte;
var
   u32Byte : unsigned32;
   u32Bit  : unsigned32;
begin
     //Die Position des Bytes ist Index / 8 -1
     u32Byte:=(u32Index shr 3);

     if (u32Byte < Self.Size) then
        begin
             //Die Position des Bits ist 7 - (Index - (ByteIndex * 8))
             //Da beim Pushe die ersten Bits im Byte ganz nach oben geschoben werden
             u32Bit:=7 - (u32Index - (u32Byte shl 3));

             //Gesuchtes Bit extrahieren
             result:=( Self.aBuffer[u32Byte] shr u32Bit );
             result:=result and 1;
        end
     else
        begin
             Self.Error:=TRUE;
        end;
end;


/////////////////////////////////////////////////////////////////////////////////////
// Den Stack von hinten nach vorne durchlesen
procedure TBitshifter.resetfind();
begin
     //Das älteste Bit liegt ganz unten, also auf Index 0
     Self.u32OldestBit:=0;
end;


function TBitshifter.findoldestbit():Byte;
begin
     Self.ResetFind();
     //Und einfach mit der internen Funktion lesen
     result:=Self.FindNextBit();
end;

function TBitshifter.findnextbit():Byte;
begin
     //Und einfach mit der internen Funktion lesen
     result:=Self.ReadBit(Self.u32OldestBit);

     //Position anpassen
     inc (Self.u32OldestBit);
end;

/////////////////////////////////////////////////////////////////////////////////////
//Den Stack umstülpen
//indem wir von Links und Rechts zur Mitte hin die Bytes vertauschen
procedure TBitshifter.revert  ();
var
   u32Left : unsigned32;
   u8Temp  : Byte;
   u32Right: unsigned32;
   u32Index: unsigned32;
begin
     if (Self.Size > 0) then
        begin
             u32Left :=0;
             u32Right:=Self.Size - 1;
             for u32Index:=0 to Self.Size shr 1 do
                 begin
                      u8Temp:=Self.aBuffer[u32Left];
                      Self.aBuffer[u32Left]:=Self.aBuffer[u32Right];
                      Self.aBuffer[u32Right]:=u8Temp;

                      inc(u32Left);
                      dec(u32Right);
                 end;
        end;
end;


end.
