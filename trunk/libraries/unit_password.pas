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

{$IFDEF FPC}
        //Einstellungen f¸r den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
unit Unit_Password;
////////////////////////////////////////////////////////////////////////////////
///
/// Unit zur einfachen Erzeugung von Kennwˆrtern
/// (c) 2005 Borg@Sven-of-Nine.de
///
///
/// Die Funktion StringToPass benutzt die Eigenart eines PRNG um aus einem
/// String aufgrund der Charakteristika des RNG eine immer gleich Passphrase
/// zu erzeugen
////////////////////////////////////////////////////////////////////////////////
interface
uses Unit_TypeDefs;


//Aus einem String eine LongInt machen (Hashen)
function  Pass_StringToHash(sInput:LongString):Unsigned32;

//Einen mit Zahlen gef¸llten String erzeugen
function  Pass_CreateRandomNumber    (u32Size:Unsigned32;u32Seed:unsigned32=0):LongString;

//Einen mit Zufallszeichen gef¸llten String erzeugen
function  Pass_CreateRandomString    (u32Size:unsigned32;bIncludeNumbers:Boolean;u32Seed:unsigned32=0):LongString;

//Ein "synthetische" Wort erzeugen, das sich aussprechen l‰sst
function  Pass_CreateSyntheticString (u32Size:unsigned32;u32NumbersToAppend:unsigned32=0):LongString;

//Einen "richtigen" Namen erzeugen
function  Pass_CreateNameString      (u32Size:unsigned32;u32NumbersToAppend:unsigned32=0):LongString;

//Aus einem String ein Kennwort ala WinXP erzeugen
function Pass_StringToPass  (sInput:LongString):ShortString;
function Pass_StringToNumber(sInput:LongString):ShortString;

implementation
uses  Class_Random
     ,Class_Checksum
     ;
     
////////////////////////////////////////////////////////////////////////////////
/// Hilfsarrays um schneller arbeiten zu kˆnnen
////////////////////////////////////////////////////////////////////////////////
const
     ALPHASET  :array[0..51] of Char =
               (
                'A','B','C','D','E','F','G','H','I','J',
                'K','L','M','N','O','P','Q','R','S','T',
                'U','V','W','X','Y','Z',
                'a','b','c','d','e','f','g','h','i','j',
                'k','l','m','n','o','p','q','r','s','t',
                'u','v','w','x','y','z'
                );
     NUMBERSET :array[0..9] of Char =
               (
                '0','1','2','3','4','5','6','7','8','9'
               );
     ALPHANUMSET:array[0..61] of Char =
               (
                'A','B','C','D','E','F','G','H','I','J',
                'K','L','M','N','O','P','Q','R','S','T',
                'U','V','W','X','Y','Z',
                'a','b','c','d','e','f','g','h','i','j',
                'k','l','m','n','o','p','q','r','s','t',
                'u','v','w','x','y','z',
                '0','1','2','3','4','5','6','7','8','9'
                );

     KONSONANTEN : Array [0..30] of TinyString =
                   (
                    'b','c','d','f','g',
                    'h','k','l','m','n',
                    'o','p','qu','r','s',
                    't','v','w','x','z',
                    'ck','ph','tz','st','nn',
                    'mm','kl','pt','ch','tt',
                    'nd'
                   );

     VOKALE      : Array [0..9] of TinyString =
                   (
                    'a','e','i','o','u',
                    'y','ie','au','ou','eu'
                   );




////////////////////////////////////////////////////////////////////////////////
///
/// Hashfunktion
/// Erzeugt aus einem String eine Kennzahl.
/// Im Moment wird der Adler32 Algorithmus verwandt
/// Siehe http://www.ietf.org/rfc/rfc1950.txt
///
/// Adler32 hat zwar f¸r kurze Datenstrˆme ein Problem, da der Puffer nicht
/// ¸berl‰uft, aber die Berechnung ist sehr schnell durchf¸hrbar.
/// Um einen Puffer¸berlauf zu provozieren wird der Eingabestring auf (mindestens)
/// 134 Zeichen gepadded
////////////////////////////////////////////////////////////////////////////////
//Aus einem String eine Unsigned32 machen (Adler32)
function  Pass_StringToHash(sInput:LongString):unsigned32;
var
   HASH : TADLER32; 
begin
     HASH:=TADLER32.Create();

     result:=HASH.fromstring(sInput).u32Checksum;

     HASH.Free();
end;

///////////////////////////////////////////////////////////////////////////////
///
/// "Erzeugerfunktionen"
/// Die meisten Verfahren sind trivial f¸hren aber dennoch (bei hinreichender
/// L‰nge) zu sicheren Kennwˆrten.
///
////////////////////////////////////////////////////////////////////////////////
//Einen String mit Zufallszahlen erzeugen (Nicht empfohlen)
function Pass_CreateRandomNumber(u32Size:Unsigned32;u32Seed:unsigned32):LongString;
var RNG : TRandom;
begin
     RNG:=TRandom.Create(u32Seed);

     //Ergenis initialisieren
     result:='';

     //Alle durcharbeiten
     while (u32Size>0) do
           begin
                //Zufallszeichen aus dem Zahlenarray anh‰ngen
                result:=result+NUMBERSET[RNG.Get(Length(NUMBERSET))];
                dec(u32Size);
           end;
     RNG.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String mit Zufallszeichen f¸llen (bei > 8 Zeichen sehr sicher)
function  Pass_CreateRandomString(u32Size:unsigned32;bIncludeNumbers:Boolean;u32Seed:unsigned32):LongString;
var
   RNG : TRandom;
begin
     RNG:=TRandom.Create(u32Seed);
     //Ergebnis initialisieren
     result:='';
     //Alles Durcharbeiten
     while (u32Size>0) do
           begin
                //Zahle mit benutzen ?
                if (bIncludeNumbers) then
                   begin
                        //Zufallszeichen aus dem Array anh‰ngen
                        result:=result+ALPHANUMSET[RNG.Get(Length(ALPHANUMSET))];
                   end
                else
                   begin
                        //Zufallszeichen aus dem Array anh‰ngen
                        result:=result+ALPHASET[RNG.Get(Length(ALPHASET))];
                   end;
                dec(u32Size);
           end;
     RNG.Free();
end;

////////////////////////////////////////////////////////////////////////////////
// Einen "synthetischen" String erzeugen (ausreichend sicher und leicht zu merken
// Indem sich Vokale und Konsonateten stets abwechseln. Dadurch entsteht ein "Wort"
// welches aussprechbar und damit leicht memorierbar ist
// Mit cNumbersToAppend kann gesteuert werden, ob Zufallszahlen angef¸gt werden.
//
// Ein typisches Ergebnis w‰re z.B. dimophuki42
function Pass_CreateSyntheticString(u32Size:unsigned32;u32NumbersToAppend:unsigned32):LongString;
var
   bSwitch : Boolean;
   s32Size : Signed32;
   sAdd    : ShortString;
   RNG     : TRandom;
begin
     RNG:=TRandom.Create();

     result:='';

     //Start zuf‰llig bestimmen
     bSwitch:=Boolean(RNG.GetBit());

     //Grˆﬂe des eigentlichen Strings bestimmen
     s32Size:=u32Size-u32NumbersToAppend;
     while (s32Size>0) do
           begin
                //Immer zwischen Vokal und Konsonant wechseln
                if (bSwitch) then
                   begin
                        //Zufallszeichen aus dem Array anh‰ngen
                        sAdd:=VOKALE[RNG.Get(Length(VOKALE))];
                   end
                else
                   begin
                        //Zufallszeichen aus dem Array anh‰ngen
                        sAdd:=Konsonanten[RNG.Get(Length(VOKALE))];
                   end;
                //Umschalten
                bSwitch:= not bSwitch;

                //gefundenen String anf¸gen
                result:=result+sAdd;

                //Z‰hler je nach L‰nge des Fragmentes anpassen
                dec(s32Size,Length(sAdd));
           end;
     //Ergebnis passend machen
     result:=copy(result,1,u32Size-u32NumbersToAppend);

     //Erstes Zeichen groﬂschreiben
     if (Length(result)>1) then
        begin
             result[1]:=UpCase(result[1]);
        end;

     RNG.Free();

     //Und evtl. Zahlen anf¸gen
     result:=result+Pass_CreateRandomNumber(u32Size-Unsigned32(Length(result)))
end;

////////////////////////////////////////////////////////////////////////////////
// Einen Name erzeugen
// Erzeugt aus vorgegebenen N-Phonen einen realistisch klingenden Namen

// #################################################################################################
//TODO
// #################################################################################################

function Pass_CreateNameString(u32Size:Unsigned32;u32NumbersToAppend:unsigned32):LongString;
begin
     result:=Pass_CreateSyntheticString(u32Size,u32NumbersToAppend);
end;

////////////////////////////////////////////////////////////////////////////////
//Aus einem String ein Kennwort ala WinXP erzeugen
function Pass_StringToPass(sInput:LongString):ShortString;
var
   u32Count : unsigned32;
   RNG      : TRandom;
begin
     result:='';

     //Den Zufallsgenerator mit dem Stringhash initialisieren
     RNG:=TRandom.Create();
     RNG.Seed(sInput);

     //F¸nf Blˆcke machen
     u32Count:=5;
     while (u32Count > 0) do
           begin
                //Eine Erste Sequenz holen
                result:=result+Pass_CreateRandomString(5,TRUE,RNG.GetLongWord())+'-';

                //Runterz‰hlen
                dec (u32Count);
           end;
     //Letzten Strich wieder rausnehmen
     Delete(result,Length(result),1);

     //Alles ohne Unit Sysutils groﬂschreiben
     u32Count:=1;
     while (u32Count <= Length(result)) do
           begin
                Result[u32Count]:=UpCase(Result[u32Count]);
                inc(u32Count);
           end;
     RNG.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Aus einem String ein Kennwort mit Zahlen erzeugen
function Pass_StringToNumber(sInput:LongString):ShortString;
var
   iCount : Integer;
   crc    : TCRC32;
   rnd    : TRandom;
begin
     result:='';

     //Den Zufallsgenerator mit dem Stringhash initialisieren
     crc:=TCRC32.create();
     rnd:=TRandom.Create(crc.fromstring(sInput).u32Checksum);
     crc.free;

     //F¸nf Blˆcke machen
     iCount:=5;
     while (iCount > 0) do
           begin
                //Eine Erste Sequenz holen
                result:=result+Pass_CreateRandomNumber(5,rnd.GetLongWord)+'-';

                //Runterz‰hlen
                dec (iCount);
           end;
     //Letzten Strich wieder rausnehmen
     Delete(result,Length(result),1);

     //Alles ohne Unit Sysutils groﬂschreiben
     iCount:=1;
     while (iCount <= Length(result)) do
           begin
                Result[iCount]:=UpCase(Result[iCount]);
                inc(iCount);
           end;

     rnd.free;
end;

initialization

end.
