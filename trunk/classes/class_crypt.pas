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
        //Einstellungen f�r den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
unit class_crypt;
////////////////////////////////////////////////////////////////////////////////
/// Pseudo-Onetime-Pad (c) 2005 Borg@Sven-of-Nine.de
///
/// ben�tigt die Class_Random
///
/// Implementiert eine PseudoOneTimePad-StreamCipher
///
/// Sensible Daten w�rde ich damit nicht speichern, aber f�r den Normalanwender
/// solltes es reichen, um Manipulationen an Ini-Dateien zu verhindern
///
///
/// Ist das Scramble-Flag gesetzt, so wird ein String in beim Ver / Entschl�sseln
/// um Zufallswerte erweitert. Nach einer Verschl�sselung mit Scramble=TRUE
/// funktioniert die Byteweise entschl�sselung nicht mehr. Der String mu� im
/// ganzen entschl�sselt werden
///
////////////////////////////////////////////////////////////////////////////////
interface
uses Unit_TypeDefs,Class_Checksum,Class_Random;


type TStreamCrypt = class (TObject)
     private
            EncChecksum : TAdler32;
            DecChecksum : TAdler32;
            
            EncRandom   : TRandom;
            DecRandom   : TRandom;

            //Die letzten Bearbeiteten Bytes
            u8DecLast: Byte;
            u8EncLast: Byte;

            //Einen String zuf�llig dehnen
            bRNDPadding : Boolean;
     protected

     public
           constructor create();
           destructor  free();

           //Ver und Entschl�sselung initialisieren
           procedure Init(sPassword:Longstring);
           procedure EncryptionInit(sPassword:Longstring);
           procedure DecryptionInit(sPassword:Longstring);

           //Einen String ver / entschl�sseln
           function Encrypt(sInput:Longstring):Longstring; overload;
           function Decrypt(sInput:Longstring):Longstring; overload;

           function Encrypt(u8Input:Byte):Byte; overload;
           function Decrypt(u8Input:Byte):Byte; overload;

           //Die einzelnen Pr�fsummen lesen
           function EncryptionCheckSum():Longstring;
           function DecryptionCheckSum():Longstring;

           //Einen String mit Zufallswerten strecken
           property scramble : boolean read bRNDPadding write bRNDPadding;
end;

////////////////////////////////////////////////////////////////////////////////
implementation
uses sysutils; //Wegen IntToHex

////////////////////////////////////////////////////////////////////////////////
constructor TStreamCrypt.create();
begin
     //Die einzelnen Klassen klarmachen
     Self.EncChecksum:=TAdler32.Create();
     Self.DecChecksum:=TAdler32.Create();
     Self.EncRandom  :=TRandom.Create();
     Self.DecRandom  :=TRandom.Create();

     //Alles initialisieren
     Self.Init('sven-of-nine');

     Self.Scramble:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////
destructor TStreamCrypt.Free();
begin
     Self.EncChecksum.Free;
     Self.DecChecksum.Free;
     Self.EncRandom.Free;
     Self.DecRandom.Free;
end;

procedure TStreamCrypt.Init(sPassword:Longstring);
begin
     Self.EncryptionInit(sPassword);
     Self.DecryptionInit(sPassword);
end;

////////////////////////////////////////////////////////////////////////////////
// Die Pr�fsummen und Zufallsgeneratoren initialisieren
procedure TStreamCrypt.EncryptionInit(sPassword:Longstring);
begin
     Self.EncChecksum.Init();
     Self.EncRandom.Seed(sPassword);
end;

////////////////////////////////////////////////////////////////////////////////
// Die Pr�fsummen und Zufallsgeneratoren initialisieren
procedure TStreamCrypt.DecryptionInit(sPassword:Longstring);
begin
     Self.DecChecksum.Init();
     Self.DecRandom.Seed(sPassword);
end;

////////////////////////////////////////////////////////////////////////////////
//Den aktuellen Stand der Pr�fsumme auslesen
function  TStreamCrypt.EncryptionChecksum():LongString;
begin
     Result:=Self.EncChecksum.finalize().sChecksum;
end;

function  TStreamCrypt.DecryptionChecksum():LongString;
begin
     Result:=Self.DecChecksum.finalize().sChecksum;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Byte verschl�sseln
//Dazu wird das Byte zuerst per XOR mit dem letzten Byte, da� an die Verschl�sselung
//gesandt wurde verkn�pft. Danach wird eine Zufallszahl angefordert und das aus
//der vorherigen Rechnung enstandene Ergebnis nochmals "gexort".
//Folgende Vorteile :
//Wird in der verschl�sselten Datei manipuliert, sind ab der Manipulationstelle
//alle weiteren Bytes mit hoher Wahrscheinlichkeit nicht mehr richtig zu entschl�sseln.
//Durch die Verkn�pfung mit dem Zufallsgenerator werden statistische Angriffe wirkungslos.
//Nachteile :
//Durch den kleinen Schl�sselbereich (32Bit des RNG) kann relatisch schnell ein
//KnownPlainttext-Angriff durchgef�hrt werden.
//Eine reine BruteForce-Attacke kann zwar relativ schnell entschl�sseln
//(256XOR-M�glichkeiten), aber da jedes Byte (pseudo) zuf�llig verschl�sselt wurde
//kann jeder beliebige Text "herausgelesen" werden.
function TStreamCrypt.Encrypt(u8Input:Byte):Byte;
begin
     //Byte auf die Pr�fsumme draufhauen
     Self.EncChecksum.add(u8Input);

     //Mit letzten Byte verkn�pfen
     result:=u8Input xor Self.u8EncLast;

     //Mit Zufallszahl verkn�pfen
     result:=result xor Self.EncRandom.GetByte();

     //Byte merken
     u8EncLast:=u8Input;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Byte entschl�sseln
//Genau umgekehrt wie verschl�sseln aufgrund der Symetrischen Eigenschaften
//der XOR-Operation
function TStreamCrypt.Decrypt(u8Input:Byte):Byte;
begin
     //Entschl�sseln
     result:=u8Input xor Self.DecRandom.GetByte();

     //Vorg�ngerbyte rausholen
     result:=result xor u8DecLast;

     //Ergebnis f�r n�chsten Durchlauf merken
     u8DecLast:=result;

     //Byte auf die Pr�fsumme draufhauen
     Self.DecChecksum.add(result);
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String verschl�sseln
function  TStreamCrypt.Encrypt(sInput:LongString):LongString;
var
   lwCount : longword;
begin
     lwCount:=1;
     result:='';
     //Jedes Zeichen einzeln verschl�sseln
     while ( lwCount <= LongWord (Length(sInput))) do
           begin
                //Und als Hex an die Ausgabe anf�gen
                result:=result + IntToHex( Self.Encrypt ( Ord ( sInput[lwCount] ) ) ,2);

                //Scramble ?
                if (Self.Scramble) then
                   begin
                        //Dann einfach zuf�llig ein Zeichen anh�ngen
                        if (Self.EncRandom.GetBit <> 0) then
                           begin
                                result:=result + IntToHex (Self.EncRandom.GetByte(),2);
                           end;
                   end;

                inc(lwCount);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String entschl�sseln
function  TStreamCrypt.Decrypt(sInput:LongString):LongString;
var
   lwCount : Longword;
   sPair   : LongString;
   bPair   : Byte;
begin
     result:='';
     //Zur entschl�sselung m�ssen wir den String immer in P�rchen zerlegen
     lwCount:=1;
     while (lwCount <= LongWord(Length(sInput))) do
           begin
                //Hex holen
                sPair:=Copy(sInput,lwCount,2);
                bPair:=Byte ( StrToIntDef('$'+sPair,0) );

                //Und dekodieren
                result:=result+Chr( Self.Decrypt(bPair) );

                //Scramble ?
                if (Self.Scramble) then
                   begin
                        //Dann einfach zuf�llig ein Zeichen �berspringen
                        if (Self.DecRandom.GetBit <> 0) then
                           begin
                                //Um den Zufall Synchron zu halten, holen
                                //wir uns hier noch ein Byte
                                Self.DecRandom.GetByte();
                                inc(lwCount,2);
                           end;
                   end;

                //N�chstes p�rchen
                inc(lwCount,2);
           end;
end;

end.
