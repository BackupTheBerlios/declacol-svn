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
        //Einstellungen für den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
unit class_crypt;
////////////////////////////////////////////////////////////////////////////////
/// Pseudo-Onetime-Pad (c) 2005 Borg@Sven-of-Nine.de
///
/// benötigt die Class_Random
///
/// Implementiert eine PseudoOneTimePad-StreamCipher
///
/// Sensible Daten würde ich damit nicht speichern, aber für den Normalanwender
/// solltes es reichen, um Manipulationen an Ini-Dateien zu verhindern
///
///
/// Ist das Scramble-Flag gesetzt, so wird ein String in beim Ver / Entschlüsseln
/// um Zufallswerte erweitert. Nach einer Verschlüsselung mit Scramble=TRUE
/// funktioniert die Byteweise entschlüsselung nicht mehr. Der String muß im
/// ganzen entschlüsselt werden
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

            //Einen String zufällig dehnen
            bRNDPadding : Boolean;
     protected

     public
           constructor create();
           destructor  free();

           //Ver und Entschlüsselung initialisieren
           procedure Init(sPassword:Longstring);
           procedure EncryptionInit(sPassword:Longstring);
           procedure DecryptionInit(sPassword:Longstring);

           //Einen String ver / entschlüsseln
           function Encrypt(sInput:Longstring):Longstring; overload;
           function Decrypt(sInput:Longstring):Longstring; overload;

           function Encrypt(u8Input:Byte):Byte; overload;
           function Decrypt(u8Input:Byte):Byte; overload;

           //Die einzelnen Prüfsummen lesen
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
// Die Prüfsummen und Zufallsgeneratoren initialisieren
procedure TStreamCrypt.EncryptionInit(sPassword:Longstring);
begin
     Self.EncChecksum.Init();
     Self.EncRandom.Seed(sPassword);
end;

////////////////////////////////////////////////////////////////////////////////
// Die Prüfsummen und Zufallsgeneratoren initialisieren
procedure TStreamCrypt.DecryptionInit(sPassword:Longstring);
begin
     Self.DecChecksum.Init();
     Self.DecRandom.Seed(sPassword);
end;

////////////////////////////////////////////////////////////////////////////////
//Den aktuellen Stand der Prüfsumme auslesen
function  TStreamCrypt.EncryptionChecksum():LongString;
begin
     Result:=Self.EncChecksum.finalize().sChecksum;
end;

function  TStreamCrypt.DecryptionChecksum():LongString;
begin
     Result:=Self.DecChecksum.finalize().sChecksum;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Byte verschlüsseln
//Dazu wird das Byte zuerst per XOR mit dem letzten Byte, daß an die Verschlüsselung
//gesandt wurde verknüpft. Danach wird eine Zufallszahl angefordert und das aus
//der vorherigen Rechnung enstandene Ergebnis nochmals "gexort".
//Folgende Vorteile :
//Wird in der verschlüsselten Datei manipuliert, sind ab der Manipulationstelle
//alle weiteren Bytes mit hoher Wahrscheinlichkeit nicht mehr richtig zu entschlüsseln.
//Durch die Verknüpfung mit dem Zufallsgenerator werden statistische Angriffe wirkungslos.
//Nachteile :
//Durch den kleinen Schlüsselbereich (32Bit des RNG) kann relatisch schnell ein
//KnownPlainttext-Angriff durchgeführt werden.
//Eine reine BruteForce-Attacke kann zwar relativ schnell entschlüsseln
//(256XOR-Möglichkeiten), aber da jedes Byte (pseudo) zufällig verschlüsselt wurde
//kann jeder beliebige Text "herausgelesen" werden.
function TStreamCrypt.Encrypt(u8Input:Byte):Byte;
begin
     //Byte auf die Prüfsumme draufhauen
     Self.EncChecksum.add(u8Input);

     //Mit letzten Byte verknüpfen
     result:=u8Input xor Self.u8EncLast;

     //Mit Zufallszahl verknüpfen
     result:=result xor Self.EncRandom.GetByte();

     //Byte merken
     u8EncLast:=u8Input;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Byte entschlüsseln
//Genau umgekehrt wie verschlüsseln aufgrund der Symetrischen Eigenschaften
//der XOR-Operation
function TStreamCrypt.Decrypt(u8Input:Byte):Byte;
begin
     //Entschlüsseln
     result:=u8Input xor Self.DecRandom.GetByte();

     //Vorgängerbyte rausholen
     result:=result xor u8DecLast;

     //Ergebnis für nächsten Durchlauf merken
     u8DecLast:=result;

     //Byte auf die Prüfsumme draufhauen
     Self.DecChecksum.add(result);
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String verschlüsseln
function  TStreamCrypt.Encrypt(sInput:LongString):LongString;
var
   lwCount : longword;
begin
     lwCount:=1;
     result:='';
     //Jedes Zeichen einzeln verschlüsseln
     while ( lwCount <= LongWord (Length(sInput))) do
           begin
                //Und als Hex an die Ausgabe anfügen
                result:=result + IntToHex( Self.Encrypt ( Ord ( sInput[lwCount] ) ) ,2);

                //Scramble ?
                if (Self.Scramble) then
                   begin
                        //Dann einfach zufällig ein Zeichen anhängen
                        if (Self.EncRandom.GetBit <> 0) then
                           begin
                                result:=result + IntToHex (Self.EncRandom.GetByte(),2);
                           end;
                   end;

                inc(lwCount);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String entschlüsseln
function  TStreamCrypt.Decrypt(sInput:LongString):LongString;
var
   lwCount : Longword;
   sPair   : LongString;
   bPair   : Byte;
begin
     result:='';
     //Zur entschlüsselung müssen wir den String immer in Pärchen zerlegen
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
                        //Dann einfach zufällig ein Zeichen überspringen
                        if (Self.DecRandom.GetBit <> 0) then
                           begin
                                //Um den Zufall Synchron zu halten, holen
                                //wir uns hier noch ein Byte
                                Self.DecRandom.GetByte();
                                inc(lwCount,2);
                           end;
                   end;

                //Nächstes pärchen
                inc(lwCount,2);
           end;
end;

end.
