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
unit class_ini;
////////////////////////////////////////////////////////////////////////////////
///
/// Borg@Sven-of-Nine.de
///
/// Ini-Unit mit (fast) beliebig vielen und langen Einträgen
///
/// Features :
/// - Portabler Code der auch unter Lazarus problemlos kompiliert
/// - Verwaltung der Ini-EInträge im Speicher (dadurch sehr schnell)
/// - Folgende Datentypen können direkt gespeichert werden :
///   >FloatingPoint
///   >Byte / Integer / Unsigned
///   >Boolean
///   >Date/Time
///   >String
/// - Möglichkeit Einträge gesammelt auszulesen (alle sektionen / keys values)
/// - Import aus String (z.B. aus Download)
/// - Verschlüsselung der Datei (nicht stark aber immerhin)
/// - Methode SaveToFile
////////////////////////////////////////////////////////////////////////////////

interface

uses //Variants,        //Variantfunktionen (Wenn notwendig)
     Classes,         //TStringList
     Unit_TypeDefs,   //Typendefinitionen
     class_crypt;      //Die Stromverschlüsselung


////////////////////////////////////////////////////////////////////////////////
/// Unsere Eigen Basisklassen
////////////////////////////////////////////////////////////////////////////////
type TIni = class(TObject)
     protected
              //Name der aktuell offenen Datei
              sLastFilename : LongString;
              //Hilfsvariable
              lwIndex    : LongWord;

              //Datei beim schreiben/lesen verschlüsseln ?
              bEncrypt   : Boolean;
              sEncrypt   : Longstring;
              StreamCrypt: TStreamCrypt;

              //Flag ob wir eine Ini haben
              bLoaded    : Boolean;
     private
              //Verschlüsselung
              procedure InitEncryption();
              function Encode(sInput :LongString):LongString;
              function Decode(sInput :LongString):LongString;

              //Die Ini kpl löschen
              procedure DelIni();

              //Suche im Baum
              function FindSection (sSection:ShortString)                : Signed32;
              function FindKey     (sSection,sKey:ShortString)           : TPoint;

              //Fügt eine Section hinzu, wenn sie nicht existiert
              function AddSection(sSection:ShortString):Integer;

              //Löscht eine Section

              //Fügt hinzu oder überschreibt ein Datum
              function AddKey      (sSection,sKey:ShortString;Value:Longstring):TPoint;

     public
            //Dateifunktionen
              function LoadFromFile(sFilename:LongString='') :Boolean;
              function ReloadFromFile()               :Boolean;
              function SaveToFile  (sFilename:LongString='') :Boolean;

              function Import(sIni:LongString):Boolean;

              //Zugriffsfunktionen (da Variant nicht umfangreich genug ist alles mit overload)
              function  Read  (sSection,sKey : ShortString; DefaultValue : Longstring;var Value : Longstring)  :Boolean; overload;
              procedure Write (sSection,sKey : ShortString; Value        : LongString);                                  overload;

              function  Read  (sSection,sKey : ShortString; DefaultValue : Unsigned32;var Value : Unsigned32)  :Boolean; overload;
              procedure Write (sSection,sKey : ShortString; Value        : Unsigned32);                                  overload;

              function  Read  (sSection,sKey : ShortString; DefaultValue : ShortFloat;var Value : ShortFloat)  :Boolean; overload;
              procedure Write (sSection,sKey : ShortString; Value        : ShortFloat);                                  overload;

              function  Read  (sSection,sKey : ShortString; DefaultValue : LongFloat;var Value : LongFloat)  :Boolean; overload;
              procedure Write (sSection,sKey : ShortString; Value        : LongFloat);                                  overload;

              function  Read  (sSection,sKey : ShortString; DefaultValue : Signed32;var Value : Signed32)  :Boolean; overload;
              procedure Write (sSection,sKey : ShortString; Value        : Signed32);                                  overload;

              function  Read  (sSection,sKey : ShortString; DefaultValue : Boolean;var Value : Boolean)  :Boolean; overload;
              procedure Write (sSection,sKey : ShortString; Value        : Boolean);                                  overload;

              function  Read  (sSection,sKey : ShortString; DefaultValue : TDateTime;var Value : TDateTime)  :Boolean; overload;
              procedure Write (sSection,sKey : ShortString; Value        : TDateTime);                                  overload;

              procedure Clear ();

              //Listen erzeugen
              //Wenn der Wert nicht String ist, dann wird er entsprechend gewandelt
              function  ReadSections (                      var slSections : TStringList ) : Boolean;
              function  ReadKeys     (sSection:ShortString; var slKeys     : TStringList ) : Boolean;
              function  ReadValues   (sSection:ShortString; var slValues   : TStringList ) : Boolean;

              //Löschen
              procedure DelSection(sSection:ShortString);           overload;
              procedure DelSection(lwSection:longword);             overload;
              procedure DelKey    (sSection,sKey :ShortString);     overload;
              procedure DelKey    (lwSection,lwKey :LongWord);      overload;

              //Statusfunktionen
              function  IniSize         ()                           :Unsigned32;
              function  SectionSize     (sSection  : ShortString)    :Unsigned32; overload;
              function  SectionSize     (u32Section: unsigned32)     :Unsigned32; overload;
              function  SectionExists   (sSection  : ShortString)    :Boolean;
              function  KeyExists       (sSection,sKey:Shortstring)  :Boolean;

              //Properties
              property  Encrypt  : Boolean     read bEncrypt   write bEncrypt   default FALSE;
              property  Password : LongString  read sEncrypt   write sEncrypt;

              //Konstruktor
              constructor Create();
end;


implementation
uses Sysutils,          //Diverse Stringfunktionen
     Unit_MultiPlatform, //Für MultiPlatform_CreateConfigPath()
     Unit_Strings,       //Wegen StringToBool
     Unit_Time           //Wegen UnixTimStamp
     ;
////////////////////////////////////////////////////////////////////////////////
/// Typdefinitionen
////////////////////////////////////////////////////////////////////////////////
//Struktur eines Ini-Eintrages
type TKeyStruct = record
     sKey      : ShortString;
     sValue    : LongString;
end;
type PKeyStruct = ^TKeyStruct;

//Struktur eines Sektioneintrages
type TSectionStruct = record
     sSection  : ShortString;
     aKeys     : array of PKeyStruct;
end;
Type PSectionStruct = ^TSectionStruct;
////////////////////////////////////////////////////////////////////////////////
/// Variablendefinitionen
////////////////////////////////////////////////////////////////////////////////
var
   aIniTable       : array of PSectionStruct;
   pNewKey         : PKeyStruct;
   pNewSection     : PSectionStruct;

////////////////////////////////////////////////////////////////////////////////
/// Konstruktor / Destruktor
////////////////////////////////////////////////////////////////////////////////
//Konstruktor
constructor TIni.Create();
begin
     //Loaded Initialisieren
     Self.bLoaded  :=FALSE;

     Self.StreamCrypt:=TStreamCrypt.Create();
end;

////////////////////////////////////////////////////////////////////////////////
// Verschlüsselung
// alle Lese und Schreibströme werden hier durchgeleitet
////////////////////////////////////////////////////////////////////////////////
procedure TIni.InitEncryption();
begin
     //Den Zufallsgenerator seeden
     Self.StreamCrypt.Init(sEncrypt);
     Self.StreamCrypt.scramble:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
function  TIni.Encode(sInput :LongString):LongString;
begin
     //Verschlüsselung angefragt ?
     if (bEncrypt) then
        begin
             //Dann Stromcypher ansetzen
             result:=Self.StreamCrypt.Encrypt(sInput);
        end
     else
        begin
             result:=sInput;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function  TIni.Decode(sInput:LongString):LongString;
begin
     //Verschlüsselung angefragt ?
     if (bEncrypt) then
        begin
             //Dann Stromcypher ansetzen
             result:=Self.StreamCrypt.Decrypt(sInput);
        end
     else
        begin
             result:=sInput;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Suchfunktionen
////////////////////////////////////////////////////////////////////////////////
//Den Index einer Sektion holen
function TIni.FindSection(sSection:ShortString):Signed32;
var
   s32Index : Signed32;
begin
     //Fehler annehmen
     result :=-1;
     
     //Fehler abfangen
     if (sSection='') then Exit;

     //Nur in Großschrift arbeiten
     sSection:=UpperCase(sSection);

     //Und von hinten nach vorne alles durchgehen
     s32Index:=signed32(Self.IniSize())-1;
     while (s32Index >= 0) do
           begin
                //Erstes Zeichen identisch ?
                if (sSection[1] = aIniTable[s32Index]^.sSection[1]) then
                   begin
                        //Rest identisch ?
                        if (sSection = aIniTable[s32Index]^.sSection) then
                           begin
                                //Position merken
                                result:=s32Index;

                                //Und aus der Schleife springen
                                break;
                           end;
                   end;
                //Nächster Eintrag
                dec(s32Index);
           end;
end;
////////////////////////////////////////////////////////////////////////////////
//Den Index eines Keys holen
function TIni.FindKey(sSection,sKey:ShortString):TPoint;
var
   s32Index : Signed32;
begin
     //Fehler anehmen
     Result.x:=-1;
     Result.y:=-1;

     //Sektion suchen
     Result.x:=Self.FindSection(sSection);

     //Nichts gefunden
     if (Result.x < 0) then
        begin
             //Abbruch
             Exit;
        end;

     //Immer in Großschrift arbeiten
     sKey:=Uppercase(sKey);


     //Anzahl der Keys holen
     s32Index:=signed32 ( Self.SectionSize( LongWord ( Result.X ) ) ) -1;

     //Und von hinten nach vorne alles durchgehen
     while (s32Index >= 0) do
           begin
                //Erstes Zeichen identisch ?
                if (sKey[1] = aIniTable[Result.X]^.aKeys[s32Index]^.sKey[1]) then
                   begin
                        //Rest identisch ?
                        if (sKey = aIniTable[Result.X]^.aKeys[s32Index]^.sKey) then
                           begin
                                //Position merken
                                result.y:=s32Index;

                                //Und aus der Schleife springen
                                break;
                           end;
                   end;
                //Nächster Eintrag
                dec(s32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Inifunktionen
////////////////////////////////////////////////////////////////////////////////
procedure TIni.DelIni();
var
   lwIniSize : LongWord;
   lwSecSize : Longword;
begin
     //Jede Sektion löschen
     while ( Self.IniSize > 0) do
           begin
                lwIniSize:=Self.IniSize-1;
                //Jeden Key löschen
                while ( Self.SectionSize(lwIniSize) > 0) do
                      begin
                           lwSecSize:=Self.SectionSize(lwIniSize)-1;

                           //Und löschen
                           Self.DelKey (lwIniSize,lwSecSize);
                      end;

                //Alles Keys raus, dann Sektion löschen
                Self.DelSection(lwIniSize);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Sektionfunktionen
////////////////////////////////////////////////////////////////////////////////
//Eine Sektion zufügen
function TIni.AddSection(sSection:ShortString):Integer;
begin
     //Sektion suchen
     sSection:=Trim(sSection);
     result:=FindSection(sSection);

     //Existiert ?
     if (result>=0) then
        begin
             //Den Index zurückliefern
             exit;
        end;

     //Existiert nicht, dann neu erzeugen
     //Aktuelle größe lesen
     result:=Self.IniSize;

     //Neu Dimensionieren
     SetLength(aIniTable,result+1);

     //Grundwerte initialisieren
     new(pNewSection);
     pNewSection^.sSection:=UpperCase(sSection);

     //Und Section initialisieren
     aIniTable[result]:=pNewSection;
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Sektion löschen
procedure TIni.DelSection(lwSection:LongWord);
var
   lwCount : LongWord;
begin
     //Fehler abfangen
     if (lwSection >= Self.IniSize()) then
        begin
             exit;
        end;

     //Existiert => dann löschen
     //Im zeichen der Kompatibilität hier ohne TList oder dergleichen


     //Alle Keys in dieser Section löschen
     while (Self.SectionSize(lwSection) > 0 ) do
           begin
                Self.DelKey(lwSection,0);
           end;

     //Eintrag löschen
     Dispose(aIniTable[lwSection]);

     //Alle Sektionen eins runterkopieren
     lwCount:=lwSection+1;
     while (lwCount < Self.IniSize) do
           begin
                //Einfach die Zeiger kopieren
                aIniTable[lwCount-1]:=aIniTable[lwCount];
                inc(lwCount);
           end;

     //Array kleinermachen
     SetLength(aIniTable,Self.IniSize-1);
end;


procedure TIni.DelSection(sSection:ShortString);
begin
     //Suchen und Vernichten
     Self.DelSection(FindSection(sSection));
end;

////////////////////////////////////////////////////////////////////////////////
/// Keyfunktionen
////////////////////////////////////////////////////////////////////////////////
//Fügt hinzu oder überschreibt ein Datum
function TIni.AddKey (sSection,sKey:ShortString;Value:Longstring) :TPoint;
begin
     //Positionen Trimmen
     sSection:=Trim(sSection);
     sKey    :=Trim(sKey);
     
     //Fehler annehmen
     result.x:=-1;
     result.y:=-1;

     //Leer ?
     if (sSection='') or (sKey='') then Exit;

     //Sektion und Key immer in Großschrift
     sSection:=UpperCase(sSection);
     sKey    :=UpperCase(sKey);


     //Prüfen, ob es Section und Key schon gibt
     result:=FindKey(sSection,sKey);

     //Section existiert nicht
     if (result.x < 0) then
        begin
             //Neue Sektion erzeugen
             result.x:=Self.AddSection(sSection);
        end;

     //Key existiert nicht
     if (result.y < 0) then
        begin
             //Key-Array-Größe holen (Ist auch unser neuer Index)
             result.y:=Self.SectionSize( LongWord (result.x) );

             //Erweitern
             SetLength(aIniTable[result.x]^.aKeys,Result.y+1);

             //Neuen Key erzeugen
             New(pNewKey);

             //Und im Array ablegen
             aIniTable[result.x]^.aKeys[result.y]:=pNewKey;
        end;

     //Und Werte speichern
     aIniTable[result.x]^.aKeys[result.y]^.sKey   :=sKey;
     aIniTable[result.x]^.aKeys[result.y]^.sValue :=Value;

end;

////////////////////////////////////////////////////////////////////////////////
//Entfernt ein Datum
procedure TIni.DelKey     (lwSection,lwKey :LongWord);
var
   lwCount : LongWord;
begin
     //Fehler abfangen
     if ( lwSection >= Self.IniSize) then
        begin
             Exit;
        end;
     if ( lwKey     >= Self.SectionSize(lwSection)) then
        begin
             exit;
        end;

     //Eintrag löschen
     Dispose(aIniTable[lwSection]^.aKeys[lwKey]);

     //Alle Zeiger eins runterkopieren
     lwCount:=lwKey+1;
     while (lwCount < Self.SectionSize(lwSection) ) do
           begin
                //Einfach runterkopieren
                aIniTable[lwSection]^.aKeys[lwCount-1]:=aIniTable[lwSection]^.aKeys[lwCount];
                inc(lwCount);
           end;
     //Neue Größe setzen
     SetLength(aIniTable[lwSection]^.aKeys,Self.SectionSize(lwSection)-1);
end;

procedure TIni.DelKey      (sSection,sKey:ShortString);
var
   rPos : TPoint;
begin
     //Find liefert eine Point-Struktur zurück,
     //wober x die Sektion addressier und y den Key
     rPos:=Self.FindKey(sSection,sKey);

     //Fehler abfangen
     if (rPos.x <0) then Exit;
     if (rPos.y <0) then Exit;

     Self.DelKey(rPos.x,rPos.y);
end;

////////////////////////////////////////////////////////////////////////////////
/// Dateifunktionen
////////////////////////////////////////////////////////////////////////////////
//Eine Ini-Datei öffnen und dann parsen
function TIni.LoadFromFile(sFilename:LongString=''):Boolean;
var
   fIn   : TextFile;
   sIn   : LongString;
   sIni  : LongString;
begin
     //Fehler annehmen
     result:=FALSE;

     //Wenn kein Dateiname übergeben wurde, diesen automatisch erzeugen
     if (sFilename='') then
        begin
             sFilename:=Multiplatform_CreateConfigFilename();
        end;


     //Datei existiert ?
     if (not FileExists(sFilename) ) then
        begin
             exit;
        end;

     //Letzten Dateinamen merken
     sLastFilename:=sFilename;

     //Alles löschen
     Self.DelIni;
     sIni:='';

     //Ini als String laden
     //Und dann über die interne Import-Funktion einlesen

     //Die Entschlüsselung initialisieren
     Self.InitEncryption();

     //Datei in einen String einlesen
     try
//fIn muß hier nicht initialisiert werden, da Assign dies übernimmt

        System.Assign(fIn,sFilename);
        System.Reset(fIn);
        while (not EOF(fIn)) do
              begin
                   //Jede Zeilen lesen
                   System.ReadLn(fIn,sIn);

                   //Und anhängen
                   sIni:=sIni + Self.Decode(sIn)+#13+#10;
              end;
        System.Close(fIn);
        result:=TRUE;
     except
     end;

     //Importieren
     Self.Import(sIni);
     sIn:='';
     sIni:='';
end;

function TIni.ReloadFromFile():Boolean;
begin
     //Letzten Ladenamen benutzen
     result:=Self.LoadFromFile(sLastFilename);
end;

////////////////////////////////////////////////////////////////////////////////
//Die Ini-Datei in die Datei speichern
function TIni.SaveToFile(sFilename:LongString=''):Boolean;
var
   fOut      : TextFile;
   sOut      : LongString;
   lwSection : LongWord;
   lwKey     : LongWord;
begin
     //Denn Dateinamen müssen wir nicht checken,
     //da Assign bei jedem Fehler eine Ausnamhe macht

     //Wenn kein Dateiname übergeben wurde, diesen automatisch erzeugen
     if (sFilename='') then
        begin
             sFilename:=Multiplatform_CreateConfigFilename();
        end;

     //Als Ergebnis Erfolg annehmen
     result:=TRUE;
     try
        //Alte Datei löschen
        DeleteFile(sFilename);



        //Kanal öffnen
        System.Assign(fOut,sFilename);

        //Zurücksetzen
        System.ReWrite(fOut);

        //Die Verschlüsselung initialisieren
        Self.InitEncryption();

        //Und alles schreiben
        lwSection:=0;
        while (lwSection < Self.IniSize) do
              begin
                   //Sektionsname schreiben
                   sOut:='['+aIniTable[lwSection]^.sSection+']';
                   System.WriteLn(fOut,Self.Encode(sOut));

                   //Und alle schreiben
                   lwKey:=0;
                   while (lwKey < Self.SectionSize(lwSection)) do
                         begin
                              //Key=Value bauen
                              sOut :=aIniTable[lwSection]^.aKeys[lwKey]^.sKey;
                              sOut :=sOut + '='+ aIniTable[lwSection]^.aKeys[lwKey]^.sValue;
                              //Und schreiben
                              System.Writeln(fOut,Self.Encode(sOut));
                              inc(lwKey);
                         end;
                   //Leerzeilen
                   System.WriteLn(fOut,Self.Encode(''));
                   inc(lwSection);
              end;

        //Schließen
        System.Close(fOut);
     except
           //Bei Exception Fehler melden
           result:=FALSE;
     end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String in seine Inbestandteile zerlegen
function TIni.Import(sIni:LongString):Boolean;

//Interne Hilfsfunktion
function SplitLine(sInput:LongString;var sValue:LongString;var sKey:ShortString):Boolean;
var
   u32Pos : Unsigned32;
begin
     result:=FALSE;
     sValue :='';
     sKey   :='';

     //Die Eingabe am Gleichheitszeichen spalten
     u32Pos:=Pos('=',sInput);
     
     //Nix gefunden, dann Ende
     if (u32Pos=0) then
        begin
             Exit;
        end;

     //Und werte rausziehen
     sValue:=LongString (Copy(sInput,u32Pos+1,Length(sInput)));
     sKey  :=ShortString(Copy(sInput,       1,      u32Pos-1));

     //Erfolg
     result:=TRUE;
end;

var
   sSection : LongString;
   sLine    : LongString;
   pIni     : PChar;
   pEnd     : PChar;
   lwStop   : LongWord;
   sVal     : LongString;
   sKey     : ShortString;
begin
     //Fehler annehmen
     result:=FALSE;

     //Quatsch rauswerfen
     sIni:=Trim(sIni);
     //Nix drin ?
     if (sIni='') then Exit;


     //Zeiger machen
     pIni:=Addr(sIni[1]);

     //Sektionsname vorgeben
     sSection:='default';

     //Variablen initialisieren
     sVal:='';
     sKey:='';

     //Und nun gehen wir den ganzen String durch
     repeat
           //Ende der Zeile suchen
           pEnd:=StrPos(pIni,#10);

           //Kein Umbruch mehr
           if (pEnd=nil) then
              begin
                   //Dann den Rest nehmen
                   sLine:=LongString(pIni);
              end
           else
              begin
                   //Zeile auskopieren
                   sLine:=Trim(Copy(LongString(pIni),1,pEnd-pIni));
              end;

           //Was in der Zeile drin ?
           if (sLine<>'') then
              begin
                   //Anfänge auswerten
                   case (sLine[1]) of
                        //Ein Kommentar => Ignorieren
                        ';' : ;
                        //Eine Sektion => neue Sektion erzeugen
                        '[' :  begin
                                    //Ende holen  (ohne die Klammer)
                                    lwStop:=Pos(']',sLine)-2;
                                    //Und Sektionsname setzen
                                    if (lwStop=0) then lwStop:=Length(sLine)-1;
                                    sSection:=Copy(sLine,2,lwStop);
                               end;
                   else
                       //Kein Kommentar und Keine Section ?
                       //Dann kann es nur noch Humbug oder ein Key sein
                       if (SplitLine(sLine,sVal,sKey)) then
                          begin
                               //Was gefunden, dann zufügen
                               Self.AddKey(sSection,sKey,sVal);

                          end;
                   end;
              end;


           pIni:=pEnd+1;
     //Bis nix mehr kommt
     until (pEnd=nil);
end;


////////////////////////////////////////////////////////////////////////////////
/// Zugriffsfunktionen
////////////////////////////////////////////////////////////////////////////////
//Einen Wert aus der Ini lesen
function  TIni.Read (sSection,sKey:ShortString;DefaultValue:Longstring;var Value:Longstring):Boolean;
var
   rPos : TPoint;
begin
     //Rückgabe initialisieren
     Value  := DefaultValue;
     result := FALSE;

     //Suchen
     rPos:=Self.FindKey(sSection,sKey);

     //Was gefunden ?
     if (rPos.x < 0) then Exit;
     if (rPos.y < 0) then Exit;


     //Ergebnis holen
     Value  := aIniTable[rPos.x]^.aKeys[rPos.y]^.sValue;
     result := TRUE;
end;

//Zum lesen anderen Datentype als String diese aus String konvertieren
function  TIni.Read  (sSection,sKey : ShortString; DefaultValue : Unsigned32;var Value : Unsigned32)  :Boolean;
var
   sValue : LongString;
begin
     sValue:='?';
     //Eintrag als String lesen
     result:=Self.Read(sSection,sKey,sValue,sValue);

     //Und konvertieren
     Value:=StrToIntDef(sValue,DefaultValue);
end;

//Zum lesen anderen Datentype als String diese aus String konvertieren
function  TIni.Read  (sSection,sKey : ShortString; DefaultValue : Signed32;var Value : Signed32)  :Boolean;
var
   sValue : LongString;
begin
     sValue:='?';
     //Eintrag als String lesen
     result:=Self.Read(sSection,sKey,sValue,sValue);

     //Und konvertieren
     Value:=StrToIntDef(sValue,DefaultValue);
end;

//Zum lesen anderen Datentype als String diese aus String konvertieren

function  TIni.Read  (sSection,sKey : ShortString; DefaultValue : ShortFloat;var Value : ShortFloat)  :Boolean;
var
   sValue : LongString;
begin
     sValue:='?';
     //Eintrag als String lesen
     result:=Self.Read(sSection,sKey,sValue,sValue);

     //Und konvertieren
     //Bißchen umständlich, aber Delphikompatibel
     //Da StrToFloat Extended zurückgibt, wir aber mit Shortfloat
     //arbeiten kann kein Informationsverlust auftreten
     try
        Value:=StrToFloat(sValue);
     except
        Value:=DefaultValue
     end;
end;

//Zum lesen anderen Datentype als String diese aus String konvertieren
function  TIni.Read  (sSection,sKey : ShortString; DefaultValue : LongFloat;var Value : LongFloat)  :Boolean;
var
   sValue : LongString;
begin
     sValue:='?';
     //Eintrag als String lesen
     result:=Self.Read(sSection,sKey,sValue,sValue);

     //Und konvertieren
     //Bißchen umständlich, aber Delphikompatibel
     //Da StrToFloat Extended zurückgibt, wir aber mit Longfloat
     //arbeiten kann kein Informationsverlust auftreten
     try
        Value:=StrToFloat(sValue);
     except
        Value:=DefaultValue
     end;
end;

//Zum lesen anderen Datentype als String diese aus String konvertieren
function  TIni.Read  (sSection,sKey : ShortString; DefaultValue : Boolean;var Value : Boolean)  :Boolean;
var
   sValue : LongString;
begin
     sValue:='?';
     //Eintrag als String lesen
     result:=Self.Read(sSection,sKey,sValue,sValue);

     //Und konvertieren
     if (result) then
        begin
             Value:=String_StringToBoolean(sValue);
        end
     else
        begin
             Value:=DefaultValue;
        end;
end;

//Zum lesen anderen Datentype als String diese aus String konvertieren
function  TIni.Read  (sSection,sKey : ShortString; DefaultValue : TDateTime;var Value : TDateTime)  :Boolean;
var
   sValue : LongString;
begin
     sValue:='?';
     //Eintrag als String lesen
     result:=Self.Read(sSection,sKey,sValue,sValue);

     //Und konvertieren
     if (result) then
        begin
             Value:=Time_UnixTimestampToDateTime(StrToIntDef(sValue,Time_DateTimeToUnixTimestamp(DefaultValue)))
        end
     else
        begin
             Value:=DefaultValue;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Wert aus die Ini lesen
procedure TIni.Write(sSection,sKey:ShortString;Value:Longstring);
begin
     //Einfach AddKey aufrufen, da diese Funktion genau das tut, was wir wollen
     Self.AddKey(sSection,sKey,Value);
end;

//Zum schreiben anderen Typen als String diese in String konvertieren
procedure TIni.Write (sSection,sKey : ShortString; Value        : Unsigned32);
begin
     Self.AddKey(sSection,sKey,IntToStr(Value));
end;

//Zum schreiben anderen Typen als String diese in String konvertieren
procedure TIni.Write (sSection,sKey : ShortString; Value        : Signed32);
begin
     Self.AddKey(sSection,sKey,IntToStr(Value));
end;

//Zum schreiben anderen Typen als String diese in String konvertieren
procedure TIni.Write (sSection,sKey : ShortString; Value        : ShortFloat);
begin
     Self.AddKey(sSection,sKey,FloatToStr(Value));
end;

//Zum schreiben anderen Typen als String diese in String konvertieren
procedure TIni.Write (sSection,sKey : ShortString; Value        : LongFloat);
begin
     Self.AddKey(sSection,sKey,FloatToStr(Value));
end;

//Zum schreiben anderen Typen als String diese in String konvertieren
procedure TIni.Write (sSection,sKey : ShortString; Value        : Boolean);
begin
     Self.AddKey(sSection,sKey,String_BooleanToString(Value));
end;

//Zum schreiben anderen Typen als String diese in String konvertieren
procedure TIni.Write (sSection,sKey : ShortString; Value        : TDateTime);
begin
     Self.AddKey(sSection,sKey,IntToStr(Time_DateTimeToUnixTimeStamp(Value)));
end;


////////////////////////////////////////////////////////////////////////////////
//Ini löschen
procedure TIni.Clear ();
begin
     //Interne Funktion aufrufen
     Self.DelIni();
end;


////////////////////////////////////////////////////////////////////////////////
//Alle Sektionen lesen
function  TIni.ReadSections (var slSections : TStringList ) : Boolean;
var
   lwCount : LongWord;
begin
     //Ergebnis löschen
     slSections.Clear;

     //Einfach alle Sektionen ausgeben
     lwCount:=0;
     while (lwCount < Self.IniSize) do
           begin
                //Name der Liste zufügen
                slSections.Add(aIniTable[lwCount]^.sSection);
                inc(lwCount);
           end;

     //Fertig;
     result:=slSections.Count>0;
end;

////////////////////////////////////////////////////////////////////////////////
//Alle Keys einer Sektion lesen
function  TIni.ReadKeys (sSection:ShortString;var slKeys : TStringList ) : Boolean;
var
   rPos    : TPoint;
begin
     //Ergebnis löschen
     slKeys.Clear;
     result:=FALSE;

     //Sektion suchen
     rPos.x:=Self.FindSection(sSection);

     //Gefunden ?
     if (rPos.x < 0) then Exit;

     //Alles durchgeben
     with (aIniTable[rPos.x]^) do
          begin
               //Alle Keys durchgehen
               rPos.y:=0;
               while (rPos.y < integer( Self.SectionSize( LongWord ( rPos.x ) ) ) ) do
                   begin
                        //Und die Namen aufaddieren
                        slKeys.Add(aKeys[rPos.y]^.sKey);


                        inc(rPos.y);
                   end;
          end;
end;

////////////////////////////////////////////////////////////////////////////////
//Alle Values einer Sektion lesen
function  TIni.ReadValues (sSection:ShortString;var slValues : TStringList ) : Boolean;
var
   rPos    : TPoint;
begin
     //Ergebnis löschen
     slValues.Clear;
     result:=FALSE;

     //Sektion suchen
     rPos.x:=Self.FindSection(sSection);

     //Gefunden ?
     if (rPos.x < 0) then Exit;

     //Alles durchgeben
     with (aIniTable[rPos.x]^) do
          begin

               //Alle Keys durchgehen
               rPos.y:=0;
               while (rPos.y < integer( Self.SectionSize( LongWord (rPos.x) ) ) ) do
                   begin
                        //Und die Namen aufaddieren
                        slValues.Add(aKeys[rPos.y]^.sValue);

                        inc(rPos.y);
                   end;
          end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Statusfunktionen
////////////////////////////////////////////////////////////////////////////////
//Anzahl der Sektionen
function TIni.IniSize():Unsigned32;
begin
     result:=Length(aIniTable);
end;

//Anzahl der Keys in einer Sektion
function TIni.SectionSize (u32Section : Unsigned32) :Unsigned32 overload;
begin
     result:=0;

     //Fehler abfangen
     if (u32Section >= Self.IniSize) then
        begin
             exit;
        end;

     //Größe der Sektion holen
     result:=Length(aIniTable[u32Section]^.aKeys);

end;

//Größe einer Sektion
function TIni.SectionSize(sSection:ShortString):Unsigned32 overload;
begin
     //Einfach die LongWordFunktion aufrufen
     result:=Self.SectionSize(Self.FindSection(sSection));
end;

//Existiert eine Sektion ?
function  TIni.SectionExists   (sSection  : ShortString)    :Boolean;
begin
     //Einfach über die Suchfunktion realisieren
     result:=Self.FindSection(sSection) >= 0;
end;

//Existiert ein Key ?
function  TIni.KeyExists       (sSection,sKey:Shortstring)  :Boolean;
var
   rResult : TPoint;
begin
     //Einfach über die Suchfunktion realisieren
     rResult:=Self.FindKey(sSection,sKey);
     
     //Y in Point ist der Key
     result:=rResult.y >= 0;
end;

end.
