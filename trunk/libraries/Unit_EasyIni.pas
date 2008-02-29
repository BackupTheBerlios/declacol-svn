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
///////////////////////////////////////////////////////////////////////////////////////
///
/// Simpler Zugriff auf Ini-Dateien (fast) beliebiger Größe
///
///
///////////////////////////////////////////////////////////////////////////////////////
unit Unit_EasyIni;

interface
uses classes,Forms;

type
  TEasyIni = class(TObject)
  private
    { Private-Deklarationen }
    function DateTimeToTimeStamp(date:TDateTime):LongInt;
    function TimeStampToDateTime(timestamp:LongInt):TDateTime;

    function FindSection      (Section:String):Integer;
    function FindNextSection  (SectionIndex:Integer):Integer;
    function FindKey          (Section,Key:String):Integer;
  public
    constructor Create();

    { Public-Deklarationen }
    function Open (Filename:String):Boolean;
    function Import (var Ini : TStringList):Boolean;
    function Flush():Boolean;
    function Close():Boolean;

    function Read (Section,Key:string;Default:Variant):Variant;
    function Write(Section,Key:string;Value  :Variant):Boolean;

    function ReadSections():TStringList;
    function ReadKeys    (Section:String):TStringList;
    function ReadValues  (Section:String):TStringList;

    function DeleteSection (Section:String):Boolean;
    function DeleteKey     (Section,Key:String):Boolean;

    function SectionExists(Section:String):Boolean;
    function KeyExists    (Section,Key:String):Boolean;


    function SaveComponent(Section:String;Comp:TComponent):Boolean;
    function LoadComponent(Section:String;Comp:TComponent;Position:Boolean=TRUE):Boolean;

  end;



implementation

uses SysUtils,StdCTRLs,Spin;

var
   IniPath   : String;
   IniLoaded : Boolean=FALSE;
    IniList  : TStringList;

const
     UNIXStartDate : TDateTime=255690.0;

///////////////////////////////////////////////////////////////////////////////////////////////
//Hilfsfunktionen um Datum in der Registry zu speichern
///////////////////////////////////////////////////////////////////////////////////////////////
function TEasyIni.DateTimeToTimeStamp(date:TDateTime):LongInt;
begin
     UNIXStartDate:=EncodeDate(1970,1,1)+EncodeTime(2,0,0,0);
     Result:=Round(( date - UNIXStartDate)*86400);
end;

function TEasyIni.TimeStampToDateTime(timestamp:LongInt):TDateTime;
begin
     UNIXStartDate:=EncodeDate(1970,1,1)+EncodeTime(2,0,0,0);
     Result:=(timestamp / 86400)+UNIXStartDate;
end;


constructor TEasyIni.Create();
begin
     IniLoaded:=FALSE;
     IniList:=TStringList.Create;
end;

////////////////////////////////////////////////////////////////////////////////////
//Den Index einer Sektion (Mit dem Sektionsnamen) zurückgeben ansonsten -1
function TEasyIni.FindSection   (Section:String):Integer;
var
   iTemp : integer;
begin
     //Fehler abfangen
     Result:=-1;
     if (not Iniloaded)   then exit;
     if (IniList.Count<1) then exit;

     //Alles in kleinbuchstaben
     Section:=lowerCase('['+Section+']');

     //Und die komplette Liste durchsuchen
     iTemp:=IniList.Count-1;
     While (iTemp>=0) do
         begin
              //Stufenweise vergleichen
              if (IniList[iTemp]<>'') then
                 begin
                      if (IniList[iTemp][1]='[') then
                         begin
                              if (lowercase(IniList[iTemp]) = Section) then
                                 begin
                                      //Und gefundene Position merken
                                      Result:=iTemp;
                                      iTemp:=0;
                                 end;
                         end;
                 end;
              dec(iTemp);
         end;
end;

////////////////////////////////////////////////////////////////////////////////////
//Den Index einer Sektion (Mit dem Sektionsnamen) zurückgeben ansonsten -1
function TEasyIni.FindNextSection   (SectionIndex:Integer):Integer;
begin
     //Fehler abfangen
     Result:=-1;
     if (not Iniloaded)   then exit;
     if (IniList.Count<1) then exit;

     //Der aktuelle Index sollte den Sektionstitel ohne einen Key in der Sektion bezeichnen.
     //Daher müssen wir einfach nach dem nächsten Auftauchen von '[' suchen

     While (SectionIndex < IniList.Count-1) do
           begin
                inc(SectionIndex);
                //Und die Strings vergleichen
                if (IniList[SectionIndex]<>'') then
                   begin
                        if (IniList[SectionIndex][1]='[') then
                           begin
                                //Wenn gefunden, dann Suche abbrechen
                                Result:=SectionIndex;
                                SectionIndex:=IniList.Count;
                           end;
                   end;
           end;
end;


////////////////////////////////////////////////////////////////////////////////////
//Den Index eines Keys in einer Sektion zurückgeben ansonsten -1
function TEasyIni.FindKey       (Section,Key:String):Integer;
var
   iTemp : Integer;
   sTemp : String;
   iStart: integer;
   iStop : integer;
begin
     Result:=-1;
     //Nach unserer Sektion suchen (Start- und Stop-Index)
     iStart:=FindSection(Section);
     iStop :=FindNextSection(iStart);

     //Fehler abfangen
     if (iStart<0) then Exit;
     if (iStop <0) then iStop:=IniList.Count;

     //Und jetzt die Liste durchpflügen, bis zur nächsten Sektion
     key:=key+'=';
     iTemp:=Length(Key);
     if (iTemp>0) then
        begin
             while (iStart < iStop) do
                   begin
                        //Unseren Anfang auskopieren
                        sTemp:=Copy(IniList[iStart],1,iTemp);
                        //Und mit unserem Key vergleichen
                        if (sTemp=key) then
                           begin
                                result:=iStart;
                                iStart:=iStop;
                           end;
                        //Nächsten Wert indexieren
                        inc(iStart);
                   end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////////
//Eine komplette Sektion löschen
function TEasyIni.DeleteSection    (Section:String):Boolean;
var
   iStart : integer;
   iStop  : integer;
begin
     Result:=FALSE;

     //Start der Sektion finden
     iStart:=FindSection(Section);

     //Nix gefunden ?
     if (iStart<0) then Exit;

     //Start der nächsten Sektion suchen
     iStop:=FindNextSection(iStart);

     //Kein Ende gefunden, dann ist unsere Sektion ganz am Ende der Tabelle
     if (iStop<0) then iStop:=IniList.Count;

     //Und von hinten anfangen zu löschen
     while (iStop > iStart) do
           begin
                dec(iStop);
                IniList.Delete(iStop);
           end;

     Result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////////
//Einen Key löschen
function TEasyIni.DeleteKey           (Section,Key:String):Boolean;
var
   iTemp : integer;
begin
     Result:=FALSE;
     //Index des Eintrages suchen
     iTemp:=FindKey(Section,Key);
     if (iTemp<0) then Exit;

     //Eintrag einfach löschen
     IniList.Delete(iTemp);

     Result:=TRUE;
end;


////////////////////////////////////////////////////////////////////////////////////
///Eine Ini-Datei öffnen
function TEasyIni.Open (Filename:String):Boolean;
begin
     Result:=TRUE;
     IniLoaded:=FALSE;
     IniPath:='';

     //Datei besteht nicht, dann erzeugen
     if (FileExists(Filename)=FALSE) then
        begin
             try
                IniList.Clear;
                IniList.SaveToFile(Filename);
                IniLoaded:=TRUE;
             except
             end;


        end
     else
        begin
             try
                //Aus der Datei laden
                IniList.LoadFromFile(Filename);

                //Alle OK
                IniLoaded:=TRUE;
             except
                Result:=FALSE;
             end;
        end;

     //Dateiname merken
     IniPath:=Filename;
end;


////////////////////////////////////////////////////////////////////////////////////
/// Eine Ini-Datei per Stringlist importieren
function TEasyIni.Import (var Ini : TStringList):Boolean;
begin
     IniLoaded:=FALSE;
//     IniPath:='';

     try
        if (Ini<>nil) then
           begin
                IniList.AddStrings(Ini);
           end
        else
           begin
                IniList.Assign(Ini);
           end;
        IniLoaded:=TRUE;
        Result:=TRUE;
     except
        result:=FALSE;   
     end;
end;

////////////////////////////////////////////////////////////////////////////////////
function TEasyIni.Flush():Boolean;
begin
     Result:=FALSE;
     //Ist eine Ini-Datei geladen ?
     if (IniLoaded) then
        begin
             //Gibt es den Dateipfad ?
             if (IniPath<>'') then
                begin
                     //Dann speichern
                     IniList.SaveToFile(IniPath);
                     Result:=TRUE;
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////////
function TEasyIni.Close():Boolean;
begin
     IniPath:='';
     IniList.Free;
     IniLoaded:=FALSE;
     Result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////////
//Einen Key lesen
function TEasyIni.Read (Section,Key:string;Default:Variant):Variant;
var
   iTemp : integer;
   sTemp : String;
begin
     //Fehler abfagen
     Result:=Default;

     //Unseren Wert suchen
     iTemp:=FindKey(Section,Key);
     if (iTemp<0) then Exit;

     //Das Ergebnis auskopieren
     sTemp:=Copy(IniList[iTemp],Pos('=',IniList[iTemp])+1,Length(IniList[iTemp]) );

     //Und dem Datentyp entsprechend aufarbeiten
     case (VarType(Default)) of
          varInteger : Result:=StrToIntDef(sTemp,Default);
          varByte    : Result:=StrToIntDef(sTemp,Default);
          varBoolean : if (sTemp='1') then Result:=TRUE else Result:=FALSE;
          varString  : Result:=sTemp;
          varDate    : Result:=TimeStampToDateTime(StrToIntDef(sTemp,0));
     end;
end;

////////////////////////////////////////////////////////////////////////////////////
///Einen Key schreiben
function TEasyIni.Write(Section,Key:string;Value  :Variant):Boolean;
var
   iTemp : Integer;
   sTemp : String;
begin
     //Fehler abfagen
     Result:=FALSE;

     //Gibt es diesen Key schon ?
     iTemp:=FindKey(Section,Key);
     if (iTemp<0) then
        begin
             //Gesuchte Sektion finden
             iTemp:=FindSection(Section);

             //Gibt es die Sektion schon ?
             if (iTemp<0) then
                begin
                     //Nein, dann neu anfügen
                     IniList.Add('['+Uppercase(Section)+']');
                     iTemp:=IniList.Count-1;
                end;

             //Nächste Zeile
             inc(iTemp);
             //Keine nächste Zeile da ?
             if (iTemp>=IniList.Count) then
                begin
                     //Dann fügen wir den Key an
                     IniList.Add(key+'='+'foo');
                     iTemp:=IniList.Count-1;
                end
             else
                begin
                     //Ansonsten fügen wir den Key ein
                     IniList.Insert(iTemp,key+'='+'foo');
                end;
        end;

     //So, jetzt sollte auf jeden Fall ein Key mit unserem Namen vorleigen
     //Also einfach nochmal suchen
//     iTemp:=FindKey(Section,Key);
     if (iTemp>=0) then
        begin
             sTemp:='';
             //Und unsere Werte einschreiben
             //Und dem Datentyp entsprechend aufarbeiten
             case (VarType(Value)) of
                  varInteger : sTemp:=IntToStr(Value);
                  varByte    : sTemp:=IntToStr(Value);
                  varBoolean : if (Value) then sTemp:='1' else sTemp:='0';
                  varString  : sTemp:=Value;
                  varDate    : sTemp:=IntToStr(DateTimeToTimeStamp(Value));
             end;

             //Wenn etwas zugeordnet wurde, dann den Wert einschreiben
             if (sTemp<>'') then
                begin
                     IniList[iTemp]:=key+'='+sTemp;
                     Result:=TRUE;
                end
             else
                begin
                     //Kein bekannter Datentyp, dann den Wert lieber löschen
                     IniList.Delete(iTemp);
                end;
        end;

end;


////////////////////////////////////////////////////////////////////////////////////
///Alle Sektionen lesen und als Stringliste ausgeben
function TEasyIni.ReadSections():TStringList;
var
   iTemp : integer;
   sTemp : string;
   iPos  : integer;
begin
     Result:=TStringList.Create;
     //Fehler abfangen
     if (not IniLoaded) then Exit;

     //Einfach alles, wes mit "[" anfängt zurückgeben
     iTemp:=IniList.Count-1;
     while (iTemp>=0) do
           begin
                sTemp:=trim(IniList[iTemp]);
                //Eintrag in der Liste ?
                if (sTemp<>'') then
                   begin
                        //Ein Sektionsbeginn ?
                        if (sTemp[1]='[') then
                           begin
                                //Auch ein dazugehöriges Ende ?
                                iPos:=Pos(']',sTemp);
                                if (iPos>0) then
                                   begin
                                        //Dann auskopieren
                                        Result.Add(UpperCase(Copy(sTemp,2,iPos-2)));
                                   end;
                           end;
                   end;
                dec (iTemp);
           end;
     //Nochmal schnell sortieren
//     Result.Sort;
end;

////////////////////////////////////////////////////////////////////////////////////
///Alle Keys einer Sektion lesen und als Stringliste ausgeben
function TEasyIni.ReadKeys(Section:String):TStringList;
var
   iTemp : integer;
   sTemp : string;
   iStart: integer;
   iStop : integer;
begin
     Result:=TStringList.Create;
     //Fehler abfangen
     if (not IniLoaded) then Exit;

     //Nach unserer Sektion suchen (Start- und Stop-Index)
     iStart:=FindSection(Section);
     iStop :=FindNextSection(iStart);

     //Fehler abfangen
     if (iStart<0) then Exit;
     if (iStop <0) then iStop:=IniList.Count;

     while (iStart<iStop) do
           begin
                sTemp:=trim(IniList[iStart]);
                //Was drin ?
                if (sTemp<>'') then
                   begin
                        //Kein Kommentar ?
                        if (sTemp[1]<>';') then
                           begin
                                //Eine Zuweisung ?
                                iTemp:=Pos('=',sTemp);
                                if (iTemp>0) then
                                   begin
                                        Result.Add(Copy(sTemp,1,iTemp-1));
                                   end;
                           end;
                   end;
                inc(iStart);
           end;
end;
////////////////////////////////////////////////////////////////////////////////////
///Alle Values einer Sektion lesen und als Stringliste ausgeben
function TEasyIni.ReadValues(Section:String):TStringList;
var
   iTemp : integer;
   sTemp : string;
   iStart: integer;
   iStop : integer;
begin
     Result:=TStringList.Create;
     //Fehler abfangen
     if (not IniLoaded) then Exit;

     //Nach unserer Sektion suchen (Start- und Stop-Index)
     iStart:=FindSection(Section);
     iStop :=FindNextSection(iStart);

     //Fehler abfangen
     if (iStart<0) then Exit;
     if (iStop <0) then iStop:=IniList.Count;

     while (iStart<iStop) do
           begin
                sTemp:=trim(IniList[iStart]);
                //Was drin ?
                if (sTemp<>'') then
                   begin
                        //Kein Kommentar ?
                        if (sTemp[1]<>';') then
                           begin
                                //Eine Zuweisung ?
                                iTemp:=Pos('=',sTemp);
                                if (iTemp>0) then
                                   begin
                                        Result.Add(Copy(sTemp,iTemp+1,Length(sTemp)));                                        
                                   end;
                           end;
                   end;
                inc(iStart);
           end;
end;


////////////////////////////////////////////////////////////////////////////////////
///Existiert eine Sektion ?
function TEasyIni.SectionExists(Section:String):Boolean;
begin
     if (FindSection(Section)<0) then Result:=FALSE else Result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////////
///Existiert ein Key
function TEasyIni.KeyExists    (Section,Key:String):Boolean;
begin
     if (FindKey(Section,Key)<0) then Result:=FALSE else Result:=TRUE;
end;


///////////////////////////////////////////////////////////////////////////////////////////////
//Speichert die wichtigsten Werte eine Componente (und deren Kinder) in die Ini
function TEasyIni.SaveComponent(Section:String;Comp:TComponent):Boolean;
var
   z    : integer;
begin
     Result:=TRUE;

     //Typecasten
     Comp:=Comp as TForm;

     //Enumerieren
     z:=Comp.ComponentCount-1;

     //Alle Einträge erstmal entfernen
//     DeleteSection(Section);

     //Formular sichern
     with Comp as TForm do
          begin
               Write(Section,Name+'.Top',Top);
               Write(Section,Name+'.Left',Left);
          end;

     //Nun Alle Komponenten durchnummerieren und deren Eigenschaften speichern
     try
     while (z>=0) do
           begin
                if (Comp.Components[z] is TButton) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TButton do
                             begin
                                  Write(Section,Name+'.Enabled',Enabled);
                             end;
                   end;

                //CheckBox
                if (Comp.Components[z] is TCheckBox) then
                   begin
                        with Comp.Components[z] as TCheckBox do
                             begin
                                  Write(Section,Name+'.Enabled',Enabled);
                                  Write(Section,Name+'.Checked',Checked);
                             end;
                   end;

                //Radiobutton
                if (Comp.Components[z] is TRadioButton) then
                   begin
                        with Comp.Components[z] as TRadioButton do
                             begin
                                  Write(Section,Name+'.Enabled',Enabled);
                                  Write(Section,Name+'.Checked',Checked);
                             end;
                   end;

                //EditBox ?
                if (Comp.Components[z] is TEdit) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TEdit do
                             begin
                                  Write(Section,Name+'.Enabled',Enabled);
                                  Write(Section,Name+'.Text',Text);
                             end;
                   end;

                //ComboBox
                if (Comp.Components[z] is TComboBox) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TComboBox do
                             begin
                                  Write(Section,Name+'.Enabled',Enabled);
                                  Write(Section,Name+'.ItemIndex',ItemIndex);
                             end;
                   end;

                //ComboBox
                if (Comp.Components[z] is TSpinEdit) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TSpinEdit do
                             begin
                                  Write(Section,Name+'.Enabled',Enabled);
                                  Write(Section,Name+'.Value',Value);
                             end;
                   end;


                dec(z);
           end;
     except
           Result:=FALSE;
     end;
end;



///////////////////////////////////////////////////////////////////////////////////////////////
//Lädt die wichtigsten Werte eine Componente (und deren Kinder) in die Registry
function TEasyIni.LoadComponent(Section:String;Comp:TComponent;Position:Boolean=TRUE):Boolean;
var
   z    : integer;
begin
     Result:=TRUE;

     //Typecasten
     Comp:=Comp as TForm;

     //Enumerieren
     z:=Comp.ComponentCount-1;

     //Formular laden
     if (Position=TRUE) then
        begin
             with Comp as TForm do
                  begin
                       Top :=Read(Section,Name+'.Top' ,Top);
                       Left:=Read(Section,Name+'.Left',Top);
                  end;
        end;

     //Nun Alle Komponenten durchnummerieren und deren Eigenschaften laden
     while (z>=0) do
           begin
                if (Comp.Components[z] is TButton) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TButton do
                             begin
                                  if (KeyExists(Section,Name+'.Enabled')) then
                                     begin
                                          Enabled:=Read(Section,Name+'.Enabled',True);
                                     end;
                             end;
                   end;

                if (Comp.Components[z] is TCheckBox) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TCheckBox do
                             begin
                                  if (KeyExists(Section,Name+'.Enabled')) then
                                     begin
                                          Enabled:=Read(Section,Name+'.Enabled',TRUE);
                                     end;
                                  if (KeyExists(Section,Name+'.Checked')) then
                                     begin
                                          Checked:=Read(Section,Name+'.Checked',FALSE);
                                     end;
                             end;
                   end;

                if (Comp.Components[z] is TRadioButton) then
                   begin
                        with Comp.Components[z] as TRadioButton do
                             begin
                                  if (KeyExists(Section,Name+'.Enabled')) then
                                     begin
                                          Enabled:=Read(Section,Name+'.Enabled',TRUE);
                                     end;

                                  if (KeyExists(Section,Name+'.Checked')) then
                                     begin
                                          Checked:=Read(Section,Name+'.Checked',FALSE);
                                     end;
                             end;
                   end;

                if (Comp.Components[z] is TEdit) then
                   begin
                        with Comp.Components[z] as TEdit do
                             begin
                                  if (KeyExists(Section,Name+'.Enabled')) then
                                     begin
                                          Enabled:=Read(Section,Name+'.Enabled',TRUE);
                                     end;
                                  if (KeyExists(Section,Name+'.Text')) then
                                     begin
                                          Text:=Read(Section,Name+'.Text','Text');
                                     end;
                             end;
                   end;

                if (Comp.Components[z] is TComboBox) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TComboBox do
                             begin
                                  if (KeyExists(Section,Name+'.Enabled')) then
                                     begin
                                          Enabled:=Read(Section,Name+'.Enabled',TRUE);
                                     end;
                                  if (KeyExists(Section,Name+'.ItemIndex')) then
                                     begin
                                          ItemIndex:=Read(Section,Name+'.ItemIndex',-1);
                                     end;
                             end;
                   end;

                if (Comp.Components[z] is TSpinEdit) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TSpinEdit do
                             begin
                                  if (KeyExists(Section,Name+'.Enabled')) then
                                     begin
                                          Enabled:=Read(Section,Name+'.Enabled',TRUE);
                                     end;
                                  if (KeyExists(Section,Name+'.Value')) then
                                     begin
                                          Value:=Read(Section,Name+'.Value',Value);
                                     end;
                             end;
                   end;


                dec(z);
           end;
end;

end.
