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
////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Einfache Komponente, um eine Spieleliste zu verwalten
// (c) 2004 Borg@Sven-of-Nine.de
//
// v 0.1
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////

unit Unit_GameList;

interface
uses Unit_StringFunctions,
     Unit_Compression,
     Unit_ScreenFunctions,
     Unit_Checksum,
     Unit_FileFunctions,
     Unit_Grafix,
     Unit_GetGlobalDirs,
     JPEG,
     SysUtils,
     Windows,
     Classes,
     Graphics,
     Forms;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Ein paar Konstanten
////////////////////////////////////////////////////////////////////////////////////////////////////////////
const
     SortGL_ByName       = 0;
     SortGL_ByGenre      = 1;
     SortGL_ByDeveloper  = 2;
     SortGL_ByRating     = 3;
     SortGLReverse_ByName       = 32;
     SortGLReverse_ByGenre      = 33;
     SortGLReverse_ByDeveloper  = 34;
     SortGLReverse_ByRating     = 35;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Unsere Struktur, um einen Spieleeintrag zu verwalten
////////////////////////////////////////////////////////////////////////////////////////////////////////////
type
    PGameEntry = ^TGameEntry;
    TGameEntry = packed record
    sName       :String;         //Name des Spieles
    sDevel      :String;         //Name des Herstellers
    sGenre      :String;         //Genre
    bRating     :Byte;           //Wertung
    hDescrip    :String;         //Beschreibung als Hex-String;
    sDate       :String;         //Herstellungsdatum
    sExePath    :String;         //Pfad der Startdatei
    sExeParam   :String;         //Parameter für die Startdatei
    sSetupPath  :String;         //Name der Setup-Datei
    dwCRC       :Cardinal;       //Prüfsumme der Startdatei
    bWin95      :Boolean;        //Win 95 Emulation ?
    bWin98      :Boolean;        //Win 98 Emulation ?
    bVDMS       :Boolean;        //Sound-Emulation ? (>Win2k)
    bDOSBox     :Boolean;        //Soll das Programm mit DOSBox gestartet werden
    bCDDosBox   :Boolean;        //Soll eine CD in der DOSBox emuliert werden
    sCDDosBox   :String;         //Pfad zum Verzeichnis, welches die CD darstellt
    sCDLabel    :String;         //Name der von DOSBox emulierten CD

    bCDDaemon   :Boolean;        //CD-Emulation ? (nur bei Installierten Daemon-Tools)
    sIsoFile    :String;         //CD-Emulation ISO-Datei

    bScreen     :Boolean;        //Eigene Auflösung ?
    wScreen     :Byte;           //Index der gewählten Auflösung
    TScreen     :TScreenMode;    //Eigene Auflösung

    wSpeed      :Byte;           //CPU-Speed 1-100%
    bDelay      :Byte;           //Delay, bevor die Geschwindigkeit gedrosselt wird
    sPic        :String;         //Datei des Screenshots

    wUID        :Cardinal        //ID zur sicheren Identifizierung des Spieles
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Das Zentrale Objekt
////////////////////////////////////////////////////////////////////////////////////////////////////////////
type TGameList = class (TObject)
     private
     protected
           //Die Spieledaten in eine Stringliste parsen
           function DataToStringList(Section:String;Data:TGameEntry):TStringList;
           function StringListToData(StringList:TStringList):Boolean;
     public
           //Fortschrittszähler
           Progress : Integer;
           //Zentrales Array
           Entry :packed array of TGameEntry;
           //Den übergebenen Eintrag mit Standardwerten initialisieren
           function  InitGameEntry(var Data:TGameEntry):Boolean;
           //Einen Eintrag zufügen (UID wird automatisch erzeugt)
           function  Add(Game:TGameEntry):Boolean;
           //Einen Eintrag updaten
           function  Update(Index:LongInt;Game:TGameEntry):Boolean;
           //Einen Eintrag löschen
           function  Delete(Index:LongInt):Boolean;
           //Alle Einträge löschen
           function Clear():Boolean;
           //Einen Eintrag auslesen
           function  Get(Index:LongInt):TGameEntry;
           //Einen Eintrag finden
           function  Find(UID:Cardinal):LongInt; overload;
           function  Find(Name:String):LongInt; overload;
           function  FindFullText(Text:String):LongInt;

           //Die Spieleliste sortieren
           procedure Sort(Mode:Integer=SortGL_ByName);
           //Anzahl der Einträge zählen
           function  Count():LongInt;

           //Die ganzen Einträge in einer Ii-Datei schreiben oder daraus lesen
           procedure LoadFromIni(Filename:String);
           procedure AddFromIni(Filename:String);
           procedure SaveToIni(Filename:String);

           //Den Beschreibungstext kodieren und dekodieren
           function Decode(Text:String):String;
           function Encode(Text:String):String;

           //Fremde Formate importieren
           function ALGFileToGameEntry(FileName:String;var Game:TGameEntry;Screenshot:TJPEGImage):Boolean;

           //Fremde Formate Exportieren
           function SaveToCSV(Filename:String):Boolean;


end;

implementation

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag zufügen (UID wird automatisch erzeugt
function TGameList.Add(Game:TGameEntry):Boolean;
begin
     Result:=TRUE;
     SetLength(Entry,Length(Entry)+1);

     //Eine einzigartige UID erzeugen
     while (Find(Game.wUID)<>-1) do Game.wUID:=GettickCount;

     //Den Beschreibungseintrag kodieren
     Game.hDescrip:=Encode(Game.hDescrip);

     //Und das Spiel hinzufügen
     Entry[Length(Entry)-1]:=Game;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen schon bestehenden Eintrag updatem
function  TGameList.Update(Index:LongInt;Game:TGameEntry):Boolean;
begin
     //Fehler abfangen
     Result:=FALSE;
     if (Index >= Length(Entry)) then Exit;
     if (Index < 0)              then Exit;

     Result:=TRUE;

     //Den Beschreibungseintrag kodieren

     //Dekodieren geht vieeel schneller, daher erstmal vergleichen ob neues komprimieren notwendig ist
     if (StringToHash(Decode(Entry[Index].hDescrip))<>StringToHash(Game.hDescrip)) then
        begin
             Game.hDescrip:=Encode(Game.hDescrip);
        end
     else
        begin
             Game.hDescrip:=Entry[Index].hDescrip;
        end;


     //Und rein damit
     Entry[Index]:=Game;
end;
////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen bestehenden Eintrag löschen
function TGameList.Delete(Index:LongInt):Boolean;
var
   z : LongInt;
begin
     //Fehler abfangen
     Result:=FALSE;
     if (Index >= Length(Entry)) then Exit;
     if (Index < 0)                 then Exit;

     //Einfach alle nachfolgenden Eintrag um eins nach vorn schieben
     Result:=TRUE;
     for z:=Index to Length(Entry)-2 do
         begin
              Entry[z]:=Entry[z+1];
         end;
     //Arraylänge anpassen
     SetLength(Entry,Length(Entry)-1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Die komplette Liste löschen
function TGameList.Clear():Boolean;
begin
     Result:=TRUE;
     Entry:=nil;
     SetLength(Entry,0);
end;
////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Die Anzahl der Einträge auslesen
function TGameList.Count():LongInt;
begin
     Result:=Length(Entry);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag holen
function TGameList.Get(Index:LongInt):TGameEntry;
begin
     //Ausgabe zumindest initialisieren
     InitGameEntry(Result);

     //Fehler abfangen
     if (Index >= Length(Entry)) then Exit;
     if (Index < 0)              then Exit;

     //Eintrag holen
     Result:=Entry[Index];

     //Den Beschreibungseintrag dekodieren
     Result.hDescrip:=Decode(Result.hDescrip);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag anhande seiner ID (wird zufällig erzeugt) finden
function TGameList.Find(UID:Cardinal):LongInt;
var
   z : LongInt;
begin
     //Noch nix gefunden
     Result:=-1;

     z:=0;
     while (z < Count) do
           begin
                //Ist die UID gefunden, dann Index merken und abbrechen
                if (UID=Entry[z].wUID) then
                   begin
                        Result:=z;
                        z:=Count;
                   end;
                inc(z);
           end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag anhand seines Names finden
function TGameList.Find(Name:String):LongInt;
var
   z : LongInt;
begin
     Result:=-1;

     z:=0;
     while (z < Count) do
           begin
                if (Name[1]=Entry[z].sName[1]) then
                if (Name=Entry[z].sName) then
                   begin
                        Result:=z;
                        z:=Count;
                   end;
                inc(z);
           end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Volltextsuche über alle Textfelder
function  TGameList.FindFullText(Text:String):LongInt;
var
   z : LongInt;
   g : TGameEntry;
begin
     //Noch nix gefunde
     Result:=-1;

     //Fehler abfangen
     if (Length(Text)=0) then Exit;

     z:=0;
     while (z < Count) do
           begin
                //Eintrag holen
                g:=Get(z);
                //Durchsuchen
                if (Pos(Text,g.sName)>0) or
                   (Pos(Text,g.sDevel)>0) or
                   (Pos(Text,g.sGenre)>0) or
                   (Pos(Text,g.hDescrip) >0) then
                   begin
                        //Evtl. Index merken und abbrechen
                        Result:=z;
                        z:=Count;
                   end;
                inc(z);
           end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Die komplette Liste mit Quicksort sortieren
procedure TGameList.Sort(Mode:Integer);
var
   m : Integer;
   mo: Integer;
   n : Integer;
   s : String;
   s1: String;
   //Unterprozedur Quicksort (kennt ja wohl jeder)
   Procedure SortQuick(Left,Right:LongInt);
   var
      i,j    : LongInt;
      x,Temp : TGameEntry;
   begin
        i:=Left;
        j:=Right;

        if (j>i) then
           begin
                x:=Entry[(Left+Right) shr 1];

                repeat
                      //Anhand des gewählten Modus sortieren
                      case (Mode) of
                      SortGL_ByDeveloper :  begin
                                                 while (LowerCase(Entry[i].sDevel) < LowerCase(x.sDevel)) do inc(i);
                                                 while (LowerCase(Entry[j].sDevel) > LowerCase(x.sDevel)) do dec(j);
                                            end;

                      SortGL_ByRating    :  begin
                                                 while (Entry[i].bRating < x.bRating) do inc(i);
                                                 while (Entry[j].bRating > x.bRating) do dec(j);
                                            end;
                      SortGL_ByGenre     :  begin
                                                 while (LowerCase(Entry[i].sGenre) < LowerCase(x.sGenre)) do inc(i);
                                                 while (LowerCase(Entry[j].sGenre) > LowerCase(x.sGenre)) do dec(j);
                                            end;
                      SortGLReverse_ByDeveloper :  begin
                                                 while (LowerCase(Entry[i].sDevel) > LowerCase(x.sDevel)) do inc(i);
                                                 while (LowerCase(Entry[j].sDevel) < LowerCase(x.sDevel)) do dec(j);
                                            end;

                      SortGLReverse_ByRating    :  begin
                                                 while (Entry[i].bRating > x.bRating) do inc(i);
                                                 while (Entry[j].bRating < x.bRating) do dec(j);
                                            end;
                      SortGLReverse_ByGenre     :  begin
                                                 while (LowerCase(Entry[i].sGenre) > LowerCase(x.sGenre)) do inc(i);
                                                 while (LowerCase(Entry[j].sGenre) < LowerCase(x.sGenre)) do dec(j);
                                            end;
                      SortGLReverse_ByName     :  begin
                                                 while (LowerCase(Entry[i].sName) > LowerCase(x.sName)) do inc(i);
                                                 while (LowerCase(Entry[j].sName) < LowerCase(x.sName)) do dec(j);
                                            end;
                      else
                                            begin
                                                 while (LowerCase(Entry[i].sName) < LowerCase(x.sName)) do inc(i);
                                                 while (LowerCase(Entry[j].sName) > LowerCase(x.sName)) do dec(j);
                                            end;
                      end;


                      if (i<=j) then
                         begin
                              Temp:=Entry[i];
                              Entry[i]:=Entry[j];
                              Entry[j]:=Temp;

                              inc(i);
                              dec(j);
                         end;
                until (i>j);
           end;
        if (j>Left)  then SortQuick(Left,J);
        if (i<Right) then SortQuick(i,Right);
   end;
begin
     //Ganzes Array sortieren
     mo:=Mode;
     SortQuick(0,Count()-1);

     Mode:=SortGL_ByName;
     m:=0;
     n:=0;
     s1:='';

     //Und nun die einzelnen Sektionen nach Namen sortieren
     while (m < Count ) do
           begin
                //Sortiermodus anpassen
                case (mo) of
                     SortGL_ByDeveloper        : s:=Entry[m].sDevel;
                     SortGL_ByGenre            : s:=Entry[m].sGenre;
                     SortGL_ByRating           : s:=IntToStr(Entry[m].bRating);

                     SortGLReverse_ByDeveloper : s:=Entry[m].sDevel;
                     SortGLReverse_ByGenre     : s:=Entry[m].sGenre;
                     SortGLReverse_ByRating    : s:=IntToStr(Entry[m].bRating);
                end;

                //Sind wir in einer neuen Sektion ?
                if (s1<>s) then
                   begin
                        if (n < m) then
                           begin
                                //Dann die komplett erkannte Sektion
                                //durchsortieren
                                SortQuick(n,m-1);
                           end;
                        n:=m;
                   end;

                //Zähler erhöhen und Strings anpassen
                s1:=s;
                inc(m);
           end;
     SortQuick(n,Count-1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen Spieleeintrag mit Standardwerten initialisieren
function TGameList.InitGameEntry(var Data:TGameEntry):Boolean;
begin
     Result:=TRUE;
     Data.sName:='_Unknown';
     Data.sDevel:='_Unknown';
     Data.sGenre:='_Unknown';
     Data.bRating:=50;
     Data.sDate:='1970';
     Data.hDescrip:='';
     Data.sExePath:='_Unknown.exe';
     Data.sExeParam:='';
     Data.sSetupPath:='_Unknown.exe';
     Data.dwCRC:=0;
     Data.bWin95:=FALSE;
     Data.bWin98:=FALSE;
     Data.bVDMS:=FALSE;
     Data.bCDDaemon:=FALSE;
     Data.bDOSBox:=FALSE;
     Data.bCDDOSBox:=FALSE;
     Data.sCDDOSBox:='unknowndir';
     Data.sIsoFile:='_Unknown.iso';
     Data.sCDLabel:='CDLABEL';
     Data.bScreen:=FALSE;
     Data.wScreen:=0;
     Data.wSpeed:=100;
     Data.bDelay:=0;
     Data.sPic:='0';
     Data.wUID:=0;
end;


////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen String kodieren
function TGameList.Decode(Text:String):String;
begin
     Result:=LZWToString(HexToLZW(Text));
end;

////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen String dekodieren
function TGameList.Encode(Text:String):String;
begin
     Result:=LZWToHex(StringToLZW(Text));
end;

/////////////////////////////////////////////////////////////////////////////////////////////////
//eine Spieleliste aus einer Ini-Datei laden
procedure TGameList.LoadFromIni(Filename:String);
var
   s : TStringList;
begin
     //Fehler abfangen
     if (FileExists(FileName)=FALSE) then Exit;

     //Stringliste erzeugen
     s:=TStringList.Create;

     //Datei laden
     s.LoadFromFile(Filename);

     //Spieleliste löschen
     Clear;

     //Fortschritt auf 0
     Progress:=0;

     //Los gehts
     StringListToData(s);
     Progress:=0;

     s.Free;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////
//einer Spieleliste aus einer Ini-Datei Einträge hinzufügen
procedure TGameList.AddFromIni(Filename:String);
var
   s : TStringList;
begin
     //Fehler abfangen
     if (FileExists(FileName)=FALSE) then Exit;

     //Stringliste erzeugen
     s:=TStringList.Create;

     //Datei laden
     s.LoadFromFile(Filename);

     //Fortschritt auf 0
     Progress:=0;

     //Los gehts
     StringListToData(s);
     Progress:=0;
     s.Free;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////
//Die Spieleliste in eine Ini-Datei schreiben
procedure TGameList.SaveToIni(Filename:String);
var
   z : LongInt;
   s : TStringList;
begin
     //Fehler abfangen
     if Count<1 then Exit;

     //Stringliste erzeugen
     s:=TStringList.Create;

     //Fortschritt auf 0
     Progress:=0;

     //Alle Einträge abarbeiten
     for z:=0 to Count-1 do
         begin
              //Ini in die Stringliste aufnehmen
              s.AddStrings(DataToStringList(IntToHex(z,10),Entry[z]));

              //Fortschritt anzeigen
                Progress:=Round((z+1)*100/Count);
                Application.ProcessMessages;
         end;
     //Datei speichern
     s.SaveToFile(Filename);
     Progress:=0;
     s.Free;
end;


/////////////////////////////////////////////////////////////////////////////////////////////////
//Auf die simple Tour aus einem Eintrag eine Stringliste machen
Function TGameList.DataToStringList(Section:String;Data:TGameEntry):TStringList;
begin
     Result:=TStringList.Create;
     Result.Add('['+Section+']');
     with Data do
          begin
               Result.Add('GameName='+Data.sName);
               Result.Add('GameDeveloper='+Capitalize(Data.sDevel));
               Result.Add('GameGenre='+Capitalize(Data.sGenre));
               Result.Add('Rating='+IntToStr(Data.bRating));
               Result.Add('Description='+Data.hDescrip);
               Result.Add('Date='+Data.sDate);
               Result.Add('GameExe='+Data.sExePath);
               Result.Add('ExeParam='+Data.sExeParam);
               Result.Add('SetupExe='+Data.sSetupPath);
               Result.Add('ExeCRC='+IntToStr(Data.dwCRC));
               if (Data.bScreen) then Result.Add('DisplayChange=1') else Result.Add('DisplayChange=0');
               Result.Add('ScreenMode='+IntToStr(Data.wScreen));
               if (Data.bWin95) then Result.Add('Win95Mode=1') else Result.Add('Win95Mode=0');
               if (Data.bWin98) then Result.Add('Win98Mode=1') else Result.Add('Win98Mode=0');
               if (Data.bVDMS) then Result.Add('VDMSMode=1') else Result.Add('VDMSMode=0');
               if (Data.bCDDaemon) then Result.Add('DaemonMode=1') else Result.Add('DaemonMode=0');
               Result.Add('IsoFile='+Data.sIsoFile);
               if (Data.bDOSBox) then Result.Add('DOSBox=1') else Result.Add('DOSBox=0');
               if (Data.bCDDOSBox) then Result.Add('DOSBoxCD=1') else Result.Add('DOSBoxCD=0');
               Result.Add('DOSBoxCDDir='+Data.sCDDOSBox);
               Result.Add('DOSBoxCDLabel:='+Data.sCDLabel);
               Result.Add('CPUSpeed='+IntToStr(Data.wSpeed));
               Result.Add('Delay='+IntToStr(Data.bDelay));
               Result.Add('Pic='+Data.sPic);
               Result.Add('UID='+IntToStr(Data.wUID));
          end;
end;


/////////////////////////////////////////////////////////////////////////////////////////////////
//Eine Stringliste kpl. parsen und die gefundenen Einträge der Spieleliste zufügen
function TGameList.StringListToData(StringList:TStringList):Boolean;
var
   z,x   : Integer;
   key   : String;
   lkey  : Integer;
   value : String;
   temp  : String;
   game  : TGameEntry;
begin
     Result:=FALSE;
     if (StringList.Count=0) then Exit;

     //Erstmal alles Trimmen
     for z:=0 to StringList.Count-1 do
         begin
              StringList[z]:=Trim(StringList[z]);
         end;

     //Erste Section finden !
     z:=0;
     x:=0;
     while (z< StringList.Count) and (StringList[z][1]='[') and (Pos(']',StringList[z])<>0) do inc(z);

     //Daten initialisieren
     InitGameEntry(game);

     while (z < StringList.Count) do
           begin
                //Key gefunden
                if (Pos('=',StringList[z])>0) then
                   begin
                        //Key und Value extrahieren
                        temp:=StringList[z];
                        Key:=LowerCase(Copy(temp,1,Pos('=',temp)-1));
                        lkey:=Length(Key);
                        Value:=Copy(temp,lkey+2,Length(temp)-lkey);

                        //Und auf die Harte Tour zuordnen
                         with Game do
                              begin
                                   if (Key='gamename')         then sName:=ShortString(Value);
                                   if (Key='gamedeveloper')    then sDevel:=ShortString(Value);
                                   if (Key='gamegenre')        then sGenre:=ShortString(Value);
                                   if (Key='rating')           then bRating:=StrToIntDef(Value,0);
                                   if (Key='description')      then hDescrip:=Value;
                                   if (Key='date')             then sDate:=Value;

                                   if (Key='gameexe')          then sExePath:=ShortString(Value);
                                   if (Key='exeparam')         then sExeParam:=ShortString(Value);
                                   if (Key='setupexe')         then sSetupPath:=ShortString(Value);
                                   if (Key='isofile')          then sIsoFile:=ShortString(Value);

                                   if (Key='execrc')           then dwCRC:=StrToIntDef(Value,0);
                                   if (Key='displaychange')    then if (Value='1') then bScreen:=TRUE else bScreen:=FALSE;
                                   if (Key='screenmode')       then wScreen:=StrToIntDef(Value,0);

                                   if (Key='win95mode')        then if (Value='1') then bwin95:=TRUE else bwin95:=FALSE;
                                   if (Key='win98mode')        then if (Value='1') then bwin98:=TRUE else bwin98:=FALSE;
                                   if (Key='vdmsmode')         then if (Value='1') then bvdms:=TRUE  else bvdms:=FALSE;
                                   if (Key='daemonmode')       then if (Value='1') then bCDdaemon:=TRUE  else bCDdaemon:=FALSE;

                                   if (Key='dosbox')           then if (Value='1') then bdosbox:=TRUE  else bdosbox:=FALSE;
                                   if (Key='dosboxcd')         then if (Value='1')then bCDDOSBox:=TRUE else bCDDOSBox:=FALSE;
                                   if (Key='dosboxcddir')      then sCDDOSBox:=ShortString(Value);
                                   if (Key='dosboxcdlabel')    then sCDLabel:=ShortString(Value);

                                   if (Key='cpuspeed')         then wSpeed:=StrToIntDef(Value,100);
                                   if (Key='delay')            then bDelay:=StrToIntDef(Value,100);

                                   if (Key='pic')              then sPic:=ShortString(Value);
                                   if (Key='uid')              then wuid:=StrToIntDef(Value,0);
                              end;
                   end;

                //Sectionsende ?
                if(Length(StringList[z])>0) then
                if (StringList[z][1]='[') and (Pos(']',StringList[z])<>0) and (Pos('=',StringList[z])=0)then
                   begin
                        //Hier kein AddGame benutzen, um sinnloses umkodieren zu
                        ///vermeiden
                        SetLength(Entry,Length(Entry)+1);

                        //Eine einzigartige UID erzeugen (wenn noch keine besteht)
                        while (Find(Game.wUID)<>-1) do Game.wUID:=GettickCount;

                        //Und das Spiel hinzufügen
                        Entry[Length(Entry)-1]:=Game;
                   end;

                Progress:=Round((z+1)*100/StringList.Count);

                if (x>200) then
                   begin
                        x:=0;
                        Application.ProcessMessages;
                   end;
                inc(x);
                inc(z);
          end;
     //Noch ein letzer Eintrag ?
     if (Game.sName<>'') and (Game.sExePath<>'') then
        begin
             //Da der Text hier unkodiert vorliegt, erstmal dekodieren, da
             //die Add-Function von selbst eine kodierung vornimmt
             Game.hDescrip:=Decode(Game.hDescrip);
             Add(Game);
        end;
     Result:=TRUE;
end;


/////////////////////////////////////////////////////////////////////////////////////////////////
//Fremde formate importieren
/////////////////////////////////////////////////////////////////////////////////////////////////
//ALG-File (Abandonloader)
function TGameList.ALGFileToGameEntry(FileName:String;var Game:TGameEntry;Screenshot:TJPEGImage):Boolean;
var
   s          : String;
   z          : Integer;
   filehandle : Integer;
   buffer     : Array of Byte;
   TempBMP    : TBitmap;

   //Unterfunktion um einzelne Dateiteile leichter lesen zu können
   function ReadSection(Offset:Cardinal;Size:Cardinal):Boolean;
            begin
                 Result:=FALSE;
                 Buffer:=nil;
                 SetLength(Buffer,Size);
                 if FileSeek(FileHandle,Offset,0) =-1 then
                    begin
                         Exit;
                    end;
                 if FileRead(FileHandle,Buffer[0],Size) = Length(Buffer) then
                    begin
                         Result:=TRUE;
                    end;
            end;
begin
     Result:=FALSE;

     //Spieleeintrag mit Standardwerten füllen
     InitGameEntry(Game);

     //Grafikpuffer erzeugen
     TempBMP:=TBitmap.Create;
     with TempBMP do
     begin
          Width:=220;
          Height:=175;
          Canvas.Brush.Color:=clBlack;
          Canvas.FillRect(Canvas.ClipRect);
          Canvas.Brush.Style:=bsClear;
          Canvas.Font.Color:=clGreen;
          Canvas.Font.Size:=13;
          Canvas.TextOut(33,70,'No Screenshot found');
     end;

     if (Screenshot=nil) then Screenshot:=TJPEGImage.Create;

     //Datei öffnen
     FileHandle:=FileOpen(Filename,fmOPENREAD);
     if (FileHandle>0) then
        begin
             //Checken, ob es eine ALG-Datei ist
             ReadSection(0,8);
             if String(Buffer)='ABANGAME' then
                begin
                     //OK, der Header stimmt. ab gehts

                     //Name des Spiels
                     if (ReadSection($0c ,260)) then Game.sName:=UnZeroString(String(Buffer)) else Game.sName:='Not Identified';

                     //Entwickler
                     if (ReadSection($110,260)) then Game.sDevel:=UnZeroString(String(Buffer)) else Game.sDevel:='Not Identified';

                     //Genre
                     if (ReadSection($214,260)) then Game.sGenre:=UnZeroString(String(Buffer)) else Game.sGenre:='Not Identified';

                     //Pfad
                     if (ReadSection($318,1040)) then s:=AddBackSlash(UnZeroString(String(Buffer))) else s:='';

                     //GameExe
                     if (ReadSection($41c,260)) then Game.sExePath:=s+UnZeroString(String(Buffer)) else Game.sExePath:=s;

                     //SetupExe
                     if (ReadSection($520,260)) then Game.sSetupPath:=s+UnZeroString(String(Buffer)) else Game.sSetupPath:=s;

                     //WebSite
                     if (ReadSection($930,260)) then Game.hDescrip:=UnzeroString(String(Buffer))+Chr(13);

                     //Description
                     if (ReadSection($d30,8192)) then Game.hDescrip:=Game.hDescrip+UnZeroString(String(Buffer));

                     //Bitmap angehängt
                     if (ReadSection($2b9c,2)) then
                        begin
                             //Bitmap header gefunden ?
                             if (String(Buffer)='BM') then
                                begin
                                     //Bitmap holen und konvertieren
                                     ReadSection($2b9c,65535*4);
                                     //Tempverzeichnis holen
                                     s:=AddBackSlash(GetTempDir);

                                     //Ausgeschnittene Daten speichern
                                     Str2File(s+'tmp.bmp',String(Buffer));

                                     //Und als Bitmap laden
                                     TempBMP.LoadFromFile(s+'tmp.bmp');

                                     //Resizen
                                     ResampleBitMap(TempBMP,225,170);

                                     //In JPEG konvertieren
                                     ScreenShot.Assign(TempBMP);

                                     //Temporär speichern
                                     ScreenShot.SaveToFile(s+'tmp.jpg');

                                     //Dateiname erzeugen
                                     Game.sPic:=FileToHash(s+'tmp.jpg');

                                     //Temporäre Dateien löschen
                                     DelData(s+'tmp.jpg');
                                     DelData(s+'tmp.bmp');
                                end;
                        end
                     else
                        begin
                             ScreenShot.Assign(TempBMP);
                        end;

                     //GameSettings
                     ReadSection($297c,1);
                     z:=Ord(Buffer[0]);
                     //Bit 1 => VDMS
                     //Bit 2 => Win 95-Modus
                     if (z and 1)= 1 then Game.bVDMS:=TRUE else Game.bVDMS:=FALSE;
                     if (z and 2)= 2 then Game.bWin95:=TRUE else Game.bWin95:=FALSE;
                     Result:=TRUE;
                end;
             FileClose(FileHandle);
        end;
     TempBMP.Free;
end;


/////////////////////////////////////////////////////////////////////////////////////////////////
//Fremde Formate exportieren
/////////////////////////////////////////////////////////////////////////////////////////////////
//Export in CSV-Format
function TGameList.SaveToCSV(Filename:String):Boolean;
var
   s     : String;
   d,z   : LongInt;
   FH    : Integer;
   sep   : String;
   Game  : TGameEntry;
begin
     Progress:=0;
     Result:=FALSE;
     sep:='|';
     DelData(Filename);
     FH:=FileCreate(Filename);
     if (FH<>-1) then
        begin
             d:=0;
             z:=0;
             s:='';
             s:=s+'Name'+Sep;
             s:=s+'Developer'+Sep;
             s:=s+'Genre'+Sep;
             s:=s+'Rating'+Sep;
             s:=s+'ExePath'+Sep;
             s:=s+'Commandline'+Sep;
             s:=s+'SetupPath'+Sep;
             s:=s+'Win95Mode'+Sep;
             s:=s+'Win98Mode'+Sep;
             s:=s+'VDMS'+Sep;
             s:=s+'DOSBOX'+Sep;
             s:=s+'Use D-Tools'+Sep;
             s:=s+'ISOFile'+Sep;
             s:=s+'Use DOSBoxCD'+Sep;
             s:=s+'DOSBOXCDDir'+Sep;
             s:=s+'Change Screen'+Sep;
             s:=s+'Screen Mode'+Sep;
             s:=s+'CPU Speed'+Sep;
             s:=s+'Startup Delay'+Sep;
             s:=s+'Screenshot'+Sep;
             s:=s+'UniqueID'+Chr($0d)+Chr($0a);

             while (z <= Count) do
                   begin
                        Progress:=Round(z*100/Count);

                        FileWrite(FH,s[1],Length(s));

                        Game:=Get(z);

                        s:='';
                        s:=s+Game.sName+Sep;
                        s:=s+Game.sDevel+Sep;
                        s:=s+Game.sGenre+Sep;
                        s:=s+IntToStr(Game.bRating)+Sep;
                        s:=s+Game.sExePath+Sep;
                        s:=s+Game.sExeParam+Sep;
                        s:=s+Game.sSetupPath+Sep;
                        s:=s+Bool2Str(Game.bWin95)+Sep;
                        s:=s+Bool2Str(Game.bWin98)+Sep;
                        s:=s+Bool2Str(Game.bVDMS)+Sep;
                        s:=s+Bool2Str(Game.bDOSBox)+Sep;
                        s:=s+Bool2Str(Game.bCDDaemon)+Sep;
                        s:=s+Game.sIsoFile+Sep;
                        s:=s+Bool2Str(Game.bCDDOSBox)+Sep;
                        s:=s+Game.sCDDOSBox+Sep;
                        s:=s+Game.sCDLabel+Sep;
                        s:=s+Bool2Str(Game.bScreen)+Sep;
                        s:=s+IntToStr(Game.wScreen)+Sep;
                        s:=s+IntToStr(Game.wSpeed)+Sep;
                        s:=s+IntToStr(Game.bDelay)+Sep;
                        s:=s+Game.sPic+'.jpg'+Sep;
                        s:=s+IntToStr(Game.wUID)+Chr($0d)+Chr($0a);

                        //Und nun die Heimatverzeichnisse konvertieren
                        s:=SearchAndReplace(s,'%homedir%',GetHomeDir);

                        inc(d);
                        if (d>20) then
                           begin
                                d:=0;
                                Application.ProcessMessages;
                           end;

                        inc(z);
                   end;
             Result:=TRUE;
             FileClose(FH);
        end;
     Progress:=0;
end;


end.
