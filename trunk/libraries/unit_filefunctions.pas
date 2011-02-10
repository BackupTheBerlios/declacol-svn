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
//////////////////////////////////////////////////////////////////////////////////////////////////
//  v0.1.3.1
//  Unit mit nützlichen Dateifunktionen die Delphi nicht bietet
//  (c) 2004/2005  Borg@Sven-of-Nine.de
//
//////////////////////////////////////////////////////////////////////////////////////////////////
// Von Windows definierte Konstanten für Laufwerkstypen
// DRIVE_REMOVABLE
// DRIVE_FIXED
// DRIVE_REMOTE
// DRIVE_CDROM
// DRIVE_RAMDISK
//////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//
//
//
//
//
//
//
//
//////////////////////////////////////////////////////////////////////////////////////////////////
unit Unit_FileFunctions;

interface
uses unit_stringfunctions,Unit_TypeDefs,Windows,SysUtils,Classes,FileCTRL,Forms,SHlObj,ShellAPI;

const
     DRIVE_ALL = High(UINT);

const
   MAX_BUFFER = 1024*1024;

//////////////////////////////////////////////////////////////////////////////////////////////////
function CopyData(Source,Target:LongString):Boolean;
function CopyDataUnfragged(Source,Target:LongString;var checksum:unsigned16):Boolean;

function DelData(Source:LongString):Boolean;
function Deltree(Startpath:LongString):Boolean;
function CopyDir(SourcePath,TargetPath:LongString;HWND:THandle):Boolean;
function MoveDir(SourcePath,TargetPath:LongString;HWND:THandle):Boolean;


//////////////////////////////////////////////////////////////////////////////////////////////////
function BrowseDialog(const Title: Longstring): Longstring;

//////////////////////////////////////////////////////////////////////////////////////////////////
function SetAttrib(Source:LongString;Attr:LongWord):Boolean;
function RemoveAttrib(Source:LongString;Attr:LongWord):Boolean;
function GetAttrib(Source:LongString;Attr:LongWord):Boolean;

//////////////////////////////////////////////////////////////////////////////////////////////////
function CodeFile(Filename:LongString;CodeByte:Byte):Boolean;

//////////////////////////////////////////////////////////////////////////////////////////////////
function GetFileVersion(path : Longstring) : Longstring;
function GetFileSizeEx(filename:Longstring):unsigned64;
function GetPCId():LongString;
procedure delay(msec:longint);

//////////////////////////////////////////////////////////////////////////////////////////////////
function EnumerateDrives(Filter:UINT=DRIVE_ALL):TStringList;
function DirScan (StartPath,Filter:LongString;Hide:LongString;SubDir:integer):TStringList;
function FileScan(StartPath,Filter:LongString;Hide:LongString;SubDir:integer):TStringList;

function DirEmpty(Startpath:LongString):Boolean;
function CleanDir(Startpath:LongString;Filter:LongString):Boolean;
implementation

////////////////////////////////////////////////////////////////////////////////////////////////
//Dateien kopieren
function CopyData(Source,Target:LongString):Boolean;
begin
     Result:=FALSE;

     //Alle Verzeichnisse anlegen
     if (Target<>'') and (ExtractFilePath(Target)<>'') then  Result:=ForceDirectories(ExtractFilePath(Target));

     //Und nun die Datei kopieren
//     if (Result=TRUE) then Result:=Result and CopyFile(PChar(Source),PChar(Target),FALSE);
     CopyFile(PChar(Source),PChar(Target),FALSE);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Dateien so kopieren, das sie nicht fragmentiert werden
//Dabei wird eine einfache Prüfsumme (Summe aller Bytes erzeugt)
function CopyDataUnfragged(Source,Target:LongString;var checksum:Unsigned16):Boolean;
var
   fpIn    : TFilehandle;
   fpOut   : TFilehandle;
   buf     : array of Byte;
   u32SLow : unsigned32;
   u32SHi  : unsigned32;
   s64Size : Signed64;
   u32check: unsigned32;
   u32Count: unsigned32;
begin
     result:=FALSE;
     u32check:=0;

     fpIn :=FileOpen(ExpandFileName(Source),fmOPENREAD);
     //Öffnen OK ?
     if (fpIn<>INVALID_HANDLE_VALUE) then
        begin
             //Den Zielpfad erzwingen
             ForceDirectories(ExtractFilePath(ExpandFilename(Target)));

             //Ziel öffnen
             fpOut :=FileCreate(ExpandFileName(Target));
             //Öffnen OK
             if (fpOut<>INVALID_HANDLE_VALUE) then
                begin
                     //Größe der Quelle holen
                     SetLastError(NO_ERROR);
                     u32SLow:=GetFileSize(fpIn,@u32SHi);
                     if (GetLastError()=NO_ERROR) then
                        begin
                             s64Size:=u32SLow +  (u32SHi shl 32);
                             //Größe des Zieles setzen, um undefragmentiert zu kopieren
                             FileSeek(fpOut,s64Size,0);
                             SetEndOfFile(fpOut);
                        end;

                     //An der Anfang der Datei springen
                     FileSeek(fpOut,0,0);

                     //Puffer initialisieren
                     SetLength(buf,MAX_BUFFER);

                     //Und kopieren
                     u32check:=0;
                     repeat
                           //Lesen
                           u32SLow:=FileRead(fpIn,buf[0],SizeOf(Buf));

                           //Schreiben
                           if (u32SLow>0) then
                              begin
                                   FileWrite(fpOut,buf[0],u32SLow);
                                   result:=TRUE;

                                   //Prüfsumme
                                   for u32Count:=0 to u32SLow -1 do
                                       begin
                                            inc (u32check,buf[u32Count]);
                                       end;
                              end;

                           //Überlauf abfangen
                           u32Check:=u32Check and $0000ffff;

                     until (u32SLow=0);
                     //Puffer freimachen
                     SetLength(buf,0);
                     CloseHandle(fpOut);
                end;
             CloseHandle(fpIn);                
        end;
     //Checksumme übergeben
     checksum:=unsigned16(u32Check and $0000ffff);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Dateien löschen
function DelData(Source:LongString):Boolean;
begin
     //Schreibschutz löschen
     RemoveAttrib(Source,faHIDDEN);
     RemoveAttrib(Source,faREADONLY);
     //Datei löschen
     Result:=DeleteFile(PChar(Source));
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Attribut Attr entfernen
function RemoveAttrib(Source:LongString;Attr:LongWord):Boolean;
var
   Attrib:LongWord;
begin
     Result:=FALSE;
     //Attribute holen
     Attrib:=GetFileAttributes(PChar(Source));
     if (Attrib <> $FFFFFFFF ) then
        begin
             //Und ohne gewünschtes Attribut wieder setzen
             if (SetFileAttributes(PChar(Source),Attrib and not Attr)) then
                begin
                     Result:=TRUE;
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Attribut Attr setzen
function SetAttrib(Source:LongString;Attr:LongWord):Boolean;
var
   Attrib:LongWord;
begin
     Result:=FALSE;
     Attrib:=GetFileAttributes(PChar(Source));
     if (Attrib <> $FFFFFFFF ) then
        begin
             //Und mit gewünschtes Attribut wieder setzen
             if SetFileAttributes(PChar(Source),Attrib or Attr) then
                begin
                     Result:=TRUE;
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Testen, ob Attribut Attr gesetzt ist
function GetAttrib(Source:LongString;Attr:LongWord):Boolean;
var
   Attrib:LongWord;
begin
     Result:=FALSE;
     //Attribute holen
     Attrib:=GetFileAttributes(PChar(Source));
     if (Attrib <> $FFFFFFFF ) then
        begin
             if ( Attrib and Attr = Attr) then
                begin
                     Result:=TRUE;
                end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Netzwerkname des PC's lesen
function GetPCId():LongString;
var
   pcName : pchar;   // Holds the computer name
   dwSize : dword;   // Size of the buffer holding the name
   z      : integer;
begin
     //PC-Name holen
     try
        dwSize := MAX_COMPUTERNAME_LENGTH + 1;
        pcName := strAlloc( dwSize );

        if getComputerName( pcName, dwSize )=FALSE then pcName:='NA';
     finally
     end;

     //Aus dem Buffer einen String machen
     for z:=0 to dwSize do
         begin
              if pcName[z]='\' then pcName[z]:=' ';
         end;

     //Fertig
     Result:= trim ( string(pcName) );
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Dateigröße
function GetFileSizeEx(filename:Longstring):unsigned64;
var
   u32Hi : unsigned32;
   u32Lo : unsigned32;
   fpIn  : TFilehandle;
begin
     result:=0;
     fpIn:=fileopen(filename,fmOPENREAD);
     if (fpIn <> INVALID_HANDLE_VALUE) then
        begin
             u32Lo:=windows.GetFileSize(fpIn,@u32Hi);
             if (GetLastError()=NO_ERROR) then
                begin
                     result:=u32Lo + (unsigned64(u32Hi) shl 32);
                end;
             closehandle(fpIn);
        end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Dateiversion lesen
function GetFileVersion(path : longstring) : longstring;
var
   nZero,nSize : cardinal;
   lpData      : pointer;
   p           : pointer;
begin
     result:='Unknown';
     nSize:=GetFileVersionInfoSize(pchar(path),nZero);
     if nSize>0 then
        begin
             getmem(lpData,nSize);
             if GetFileVersionInfo(pchar(path),0,nSize,lpData) then
             if VerQueryValue(lpData,'\',p,nZero) then
                begin
                     result:=inttostr(tVSFIXEDFILEINFO(p^).dwFileVersionMS shr 16)+'.'+
                     inttostr(tVSFIXEDFILEINFO(p^).dwFileVersionMS and $FFFF)+'.'+
                     inttostr(tVSFIXEDFILEINFO(p^).dwFileVersionLS shr 16)+'.'+
                     inttostr(tVSFIXEDFILEINFO(p^).dwFileVersionLS and $FFFF);
                end;
             freemem(lpData,nSize);
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Browse Dialog
////////////////////////////////////////////////////////////////////////////////////////////////
function BrowseDialog(const Title: longstring): longstring;
var lpItemID : PItemIDList;
    BrowseInfo : TBrowseInfo;
    DisplayName : array[0..MAX_PATH] of char;
    TempPath : array[0..MAX_PATH] of char;
begin
{$IFNDEF FPC}
     Result:='';
     FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
     with BrowseInfo do
         begin
          hwndOwner := 0;
          pszDisplayName := @DisplayName;
          lpszTitle := PChar(Title);
          ulFlags := BIF_RETURNONLYFSDIRS;
     end;
     lpItemID := SHBrowseForFolder(BrowseInfo);
     if lpItemId <> nil then
     begin
          SHGetPathFromIDList(lpItemID, TempPath);
          Result := Strcat(TempPath,'\');
          GlobalFreePtr(lpItemID);
     end;

     
     Result:=ExcludeTrailingPathDelimiter(Result);
     Result:=ExcludeTrailingPathDelimiter(Result);
{$ENDIF}
     Result:=IncludeTrailingPathDelimiter(Result);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
// MSec Millisekunden warten
procedure delay(msec:longint);
var
   start,stop:longint;
begin
     start := gettickcount;
     repeat
           stop := gettickcount;
           application.processmessages;
     until (stop - start ) >= msec;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Aus einer Verzeichnisstruktur alle Dateien löschen, auf die Filter passt. Und ggf.
//Die Verzeichnisse mitlöschen
function Deltree(Startpath:LongString):Boolean;
var
   SRF     : TSearchRec;
   OK      : Integer;
   s       : LongString;
begin
    startpath:=IncludeTrailingPathDelimiter(startpath);

     Result:=TRUE;
     try
     //Und nun die Verzeichnisse durchsuchen ?
     OK:=FindFirst(Startpath+'*.*',faANYFILE,SRF);
     while (OK=0) do
           begin
                if (SRF.Name[1]<>'.') then
                //Verzeichnis ? dann Scannen
                if ((SRF.Attr and faDirectory) = faDirectory) then
                   begin
                        s:=IncludeTrailingPathDelimiter(Startpath+SRF.Name);
                        DelTree(s);
                        Result:=Result and RemoveDir(s);
                   end
                else
                   begin
                        //Ansonsten Dateien löschen
                        DelData(StartPath+SRF.Name);
                   end;
                OK:=FindNext(SRF);
           end;
        FindClose(SRF);
        RemoveDir(StartPath);
     except
     end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
// Laufwerke als Stringliste zurückgeben, die im System zu finden sind
function EnumerateDrives(Filter:UInt=DRIVE_ALL):TStringList;
var
   x,y,z : Cardinal;
   s     : Longstring;
begin
     Result:=TStringList.Create;

     //Bitmuster der Laufwerke lesen
     z:=GetLogicalDrives();
     x:=1;
     y:=0;
     //Alle Buchstaben durchsuchen
     while (y < 27) do
           begin
                //Ein Laufwerk gefunden ?
                if (z and x)=x then
                   begin
                        //Pfad auf die harte Tour zusammenbasteln
                        s:=Chr(65+y)+':\';

                        //Filter ?
                        if (FILTER = DRIVE_ALL) then
                           begin
                                Result.Add(s);
                           end
                        else
                            begin
                                 //Ja => Nur Laufwerke von unserem Typ nehmen
                                 if  ( GetDriveType(PCHar(s)) = Filter) then
                                     begin
                                          //Hinzufügen
                                          Result.Add(s);
                                     end;
                            end;

                   end;
           //Nächsten Binärwert holen
           x:=x shl 1;
           //Nächsten Bchstaben ansprechen
           inc(y);
      end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Startverzeichnis (und evtl Unterverzeichnisse) nach Dateien durchsuchen, auf die Filter passt
//Hide bezeichnet ein Zeichen, der, wenn er am Anfang des Namens steht ignoriert wird (z.B: '.');
function FileScan(StartPath,Filter:LongString;Hide:LongString;SubDir:integer):TStringList;
var
   SRF     : TSearchRec;
   OK      : Integer;
   s       : LongString;
begin
     //Vorbereitungen
     Result:=TStringlist.Create;
     Result.Clear;
     Startpath:=IncludeTrailingPathDelimiter(Startpath);

     //Erstmal alle Dateien holen
     OK:=FindFirst(Startpath+Filter,faANYFILE,SRF);
     while (OK=0) do
           begin
                if (SRF.Name[1]<>Hide) then
                   begin
                        if (SRF.Name[1]<>'.') then
                           begin
                                if ((SRF.Attr and faDirectory) <> faDirectory) then
                                   begin
                                        Result.Add(StartPath+SRF.Name);
                                   end
                                else
                                   begin
//                                        s:=AddBackSlash(Startpath+SRF.Name);
//                                        Result.Add(s);
                                   end;
                           end;
                   end;
                OK:=FindNext(SRF);
           end;
     FindClose(SRF);

     //Und nun die Verzeichnisse durchsuchen ?
     if (SubDir>0) then
        begin
             OK:=FindFirst(Startpath+'*.*',faANYFILE,SRF);
             while (OK=0) do
                   begin
                        if (SRF.Name[1]<>Hide) then
                           begin
                                if (SRF.Name[1]<>'.') then
                                   begin
                                        if ((SRF.Attr and faDirectory) = faDirectory) then
                                           begin
                                                s:=IncludeTrailingPathDelimiter(Startpath+SRF.Name);
                                                Result.AddStrings(FileScan(s,Filter,Hide,SubDir-1));
                                           end;
                                   end;
                           end;
                        OK:=FindNext(SRF);
                   end;
             FindClose(SRF);
        end;
     Result.Sort;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Startverzeichnis (und evtl Unterverzeichnisse) nach Verzeichnissen durchsuchen, auf die Filter passt
//Hide bezeichnet ein Zeichen, der, wenn er am Anfang des Namens steht ignoriert wird (z.B: '.');
function DirScan(StartPath,Filter:LongString;Hide:LongString;SubDir:integer):TStringList;
var
   SRF     : TSearchRec;
   OK      : Integer;
   s       : String;
begin
     //Vorbereitungen
     Result:=TStringlist.Create;
     Result.Clear;
     Startpath:=IncludeTrailingPathDelimiter(Startpath);

     //Erstmal alle Verzeichnisse holen
     OK:=FindFirst(Startpath+Filter,faANYFILE,SRF);
     while (OK=0) do
           begin
                if (SRF.Name[1]<>Hide) then
                   begin
                        if (SRF.Name[1]<>'.') then
                           begin
                                if ((SRF.Attr and faDirectory) <> faDirectory) then
                                   begin
//                                        Result.Add(StartPath+SRF.Name);
                                   end
                                else
                                   begin
                                        s:=IncludeTrailingPathDelimiter(Startpath+SRF.Name);
                                        Result.Add(s);
                                   end;
                           end;
                   end;
                OK:=FindNext(SRF);
           end;
     FindClose(SRF);

     //Und nun die Verzeichnisse durchsuchen ?
     if (SubDir>0) then
        begin
             OK:=FindFirst(Startpath+'*.*',faANYFILE,SRF);
             while (OK=0) do
                   begin
                        if (SRF.Name[1]<>Hide) then
                           begin
                                if (SRF.Name[1]<>'.') then
                                   begin
                                        if ((SRF.Attr and faDirectory) = faDirectory) then
                                           begin
                                                s:=IncludeTrailingPathDelimiter(Startpath+SRF.Name);
                                                Result.AddStrings(DirScan(s,Filter,Hide,SubDir-1));
                                           end;
                                   end;
                           end;
                        OK:=FindNext(SRF);
                   end;
             FindClose(SRF);
        end;
     Result.Sort;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
// Prüfen, ob ein Verzeichnis leer ist
function DirEmpty(Startpath:LongString):Boolean;
var
   SRF     : TSearchRec;
   OK      : Integer;
begin
     Result:=TRUE;
     try
     //Und nun die Verzeichnisse durchsuchen ?
     OK:=FindFirst(Startpath+'*.*',faANYFILE,SRF);
     while (OK=0) do
           begin
                //Was ausser den Traversen drin ?
                if (SRF.Name[1]<>'.') then
                   begin
                        OK:=-1;
                        Result:=FALSE;
                   end
                else
                   begin
                        OK:=FindNext(SRF);
                   end;
           end;
        FindClose(SRF);
     except
     end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Aus einer Verzeichnisstruktur alle Dateien löschen, auf die Filter passt. Und ggf.
//Die Verzeichnisse mitlöschen
function CleanDir(Startpath:LongString;Filter:LongString):Boolean;
var
   SRF     : TSearchRec;
   OK      : Integer;
   s       : LongString;
begin
     Result:=TRUE;
     try
     //Und nun die Verzeichnisse durchsuchen ?
     OK:=FindFirst(Startpath+'*.*',faANYFILE,SRF);
     while (OK=0) do
           begin
                if (SRF.Name[1]<>'.') then
                //Verzeichnis ? dann Scannen
                if ((SRF.Attr and faDirectory) = faDirectory) then
                   begin
                        s:=IncludeTrailingPathDelimiter(Startpath+SRF.Name);
                        DelTree(s);
                        Result:=Result and RemoveDirectory(PChar(s));
                   end
                else
                   begin
                        //Ansonsten Dateien löschen
                        //READONLY-Attribut wird automatisch entfernt
                        DelData(StartPath+SRF.Name);
                   end;
                OK:=FindNext(SRF);
           end;
        FindClose(SRF);
     except
     end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Ein Verzeichnis kpl. kopieren
function CopyDir(SourcePath,TargetPath:LongString;HWND:THandle):Boolean;
var
   FOS : TSHFileOpStruct;
begin
{$IFNDEF FPC}
     with FOS do
          begin
//               Wnd       :=HWND;
               wFunc     :=FO_COPY;
               //Die Dateinamen  müssen mit einer doppelten Null enden
               pFrom     :=PChar(SourcePath+#00+#00);
               pTo       :=PChar(TargetPath+#00+#00);
               fFlags    :=FOF_noConfirmMkDir;
          end;
     SHFileOperation(FOS);
     Result:=TRUE;
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Ein Verzeichnis kpl. verschieben
function MoveDir(SourcePath,TargetPath:LongString;HWND:THandle):Boolean;
var
   FOS : TSHFileOpStruct;
begin
{$IFNDEF FPC}
     with FOS do
          begin
//               Wnd       :=HWND;
               wFunc     :=FO_MOVE;
               //Die Dateinamen  müssen mit einer doppelten Null enden
               pFrom     :=PChar(SourcePath+#00+#00);
               pTo       :=PChar(TargetPath+#00+#00);
               fFlags    :=FOF_noConfirmMkDir;
          end;
     SHFileOperation(FOS);
     Result:=TRUE;
{$ENDIF}
end;



////////////////////////////////////////////////////////////////////////////////////////////////
/// Eine Datei XOR encoden
function CodeFile(Filename:LongString;CodeByte:Byte):Boolean;
var
   FP     : Integer;
   Puffer : Array[0..65535] of Byte;
   iTemp  : Integer;
   iTemp1 : Integer;
   iPos   : Cardinal;
begin
     Result:=FALSE;
     //Datei öffnen
     FP:=FileOpen(Filename,fmOPENREADWRITE);
     if (FP<>-1) then
        begin
             //Anfang suchen
             iPos:=0;
             FileSeek(FP,iPos,0);

             //Und los gehts
             repeat
                   iTemp:=SizeOf(Puffer);
                   iTemp:=FileRead(Fp,Puffer,iTemp);
                   if (iTemp>0) then
                      begin
                           //Alte Position anspringen
                           FileSeek(FP,iPos,0);

                           //Encoden
                           for iTemp1:=0 to SizeOf(Puffer)-1 do
                               begin
                                    Puffer[iTemp1]:=Puffer[iTemp1] xor CodeByte;
                               end;


                           //Und XOR-Daten drüberschreiben
                           FileWrite(FP,Puffer,iTemp);

                           //Positionszeiger anpassen
                           iPos:=iPos + Cardinal(iTemp);
                           Result:=TRUE;
                      end;
             until (iTemp<=0);
        end;
     FileClose(FP);
end;


end.
