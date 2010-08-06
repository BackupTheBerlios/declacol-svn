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
unit Unit_Filesystem;
//////////////////////////////////////////////////////////////////////////////////////////
///
/// Dateisystemfunktionen
///
/// (c) 2006 Borg@Sven-of-Nine.de
///
//////////////////////////////////////////////////////////////////////////////////////////

interface
uses
    Unit_TypeDefs,
    {$IFDEF WIN32}
    windows
    {$ENDIF}
    ;

const
     //Datenpuffergröße für Dateifunktionen
     FS_MAX_BUFFER = 1024*1024;

//Struct der Filetimeeinträge
type TFiletimeStruct = record
     Creation     : TIMESTAMP;
     LastAccess   : TIMESTAMP;
     LastWrite    : TIMESTAMP;
end;

//Ein paar Dateidatumsfunktionen
function  FS_GetFileTimes(Filename:LongString):TFileTimeStruct;
function FS_SetFileTime(FileName: LongString; NewDateTime: TDateTime): Boolean;

//Dateifunktionen
function FS_CopyFile(Source,Target:LongString):Boolean;

//Eine Temporäre Datei erzeugen und den Handle zurückgeben
function FS_CreateTempFile(u64Size:unsigned64;var Filename : Longstring):THandle;

//Verzeichnisfunktionen
function FS_IsDirectory(sPath:LongString)      :Boolean;
function FS_ForceDirectories(sPath:LongString) :Boolean;


implementation
uses
{$IFDEF LINUX}
    unix,
{$ENDIF}
{$IFDEF UNIX}
    unix,
{$ENDIF}
    Sysutils,
    Unit_MultiPlatform,
    Unit_Strings
;

//////////////////////////////////////////////////////////////////////////////////////////
//Dateidatumsstruktur laden
function  FS_GetFileTimes(Filename:LongString):TFileTimeStruct;
var
   FileHandle : THandle;
begin
  FileHandle := FileOpen(FileName, fmOpenRead);
  if (FileHandle <> INVALID_HANDLE_VALUE) then
    begin
      GetFileTime(Filehandle,PFileTime(result.Creation),PFileTime(result.LastAccess),PFileTime(result.lastwrite));
      CloseHandle(Filehandle);
    end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
//Dateidatumsstruktur schreiben
function FS_SetFileTime(FileName: LongString; NewDateTime: TDateTime): Boolean;
var
   FileHandle : Integer;
   FileTime   : TFileTime;
   LFT        : TFileTime;
   LST        : TSystemTime;
begin
     result := False;
     try
        DecodeDate(NewDateTime, LST.wYear, LST.wMonth, LST.wDay);
        DecodeTime(NewDateTime, LST.wHour, LST.wMinute, LST.wSecond, LST.wMilliSeconds);

        if SystemTimeToFileTime(LST, LFT) then
           begin
                if LocalFileTimeToFileTime(LFT, FileTime) then
                   begin
                        FileHandle := FileOpen(FileName, fmOpenReadWrite or fmShareExclusive);
                        result:=SetFileTime(FileHandle, nil, nil, @FileTime);
                   end;
           end;
     finally
        FileClose(FileHandle);
     end;
end;
//////////////////////////////////////////////////////////////////////////////////////////
// Prüfen, ob ein Verzeichnis existiert
function FS_IsDirectory(sPath:LongString):Boolean;
begin
     result:=FALSE;
end;

//////////////////////////////////////////////////////////////////////////////////////////
//Alle Verzeichnisse eines Pfades erzeugen (sofern diese nicht schon existieren)
function FS_ForceDirectories(sPath:LongString):Boolean;
var
   pStart : PChar;
   pFound : PChar;
   lwPos  : LongWord;
   sDir   : LongString;
   sSlash : LongString;
begin
     Result:=FALSE;

     //Slash des Datisystems holen
     sSlash:=Multiplatform_GetSlash();


     //Backslash entfernen
     sPath:=String_Remove(sPath,sSlash);

     //Laufwerk holen
     sDir:=String_Append(ExtractFileDrive(sPath),sSlash);

     //Dummheiten abfangen
     if ( Length(sDir) < 3 ) then Exit;
     if ( sPath='')          then Exit;

     //Wenn das Verzeichnis schon besteht, dann TRUE zurückgeben
{
     if (CL_IsDirectory(sPath)) then
        begin
             result:=TRUE;
             Exit;
        end;
}
     //Zeiger holen
     pStart:=Addr(sPath[1]);

     //Den ersten Backslash suchen
     pFound:=StrPos(pStart,'\');

     //Und alle Teile des Strings durcharbeiten
     while (pFound<>nil) do
           begin
                inc(pFound);
                pStart:=pFound;
                pFound:=StrPos(pStart,'\');

                //Stringlänge bestimmen
                if (pFound<>nil) then
                   begin
                        lwPos:=pFound-pStart;
                   end
                else
                   begin
                        lwPos:=StrLen(pStart);
                   end;

                //Pfad stufenweise zusammenkopieren
                sDir:=sDir+String_Append(Copy(String(pStart),1,lwPos),'\');

                //Verzeichnis erzeugen
                Result:=CreateDir(sDir);
           end;

end;


//////////////////////////////////////////////////////////////////////////////////////////
// Eine Datei so kopieren, daß Windows die Möglichkeit hat, diese unframentiert abzulegen
function FS_CopyFile(Source,Target:String):Boolean;
{$IFDEF LINUX}
begin
     result:=FALSE;
end;
{$ENDIF}

{$IFDEF WIN32}
//Windowsversion des Kopierers
var
   fpIn     : signed32;
   fpOut    : signed32;
   buf      : array of Byte;
   u32SLow  : unsigned32;
   u32SHi   : unsigned32;
   s64Size  : signed64;
begin
     result:=FALSE;
     fpOut:=-1;

     fpIn :=FileOpen(ExpandFileName(Source),fmOPENREAD);
     if (fpIn<>-1) then
        begin
             //Den Zielpfad erzwingen
             FS_ForceDirectories(ExtractFilePath(ExpandFilename(Target)));

             //Zieldatei erzeugen
             fpOut :=FileCreate(ExpandFileName(Target));
             if (fpOut<>-1) then
                begin
                     //Größe initialisieren
                     s64Size:=High(signed64);

                     //Größe der Quelle holen
                     SetLastError(NO_ERROR);
                     u32SLow:=GetFileSize(fpIn,@u32SHi);

                     //Kein Fehler ?
                     if (GetLastError()=NO_ERROR) then
                        begin
                             s64Size:=u32SLow +  (u32SHi shl 16);
                             //Größe des Zieles setzen, um undefragmentiert zu kopieren
                             FileSeek(fpOut,s64Size,0);
                             SetEndOfFile(fpOut);
                        end;

                     //An der Anfang der Datei springen
                     FileSeek(fpOut,0,0);

                     //Puffer initialisieren
                     if (s64Size>FS_MAX_BUFFER) then
                        begin
                             SetLength(buf,FS_MAX_BUFFER);
                        end
                     else
                        begin
                             SetLength(buf,s64Size+512);
                        end;

                     //Und kopieren
                     repeat
                           //Lesen
                           u32SLow:=FileRead(fpIn,buf[0],Length(Buf));

                           //Schreiben
                           if (u32SLow>0) then
                              begin
                                   FileWrite(fpOut,buf[0],u32SLow);
                                   result:=TRUE;
                              end;
                     until (u32SLow=0);
                end;
        end;
     if (fpIn <>-1) then CloseHandle(fpIn);
     if (fpOut<>-1) then CloseHandle(fpOut);
     //Puffer freimachen
     SetLength(buf,0);
end;
{$ENDIF}


function FS_CreateTempFile(u64Size:unsigned64;var Filename : Longstring):THandle;
{$IFDEF LINUX}
begin
     result:=FALSE;
end;
{$ENDIF}

{$IFDEF WIN32}
var
   TempPath : Array[0..MAX_PATH] of Char;
   TempFile : Array[0..MAX_PATH] of Char;
   Buff     : Array[0..1023]     of Byte;
   u32Lo    : unsigned32;
   u32Hi    : unsigned32;
begin
     FillChar(TempFile[0],MAX_PATH,#00);
     FillChar(TempPath[0],MAX_PATH,#00);
     FillChar(Buff[0]    ,Length(Buff),#00);

     //Temp-Pfad holen
     GetTempPath(MAX_PATH,@TempPath[0]);

     //TempFilename erzeugen
     GetTempFileName(@TempPath[0],'~',0,@TempFile[0]);

     Filename:=TempFile;

     //Und die Datei öffenen
     result:=CreateFile ( @TempFile[0],
                          GENERIC_READ or GENERIC_WRITE,
                          FILE_SHARE_READ or FILE_SHARE_WRITE,
                          0,
                          OPEN_EXISTING or CREATE_ALWAYS,
                          FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE,
                          0);

     //Dateimit Nullen initialisieren
     if (result <> INVALID_HANDLE_VALUE) then
        begin
             u32Lo:=unsigned32 ( u64Size div Length(Buff) );

              while (u32Lo > 0) do
                   begin
                        FileWrite(result,Buff[0],Length(Buff));
                        dec(u32Lo);
                   end;
             SetFilePointer(result,0,0,FILE_BEGIN);
        end;
end;
{$ENDIF}

end.

