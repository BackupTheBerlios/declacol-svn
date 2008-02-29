////////////////////////////////////////////////////////////////////////////////
///
/// Threadklasse zum Dateidownload
///
////////////////////////////////////////////////////////////////////////////////
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

unit class_download;
interface
uses unit_typedefs,classes,WinInet,Windows,SysUtils,FileCtrl;

type pDownload = ^rDownload;
     rDownload = record
     sURL     : Longstring;
     sTarget  : Longstring;
     u32Port  : unsigned32;
     u32Retry : unsigned32;
end;


type PDownloader = ^TDownloader;
     TDownloader = class(TThread)
     protected
              //Frei zu vergebende IDs
              u32ID       : unsigned32;
              pPartner    : Pointer;
              pSelf       : Pointer;

              //URL des Downloads
              sURL        : Longstring;
              u32Port     : unsigned32;
              //HTTP-Header
              sReferer    : Longstring;
              sAgent      : longstring;

              //Proxy ?
              sProxy      : Longstring;
              u32Proxy    : unsigned32;

              //Zugangsdaten, wenn notwendig
              sUser       : Longstring;
              sPassword   : Longstring;

              //Pfad zur Zieldatei
              sTargetPath : Longstring;
              sTargetName : Longstring;
              sTempName   : Longstring;

              //Größe der Datei
              u32Size     : unsigned32;

              //Schon gelesene Daten
              u32Load     : unsigned32;

              //Geschwindigkeit
              u32Speed    : unsigned32;
              u32Ticker   : unsigned32;

              //Fortschritt in Prozent
              u32Progress : unsigned32;

              //Download erfolgreich
              bDone       : Boolean;

              //HTTP-Status
              u32Status   : unsigned32;

              procedure Execute; override;
     private
              function SearchAndReplace (sSrc, sLookFor, sReplaceWith : string) : string;
              function ExtractURL       (Input:String):String;
              function ExtractFile      (Input:String):String;
              function GetHTTPInfo(hRequest:hInterNet;Query:unsigned32;Buffer:Pointer;var PufferSize:Cardinal):Boolean;

     public
              constructor Create();

              //Frei belegbare IDs
              property ID         : unsigned32 read u32ID       write u32ID;
              property Partner    : pointer    read pPartner    write pPartner;
              property MySelf     : pointer    read pSelf       write pSelf;

              //Die Downloaddaten
              property URL        : longstring read sURL        write sURL;
              property Port       : unsigned32 read u32Port     write u32Port;
              property Agent      : longstring read sAgent      write sAgent;
              property Referer    : longstring read sReferer    write sReferer;

              //Proxy
              property Proxy      : longstring read sProxy      write sProxy;
              property ProxyPort  : unsigned32 read u32Proxy    write u32Proxy;

              //Dateiname
              property TargetPath : longstring read sTargetPath write sTargetPath;
              property TargetName : longstring read sTargetName write sTargetName;
              property TempName   : longstring read sTempName   write sTempName;

              //Fortschritt
              property Filesize   : unsigned32 read u32Size;
              property Load       : unsigned32 read u32Load;
              property Progress   : unsigned32 read u32Progress;
              property Speed      : unsigned32 read u32Speed;

              //Kennwort (falls notwendig)
              property User       : longstring read sUser       write sUser;
              property Password   : longstring read sPassword   write sPassword;

              //HTTP-Status
              property Status     : unsigned32 read u32Status;
              //Done-Flag
              property Done       : Boolean    read bDone;
end;


implementation

////////////////////////////////////////////////////////////////////////////////
constructor TDownloader.Create();
begin
     Inherited Create(TRUE);
end;


////////////////////////////////////////////////////////////////////////////////
/// Hautpschleife für den Download
procedure TDownloader.Execute();
var
   hSession  : HInternet;
   hConnect  : HInternet;
   hRequest  : HInternet;

   PufferSize: Cardinal;
   Puffer    : Array[0..65535]of Char;
   sTemp     : Longstring;
   FP        : Integer;
begin
     Self.bDone:=FALSE;
     Self.u32Status:=0;
     Self.u32Load:=0;

     //Pfad richtig setzen
     Self.TargetPath:=IncludeTrailingBackslash(Self.TargetPath);
     ForceDirectories(Self.TargetPath);

     //Temporäre Daten setzen
     Self.sTempName:=Self.sTargetName + '.temp';

     try
     //Internet-Zugang anfragen
     if (Self.sProxy<>'') then
        begin
             sTemp:=Self.sProxy+':'+IntToStr(Self.u32Proxy);
             hSession:=InternetOpen   (PChar(Self.Agent),INTERNET_OPEN_TYPE_PROXY,PCHar(sTemp),nil,0);
        end
     else
        begin
             hSession:=InternetOpen   (PChar(Self.Agent),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
        end;

     if (hSession<>nil) then
        begin
             //Connect zum Server ?
             hConnect:=InternetConnect(hSession,PChar(ExtractURL(Self.URL)),Port,PChar(Self.User),PChar(Self.Password),INTERNET_SERVICE_HTTP,0,0);
             if (hConnect<>nil) then
                begin
                     //Request starten
                     hRequest:=HTTPOpenRequest(hConnect,PChar('GET'),PChar(ExtractFile(URL)),PChar('HTTP/1.0'),PChar(Referer),nil,INTERNET_FLAG_RELOAD,0);
                     if (hRequest<>nil) then
                        begin
                             //Und die Anfrage abschicken
                             if (HttpSendRequest(hRequest,nil,0,nil,0)) then
                                begin
                                     //Die Puffergröße anfragen
                                     PufferSize:=SizeOf(Puffer);
                                     Self.GetHTTPInfo(hRequest,HTTP_QUERY_STATUS_CODE,@Puffer[0],PufferSize);
                                     Self.u32Status:=StrToIntDef(Copy(Puffer,1,PufferSize),HTTP_STATUS_BAD_REQUEST);

                                     //Größe anfragen
                                     PufferSize:=SizeOf(Puffer);
                                     Self.GetHTTPInfo(hRequest,HTTP_QUERY_CONTENT_LENGTH,@Puffer[0],PufferSize);
                                     Self.u32Size:=StrToIntDef(Copy(Puffer,1,PufferSize),High(unsigned32));

                                     //Erkennung des Dateinamens
                                     PufferSize:=SizeOf(Puffer);
                                     if (Self.GetHTTPInfo(hRequest,HTTP_QUERY_CONTENT_DISPOSITION,@Puffer[0],PufferSize)) then
                                        begin
                                             sTemp:=String(Puffer);
                                             if (Pos('filename=',sTemp)>0) then
                                                begin
                                                     Self.sTargetName:=trim(copy(sTemp,Pos('filename=',sTemp)+length('filename='),length(sTemp)));
                                                end;
                                        end;

                                     //Status OK
                                     if (Self.u32Status<>HTTP_STATUS_NOT_FOUND) then
                                        begin
                                             //evtl. Vorhandene Dateien löschen
                                             DeleteFile(Self.TargetPath+Self.TempName);
                                             DeleteFile(Self.TargetPath+Self.TargetName);

                                             //Datei öffnen
                                             FP:=FileCreate(Self.TargetPath+Self.TempName);
                                             if (FP>-1) then
                                                begin
                                                     Self.u32Ticker:=GetTickCount();
                                                     //Und Antwort lesen
                                                     while (InternetReadFile(hRequest,@Puffer,SizeOf(Puffer),PufferSize)) and
                                                           (PufferSize<>0) and
                                                           (not Self.Terminated)
                                                           do
                                                           begin
                                                                if (PufferSize<>0) then
                                                                   begin
                                                                        FileWrite(FP,Puffer,PufferSize);
                                                                        //Geschwindigkeit ausrechnen
                                                                        u32Ticker:=(GetTickCount() - Self.u32Ticker) + 1;
                                                                        u32Speed :=( (PufferSize shl 10) div Self.u32Ticker);
                                                                        Self.u32Ticker:=GetTickCount();

                                                                        //Fortschritt anpassen
                                                                        inc(Self.u32Load,PufferSize);

                                                                        //Prozente bestimmen
                                                                        Self.u32Progress:=round(Self.u32Load * 100 / Self.u32Size);
                                                                   end;
                                                           end;
                                                  end;
                                             //Download OK
                                             FileClose(FP);
                                             bDone:=Self.u32Load >= Self.u32Size;
                                        end;
                                end;
                             InternetCloseHandle(hRequest);
                        end;
                     InternetCloseHandle(hConnect);
                end;
             InternetCloseHandle(hSession);
        end;
     except
     end;

     //Downloadstatus setzen
     Self.bDone:=(Self.u32Size>0) AND (Self.u32Size=Self.u32Load) AND (Not Self.Terminated);

     //Datei umbenennen
     if (Self.bDone) then
        begin
             renamefile(Self.TargetPath+Self.TempName,Self.TargetPath+Self.TargetName);
        end;

end;


////////////////////////////////////////////////////////////////////////////////
///
/// Interne Hilfsfunktion
///
////////////////////////////////////////////////////////////////////////////////
function TDownloader.SearchAndReplace (sSrc, sLookFor, sReplaceWith : string) : string;
var
    nPos        : integer;
    nLenLookFor : integer;
begin
  nPos        := Pos(sLookFor, sSrc);
  nLenLookFor := Length(sLookFor);

  while (nPos > 0) do
	begin
             Delete(sSrc, nPos, nLenLookFor);
             Insert(sReplaceWith, sSrc, nPos);
             nPos := Pos(sLookFor, sSrc);
        end;
  Result := sSrc;
end;

//Infodaten ziehen
function TDownloader.GetHTTPInfo(hRequest:hInterNet;Query:unsigned32;Buffer:Pointer;var PufferSize:Cardinal):Boolean;
var
   Index       : Cardinal;
   u32RealSize : Cardinal;
   Tracer      : ^Byte;
begin
     result:=FALSE;

     //Speicher cleanen
     Tracer:=Buffer;
     for u32RealSize:=0 to PufferSize -1 do
         begin
              Tracer^:=0;
              inc(Tracer);
         end;

     Index:=0;
     //Größe des angefragten Wertes holen
     u32RealSize:=0;
     HTTPQueryInfo(hRequest,Query,Buffer,u32RealSize,Index);

     //Wenn die benötigte Puffergröße kleiner als die wahre, können wir öffnen
     if (u32RealSize < PufferSize) then
        begin
             result:=HttpQueryInfo(hRequest,query,Buffer,u32RealSize,Index);
             PufferSize:=u32RealSize;
        end;
end;


/////////////////////////////////////////////////////////////////////////////////
function TDownloader.ExtractURL (Input:String):String;
begin
     //Alle Slashes richtigsetzen
     Input:=SearchAndReplace(Input,'\','/');
     //HTTP löschen
     Input:=SearchAndReplace(Input,'http://','');

     //Und nun die URL holen
     Result:=Copy(Input,1,Pos('/',Input)-1);
end;

/////////////////////////////////////////////////////////////////////////////////
function TDownloader.ExtractFile(Input:String):String;
begin
     //Alle Slashes richtigsetzen
     Input:=SearchAndReplace(Input,'\','/');
     //HTTP löschen
     Input:=SearchAndReplace(Input,'http://','');

     //Und nun die URL holen
     Result:=Copy(Input,Pos('/',Input),Length(Input));
end;
end.
