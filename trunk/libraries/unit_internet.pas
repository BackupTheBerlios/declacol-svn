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
/////////////////////////////////////////////////////////////////////////////////
///
/// Einen String oder eine Datei per http laden
///
/// Internet Unit
///
/// (c) 2004 Borg@Sven-of-Nine.de
///
/// v 0.2.1
///
/////////////////////////////////////////////////////////////////////////////////
unit Unit_Internet;
interface
uses WinInet,Windows,SysUtils;

function HTTPGetString (URL,User,Pass,Referrer,UserAgent:String;var Output:String;MaxSize:integer=0;Port : Integer=INTERNET_DEFAULT_HTTP_PORT):Boolean;
function HTTPGetFile   (URL,User,Pass,Referrer,UserAgent:String;    Filename:String;Port : Integer=INTERNET_DEFAULT_HTTP_PORT):Boolean;


implementation

/////////////////////////////////////////////////////////////////////////////////
function SearchAndReplace (sSrc, sLookFor, sReplaceWith : string) : string;
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


/////////////////////////////////////////////////////////////////////////////////
function ExtractURL (Input:String):String;
begin
     //Alle Slashes richtigsetzen
     Input:=SearchAndReplace(Input,'\','/');
     //HTTP löschen
     Input:=SearchAndReplace(Input,'http://','');

     //Und nun die URL holen
     Result:=Copy(Input,1,Pos('/',Input)-1);
end;

/////////////////////////////////////////////////////////////////////////////////
function ExtractFile(Input:String):String;
begin
     //Alle Slashes richtigsetzen
     Input:=SearchAndReplace(Input,'\','/');
     //HTTP löschen
     Input:=SearchAndReplace(Input,'http://','');

     //Und nun die URL holen
     Result:=Copy(Input,Pos('/',Input),Length(Input));
end;

/////////////////////////////////////////////////////////////////////////////////
{
function is_IP(IP:String):Boolean;
begin
     Result:=FALSE;

     //Länge OK ?
     if (Length(IP)>16) then Exit;
     if (IP='')         then Exit;

     //Vier Zahlen ?
     if (Explode(IP,'.').Count<>4) then Exit;

     Result:=TRUE;
end;
}

/////////////////////////////////////////////////////////////////////////////////
function HTTPGetString (URL,User,Pass,Referrer,UserAgent:String;var Output:String;MaxSize:integer=0;Port : Integer=INTERNET_DEFAULT_HTTP_PORT):Boolean;
var
   AccTypes  : String;
   TAccTypes : LPSTR;
   hSession  : HInternet;
   hConnect  : HInternet;
   hRequest  : HInternet;

   PufferSize: Longword;
   Puffer    : Array[0..1024]of Char;
begin
     Result:=FALSE;
     Output:='';
     try
     AccTypes:='*/*'+Chr(13)+Chr(13);
     URL:=URL+Chr(0);
     //Internet offen
     hSession:=InternetOpen (PChar(UserAgent),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
     if (hSession<>nil) then
        begin
             //Connect zum Server ?
             hConnect:=InternetConnect(hSession,PChar(ExtractURL(URL)),Port,PChar(User),PChar(Pass),INTERNET_SERVICE_HTTP,0,0);
             if (hConnect<>nil) then
                begin
                     //Request starten
                     TAccTypes:=PChar(AccTypes);
                     hRequest:=HTTPOpenRequest(hConnect,'GET',PChar(ExtractFile(URL)),'HTTP/1.0',PChar(Referrer),@TAccTypes,INTERNET_FLAG_RELOAD,0);
                     if (hRequest<>nil) then
                        begin
                             //Und die Anfrage abschicken
                             if (HttpSendRequest(hRequest,nil,0,nil,0)=TRUE) then
                                begin
                                     PufferSize:=1;
                                     while (InternetReadFile(hRequest,@Puffer,SizeOf(Puffer),PufferSize)) and (PufferSize<>0) do
                                           begin
                                                Output:=Output+Copy(String(Puffer),0,PufferSize);

                                                //MaxSize abfangen
                                                if (Length(Output)>MaxSize) and (MaxSize<>0) then
                                                   begin
                                                        PufferSize:=0;
                                                        Output:=Copy(Output,1,MaxSize);
                                                   end;
                                           end;
                                     if (Length(Output)>0) then Result:=TRUE;
                                end;
                             InternetCloseHandle(hRequest);
                        end;
                     InternetCloseHandle(hConnect);
                end;
             InternetCloseHandle(hSession);
        end;
     except
     end;
end;


/////////////////////////////////////////////////////////////////////////////////
function HTTPGetFile (URL,User,Pass,Referrer,UserAgent:String; Filename:String;Port : Integer=INTERNET_DEFAULT_HTTP_PORT):Boolean;
var
   AccTypes  : String;
   TAccTypes : LPSTR;
   hSession  : HInternet;
   hConnect  : HInternet;
   hRequest  : HInternet;

   PufferSize: Longword;
   Puffer    : Array[0..1024]of Char;

   FP        : Integer;
begin
     Result:=FALSE;
     try
     AccTypes:='*/*';

     //Internet offen
     hSession:=InternetOpen   (PChar(UserAgent),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
     if (hSession<>nil) then
        begin
             //Connect zum Server ?
             hConnect:=InternetConnect(hSession,PChar(ExtractURL(URL)),Port,PChar(User),PChar(Pass),INTERNET_SERVICE_HTTP,0,0);
             if (hConnect<>nil) then
                begin
                     //Request starten
                     TAccTypes:=PChar(AccTypes);
                     hRequest:=HTTPOpenRequest(hConnect,'GET',PChar(ExtractFile(URL)),'HTTP/1.0',PChar(Referrer),@TAccTypes,INTERNET_FLAG_RELOAD,0);
                     if (hRequest<>nil) then
                        begin
                             //Und die Anfrage abschicken
                             if (HttpSendRequest(hRequest,nil,0,nil,0)=TRUE) then
                                begin
                                     //Datei öffnen
                                     FP:=FileCreate(Filename);
                                     if (FP>-1) then
                                        begin
                                             //Und Antwort lesen
                                             PufferSize:=1;
                                             while (InternetReadFile(hRequest,@Puffer,SizeOf(Puffer),PufferSize)) and (PufferSize<>0) do
                                                   begin
                                                        if (PufferSize<>0) then FileWrite(FP,Puffer,PufferSize);
                                                   end;
                                             Result:=TRUE;
                                        end;
                                     FileClose(FP);
                                end;
                             InternetCloseHandle(hRequest);
                        end;
                     InternetCloseHandle(hConnect);
                end;
             InternetCloseHandle(hSession);
        end;
     except
     end;
end;

end.
