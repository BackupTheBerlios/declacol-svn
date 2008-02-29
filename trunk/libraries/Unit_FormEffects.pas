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
unit Unit_FormEffects;
///////////////////////////////////////////////////////////////////////////////////////////////
//
// Diverse Effekt, um den Umgang mit Formularen simpler zu machen
//
// v 0.1
//
///////////////////////////////////////////////////////////////////////////////////////////////
interface
uses Windows,Graphics,Messages,Unit_ProcessFunctions,SysUtils;


    procedure SetTransparentForm(WindowHandle : THandle; AlphaValue : byte = 0;ColorCorrection:TColor=clBlack);
    procedure MakeWindowActive(wHandle: hWnd);
    procedure SetTaskBarAlpha(AValue:Byte=0);

    procedure FadeForm(WindowHandle : THandle; Speed:Integer=0;FadeIn:Boolean=FALSE);

    procedure HideFromTaskBar(Handle:THandle);

    function SaverActive():Boolean;
    function GetSaverHWND():Cardinal;
    function RunSaverAsDesktop(Path:String):Boolean;
    function StopSaver():Boolean;
    function ConfigSaver(Path:String):Boolean;

    function PreviewActive():Boolean;
    function RunPreview(Path:String;Target:Cardinal):Boolean;
    function StopPreview():Boolean;

type TSaverEntry = Record
     Name     : String;
     FileName : String;
     end;
///////////////////////////////////////////////////////////////////////////////////////////////
implementation
const
     WS_EX_LAYERED = $80000;
     LWA_COLORKEY = 1;
     LWA_ALPHA    = 2;
type
 TSetLayeredWindowAttributes = function (
     hwnd : HWND;         // Handle des Fensters
     crKey : TColor;      // Farbkorrektur
     bAlpha : byte;       // Transparenzwert
     dwFlags : DWORD      // Aktion
     ): BOOL; stdcall;

var
   ActiveSaverID:Cardinal=0;
   ActivePreviewID:Cardinal=0;

///////////////////////////////////////////////////////////////////////////////////////////////
function SaverActive():Boolean;
begin
     if (ActiveSaverID>0) then Result:=TRUE else Result:=FALSE;
end;

function PreviewActive():Boolean;
begin
     if (ActivePreviewID>0) then Result:=TRUE else Result:=FALSE;
end;

//Holt den aktuellen Desktop-Handle
function GetSaverHWND():Cardinal;
var
   a,b,c,d : THandle;
begin
     a:=FindWindow('Progman', nil);
     b:=FindWindowEx(a, 0, 'SHELLDLL_DefView', nil);
     c:=FindWindowEx(b, 0, 'SysListView32', nil);
     d:=FindWindowEx(b, 0, 'Internet Explorer_Server', nil);
     if (d>0) then Result:=d else Result:=c;
end;


///////////////////////////////////////////////////////////////////////////////////////////////
function RunSaverAsDesktop(Path:String):Boolean;
var
   a,b,c,d      : THandle;
   Target       : THandle;
begin
     Result:=FALSE;

     if (ActiveSaverID<>0) then StopSaver;

     a:=FindWindow('Progman', nil);
     b:=FindWindowEx(a, 0, 'SHELLDLL_DefView', nil);
     c:=FindWindowEx(b, 0, 'SysListView32', nil);
     d:=FindWindowEx(b, 0, 'Internet Explorer_Server', nil);

     if (d>0) then Target:=d else Target:=c;

     ActiveSaverID:=Execute(Path,'/p '+IntToStr(Target),FALSE);
     if (ActiveSaverID>0) then
        begin
             Result:=TRUE;
        end;
end;


///////////////////////////////////////////////////////////////////////////////////////////////
function StopSaver():Boolean;
var
   hnd  : THandle;
begin
     Result:=FALSE;

     if (ActiveSaverID=0) then Exit;

     hnd:=OpenProcess(PROCESS_ALL_ACCESS,FALSE,ActiveSaverID);
     if (hnd > 0) then
        begin
             TerminateProcess(hnd,0);
             Result:=TRUE;
        end;
     ActiveSaverID:=0;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
function ConfigSaver(Path:String):Boolean;
begin
     Result:=FALSE;
     if (Execute(Path,'/c',FALSE)>0) then
        begin
             Result:=TRUE;
        end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
function RunPreview(Path:String;Target:Cardinal):Boolean;
var
   hnd : Cardinal;
begin
     Result:=FALSE;

     if (ActivePreviewID<>0) then StopPreview;

     ActivePreviewID:=Execute(Path,'/P '+IntToStr(Target),FALSE);
     if (ActivePreviewID>0) then
        begin
             Result:=TRUE;
        end;
     //Und nun die Priorität des Screensavers auf IDLE schalten
     hnd:=OpenProcess(PROCESS_ALL_ACCESS,FALSE,ActivePreviewID);
     if (hnd > 0) then
        begin
             SetPriorityClass(hnd , IDLE_PRIORITY_CLASS);
             CloseHandle(hnd);
        end;

end;

///////////////////////////////////////////////////////////////////////////////////////////////
function StopPreview():Boolean;
var
   hnd  : THandle;
begin
     Result:=FALSE;

     if (ActivePreviewID=0) then Exit;

     hnd:=OpenProcess(PROCESS_ALL_ACCESS,FALSE,ActivePreviewID);
     if (hnd > 0) then
        begin
             TerminateProcess(hnd,0);
             Result:=TRUE;
        end;
     ActivePreviewID:=0;
end;


///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
procedure SetTransparentForm(WindowHandle : THandle; AlphaValue : byte = 0;ColorCorrection:TColor=clBlack);
var
 Info: TOSVersionInfo;
 SetLayeredWindowAttributes: TSetLayeredWindowAttributes;

 begin
 //Größer Windows ME
 Info.dwOSVersionInfoSize := SizeOf(Info);
 GetVersionEx(Info);
 if (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
 (Info.dwMajorVersion >= 5) then
   begin
     //User32-Zugriff erlangen
     SetLayeredWindowAttributes := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');

     //Funktion gefunden ?
      if Assigned(SetLayeredWindowAttributes) then
       begin
            //Sind wir Transparent ( Alpha < 255)?
            if (AlphaValue < 255) then
               begin
                    //Alphawert angegeben, dann setzen
                    SetWindowLong(WindowHandle, GWL_EXSTYLE, GetWindowLong(WindowHandle, GWL_EXSTYLE) or WS_EX_LAYERED);
                    //Make form transparent
                    SetLayeredWindowAttributes(WindowHandle, ColorCorrection, AlphaValue, LWA_ALPHA);
               end
            else
               begin
                    SetLayeredWindowAttributes(WindowHandle, clBlack, AlphaValue, LWA_ALPHA);
                    //Kein Alpha, dann erweiterte Attribute löschen
                    SetWindowLong(WindowHandle, GWL_EXSTYLE, GetWindowLong(WindowHandle, GWL_EXSTYLE) and (MaxInt xor WS_EX_LAYERED));
               end;
      end;
   end;

end;



///////////////////////////////////////////////////////////////////////////////////////////////
procedure MakeWindowActive(wHandle: hWnd);
begin
  if IsIconic(wHandle) then
    ShowWindow(wHandle, SW_RESTORE)
  else
    BringWindowToTop(wHandle);

  SetActiveWindow(wHandle);
  SetFocus(wHandle);
end;


///////////////////////////////////////////////////////////////////////////////////////////////
procedure SetTaskBarAlpha(AValue:Byte=0);
begin
     SetTransparentForm(FindWindow('Shell_TrayWnd',''),AValue);
end;


///////////////////////////////////////////////////////////////////////////////////////////////
procedure HideFromTaskBar(Handle:THandle);
begin
     ShowWindow(Handle,SW_HIDE);
end;

///////////////////////////////////////////////////////////////////////////////////////////////
procedure FadeForm(WindowHandle : THandle; Speed:Integer=0;FadeIn:Boolean=FALSE);
var
   start     : integer;
   direction : integer;
begin
     if (FadeIn) then
        begin
             start:=0;
             direction:=3;
        end
     else
        begin
             start:=255;
             direction:=-3;
        end;

     while (start<256) and (start > -1) do
           begin
                SetTransparentForm(WindowHandle,start);
                start:=start+direction;
                if (Speed>0) then Sleep(Speed);
                SendMessage(WindowHandle,WM_PAINT,0,0);
           end;

end;

end.
