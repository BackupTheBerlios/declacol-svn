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
//////////////////////////////////////////////////////////////////////////////////////////
///
/// Bildschirmfunktionen (Auflösung ändern etc)
///
//////////////////////////////////////////////////////////////////////////////////////////

unit Unit_ScreenFunctions;

interface
uses Graphics,Windows;

type TScreenMode = Record
     sText : String;
     dwX   : LongWord;
     dwY   : LongWord;
     wBits : Word;
     pPixF : TPixelFormat;
     wFrq  : Word;
end;

Type
  TPOINTL = Packed Record
    x : DWORD;
    y : DWORD;
  End;
  TPrinterMonitor = Packed Record
    Case Integer Of
      0 : (Paper: Packed Record
            dmOrientation: SHORT;
            dmPaperSize: SHORT;
            dmPaperLength: SHORT;
            dmPaperWidth: SHORT;
            dmScale: SHORT;
            dmCopies: SHORT;
            dmDefaultSource: SHORT;
            dmPrintQuality: SHORT;
          End;);
      1 : (Monitor: Packed Record
            dmPosition : TPOINTL;
            dmDisplayOrientation : DWORD;
            dmDisplayFixedOutput : DWORD;
           End;)
  End;
  TDevModeA_new = packed record
    dmDeviceName: array[0..CCHDEVICENAME - 1] of AnsiChar;
    dmSpecVersion: Word;
    dmDriverVersion: Word;
    dmSize: Word;
    dmDriverExtra: Word;
    dmFields: DWORD;

    /// !!!
    Dual : TPrinterMonitor;
    /// !!!

    dmColor: SHORT;
    dmDuplex: SHORT;
    dmYResolution: SHORT;
    dmTTOption: SHORT;
    dmCollate: SHORT;
    dmFormName: array[0..CCHFORMNAME - 1] of AnsiChar;
    dmLogPixels: Word;
    dmBitsPerPel: DWORD;
    dmPelsWidth: DWORD;
    dmPelsHeight: DWORD;
    dmDisplayFlags: DWORD;
    dmDisplayFrequency: DWORD;
    dmICMMethod: DWORD;
    dmICMIntent: DWORD;
    dmMediaType: DWORD;
    dmDitherType: DWORD;
    dmICCManufacturer: DWORD;
    dmICCModel: DWORD;
    dmPanningWidth: DWORD;
    dmPanningHeight: DWORD;
  end;

function ChangeDisplay(Mode:TScreenMode):Boolean;
function CountDisplayModes():Integer;
function EnumDisplayModes(var Modes:Array of TScreenMode):Boolean;
function GetCurrentScreenMode():TScreenMode;
function BitsToPixelFormat(Bits:Byte):TPixelFormat;

procedure RefreshWallPaper();
procedure SetWallPaper(FileName:String);
implementation

uses Messages,SysUtils;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
function ChangeDisplay(Mode:TScreenMode):Boolean;
var
   Dmode:TDevMode;
   z:integer;
begin
     //Alle Displaymodes nach der gesuchten Auflösung durchsuchen
     Result:=FALSE;
     z:=0;
     DMode.dmSize:=SizeOf(DMode);
     With Mode do
     while (EnumDisplaySettings(nil,z,DMode)=TRUE) and (Result=FALSE) do
           begin
                //Unser Mode dabei ?
                if (DMode.dmPelsWidth=dwX) then
                   if (DMode.dmPelsHeight=dwY) then
                      if (DMode.dmBitsPerPel=wBits) then
                         if (DMode.dmDisplayFrequency=wfrq) or (wfrq=0) then
                            begin
                                 DMode.dmSize:=SizeOf(DMode);
                                 DMode.dmDisplayFrequency:=wfrq;

                                 if (ChangeDisplaySettings(DMode,0)=DISP_CHANGE_SUCCESSFUL) then
                                    begin
                                         PostMessage(HWND_BROADCAST,WM_DISPLAYCHANGE,wParam(wfrq),LOWord(LParam(dwX))+HIWord(LParam(dwY)));
                                         Result:=TRUE;
                                    end;
                            end;
                inc(z);
           end;
     RefreshWallpaper;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
function CountDisplayModes():Integer;
var z :integer;
    D :TDevMode;
begin
     z:=0;
     while (EnumDisplaySettings(nil,z,D)=TRUE) do inc(z);
     Result:=z+1;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
function EnumDisplayModes(var Modes:Array of TScreenMode):Boolean;
var z :integer;
    D :TDevMode;
    i : integer;
begin
     Result:=TRUE;
     z:=0;
     while (EnumDisplaySettings(nil,z,D)=TRUE) do
           begin
                with Modes[z] do
                     begin
                          dwX   :=D.dmPelsWidth;
                          dwY   :=D.dmPelsHeight;
                          wBits :=D.dmBitsPerPel;
                          wFrq  :=D.dmDisplayFrequency;
                          sText:=IntToStr(dwX)+'x'+IntToStr(dwY)+'x'+IntToStr(wBits)+' ('+IntToStr(wFrq)+' Hz)';
                     end;
                inc(z);
           end;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
function GetCurrentScreenMode():TScreenMode;
var
   DC:THandle;
begin
     //Grafik-Modus holen
     DC:=GetDC(0);
     Result.dwX  :=GetDeviceCaps(DC,HORZRES);
     Result.dwY  :=GetDeviceCaps(DC,VERTRES);
     Result.wBits:=GetDeviceCaps(DC,BITSPIXEL);
     Result.wFrq :=GetDeviceCaps(DC,VREFRESH);
     Result.pPixF:=BitsToPixelFormat(Result.wBits);
end;


function BitsToPixelFormat(Bits:Byte):TPixelFormat;
         begin
              Result:=pf32Bit;
             Case Bits of
                  1 : Result:=pf1Bit;
                  4 : Result:=pf4Bit;
                  8 : Result:=pf8Bit;
                  15: Result:=pf15Bit;
                  16: Result:=pf16Bit;
                  24: Result:=pf24Bit;
                  32: Result:=pf32Bit;
             end;
         end;


////////////////////////////////////////////////////////////////////////////////////////////////
procedure RefreshWallPaper();
begin
     SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil,SPIF_UPDATEINIFILE);
end;


procedure SetWallPaper(FileName:String);
begin
     SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(FileName),SPIF_UPDATEINIFILE or SPIF_SENDCHANGE);
end;


////////////////////////////////////////////////////////////////////////////////////////////////
procedure MonitorOff();
begin
    SendMessage(HWND_BROADCAST, WM_SYSCOMMAND, SC_MONITORPOWER, 2);
end;

procedure MonitorOn();
begin
    SendMessage(HWND_BROADCAST, WM_SYSCOMMAND, SC_MONITORPOWER, -1);
end;

end.
