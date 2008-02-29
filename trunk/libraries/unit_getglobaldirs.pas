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
unit Unit_GetGlobalDirs;

interface
uses Windows,Registry, SysUtils,Unit_StringFunctions;
/////////////////////////////////////////////////////////////////////////////////
///
/// v 0.1
///
///
///
///
///
///
/////////////////////////////////////////////////////////////////////////////////
function GetWinDir()    :String;
function GetTempDir()   :String;
function GetProgDir()   :String;
function GetSystemDir()   :String;
function GetHomeDir()   :String;
function GetWinVersion():Integer;
function GetWinVersionStr():String;

function GetCommand()   :String;


Const
WV_Unknown = -1;
WV_Win32s = 311;
WV_Win95A = 951;
WV_Win95B = 952;
WV_Win981 = 981;
WV_Win98SE= 982;
WV_Win9x  = 9;
WV_WinME  = 983;
WV_WinNT  = 1100;
WV_WinNT35= 1351;
WV_WinNT40= 1400;
WV_Win2K  = 2000;
WV_WinXP  = 2001;

implementation
/////////////////////////////////////////////////////////////////////////////////
function GetRegString(Path,Key:String):String;
var
   Reg:TRegistry;
begin
     Reg:=TRegistry.Create;
     Result:='';
     Reg.RootKey:=HKEY_LOCAL_MACHINE;
     if (Reg.OpenKeyReadOnly(Path)) then
        begin
             Result:=Reg.ReadString(Key);
             Reg.CloseKey;
        end;
     Reg.Free
end;


function GetEnvValue(Value:string)    :String;
var
   Buffer:Array[0..1024]of Char;
begin
     FillChar(Buffer,1024,0);
     GetEnvironmentVariable(PChar(Value),Buffer,1024);
     Result:=Buffer;
end;

/////////////////////////////////////////////////////////////////////////////////
function GetWinDir()   :String;
begin
     Result:=IncludeTrailingBackslash(LowerCase(GetEnvValue('windir')));
end;
/////////////////////////////////////////////////////////////////////////////////
function GetTempDir()   :String;
var
   TempPath : Array[0..MAX_PATH]of Char;
begin
     FillChar(TempPath,MAX_PATH + 1,#00);
     GetTempPath(MAX_PATH,@TempPath[0]);
     result:=TempPath;
end;

/////////////////////////////////////////////////////////////////////////////////
function GetSystemDir()   :String;
begin
     Result:=IncludeTrailingBackslash(LowerCase(GetEnvValue('SystemRoot')));
end;

/////////////////////////////////////////////////////////////////////////////////
function GetProgDir()   :String;
begin
     //Windows 2000 ?
     if (GetWinVersion>=2000) then
        begin
             Result:=IncludeTrailingBackslash(lowercase(GetEnvValue('ProgramFiles')));
        end
     else
        begin
             Result:=IncludeTrailingBackslash(lowercase(GetRegString('Software\Microsoft\Windows\CurrentVersion','ProgramFilesDir')));
        end;
end;

/////////////////////////////////////////////////////////////////////////////////
function GetHomeDir()   :String;
begin
     Result:=IncludeTrailingBackslash(lowercase(ExtractFilePath(ParamStr(0))));
end;


/////////////////////////////////////////////////////////////////////////////////
function GetWinVersion():Integer;
var
   Version:OSVersionInfo;
begin
     Result:=-1;
     Version.dwOSVersionInfoSize:=SizeOf(OSVersionInfo);
     if (GetVersionEx(Version)=FALSE) then Result:=WV_Unknown
     else
         begin
              Case Version.dwPlatFormID of
                   VER_PLATFORM_WIN32S: Result:=WV_WIN32s;

                   VER_PLATFORM_WIN32_WINDOWS:
                      Case Version.dwMinorVersion of
                           0: Result:=WV_Win95A;
                          10: Result:=WV_Win98SE;
                          90: Result:=WV_WinME;
                        else  Result:=WV_Win9x;
                      end;
                   VER_PLATFORM_WIN32_NT:
                      Case Version.dwMajorVersion of
                           3: Result:=WV_WinNT35;
                           4: Result:=WV_WinNT40;
                           5: Case Version.dwMinorVersion of
                                   0: Result:=WV_Win2k;
                                   1: Result:=WV_WinXP;
                                 else Result:=WV_WinNT;
                              end;
                      end;
              end;
         end;
end;


function GetWinVersionStr():String;
begin
     Case GetWinVersion of
          WV_Win32s  :Result:='Windows 311';
          WV_Win95A  :Result:='Windows 95A';
          WV_Win95B  :Result:='Windows 95B';
          WV_Win981  :Result:='Windows 98';
          WV_Win98SE :Result:='Windows 98SE';
          WV_Win9x   :Result:='Windows 9x';
          WV_WinME   :Result:='Windows ME';
          WV_WinNT   :Result:='Windows NT';
          WV_WinNT35 :Result:='Windows NT351';
          WV_WinNT40 :Result:='Windows NT40';
          WV_Win2K   :Result:='Windows 2000';
          WV_WinXP   :Result:='Windows XP';
          Else Result:='Unknown';
     end;
end;


/////////////////////////////////////////////////////////////////////////////////
function GetCommand()   :String;
begin
     Result:=IncludeTrailingBackslash(LowerCase(GetEnvValue('comspec')));
end;

end.
