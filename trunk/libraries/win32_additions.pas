unit win32_additions;
////////////////////////////////////////////////////////////////////////////////
///
/// Diverse Definitionen, die in den normalen Wind23-Headern fehlen
///
////////////////////////////////////////////////////////////////////////////////
interface
uses Windows,SysUtils;

const
     ntdll    = 'ntdll.dll';
     kernel32 = 'Kernel32.dll';


////////////////////////////////////////////////////////////////////////////////
//Win32 Funktionen
////////////////////////////////////////////////////////////////////////////////
function OpenThread ( dwDesiredAccess : Cardinal; bInheritedHandle:Boolean; dwThreadID : Cardinal):THandle; stdcall; external kernel32 name 'OpenThread';


////////////////////////////////////////////////////////////////////////////////
/// Hilfsfunktionen
////////////////////////////////////////////////////////////////////////////////
//Security-Token anpassen
function AdJustToken(TokenName:String): Boolean;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

function AdJustToken(TokenName:String): Boolean;
var
   TTokenHd: THandle;
   TTokenPvg: TTokenPrivileges;
   cbtpPrevious: DWORD;
   rTTokenPvg: TTokenPrivileges;
   pcbtpPreviousRequired: DWORD;
   tpResult: Boolean;
begin
     result:=FALSE;
     //Nur WinNT
   if Win32Platform = VER_PLATFORM_WIN32_NT then
      begin
           //Token öffnen
           tpResult := OpenProcessToken(GetCurrentProcess(),TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,TTokenHd) ;

           //OK ?
           if tpResult then
              begin
                   //Token greifen
                   tpResult := LookupPrivilegeValue(nil,PChar(TokenName),TTokenPvg.Privileges[0].Luid) ;
                   //Und dieses Token auf aktiv setzen
                   TTokenPvg.PrivilegeCount := 1;
                   TTokenPvg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
                   cbtpPrevious := SizeOf(rTTokenPvg) ;
                   pcbtpPreviousRequired := 0;

                   //Token anpassen
                   if tpResult then
                      begin
                           result:=Windows.AdjustTokenPrivileges ( TTokenHd,
                                                                   False,
                                                                   TTokenPvg,
                                                                   cbtpPrevious,
                                                                   rTTokenPvg,
                                                                   pcbtpPreviousRequired) ;
                      end;
              end;
      end;
end;


end.
