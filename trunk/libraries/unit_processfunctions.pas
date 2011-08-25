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

unit unit_processfunctions;
////////////////////////////////////////////////////////////////////////////////////////////////////
//  History
// 21.10.2005 Funktion SlowDow_SetCPU(Thread,Speed) eingeführt. Setzt direkt die Geschwindigkeit des Prozesses
// 20.10.2005 TThreadPack um den Eintrag Filename und Speed erweitert (wird nicht vom SlowDownProcess verändert)
// 13.02.2005 Funktion ProcessIDByFile und FileByProcessID auf lange Dateinamen umgestellt
// 12.02.2005 Funktion EnumerateProcesses gibt in den Prozesliste nun den kompletten Dateinamen an.
////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Prozess-Funktionen für Delphi
//
////////////////////////////////////////////////////////////////////////////////////////////////////
// SlowDown_Start(ProzID,Speed):TThreadPack   Bremst alle Threads des Prozesses mit der
//                                            entsprechenden ID auf Speed Prozent
//                                            CPU-Zeit herunter. Als Result wird ein
//                                            "Thread-Pack" zurückgegeben
//
// SlowDown_Stop(TThreadPack)                 Beendet die Bremse
//
// SlowDown_Faster(TThreadPack)               Erhöht die Geschwindigkeit
// SlowDown_Slower(TThreadPack)               Senkt die Geschwindigkeit
// SendCloseMessage(String)                   Sendet an alle Fenster mit dem Caption Text
//                                            eine Beenden-Nachricht
// SetPriority(Priority)                      Setzt die Priorität des eigenen Programmes
//
// ProzessIDByFile():LongWord                 Holt die ProzessID anhand des Dateinamens
// Function FirstChild(ProcID):ProcID         Findet den ersten Child-Prozess und gibt dessen
//                                            ID zurück
//
//
//WindowByProcessID(ProcID)                   Sucht einen WindowHandle zur gegebenen ProcessID
//
//Priority to Text                            Gibt den Namen einer Prioritätsklasse zurück
////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
interface
//uses Windows,SysUtils,Messages,TlHelp32,EliRT,psapi;
uses Unit_TypeDefs,Windows,SysUtils,Messages,TlHelp32,psapi,win32_additions;


CONST

//In Delphi nicht definierte Threadkonstanten
THREAD_SUSPEND_RESUME=2;

//Eigene Botschaften zur Threadsteuerung des SlowDownProzesse
//Steuerung des SlowDown Modus
USR_INCREASECPU = WM_APP + 100;           //Prozessorzeit des ChildThreads erhöhen
USR_DECREASECPU = WM_APP + 101;           //Prozessorzeit des ChildThreads verkleinern
USR_SETCPU      = WM_APP + 102;           //Setzt den Wert der Prozessorzeit direkt
USR_PAUSECPU    = WM_APP + 103;           //ChildThread anhalten
USR_STARTCPU    = WM_APP + 104;           //ChildThread starten

//Steuerung des SlowDownThreads
USR_PROCESSID   = WM_APP + 150;           //Übergibt dem Thread den Prozess der gebremst werden soll
USR_EXITTHREAD  = WM_APP + 200;           //SlowdownThread beendet sich

//Password-Konstanten
LOGON_WITH_PROFILE         = $00000001;
LOGON_NETCREDENTIALS_ONLY  = $00000002;
LOGON_ZERO_PASSWORD_BUFFER = DWORD($80000000);

     //Definition des "ThreadPacks"
type PThreadPack=^TThreadPack;
     TThreadPack=packed record
     Filename      : String;
     Speed         : integer;
     WindowHND     : LongWord;
     ThreadID      : LongWord;
     ThreadHD      : LongWord;
     ProcessID     : LongWord;
     ParentID      : Longword;
     FirstChild    : LongWord;
     CPUSpeed      : Integer;
end;

//Definition einer Processstruktur
type PProcessPack=^TProcessPack;
     TProcessPack=packed record
     ProcessID      : LongWord;
     ProcessPath    : String;
     NumberOfThread : Integer;
     ParentProcess  : LongWord;
     Window         : Boolean;
     WindowHandle   : LongWord;
     WindowCaption  : String;
     RAM            : unsigned32;
     RAMpeak        : unsigned32;
     PAGE           : unsigned32;
     PAGEpeak       : unsigned32;
end;

type

//Struktur für Prozessstarts
TSTARTUPINFOW = record
    cb: DWord;
    lpReserved: LPWSTR;
    lpDesktop: LPWSTR;
    lpTitle: LPWSTR;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: WORD;
    cbReserved2: WORD;
    lpReserved2: PBYTE;
    hStdInput: THANDLE;
    hStdOutput: THANDLE;
    hStdError: THANDLE;
  end;

//Struktur für Prozessstarts
 TCreateProcessWithLogOnW = function (Username                  : PWideChar;
                                      Domain                    : PWideChar;
                                      Password                  : PWideChar;
                                      LogOnFlag                 : LongWord;
                                      ApplicationName           : PWideChar;
                                      CommandLine               : PWideChar;
                                      CreationFlags             : LongWord;
                                      Environment               : Pointer;
                                      CurrentDir                : PWideChar;
                                const StartUpInfo               : TStartupInfoW;
                                var   ProcessInfo               : TProcessInformation
                                     ): BOOL; stdcall;
//Globale Variable die ale Watchdog von den Threads ständig geswitcht wird
var
   bThreadStatus :Boolean;
   lPerfCount    :TLargeInteger;

//Liste aller Processe im System
   ProcessList   :Array of TProcessPack;

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Funktionen, die den Umgang mit Prozessen erleichtern
////////////////////////////////////////////////////////////////////////////////////////////////////
function Execute(Source:String;Command:String;Wait:Boolean;Hidden:Boolean=FALSE):LongWord;
function ExecuteAsUser(User,Pass,Domain:WideString;Source,Command:WideString;Wait:Boolean;Hidden:Boolean=FALSE):LongWord;

//Priorität des aufrufenden Prozesses ändern
Procedure SetSelfPriority(priority:LongInt);

//Dateipfad anhand der ProzessID holen (0 = Fehler)
Function ProcessIDByFile(Filename:String):LongWord;

//Anhand der ProcessID den Dateinamen holen (''=Fehler)
Function FileByProcessID(ProcessID:LongWord):String;

//WindowHandle anhand der ProzessID holen (0=FEHLER)
Function WindowByProcessID(ProcessID:LongWord):LongWord;

//WindowHandle anhand des Dateinamens holen (0=Fehler)
Function WindowByFile(Filename:String):LongWord;

//Zählen wieviele Eltern-Fenster im System sind
Function CountParentWindows():Integer;

//Process durchforsten und Daten in PROCESSLIST ablegen
Function EnumerateProcesses()  :Boolean;

//Den Speicherverbrauch der eigenen Anwendung auf ein Minimum begrenzen
Procedure SetMinWorkMemory();

//Speicherbedarf eines Prozesses holen
function GetProcessMemorySize(ProcessID:LongWord):unsigned32;

//Ersten Childprozess des übergebenen Prozesses holen (0=Fehler)
Function FirstChild(ParentID:LongWord):LongWord;
//Elternprozess der übergebenen Prozesses holen (0=Fehler)
Function NextParent(ChildID:LongWord):LongWord;

///Allen Fensteren mit dem Title = Caption eine Close-Nachricht schicken
procedure SendCloseMessage(caption:string);

//Process mit der Pid = ID beenden (sanft bis brutal)
function KillProcess(ID:LongWord):Boolean;
function WindowsExit(RebootParam: Longword): Boolean;
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Funktionen des Slow-Down-Threads
////////////////////////////////////////////////////////////////////////////////////////////////////

Function SlowDown_Start(ProcessID:LongWord;CPUSpeed:Integer):TThreadPack;
Function SlowDown_Stop(Info:TThreadPack):Boolean;

Function SlowDown_Faster   (var Info:TThreadPack):Boolean;
Function SlowDown_Slower   (var Info:TThreadPack):Boolean;
Function SlowDown_SetSpeed (var Info:TThreadPack;CPUSpeed:Integer):Boolean;

Function SlowDown_Pause(Info:TThreadPack):Boolean;
Function SlowDown_Resume(Info:TThreadPack):Boolean;

Function SlowDown_Active(Info:TThreadPack):Boolean;
function SlowDownThread(Point:Pointer):LongWord;

////////////////////////////////////////////////////////////////////////////////////////////////////

implementation
uses SHlObj;
var
   //Einige Hilfsvariablen
   HWND   : LongWord;
   TempID : LongWord;


////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
//
//  Hier folgen die Slow-Downfunktionen inklusive des entsprechenden Threads
//
//
//
//
//
//
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////
//Einen Slowdown-Thread starten
function SlowDown_Start(ProcessID:LongWord;CPUSpeed:Integer):TThreadPack;
var
   z : Integer;
begin
     Result.CPUSpeed:=CPUSpeed;
     Result.ProcessID:=ProcessID;
     Result.ParentID:=ProcessID;
     Result.ThreadHD:=0;

     //Thread starten
     Result.ThreadHD:=CreateThread(nil,0,@SlowDownThread,nil,0,Cardinal(Result.ThreadID));
     if (Result.ThreadHD <> NULL) then
        begin
             //Warten bis der Thread gestartet ist
             z:=0;
             while (z < 10) do
                   begin
                        if (GetThreadPriority(Result.ThreadHD)<>THREAD_PRIORITY_ERROR_RETURN) then z:=999;
                        inc(z);
                        Sleep(200);
                   end;

             //Und die notwendigen Daten schicken
             PostThreadMessage(Result.ThreadID,USR_PROCESSID,0,LParam(ProcessID));
             //Und die notwendigen Daten schicken
             PostThreadMessage(Result.ThreadID,USR_SETCPU,0,LParam(CPUSpeed));

             //Child auf Self setzen
             Result.FirstChild:=FirstChild(Result.ProcessID);

             //WindowHandle holen (wenn vorhanden)
             Result.WindowHND:=WindowByProcessID(Result.ProcessID);
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Einen SlowDownThread beenden
Function SlowDown_Stop(Info:TThreadPack):Boolean;
var
   a : Cardinal;
   z : Integer;
begin
     Result:=FALSE;

     //Dem SlowDownThread eine Exit-Nachricht schicken
     PostThreadMessage(Info.ThreadID,USR_EXITTHREAD,0,0);

     //Warten bis der Thread beendet ist (Maximal 1,5 Sekunden (500 x 3 mSec))
     z:=0;
     while (z < 10) do
           begin
                GetExitCodeThread(Info.ThreadHD,a);
                if (a<>STILL_ACTIVE) then z:=999;
                inc(z);
                Sleep(100);
           end;

     //Haben wir einen Exit-Code
     //Thread beendet ?
     if (a<>STILL_ACTIVE) then
        begin
             //Handle schließen
             if (GetExitCodeThread(Info.ThreadHD,a)<>FALSE) then CloseHandle(Info.ThreadHD);
             Result:=TRUE;
        end
     else
        begin
             //OK, dann schiessen wir Ihn eben ab
             TerminateThread(Info.ThreadHD,1);
             CloseHandle(Info.ThreadHD);
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
///Status des SlowDown-Threads lesen
Function SlowDown_Active(Info:TThreadPack):Boolean;
var
   a:Cardinal;
begin
     //Gibt es einen Exit-Code ?
     if (GetExitCodeThread(Info.ThreadHD,a)=FALSE) then a:=STILL_ACTIVE+1;
     if (a=STILL_ACTIVE) then Result:=TRUE else RESULT:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Gebremste Anwendung anhalten
Function SlowDown_Pause(Info:TThreadPack):Boolean;
begin
     RESULT:=TRUE;
     //Dem SlowDownThread eine Pause-Nachricht schicken
     PostThreadMessage(Info.ThreadID,USR_PAUSECPU,0,0);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Gebremste Anwendung fortführen
Function SlowDown_Resume(Info:TThreadPack):Boolean;
begin
     RESULT:=TRUE;
     //Dem SlowDownThread eine Pause-Nachricht schicken
     PostThreadMessage(Info.ThreadID,USR_STARTCPU,0,0);
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Gebremster Anwendung mehr Prozessorzeit geben
Function SlowDown_FASTER(var Info:TThreadPack):Boolean;
begin
     RESULT:=FALSE;
     if (Info.CPUSpeed<100) then
        begin
             RESULT:=TRUE;
             //Dem SlowDownThread eine SpeedUp-Nachricht schicken
             PostThreadMessage(Info.ThreadID,USR_INCREASECPU,0,0);
             inc(Info.CPUSpeed);
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Gebremster Anwendung weniger Prozessorzeit geben
Function SlowDown_Slower(var Info:TThreadPack):Boolean;
begin
     RESULT:=FALSE;
     if (Info.CPUSpeed>1) then
        begin
             RESULT:=TRUE;
             //Dem SlowDownThread eine SpeedUp-Nachricht schicken
             PostThreadMessage(Info.ThreadID,USR_DECREASECPU,0,0);
             dec(Info.CPUSpeed);
        end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Prozessorzeit direkt setzen
Function SlowDown_SetSpeed (var Info:TThreadPack;CPUSpeed:Integer):Boolean;
begin
     RESULT:=FALSE;
     if (Info.CPUSpeed>1) then
        begin
             RESULT:=TRUE;
             //Dem SlowDownThread eine SpeedUp-Nachricht schicken
             PostThreadMessage(Info.ThreadID,USR_SETCPU,0,LParam(CPUSpeed));
             Info.CPUSpeed:=CPUSpeed;
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Thread, der für jeden Slowdownprocess gestartet wird
function SlowDownThread(Point:Pointer):LongWord;

var
   HSnap       : THandle;
   TE32        : TTHREADENTRY32;
   HProc       : THandle;
   ThreadArray : Array[0..255] of THandle;
   z,cnt       : Integer;
   Prior       : DWord;
   CPUSpeed    : Integer;
   ProcessID   : LongWord;
   DoExit      : Boolean;
   Pause       : Boolean;
   PauseOn     : Boolean;
   Mess        : TMSG;
begin
     Result:=0;
     Pause  :=FALSE;
     PauseOn:=FALSE;

     //Erstmal warten wir auf die gewünschte Process ID
     GetMessage(Mess,0,USR_PROCESSID,USR_PROCESSID);
     LParam(ProcessID):=Mess.lParam;

     //Und jetzt das Ganze für die Prozessorgeschwindigkeit
     GetMessage(Mess,0,USR_SETCPU,USR_SETCPU);
     LParam(CPUSpeed):=Mess.lParam;

     //Variablen initialisieren
     DoExit:=FALSE;

     //Falsche Werte abfangen
     if (CPUSpeed>100) then CPUSpeed:=100;
     if (CPUSpeed<  1) then CPUSpeed:=  1;

     //Jetzt aus dem gefunden Prozess unseren Thread raussuchen
     HSnap:=CreateToolHelp32Snapshot(TH32CS_SNAPTHREAD,0);
     TE32.dwSize:=SizeOf(ThreadEntry32);

     //Alle Threads des gewünschten Prozess suchen und öffnen.
     //Die jeweiligen Handles speichern wir in einem Array
     z:=0;
     if (Thread32First(HSnap,TE32) = TRUE ) then
        while (Thread32Next(HSnap,TE32)=TRUE ) do
              begin
                   //Prozess gefunden ?
                   if (TE32.th32OwnerProcessID=ProcessID) then
                      begin
                           //ThreadHandle hinzufügen
                           ThreadArray[z]:=OpenThread(THREAD_SUSPEND_RESUME,TRUE,TE32.th32ThreadID);
                           Inc(z);
                      end
              end;
     CloseHandle(HSnap);
     //Keinen aktiven Thread gefunden ? => Abbflug
     if (z=0) then ExitThread(0);

     //Anzahl der Threads merken
     cnt:=z;

     //So, nun gehts los
     //Prozess öffnen
     HProc:=OpenProcess(PROCESS_ALL_ACCESS,TRUE,ProcessID);

     If (HProc<>NULL) then
        begin
             //PriorityBoost ausschalten
             SetProcessPriorityBoost(HProc,TRUE);

             //Priorität auf IDLE schalten
             Prior:=GetPriorityClass(HProc);
             SetPriorityClass(HProc,IDLE_PRIORITY_CLASS );
             for z:=0 to cnt do SetThreadPriority(ThreadArray[z],THREAD_PRIORITY_LOWEST);

             //Und uns selbst hochschalten
             SetThreadPriority(GetCurrentThread(),THREAD_PRIORITY_TIME_CRITICAL );

             //Bremsen, solange der Prozess noch aktiv ist oder der Thread eine Exit-Nachricht bekommt
             while (WaitForSingleObject(HProc,0)=WAIT_TIMEOUT) and (DoExit=FALSE) do
                   begin
                        //Process anhalten
                        if (PauseOn = TRUE) then
                           begin
                                if (Pause=FALSE) then
                                   begin
                                        //Alle Threads des Prozesses anhalten
                                        for z:=0 to cnt do SuspendThread(ThreadArray[z]);
                                        Pause:=TRUE;
                                   end;
                           end
                        else
                            begin
                                if (Pause=TRUE) then
                                   begin
                                        //Alle Threads des Prozesses fortsetzem
                                        for z:=0 to cnt do ResumeThread(ThreadArray[z]);
                                        Pause:=FALSE;
                                   end;
                            end;


                        //Prozessorzeit limitieren
                        if (Pause=FALSE) then
                           begin
                                if (CPUSpeed<100) then
                                   begin
                                        //Alle Threads des Prozesses anhalten
                                        for z:=0 to cnt do SuspendThread(ThreadArray[z]);

                                        z:=100;
                                        if (CPUSpeed<20) then
                                           begin
                                                z:=100 + Round(100*((21-CPUSpeed)/10));
                                           end;
                                        Sleep( z- CPUSpeed );

                                        //Alle Threads des Prozesses starten
                                        for z:=0 to cnt do ResumeThread(ThreadArray[z]);
                                        Sleep( CPUSpeed  +1 );
                                   end
                                else
                                    begin
                                         Sleep(100);
                                    end;
                           end
                        else
                           begin
                                //Im Pausemodus then Thread ablegen
                                Sleep(300);
                           end;


                        //Message-Verwaltung
                        if PeekMessage(Mess,0,USR_INCREASECPU,USR_EXITTHREAD,PM_REMOVE) then
                           begin
                                case Mess.message of
                                     USR_INCREASECPU : if CPUSpeed < 100 then inc (CPUSpeed);
                                     USR_DECREASECPU : if CPUSpeed > 1   then dec (CPUSpeed);
                                     USR_SETCPU      : LParam(CPUSpeed):=Mess.lParam;

                                     USR_PAUSECPU    : PauseOn:=TRUE;
                                     USR_STARTCPU    : PauseOn:=FALSE;

                                     USR_EXITTHREAD  : DoExit:=TRUE;
                                end;
                                Mess.message:=0;
                           end;
                        bThreadStatus:=Not bThreadStatus;
                   end;


             //Die Priorität des Prozesses wieder herstellen
             SetPriorityClass(HProc,Prior);
             for z:=0 to cnt do SetThreadPriority(ThreadArray[z],THREAD_PRIORITY_NORMAL);

             //Process schließen
             CloseHandle(Hproc);

             //Alle Threads des Prozesses schließen
             //und wieder aktivieren
             for z:=0 to cnt do
                 begin
                      //Wird er Bremsthread beendet, das Ziel wieder aktivieren
                      if (ResumeThread(ThreadArray[z])<>$FFFFFFFF) then
                         //Alle Handles schließen
                         CloseHandle(ThreadArray[z]);
                 end;
        end;
     ExitThread(0);
end;

/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
//
//Weitere Processverwandte Funktionen
//
/////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////
//Minimiert den eigenen Speicherverbrauch
Procedure SetMinWorkMemory();
begin
     SetProcessWorkingSetSize(GetCurrentProcess,$ffffffff,$ffffffff);
end;

/////////////////////////////////////////////////////////////////////////////////////
//Speicherbedarf eines Prozesses lesen
function GetProcessMemorySize(ProcessID:LongWord):unsigned32;
var
  pmc      : PPROCESS_MEMORY_COUNTERS;
  cb       : Integer;
  hProcess : THandle;
begin
  //process valie?
  hProcess:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,FALSE,ProcessID);
  if ( hProcess <> NULL ) then
    begin
      //create structures
      cb := SizeOf(_PROCESS_MEMORY_COUNTERS);
      GetMem(pmc, cb);
      pmc^.cb := cb;
      //get data
      if (GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb)=TRUE) then
        begin
          result:=pmc^.WorkingSetSize;
        end
      else
        begin
          result:=0;
        end;
      FreeMem(pmc);
      CloseHandle(hProcess);
    end;
end;


/////////////////////////////////////////////////////////////////////////////////////
//Sendet an alle Prozesse mit der Caption "String" ein Close-Signal
procedure SendCloseMessage(caption:string);

//UnterFunktion für den WindowEnumerator
function EnumWindowsFunc(const hnd:THandle;const caption:string):Boolean; stdcall;
var
   cap:Array[0..1024]of Char;
   s1,s2:string;
begin
     result:=TRUE;
     //Caption greifen
     if GetWindowText(hnd,Cap,SizeOf(Cap)-1)<>0 then
        begin
             s1:=Caption;
             s2:=Cap;
             //Enthält der Name ein WildCard wird nur der Substring verglichen
             if (Caption[Length(Caption)]='*') then
                begin
                     //String ohne Wildcard holen
                     s1:=Copy(Caption,0,Length(Caption)-1 );

                     //Caption-String in der Passenden Länge holen
                     s2:=Copy(Cap,0,Length(s1) );
                end;

             //Wenn wir was gefunden haben schicken wir Close und Quit als Nachricht
             if (s1=s2) then
                begin
                     PostMessage(hnd,WM_CLOSE,0,0);
                     PostMessage(hnd,WM_QUIT,0,0);
                end;
        end;
end;

begin
     if Caption='' then Exit;
     EnumWindows(@EnumWindowsFunc,LParam(caption));
end;


/////////////////////////////////////////////////////////////////////////////////////
//Ermittelt den Windowhandle anhand der ProcessID
function WindowByProcessID(ProcessID:LongWord):LongWord;

//UnterFunktion für den WindowEnumerator
function EnumWindowsFunc(const hnd:LongWord;const ProcessID:LongWord):Boolean; stdcall;
var
   Proc      : LongWord;
begin
     result:=TRUE;
     //Process ID holen
     if (HWND=0) then
        begin
             GetWindowThreadProcessID(HND,@Proc);
             //Unsere gesuchte ID ?
             if (ProcessID=Proc) then
                begin
                     HWND:=HND;
                end;
        end;
end;
begin
     HWND:=0;
     EnumWindows(@EnumWindowsFunc,ProcessID);
     Result:=HWND;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Ermittelt Dateiname anhand des Windowhandles
function WindowByFile(Filename:String):LongWord;
//UnterFunktion für den WindowEnumerator
function EnumWindowsFunc(const hnd:LongWord;const Filename:String):Boolean; stdcall;
var
   Proc      : LongWord;
begin
     result:=TRUE;
     //Process ID holen
     if (TempID=0) then
        begin
             //ProcessID holen
             GetWindowThreadProcessID(HND,@Proc);

             //Unsere gesuchte ID ?
             if (LowerCase(FileByProcessID(Proc))=Filename) then
                begin
                     TempID:=hnd;
                end;
        end;
end;
begin
     TempID:=0;
     EnumWindows(@EnumWindowsFunc,LParam(LowerCase(Filename)));
     Result:=TempID;
end;

/////////////////////////////////////////////////////////////////////////////////////
//Priorität des eigenen Programmes ändern
Procedure SetSelfPriority(priority:LongInt);
begin
     SetPriorityClass(GetCurrentProcess() , priority);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Process ID anhand des Dateinamens bestimmen
Function ProcessIDByFile(Filename:String):LongWord;
var
   hSnap : THandle;
   hProc : THandle;
   sTemp : String;
   sFile : String;
   PE32  : TPROCESSENTRY32;
begin
     Result:=0;
     //Erstmal alle Prozesse greifen
     hSnap:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
     PE32.dwSize:=SizeOf(ProcessEntry32);

     if (Process32First(hSnap,PE32) = TRUE ) then
        while (Process32Next(hSnap,PE32)=TRUE ) do
              begin
                   //Standarddateiname holen
                   sFile:=PE32.szExeFile;

                   //Vollen Dateinamen holen (Wenn es geht)
                   hProc:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,FALSE,PE32.th32ProcessID);
                   if (hProc<>0) then
                      begin
                           SetLength(sTemp,MAX_PATH);
                           //Versuchen, dan Namen zu kriegen
                           if (GetModuleFilenameEx(hProc,0,@sTemp[1],MAX_PATH)<>0) then
                              begin
                                   //Was gefunden
                                   sFile:=sTemp;
                              end;
                           CloseHandle(hProc);
                      end;
                   //Und jetzt mal schauen

                   if (Pos(LowerCase(Filename),LowerCase(sFile))<>0) then
                      begin
                           Result:=PE32.th32ProcessID;
                      end;
              end;
     CloseHandle(hSnap);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Anhand der ProcessID den Dateinamen bestimmen
Function FileByProcessID(ProcessID:LongWord):String;
var
   hSnap : THandle;
   hProc : THandle;
   PE32  : TPROCESSENTRY32;
   sTemp : String;
begin
     Result:='';
     //Erstmal alle Prozesse greifen
     hSnap:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
     PE32.dwSize:=SizeOf(ProcessEntry32);

     if (Process32First(hSnap,PE32) = TRUE ) then
        while (Process32Next(hSnap,PE32)=TRUE ) do
              begin
                   if (PE32.th32ProcessID=ProcessID) then
                      begin
                           //Einfachen Namen lesen
                           Result:=Lowercase(ExtractFileName(PE32.szExeFile));

                           //Vollen Dateinamen holen
                           hProc:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,FALSE,PE32.th32ProcessID);
                           if (hProc<>0) then
                              begin
                                   SetLength(sTemp,MAX_PATH);
                                   //Versuchen, dan Namen zu kriegen
                                   if (GetModuleFilenameEx(hProc,0,@sTemp[1],MAX_PATH)<>0) then
                                      begin
                                           //Was gefunden
                                           Result:=sTemp;
                                      end
                                   else
                                      begin
                                           //Nein
                                      end;
                                CloseHandle(hProc);
                              end;
                      end;
              end;
     CloseHandle(hSnap);
end;



////////////////////////////////////////////////////////////////////////////////////////////////
//Erste Unterfunktion anhand der ProcessID finden
Function FirstChild(ParentID:LongWord):LongWord;
var
   hSnap : THandle;
   PE32  : TPROCESSENTRY32;
begin
     Result:=0;
     //Erstmal alle Prozesse greifen
     hSnap:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
     PE32.dwSize:=SizeOf(ProcessEntry32);

     if (Process32First(hSnap,PE32) = TRUE ) then
        while (Process32Next(hSnap,PE32)=TRUE ) do
              begin
                   if (PE32.th32ParentProcessID=ParentID) then
                      begin
                           Result:=PE32.th32ProcessID;
                      end;
              end;
     CloseHandle(hSnap);
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Erste Überfunktion anhand der ProcessID finden
Function NextParent(ChildID:LongWord):LongWord;
var
   hSnap : THandle;
   PE32  : TPROCESSENTRY32;
begin
     Result:=0;
     //Erstmal alle Prozesse greifen
     hSnap:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
     PE32.dwSize:=SizeOf(ProcessEntry32);

     if (Process32First(hSnap,PE32) = TRUE ) then
        while (Process32Next(hSnap,PE32)=TRUE ) do
              begin
                   if (PE32.th32ProcessID=ChildID) then
                      begin
                           Result:=PE32.th32ParentProcessID;
                      end;
              end;
     CloseHandle(hSnap);
end;


////////////////////////////////////////////////////////////////////////////////////////////////
/// Prozessliste erzeugen
Function EnumerateProcesses():Boolean;
var
   hSnap : THandle;
   PE32  : TPROCESSENTRY32;
   hnd   : THandle;
   z     : Integer;
   text  : Array[0..255]of Char;
   sTemp : String;
   pmc    : PPROCESS_MEMORY_COUNTERS;
   cb     : unsigned32;
begin
     SetLength(ProcessList,0);
     Result:=FALSE;

     //Speicherzugriff vorbereiten 
     cb := SizeOf(_PROCESS_MEMORY_COUNTERS);
     GetMem(pmc, cb);
     pmc^.cb := cb;

     //Erstmal alle Prozesse greifen
     hSnap:=CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
     PE32.dwSize:=SizeOf(ProcessEntry32);

     //Und nun die Felder füllen
     z:=0;
     if (Process32First(hSnap,PE32) = TRUE ) then
        begin
             while (Process32Next(hSnap,PE32)=TRUE ) do
                   begin
                        //Processstruktur mit Daten füllen
                        SetLength(ProcessList,z+1);
                        FillChar(ProcessList[z],SizeOf(ProcessList[z]),0);

                        ProcessList[z].ProcessID:=PE32.th32ProcessID;
                        ProcessList[z].NumberOfThread:=PE32.cntThreads;
                        ProcessList[z].ParentProcess:=PE32.th32ParentProcessID;
                        ProcessList[z].ProcessPath:=PE32.szExeFile;
                        ProcessList[z].Window:=FALSE;

                        //interne daten holen
                        Hnd:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,FALSE,ProcessList[z].ProcessID);
                        if (Hnd<>0) then
                           begin
                                //dateiname
                                sTemp:=StringOfChar(' ',MAX_PATH);
                                //Versuchen, dan Namen zu kriegen
                                if (GetModuleFilenameEx(Hnd,0,@sTemp[1],MAX_PATH)<>0) then
                                   begin
                                        //Was gefunden
                                        ProcessList[z].ProcessPath:=trim(sTemp);
                                   end
                                else
                                   begin
                                        //Nein
                                   end;

                                //speicher
                                if (GetProcessMemoryInfo(hnd, pmc, cb)=TRUE) then
                                  begin
                                    ProcessList[z].RAM :=pmc^.WorkingSetSize;
                                    ProcessList[z].PAGE:=pmc^.PagefileUsage;
                                    ProcessList[z].RAMpeak :=pmc^.peakWorkingSetSize;
                                    ProcessList[z].PAGEpeak:=pmc^.peakPagefileUsage;
                                  end
                                else
                                  begin
                                    ProcessList[z].RAM:=0;
                                    ProcessList[z].PAGE:=0;
                                    ProcessList[z].RAMpeak:=0;
                                    ProcessList[z].PAGEpeak:=0;
                                  end;

                                CloseHandle(Hnd);
                           end;

                        //Hat der Process ein Fenster ?
                        if (WindowByProcessID(ProcessList[z].ProcessID)<>0) then
                           begin
                                ProcessList[z].Window:=TRUE;
                                ProcessList[z].WindowHandle:=WindowByProcessID(ProcessList[z].ProcessID);
                                if (GetWindowText(ProcessList[z].WindowHandle,Text,Length(Text))>0) then
                                   begin
                                        ProcessList[z].WindowCaption:=Text;
                                   end
                                else
                                   begin
                                        ProcessList[z].WindowCaption:='';
                                        FillChar(Text,Length(Text),0);
                                   end
                           end;
                        inc(z);
                   end;
             Result:=TRUE;
        end;
     CloseHandle(hSnap);
  FreeMem(pmc);
end;



//Alle Parentfenster zählen
Function CountParentWindows():Integer;
var
   x : Integer;
          //UnterFunktion für den WindowEnumerator
          function EnumWindowsFunc(const hnd:LongWord;const Counter:PInteger):Boolean; stdcall;
                   begin
                        if (GetParent(hnd)=0) then
                           begin
                                inc(Counter^);
                           end;
                        result:=TRUE;
                   end;
begin
     x:=0;
     EnumWindows(@EnumWindowsFunc,LParam(@x));
     Result:=x;
end;

function KillProcess(ID:LongWord):Boolean;
var
   p                     : THandle;
begin
     result:=FALSE;
     //Und den Process beenden
     p:=OpenProcess(PROCESS_ALL_ACCESS,false,ID);
     if (p<>null) then
        begin
             TerminateProcess(p,0);
             Result:=TRUE;
             CloseHandle(p);
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function WindowsExit(RebootParam: Longword): Boolean;
const
   SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
begin
     //Token setzen (WinNT Umgebung)
     AdJustToken(SE_SHUTDOWN_NAME);
     Result := ExitWindowsEx(RebootParam, 0) ;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Eine Executable ausführen
//Ist Wait TRUE wird auf das Ende der Anwendung gewartet
function Execute(Source:String;Command:String;Wait:Boolean;Hidden:Boolean=FALSE):LongWord;
var
   StartInfo : TStartupInfo;
   ProcInfo  : TProcessInformation;
begin
     Result:=0;
     if (FileExists(Source)=FALSE) then Exit;

     //Wichtige Strukturen initialisieren
     FillChar(StartInfo,SizeOf(TStartupInfo),#0);
     FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
     StartInfo.cb := SizeOf(TStartupInfo);

     //Prozess evtl verstecken
     if (Hidden=TRUE) then
        begin
             StartInfo.dwFlags:=STARTF_USESHOWWINDOW;
             StartInfo.wShowWindow := SW_HIDE;
        end;

     //Und los gehts
     if CreateProcess(nil,
                    PChar(Source+' '+Command),
                    nil,
                    nil,
                    False,
                    CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
                    nil,
                    nil,
                    StartInfo,
                    ProcInfo) then
         begin
             //Auf das Ende warten ?
             if (Wait=TRUE) then
                begin
                     //Auf das Ende der Application warten
                     while (WaitForSingleObject(ProcInfo.hProcess, 500)=WAIT_TIMEOUT) do
                           Sleep(100);
                     Result:=ProcInfo.dwProcessId;
                end
             else
                begin
                     //Wert zurückgeben
                     Result:=ProcInfo.dwProcessId;
                end;
         end;

     //Alle Handles schließen
     CloseHandle(ProcInfo.hProcess);
     CloseHandle(ProcInfo.hThread);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Anwendung in einem anderen Benutzerkontext starten
function ExecuteAsUser(User,Pass,Domain:WideString;Source,Command:WideString;Wait:Boolean;Hidden:Boolean=FALSE):LongWord;
var
   SCreateProcessWithLogOnW : TCreateProcessWithLogOnW;
   ProcessInfo              : TProcessInformation;
   StartInfo                : TStartupInfoW;
begin
     Result:=0;
     //User32-Zugriff erlangen
     SCreateProcessWithLogOnW := GetProcAddress(GetModulehandle(advapi32), 'CreateProcessWithLogonW');

     //Funktion gefunden ?
     if Assigned(SCreateProcessWithLogOnW) then
        begin
             if (FileExists(Source)=FALSE) then Exit;

             //Wichtige Strukturen initialisieren
             FillChar(StartInfo,SizeOf(StartInfo),#0);
             StartInfo.cb := SizeOf(StartInfo);

             //Prozess evtl verstecken
             if (Hidden=TRUE) then
                begin
                     StartInfo.dwFlags:=STARTF_USESHOWWINDOW;
                     StartInfo.wShowWindow := SW_HIDE;
                end;

             //Und los gehts
             Source:=Source+' '+Command;
             if SCreateProcessWithLogOnW( PWideChar(User),
                                         PWideChar(Domain),
                                         PWideChar(Pass),
                                         LOGON_WITH_PROFILE,
                                         nil,//PWideChar(Source),
                                         PWideChar(Source),
                                         0,
                                         nil,
                                         nil,//PWideChar(ExtractFilePath(Source)),
                                         StartInfo,
                                         ProcessInfo) then
                begin
                     //Auf das Ende warten ?
                     if (Wait=TRUE) then
                        begin
                             //Auf das Ende der Application warten
                             while (WaitForSingleObject(ProcessInfo.hProcess, 500)=WAIT_TIMEOUT)
                                   do  Sleep(100);
                             Result:=ProcessInfo.dwProcessId;
                        end
                     else
                         begin
                              //Wert zurückgeben
                              Result:=ProcessInfo.dwProcessId;
                         end;

                //Alle Handles schließen
                CloseHandle(ProcessInfo.hProcess);
                CloseHandle(ProcessInfo.hThread);
                end;
        end;
end;

end. 
