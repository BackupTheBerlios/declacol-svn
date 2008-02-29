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

unit unit_services;
interface
uses windows,winsvc,classes;


function ServiceGetStatus(sMachine,sService : string ) : Longword;
function ServiceStart(sMachine,sService : string ) : boolean;
function ServiceStop(sMachine,sService : string ) : boolean;

function ServiceGetDisplayName(sMachine,sServiceKeyName : string ) : string;
function ServiceGetKeyName(sMachine,sServiceDispName : string ) : string;

function ServiceGetList(sMachine : string;dwServiceType: LongWord;bDisplayName:Boolean=TRUE):TStringList;

function  ServiceStatusToString(Status:Longword):String;

//Einige zusätzliche Konstanten
const
     SERVICE_ACTIVE=1;
     SERVICE_INVALID = 123456;
     SERVICE_KERNEL_DRIVER       = $00000001;
     SERVICE_FILE_SYSTEM_DRIVER  = $00000002;
     SERVICE_ADAPTER             = $00000004;
     SERVICE_RECOGNIZER_DRIVER   = $00000008;

     SERVICE_DRIVER              = (
                                    SERVICE_KERNEL_DRIVER or
                                    SERVICE_FILE_SYSTEM_DRIVER or
                                    SERVICE_RECOGNIZER_DRIVER
                                    );

     SERVICE_WIN32_OWN_PROCESS   = $00000010;
     SERVICE_WIN32_SHARE_PROCESS = $00000020;
     SERVICE_WIN32               = (
                                    SERVICE_WIN32_OWN_PROCESS or
                                    SERVICE_WIN32_SHARE_PROCESS
                                   );

     SERVICE_INTERACTIVE_PROCESS = $00000100;

     SERVICE_TYPE_ALL            = (
                                    SERVICE_WIN32 or
                                    SERVICE_ADAPTER or
                                    SERVICE_DRIVER  or
                                    SERVICE_INTERACTIVE_PROCESS
                                   );


implementation
/////////////////////////////////////////////////////////////////////////////////
//   SERVICE_STOPPED
//   SERVICE_RUNNING
//   SERVICE_PAUSED
//
//   SERVICE_START_PENDING
//   SERVICE_STOP_PENDING
//   SERVICE_CONTINUE_PENDING
//   SERVICE_PAUSE_PENDING
//
// sMachine:
//   machine name, ie: \SERVER
//   empty = local machine
// sService
//   service name, ie: Alerter
/////////////////////////////////////////////////////////////////////////////////
function ServiceGetStatus(sMachine,sService : string ) : Longword;
var
  schm     : SC_Handle;
  schs     : SC_Handle;
  ss       : TServiceStatus;
  dwStat   : Longword;
begin
  dwStat := SERVICE_INVALID;

  // connect to the service
  // control manager 
  schm := OpenSCManager(PChar(sMachine),Nil,SC_MANAGER_CONNECT);

  // if successful...
  if ( schm > 0)then
     begin
          // open a handle to
          // the specified service
          schs := OpenService(schm,PChar(sService),SERVICE_QUERY_STATUS);

          // if successful...
          if ( schs > 0)then
             begin
                  // retrieve the current status
                  // of the specified service
                  if ( QueryServiceStatus(schs,ss)) then
                     begin
                          dwStat := ss.dwCurrentState;
                     end;
                  // close service handle
                  CloseServiceHandle(schs);
             end;
          // close service control
          // manager handle
          CloseServiceHandle(schm);
     end;
  Result := dwStat;
end;


/////////////////////////////////////////////////////////////////////////////////
function ServiceStart(sMachine,sService : string ) : boolean;
var
  schm   : SC_Handle;
  schs   : SC_Handle;
  ss     : TServiceStatus;
  psTemp : PChar;
  dwChkP : Longword;
begin
     ss.dwCurrentState := High(Longword);
  
  schm := OpenSCManager(PChar(sMachine),Nil,SC_MANAGER_CONNECT);

  // if successful...
  if (schm > 0)then
     begin
          schs := OpenService(schm,PChar(sService),SERVICE_START or SERVICE_QUERY_STATUS);

          // if successful...
          if (schs > 0)then
             begin
                  psTemp := Nil;
                  if (StartService(schs,0,psTemp)) then
                     begin
                          // check status
                          if (QueryServiceStatus(schs,ss))then
                             begin
                                  while (SERVICE_RUNNING<> ss.dwCurrentState)do
                                        begin
                                             //
                                             // dwCheckPoint contains a
                                             // value that the service
                                             // increments periodically
                                             // to report its progress
                                             // during a lengthy
                                             // operation.
                                             //
                                             // save current value
                                             //
                                             dwChkP := ss.dwCheckPoint;

                                             //
                                             // wait a bit before
                                             // checking status again
                                             //
                                             // dwWaitHint is the
                                             // estimated amount of time
                                             // the calling program
                                             // should wait before calling
                                             // QueryServiceStatus() again
                                             //
                                             // idle events should be
                                             // handled here...
                                             //
                                             Sleep(ss.dwWaitHint);

                                             if (not QueryServiceStatus(schs,ss))then
                                                begin
                                                     // couldn't check status
                                                     // break from the loop
                                                     break;
                                                end;

                                             if (ss.dwCheckPoint < dwChkP) then
                                                begin
                                                     // QueryServiceStatus
                                                     // didn't increment
                                                     // dwCheckPoint as it
                                                     // should have.
                                                     // avoid an infinite
                                                     // loop by breaking
                                                     break;
                                                end;
                                        end;
                             end;
                     end;
                  // close service handle
                  CloseServiceHandle(schs);
             end;
          CloseServiceHandle(schm);
     end;

  // return TRUE if
  // the service status is running
  Result := SERVICE_RUNNING = ss.dwCurrentState;
end;

/////////////////////////////////////////////////////////////////////////////////
function ServiceStop(sMachine,sService : string ) : boolean;
var
  schm   : SC_Handle;
  schs   : SC_Handle;
  ss     : TServiceStatus;
  dwChkP : Longword;
begin
  schm := OpenSCManager(PChar(sMachine),Nil,SC_MANAGER_CONNECT);
  if (schm > 0) then
     begin
          schs := OpenService(schm,PChar(sService),SERVICE_STOP or SERVICE_QUERY_STATUS);
          if (schs > 0) then
             begin
                  if (ControlService(schs,SERVICE_CONTROL_STOP,ss)) then
                     begin
                          if (QueryServiceStatus(schs,ss)) then
                             begin
                                  while (SERVICE_STOPPED <> ss.dwCurrentState) do
                                        begin
                                             dwChkP := ss.dwCheckPoint;
                                             Sleep(ss.dwWaitHint);
                                             if (not QueryServiceStatus(schs,ss)) then
                                                begin
                                                     break;
                                                end;

                                             if (ss.dwCheckPoint < dwChkP) then
                                                begin
                                                     break;
                                                end;
                                        end;
                             end;
                     end;
                  CloseServiceHandle(schs);
             end;
          CloseServiceHandle(schm);
     end;
  Result := SERVICE_STOPPED = ss.dwCurrentState;
end;


/////////////////////////////////////////////////////////////////////////////////
function ServiceGetKeyName(sMachine,sServiceDispName : string ) : string;
var
   schm          : SC_Handle;
   nMaxNameLen   : Cardinal;
   sServiceName  : String;
begin
     Result := '';
     nMaxNameLen := 255;
     schm := OpenSCManager(PChar(sMachine),Nil,SC_MANAGER_CONNECT);
     if (schm > 0) then
        begin
             sServiceName:=StringOfChar(#0,nMaxNameLen+1);
             if ( GetServiceKeyName(schm,PChar(sServiceDispName),addr(sServiceName[1]),nMaxNameLen ) ) then
                begin
                     sServiceName[nMaxNameLen] := #0;
                     Result := sServiceName;
                end;
             CloseServiceHandle(schm);
        end;
end;

/////////////////////////////////////////////////////////////////////////////////
function ServiceGetDisplayName(sMachine,sServiceKeyName : string ) : string;
var
   schm          : SC_Handle;
   nMaxNameLen   : Cardinal;
   sServiceName  : String;
begin
     Result := '';
     nMaxNameLen := 255;
     schm := OpenSCManager(PChar(sMachine),Nil,SC_MANAGER_CONNECT);
     if (schm > 0) then
        begin
             sServiceName:=StringOfChar(#0,nMaxNameLen+1);
             if ( GetServiceDisplayName(schm,PChar(sServiceKeyName),addr(sServiceName[1]),nMaxNameLen ) ) then
                begin
                     sServiceName[nMaxNameLen] := #0;
                     result:=sServicename;
                end;
             CloseServiceHandle(schm);
        end;
end;


/////////////////////////////////////////////////////////////////////////////////
function ServiceGetList(sMachine : string;dwServiceType: LongWord;bDisplayName:Boolean=TRUE):TStringList;
const
     cnMaxServices = 4096;
type
    TSvcA = array[0..cnMaxServices] of TEnumServiceStatus;
    PSvcA = ^TSvcA;
var
   iTemp : integer;
   schm  : SC_Handle;
   nBytesNeeded  : Longword;
   nServices     : Longword;
   nResumeHandle : Longword;
   ssa           : PSvcA;
begin
     result:=TStringList.Create;
     result.Clear;
     schm := OpenSCManager(PChar(sMachine),Nil,SC_MANAGER_ALL_ACCESS);

     if (schm > 0) then
        begin
             nResumeHandle := 0;
             New(ssa);
             EnumServicesStatus ( schm,
                                  dwServiceType,
                                  SERVICE_ACTIVE or SERVICE_INACTIVE,
                                  ssa^[0],
                                  SizeOf(ssa^),
                                  nBytesNeeded,
                                  nServices,
                                  nResumeHandle
                                 );


             // assume that our initial array
             // was large enough to hold all
             // entries. add code to enumerate
             // if necessary.
             //
             for iTemp := 0 to nServices-1 do
                 begin
                      if (bDisplayname) then
                         begin
                              Result.Add( ssa^[iTemp].lpDisplayName );
                         end
                      else
                         begin
                              Result.Add( ssa^[iTemp].lpServiceName );
                         end;
                 end;
             Dispose(ssa);

             CloseServiceHandle(schm);
        end;
end;

/////////////////////////////////////////////////////////////////////////////////
function  ServiceStatusToString(Status:Longword):String;
begin
     case (Status) of
          SERVICE_INVALID           : result:='Service invalid';
          SERVICE_STOPPED           : result:='Service stopped';
          SERVICE_RUNNING           : result:='Service running';
          SERVICE_PAUSED            : result:='Service paused';
          SERVICE_START_PENDING     : result:='Service starting';
          SERVICE_STOP_PENDING      : result:='Service stopping';
          SERVICE_CONTINUE_PENDING  : result:='Service continueing';
          SERVICE_PAUSE_PENDING     : result:='Service pausing';
     else
         result:='Status invalid';
     end;
end;

end.
