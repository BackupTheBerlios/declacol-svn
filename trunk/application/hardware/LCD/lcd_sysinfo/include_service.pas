////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////
//// Include-Datei für Services
////
//// (c) 2005 Borg@Sven-of-Nine.de
////
//// Thx to Assabed (GoP)
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
const
     svc_error_scm         = 'unable to open service-manager';
     svc_error_service     = 'unable to open service';
     svc_ok_installed      = 'service installed';
     svc_ok_uninstalled    = 'service uninstalled';
     svc_ok_pending        = 'service pending for delete';
     svc_help              = 'Service-Manager (c) Borg@Sven-of-Nine.de'+#13+#13+
                         'usage :'+#13+
                         'app.exe /command /mode'+#13+#13+
                         'commands :'+#13+
                         'install   : install service'+#13+
                         'uninstall : uninstall service'+#13+#13+
                         'mode :'+#13+
                         'auto   : autostart service'+#13+
                         'manual : manually start service';
////////////////////////////////////////////////////////////////////////////////
var
   svc_dispatchtable   : array[0..1] of tservicetableentry;
   svc_sshstatushandle : service_status_handle;
   svc_ssstatus        : service_status;
   svc_stopped         : boolean;
   svc_paused          : boolean;

   //Flag für Messageboxen
   svc_bQuiet         : Boolean=FALSE;


////////////////////////////////////////////////////////////////////////////////
/// Messages ausgeben
////////////////////////////////////////////////////////////////////////////////
procedure SVC_ShowMessage(sText:String);
begin
     if (not svc_bQuiet) then
        begin
             MessageBox(0,PChar(sText),PChar(Displayname),MB_OK);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Fehler behandeln
////////////////////////////////////////////////////////////////////////////////
function SVC_HandleError(sDefault:String):String;
var
   lwError : Longword;
   sBuff   : array[0..255]of char;
begin
     result:=sDefault;
     //Bei einem Fehler diesen in seinen Text wandeln
     lwError:=GetLastError();
     if (lwError<>NO_ERROR) then
        begin
             if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,nil,lwError,0,@sBuff[0],255,nil)<>0) then
                begin
                     Result:=string(sBuff);
                end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
/// Einen Service installieren (richtig und nicht über RegHacks)
////////////////////////////////////////////////////////////////////////////////
function SVC_InstallService(sServiceName,sDisplayName,sFilename:String;iStartMode:Integer):boolean;
var
   hSVCMAN  : SC_HANDLE;
   hService : SC_HANDLE;
   sOut     : String;
begin
     result:=FALSE;
     //Service-Control-Manager öffnen
     hSVCMAN:=OpenSCManager(nil,nil,SC_MANAGER_ALL_ACCESS);
     if (hSVCMAN <> 0) then
        begin
             //Offen, dann Service installieren
             hService:=CreateService( hSVCMAN,
                                      PChar(sServiceName),
                                      PChar(sDisplayName),
                                      SERVICE_START OR SERVICE_QUERY_STATUS OR _DELETE,
                                      SERVICE_WIN32_OWN_PROCESS,
                                      //Messageboxen zulassen
//                                      SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS,
                                      iStartMode,
                                      SERVICE_ERROR_NORMAL,
                                      @sFilename[1],
                                      nil, nil, nil, nil, nil);
             //Funktioniert ?
             if (hService<>0) then
                begin
                     //Ja
                     sOut:=svc_ok_installed;
                     result:=TRUE;
                     //Dann Servicehandle schließen
                     CloseServiceHandle(hService);
                end
             else
                begin
                     //Nein => Fehler ausgeben
                     sOut:=SVC_HandleError('install error');
                end;
             //Managerhandle schließen
             CloseServiceHandle(hService);
        end
     else
        begin
             sOut:=SVC_HandleError('manager error');
        end;
     //Evtl. Nachricht ausgeben
     SVC_ShowMessage(sOut);
end;


////////////////////////////////////////////////////////////////////////////////
/// Einen Service deinstallieren (richtig und nicht über RegHacks)
////////////////////////////////////////////////////////////////////////////////
function SVC_UninstallService(sServiceName:String):boolean;
var
   hSVCMAN  : SC_HANDLE;
   hService : SC_HANDLE;
   iDelete  : Integer;
   sOut     : String;
begin
     result:=FALSE;
     //Service-Manager öffnen
     hSVCMAN:=OpenSCManager(NIL, NIL, SC_MANAGER_ALL_ACCESS);
     if (hSVCMAN<>0) then
        begin
             //Service öffnen
             hService:=OpenService(hSVCMAN, PChar(sServicename), SERVICE_ALL_ACCESS);
             if (hService<>0) then
                begin
                     //Service löschen
                     iDelete:=integer(DeleteService(hService));
                     //Löschen OK ?
                     if (iDelete<>0) then
                        begin
                             //Ja
                             sOut:=svc_ok_uninstalled;
                             result:=TRUE;
                        end
                     else
                        begin
                             //Nein
                             sOut:=SVC_HandleError('error uninstall');
                        end;
                     //Servicehandle schließen
                     CloseServiceHandle(hService);
                end
             else
                begin
                     sOut:=SVC_HandleError('error service');
                end;
             //Managerhandle schließen
             CloseServiceHandle(hSVCMAN);
        end
     else
        begin
             sOut:=SVC_HandleError('error manager');
        end;
     //Evtl. Nachricht ausgeben
     SVC_ShowMessage(sOut);
end;


////////////////////////////////////////////////////////////////////////////////
/// Routine für Service-Manager-Funktionen
////////////////////////////////////////////////////////////////////////////////
procedure ServiceHandler(fdwcontrol:integer); stdcall;
begin
     case fdwcontrol of
          SERVICE_CONTROL_STOP:
                               begin
                                    svc_stopped:=true;
                                    svc_ssstatus.dwcurrentstate:=SERVICE_STOP_PENDING;
                                    setservicestatus(svc_sshstatushandle,svc_ssstatus);
                               end;     
          SERVICE_CONTROL_PAUSE:
                               begin
                                    svc_paused:=true;
                                    svc_ssstatus.dwcurrentstate:=SERVICE_PAUSED;
                                    setservicestatus(svc_sshstatushandle, svc_ssstatus);
                               end;
          SERVICE_CONTROL_CONTINUE:
                               begin
                                    svc_paused:=false;
                                    svc_ssstatus.dwcurrentstate:=SERVICE_RUNNING;
                                    setservicestatus(svc_sshstatushandle, svc_ssstatus);
                               end;
          SERVICE_CONTROL_INTERROGATE:
                               begin
                                    setservicestatus(svc_sshstatushandle, svc_ssstatus);
                               end;
          SERVICE_CONTROL_SHUTDOWN:
                               begin
                                    svc_stopped:=true;
                               end;
     end
end;

////////////////////////////////////////////////////////////////////////////////
/// Aufruf der eigentlichen Service-Prozedur
////////////////////////////////////////////////////////////////////////////////
procedure serviceproc(dwargc:integer;var lpszargv:pchar); stdcall;
begin
     //Service-Controller öffnen
     svc_sshstatushandle:=registerservicectrlhandler(pchar(servicename), @servicehandler);
     if (svc_sshstatushandle<>0) then
        begin
             //Service-Status-Struktur mit Daten füllen
             zeromemory(@svc_ssstatus, sizeof(svc_ssstatus));
             svc_ssstatus.dwservicetype:=SERVICE_WIN32_OWN_PROCESS;
             svc_ssstatus.dwcurrentstate:=SERVICE_START_PENDING;
             svc_ssstatus.dwcontrolsaccepted:=SERVICE_ACCEPT_STOP OR SERVICE_ACCEPT_PAUSE_CONTINUE;
             svc_ssstatus.dwwaithint:=1000;
             setservicestatus(svc_sshstatushandle, svc_ssstatus);
             svc_stopped:=false;
             svc_paused:=false;
             svc_ssstatus.dwcurrentstate:=SERVICE_RUNNING;
             setservicestatus(svc_sshstatushandle,svc_ssstatus);
             service_main();
             svc_ssstatus.dwcurrentstate:=SERVICE_STOPPED;
             setservicestatus(svc_sshstatushandle,svc_ssstatus);
        end;

END;

////////////////////////////////////////////////////////////////////////////////
/// Uns selbst als Service starten
////////////////////////////////////////////////////////////////////////////////
procedure SVC_startasservice();
begin
     svc_dispatchtable[0].lpservicename:=pchar(servicename);
     svc_dispatchtable[0].lpserviceproc:=@serviceproc;
     svc_dispatchtable[1].lpservicename:=NIL;
     svc_dispatchtable[1].lpserviceproc:=NIL;
     startservicectrldispatcher(svc_dispatchtable[0]);
end;



procedure SVC_prog_main();
var
   modname : array[0..MAX_PATH-1] of char;
   bHelp          : Boolean;
   iStartMode     : Integer;
begin
     bHelp:=TRUE;
     //Unseren eigenen Namen holen
     GetModuleFilename(hInstance,@modname[0],MAX_PATH);

     //Kein Parameter, dann als Service starten
     if (ParamCount()=0) then
        begin
             SVC_StartAsService();
             Exit;
        end;

     //Parameter analysieren
     //Autostart ?
     iStartMode:=SERVICE_DEMAND_START;
     if (pos('/auto',ParamStr(2))=1) then
        begin
             iStartMode:=SERVICE_AUTO_START;
        end;

     //Pos ignoriert Groß/Klein-Schreibung
     if (pos('/install',ParamStr(1))=1) then
        begin
             //Keine Hilfe anzeigen
             bHelp:=FALSE;
             //Manuellen start erzwingen
             SVC_InstallService(ServiceName,DisplayName,string(modname),iStartMode);
        end;

     //Pos ignoriert Groß/Klein-Schreibung
     if (pos('/uninstall',ParamStr(1))=1) then
        begin
             //Keine Hilfe anzeigen
             bHelp:=FALSE;
             //Manuellen start erzwingen
             SVC_UnInstallService(ServiceName);
        end;

     //Evtl. Hilfe ausgeben
     if (bHelp) then SVC_ShowMessage(svc_help);
end;
