/////////////////////////////////////////////////////////////////////////////////
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
/// Commandline
/// /install       = install as Service (manual startup)
/// /install /auto = install as Service (automatic startup)
/// /U             = uninstall service;
/////////////////////////////////////////////////////////////////////////////////
program svc;
uses
  windows,
  winsvc;

const
  servicename='MeinService'; //Wird im Dienste manager dargestellt
  displayname='myserv';      //interner Servicename
var
   bOnLine : Boolean =FALSE;
   bOffLine: Boolean =FALSE;

procedure service_main; forward;
//Die servicekomponente einfügen
{$INCLUDE include_service.pas}

//Diese Schleife wird im Servicebetrieb durchlaufen
procedure service_main;
begin
  repeat
    if not svc_paused then
       begin
            if (not bOnline) then
               begin
                    bOffline:=FALSE;
                    bOnline:=TRUE;
                    //Der Dienst wurde aktiviert
               end;
            //Hier erledigt der Service alles was er soll
       end
    else
       begin
            if (not bOffline) then
               begin
                    bOffline:=TRUE;
                    bOnline:=FALSE;
                    //Der Dienst wurde pausiert
               end;
       end;
  until svc_stopped;
END;

/// Parameter auswerten und unseren Service installieren
begin
     //Keine Meldungen ausgeben ?
     svc_bquiet:=FALSE;
     //Und Service-Routine anspringen
     SVC_Prog_main();

     //Hier sollte nix mehr kommen
end.
