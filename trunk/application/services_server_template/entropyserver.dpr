/////////////////////////////////////////////////////////////////////////////////
///
/// (c) 2007 Borg@Sven-of-Nine.de / http://www.sven-of-nine.de
///
/// Commandline
/// /install       = install as Service (manual startup)
/// /install /auto = install as Service (automatic startup)
/// /uninstall     = uninstall service;
/////////////////////////////////////////////////////////////////////////////////
program entropyserver;
uses
  windows,
  winsvc,
  thread_service;

const
  servicename='entropyprovider';          //Interner Name des Services
  displayname='Entropy Provider';  //Name des Services in der Diensteverwaltung
var
   svcthread : tsvcthread;

procedure service_main; forward;
//Die servicekomponente einfügen
{$INCLUDE include_service.pas}


/////////////////////////////////////////////////////////////////////////////////
//Diese Schleife startet einfach den ServiceThread und wartet dann auf sein Ende
/////////////////////////////////////////////////////////////////////////////////

procedure service_main;
begin
  //Den Service-Thread erzeugen
  svcthread:=tsvcthread.Create();
  svcthread.FreeOnTerminate:=TRUE;

  //Hier können evlt. noch vorbereitungen gemacht werden

  //Thread starten
  svc_resume();

  //Auf das Ende des Threads warten
  svcthread.waitfor();
end;

/////////////////////////////////////////////////////////////////////////////////
/// Parameter auswerten und unseren Service installieren
begin
     //Keine Meldungen ausgeben ?
     svc_bquiet:=FALSE;

     //Und Service-Routine anspringen
     SVC_Prog_main();
end.
