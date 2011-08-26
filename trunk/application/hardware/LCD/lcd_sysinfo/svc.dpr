/////////////////////////////////////////////////////////////////////////////////
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
/// Commandline
/// /install       = install as Service (manual startup)
/// /install /auto = install as Service (automatic startup)
/// /uninstall     = uninstall service;
/////////////////////////////////////////////////////////////////////////////////
program svc;
uses
  windows,
  winsvc,
  sysutils,
  adCPuUsage,
  Graphics,
  Unit_EasyIni,
  Unit_LCD,
  Unit_Hardware;

const
  servicename ='LCDSysInfo'; //interner Servicename
  displayname ='LCD Systeminfo';      //Wird im Dienste manager dargestellt

var
   LCDBMP       : TBitmap;
   LCD          : TLCDDisplay;
   i64Delay     : int64;
   iContrast    : integer;
   iLPTPort     : integer;
   sIP          : String;
   bBars        : Boolean;
   bFlip        : Boolean;
   bMirror      : Boolean;
   i64TimeStamp : int64 = 0;
   bOnLine      : Boolean =FALSE;
   bOffLine     : Boolean =FALSE;
   Ini          : TEasyIni;
   iTemp        : integer;
{$INCLUDE include_image.pas}

procedure service_main; forward;
//Die servicekomponente einfügen
{$INCLUDE include_service.pas}

/////////////////////////////////////////////////////////////////////////////////
/// LCD-Funktionen
/////////////////////////////////////////////////////////////////////////////////
function InitLCD():Boolean;
begin
     try
        LCD:=TLCDDisplay.Create;
        //Alle Parameter setzen
        LCD.LPTPort:=iLPTPORT;
        LCD.Contrast:=iContrast;
        LCD.Mirror:=bMirror;
        LCD.Flip:=bFlip;
        //Und Kanal öffnen
        LCD.Open;
        //Display an
        LCD.DisplayOn;
        result:=TRUE;
     except
           result:=FALSE;
     end;
end;

procedure FreeLCD();
begin
     try
        //Kanal schließen
        LCD.Close;
        //Entladen
        LCD.Free;
     except
     end;
end;
/////////////////////////////////////////////////////////////////////////////////
/// BMP-Funktionen
/////////////////////////////////////////////////////////////////////////////////
function InitBMP():Boolean;
begin
     try
        LCDBMP:=TBitmap.Create;
        with LCDBMP do
             begin
                  Width:=96;
                  Height:=32;
                  PixelFormat:=pf1Bit;
             end;
        result:=TRUE;
     except
           result:=FALSE;
     end;
end;

procedure FreeBMP();
begin
     try
        LCDBMP.Free;
     except
     end;
end;

procedure Text2Bitmap(Text:String);
begin
     //Einen Text in das Bitmap schreiben und senden
     LCD.ClearBitmap(LCDBMP);
     LCD.PaintText(LCDBMP,48-(((LCD.FontWidth+1)*Length(Text)) shr 1),16-(LCD.FontHeight shr 1),Text);
     LCD.WriteBitmap(LCDBMP);
end;

/////////////////////////////////////////////////////////////////////////////////
/// InitData-Funktionen
/////////////////////////////////////////////////////////////////////////////////
procedure ReadConfig();
begin
     //Ini klarmachen
     Ini:=TEasyIni.Create;
     Ini.Open(ParamStr(0)+'.ini');

     //Werte lesen
     iLPTPort :=Ini.Read('LPT'     ,'port'     ,$03bc);
     iContrast:=Ini.Read('DISPLAY' ,'contrast' ,13);
     bBars    :=Ini.Read('DISPLAY' ,'bars' ,TRUE);
     iTemp    :=Ini.Read('SYSTEM'  ,'delay'    ,10000);
     sIP      :=Ini.Read('OUTPUT'  ,'ip','127.0.0.1');
     bFlip    :=Ini.Read('DISPLAY' ,'flip' ,FALSE);
     bMirror  :=Ini.Read('DISPLAY' ,'mirror' ,FALSE);
     i64Delay:=abs(iTemp);

     //Und gleich wieder schreiben
     //um die Datei mit Stanardwerten zu füllen, falls sie noch nicht
     //existiert
     Ini.Write('LPT'     ,'port'     ,iLPTPort);
     Ini.Write('DISPLAY' ,'contrast' ,iContrast);
     Ini.Write('DISPLAY' ,'bars'     ,bBars);
     Ini.Write('SYSTEM'  ,'delay'    ,integer(i64Delay));
     Ini.Write('OUTPUT'  ,'ip'       ,sIP);
     Ini.Write('DISPLAY' ,'flip'     ,bFlip);
     Ini.Write('DISPLAY' ,'mirror'   ,bMirror);

     //Fertig
     Ini.Flush;
     Ini.Close;
     Ini.Free;
end;

/////////////////////////////////////////////////////////////////////////////////
/// LCD-Funktionen
/////////////////////////////////////////////////////////////////////////////////
procedure SendInit();
var
   iX : integer;
   iY : integer;
   i  : integer;
begin
     //LCDBitmap mit startbild initialisieren
     i:=0;
     LCD.ClearBitmap(LCDBMP);
     for iY:=0 to LCDBMP.Height-1 do
         for iX:=0 to LCDBMP.Width -1 do
             begin
                  if (aImage[i]=1) then
                     LCDBMP.Canvas.Pixels[iX,iY]:=clBlack
                  else
                      LCDBMP.Canvas.Pixels[iX,iY]:=clWhite;
                  inc(i);
             end;
     LCD.WriteBitmap(LCDBMP);
end;

procedure SendData();
var
   iCPU : cardinal;
   iMem : cardinal;
   iLan : cardinal;
   iHD1 : cardinal;
   iHD2 : cardinal;
begin
     //Daten holen
     CollectCPUData();
     iCPU:=round(GetCPUUsage(GetCPUCount()-1)*100);
     iMem:=MemInfo.dwMemoryLoad;
     iLan:=LanInfoIn(sIP);
     iHD1:=HDInfo('C').lwHDLoad;
     iHD2:=HDInfo('D').lwHDLoad;


     //Bitmap erzeugen
     LCD.ClearBitmap(LCDBMP);
     LCD.FontStyle:=1;
     if (bBars) then
        begin

             //Darstellung in Balken
               LCD.PaintText(LCDBMP, 1, 1,'CPU');
               LCD.PaintBar (LCDBMP,20, 1,75,5,iCPU);

               LCD.PaintText(LCDBMP, 1, 7,'LAN');
               LCD.PaintBar (LCDBMP,20, 7,75,5,iLAN);

               LCD.PaintText(LCDBMP, 1, 13,'MEM');
               LCD.PaintBar (LCDBMP,20, 13,75,5,iMEM);

               LCD.PaintText(LCDBMP, 1, 19,'HD1');
               LCD.PaintBar (LCDBMP,20, 19,75,5,iHD1);

               LCD.PaintText(LCDBMP, 1, 25,'HD2');
               LCD.PaintBar (LCDBMP,20, 25,75,5,iHD2);
        end
     else
        begin
             //Darstellung in Säulen
               LCD.PaintText(LCDBMP,1,32-LCD.FontHeight,'CPU');
               LCD.PaintBar (LCDBMP,1,0,17,26,iCPU,FALSE);

               LCD.PaintText(LCDBMP,20,32-LCD.FontHeight,'LAN');
               LCD.PaintBar (LCDBMP,20,0,17,26,iLAN,FALSE);

               LCD.PaintText(LCDBMP,39,32-LCD.FontHeight,'MEM');
               LCD.PaintBar (LCDBMP,39,0,17,26,iMEM,FALSE);

               LCD.PaintText(LCDBMP,58,32-LCD.FontHeight,'HD1');
               LCD.PaintBar (LCDBMP,58,0,17,26,iHD1,FALSE);

               LCD.PaintText(LCDBMP,77,32-LCD.FontHeight,'HD2');
               LCD.PaintBar (LCDBMP,77,0,17,26,IHD1,FALSE);
        end;

     //Bitmap senden
     LCD.WriteBitmap(LCDBMP);

end;




//Diese Schleife wird im Servicebetrieb durchlaufen
procedure service_main();
begin
     //Konfiguration lesen
     ReadConfig();

     //Alles initialisieren
     if (not InitBMP()) then exit;
     if (not InitLCD()) then exit;
     SendInit();
     i64TimeStamp:=0;

     //Hauptschleife
     repeat
           if not svc_paused then
              begin
                   if (not bOnline) then
                      begin
                           bOffline:=FALSE;
                           bOnline:=TRUE;
                           //Der Dienst wurde aktiviert
                           //bei einer Aktivierung direkt starten
                           //Init-Screen immer für 10 Sekunden zeigen
                           i64TimeStamp:=GetTickCount()+5*1000;
                      end;

                   //Hier erledigt der Service alles was er soll
                   //Ist eine Zeiteinheit vergangen ?
                   if (i64TimeStamp < GetTickCount() ) then
                      begin
                           //Neuen Zeitstempel holen
                           i64TimeStamp:=GetTickCount()+i64Delay;

                           //Und sachen machen
                           SendData();

                           //Workingset verkleinern
                           SetProcessWorkingSetSize(GetCurrentProcess,$ffffffff,$ffffffff);
                      end;
              end
           else
              begin
                   if (not bOffline) then
                      begin
                           bOffline:=TRUE;
                           bOnline:=FALSE;
                           //Der Dienst wurde pausiert
                           Text2Bitmap('Service Paused');
                      end;
              end;
           //Halbe Sekunde schlafen
           Sleep(500);
     until svc_stopped;

     //Garbage Collector
     FreeBMP();
     FreeLCD();
end;

/// Parameter auswerten und unseren Service installieren
begin
     //Service-Routine anspringen
     SVC_Prog_main();

     //Hier sollte nix mehr kommen
end.
