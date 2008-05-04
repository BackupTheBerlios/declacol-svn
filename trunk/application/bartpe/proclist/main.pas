unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,Unit_ProcessFunctions, Menus,Unit_FormEffects, Gauges,
  adCPUUsage,Unit_StringFunctions;

type
  TFM_Main = class(TForm)
    GroupBox1: TGroupBox;
    LB_Procs: TListBox;
    TI_Refresh: TTimer;
    PopupMenu: TPopupMenu;
    Kill1: TMenuItem;
    Throttle1: TMenuItem;
    N101: TMenuItem;
    N201: TMenuItem;
    N301: TMenuItem;
    N401: TMenuItem;
    N501: TMenuItem;
    N601: TMenuItem;
    N701: TMenuItem;
    N801: TMenuItem;
    N901: TMenuItem;
    off1: TMenuItem;
    N51: TMenuItem;
    GroupBox2: TGroupBox;
    GG_CPU: TGauge;
    TI_CPU: TTimer;
    Panel1: TPanel;
    cb_windowed: TCheckBox;
    LB_NoCPU: TLabel;
    procedure TI_RefreshTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Kill1Click(Sender: TObject);
    procedure N51Click(Sender: TObject);
    procedure TI_CPUTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
    function GetActiveFilename():String;
  public
    { Public-Deklarationen }
  end;

var
  FM_Main    : TFM_Main;
  ThreadPack : TThreadPack;

implementation

{$R *.DFM}
//Den angewählten Namen aus der Liste zurückgeben
function TFM_Main.GetActiveFilename():String;
var
   iIndex : integer;
begin
     Result:='';
     //Markierten Index holen
     iIndex:=LB_Procs.ItemIndex;
     //Was gefunden ?
     if (iIndex <> -1) then
        begin
             //Filename zurückgeben
             result:=LB_Procs.Items[iIndex];
        end;
end;


procedure TFM_Main.TI_RefreshTimer(Sender: TObject);
var
   sTemp  : String;
   iCount : Integer;
   slProc : TStringList;
begin
     //Prozesse holen
     EnumerateProcesses();
     slProc:=TStringList.Create;
     for iCount:=0 to Length(ProcessList)-1 do
         begin
              if (processlist[icount].Window) or (not cb_windowed.checked) then
                 begin
                      slProc.Add(ProcessList[iCount].ProcessPath);
                 end;
         end;
     slProc.Sort;

     //Alte Markierung merken
     if (LB_Procs.ItemIndex<>-1) then
        begin
             sTemp:=LB_Procs.Items[LB_Procs.ItemIndex];
        end
     else
        begin
             sTemp:='';
        end;

     LB_Procs.Clear;
     LB_Procs.Items.Assign(slProc);

     //Alte markierung wieder setzen
     if (sTemp<>'') then
        begin
             LB_Procs.ItemIndex:=LB_Procs.Items.IndexOf(sTemp);
        end;

end;



procedure TFM_Main.FormActivate(Sender: TObject);
begin
     //Bei aktivierung auf jeden Fall refreshen
     TI_RefreshTimer(Self);
end;

//Den ausgewählten Process beenden
procedure TFM_Main.Kill1Click(Sender: TObject);
begin
     //Refresh abschalten
     TI_Refresh.Enabled:=FALSE;

     KillProcess( ProcessIDByFile(GetActiveFilename()) );

     Sleep(2000);

     //Liste refreshen
     TI_RefreshTimer(Self);

     //Refresh einschalten
     TI_Refresh.Enabled:=FALSE;
end;


procedure TFM_Main.N51Click(Sender: TObject);
var
   iCPU : integer;
begin
     //Einen alten Slowdown beenden
     if (Slowdown_Active(ThreadPack)) then SlowDown_Stop(ThreadPack);

     with (Sender as TMenuItem) do
          begin
               //Menüpunkt anhaken
               checked:=TRUE;

               //Aus dem Namen des Menüpunktes auf seine Funktion schließen
               case (StringToCaseSelect( Caption,['&5%','&10%','&20%','&30%','&40%','&50%','&60%','&70%','&80%','&90%','off'])) of
                     0 : iCPU:=5;
                     1 : iCPU:=10;
                     2 : iCPU:=20;
                     3 : iCPU:=30;
                     4 : iCPU:=40;
                     5 : iCPU:=50;
                     6 : iCPU:=60;
                     7 : iCPU:=70;
                     8 : iCPU:=80;
                     9 : iCPU:=90;
               else
                     //Im Zweifelsfall Bremse rausnehmen
                     iCPU:=100;
               end;
          end;

     //Und den neuen starten
     if (iCPU < 100) then ThreadPack:=SlowDown_Start(ProcessIDByFile(GetActiveFilename()),iCPU);
end;



procedure TFM_Main.TI_CPUTimer(Sender: TObject);
const
     cCPU : integer = 1;
begin
    //Und die CPU-Last ausgeben
    try
       CollectCPUData;

       //Für alle Prozessoren zusammen
       if (Trunc(GetCPUUsage(GetCPUCount-1)*100)>cCPU) then
          begin
               inc(cCPU);
          end
       else
          begin
               dec(cCPU);
          end;
          GG_CPU.Progress:=cCPU;
    except
          //Wenn das Performancemodul nicht geladen werden kann,
          //dan abbrechn
          LB_NoCPU.Visible:=TRUE;
          TI_CPU.Enabled:=FALSE;
    end;
end;

procedure TFM_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     //Einen alten Slowdown beenden
     if (Slowdown_Active(ThreadPack)) then SlowDown_Stop(ThreadPack);
end;

end.
