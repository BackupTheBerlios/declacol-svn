unit unit_main;

interface

uses
  unit_typedefs,unit_filefunctions,unit_processfunctions,class_runtime,unit_strings,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    gbPath: TGroupBox;
    edSource: TEdit;
    edTarget: TEdit;
    btBrowseSource: TButton;
    btBrowseTarget: TButton;
    gbOption: TGroupBox;
    btStart: TButton;
    btStop: TButton;
    cbSkipExistingFiles: TCheckBox;
    rbHighPrio: TRadioButton;
    rbLowPrio: TRadioButton;
    rbNormalPrio: TRadioButton;
    GroupBox1: TGroupBox;
    lbStatus: TLabel;
    tiStatusUpdate: TTimer;
    cbRandom: TCheckBox;
    pbProgress: TProgressBar;
    lbRuntime: TLabel;
    procedure btStartClick(Sender: TObject);
    procedure tiStatusUpdateTimer(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btBrowseSourceClick(Sender: TObject);
    procedure btBrowseTargetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure invert_button_status();
    procedure shuffle_files();
  public
    { Public-Deklarationen }
    SourceFiles :TStringlist;
  end;

var
  Form1: TForm1;
  bBusy : Boolean = FALSE;
  sStatus : longstring;
  Runtime    : TRuntime;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Runtime:=TRuntime.Create();
end;


procedure TForm1.btStartClick(Sender: TObject);
var
  dummy      : unsigned16;
  sTarget    : longstring;
  u32Counter : unsigned32;
begin
  invert_button_status();
  bBusy:=TRUE;

  if (rbHighPrio.Checked) then SetSelfPriority(HIGH_PRIORITY_CLASS);
  if (rbLowPrio.Checked)  then SetSelfPriority(IDLE_PRIORITY_CLASS);

  SourceFiles:=TStringList.Create();

  sStatus:='scanning source';
  SourceFiles:=filescan(edSource.Text,'*.*','',8192);
  pbProgress.Max:=SourceFiles.Count;
  if (cbRandom.Checked) then
    begin
      sStatus:='shuffeling files';
      shuffle_files();
    end;

  sStatus:='copying files';
  u32Counter:=0;
  pbProgress.Position:=0;
  Runtime.events_max:=SourceFiles.Count;
  Runtime.Start();

  while (SourceFiles.Count > 0) and (bBusy=TRUE) do
    begin
      sStatus:='copying ' + extractfilename(SourceFiles[0]);
      application.ProcessMessages();

      //Ziel bauen
      sTarget:=string_replace(SourceFiles[0],edSource.Text,edTarget.Text);

      //Und Datei kopieren
      if (fileexists(sTarget)=FALSE) or (cbSkipExistingFiles.Checked = FALSE) then
        begin
          if (CopyData(SourceFiles[0],sTarget)<>TRUE) then
            begin
              //Fehler aufgetreten
              bBusy:=FALSE;
              Application.MessageBox('an error occured','Error');
            end;
        end;

      Runtime.triggerevent(1);

      inc(u32Counter);
      pbProgress.Position:=u32Counter;

      SourceFiles.Delete(0);

      Application.ProcessMessages();
    end;

  sStatus:='idle';
  SourceFiles.Destroy();
  SetSelfPriority(NORMAL_PRIORITY_CLASS);
  bBusy:=FALSE;
  invert_button_status();
end;

procedure TForm1.tiStatusUpdateTimer(Sender: TObject);
begin
  lbStatus.Caption:=sStatus;
  lbRuntime.Caption:=Runtime.eta;
  Application.ProcessMessages();
end;

procedure TForm1.btStopClick(Sender: TObject);
begin
  bBusy:=FALSE;
end;

procedure TForm1.btBrowseSourceClick(Sender: TObject);
begin
  edSource.Text:=BrowseDialog('select source directory');
end;

procedure TForm1.btBrowseTargetClick(Sender: TObject);
begin
  edTarget.Text:=BrowseDialog('select target directory');
end;

procedure TForm1.invert_button_status();
begin
  btStop.Visible :=not btStop.Visible;
  btStart.Visible:=not btStart.Visible;
  pbProgress.Visible:=not pbProgress.Visible;
end;

procedure Tform1.shuffle_files();
var
  index : unsigned32;
begin
  index:=SourceFiles.Count SHL 1;

  while (index > 0) and (bBusy=TRUE) do
    begin
      SourceFiles.Exchange(random(SourceFiles.Count),random(SourceFiles.Count));

      if (index mod 100 = 0) then
        begin
          Application.ProcessMessages();
        end;

      dec(index);
    end;
end;


end.
