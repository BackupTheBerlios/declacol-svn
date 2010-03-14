unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DosCommand,unit_log, FileCtrl, ExtCtrls, ComCtrls, unit_processfunctions;

type
  TfmMain = class(TForm)
    DosCom: TDosCommand;
    gbCommand: TGroupBox;
    cbUseHeuristic: TCheckBox;
    cbShowFiles: TCheckBox;
    cbIgnorePackedFiles: TCheckBox;
    cbScanArchives: TCheckBox;
    cbScanBootsector: TCheckBox;
    cbDisinfect: TCheckBox;
    cbRename: TCheckBox;
    btUpdate: TButton;
    btScan: TButton;
    gbTarget: TGroupBox;
    DriveComboBox1: TDriveComboBox;
    dlbMain: TDirectoryListBox;
    gbLog: TGroupBox;
    Panel1: TPanel;
    btCancel: TButton;
    Animate1: TAnimate;
    mmLog: TMemo;
    lbinfected: TLabel;
    lbinfectedcount: TLabel;
    procedure DosComTerminated(Sender: TObject);
    procedure btUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btScanClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure DosComNewLine(Sender: TObject; NewLine: String;
      OutputType: TOutputType);
  private
    { Private-Deklarationen }
    RunOnce : Boolean;
    slInfected : TStringlist;
  public
    { Public-Deklarationen }
    procedure addlog(text : longstring);
  end;

const
  exe       = 'bdc.exe';

var
  fmMain    : TfmMain;
  sBaseDir  : Longstring;
  sBaseExe  : longstring;

implementation

uses unit_failure;

{$R *.dfm}
////////////////////////////////////////////////////////////////////////////////////////////////////
//RunOnce
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.FormActivate(Sender: TObject);
begin
  //RunOnce sicher implementieren
  if (Self.RunOnce=FALSE) then
    begin
      addlog('bitgui v0.1');
      addlog('(c) 2010 borg@sven-of-nine.de');
      addlog('--------------------------------------------------------------------------------------------------------------------------------------');

      //Alle Basiseinstellungen machen
      self.Caption:=application.Title;
      sBaseDir:=extractfilepath(application.ExeName);

sBasedir:='C:\Dokumente und Einstellungen\sven\Desktop\bitdefender\';

      sBaseExe:=sBaseDir + exe;
      addlog('basedir : ' + sBaseDir);

      //Exegefunden
      if (fileexists(sBaseExe)=TRUE) then
        begin
          addlog(exe + ' found');
        end
      else
        begin
          addlog(exe + ' not found');
          gbCommand.Visible:=FALSE;
        end;

      addlog('--------------------------------------------------------------------------------------------------------------------------------------');

      Self.RunOnce:=TRUE;
    end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// OnCreate
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.FormCreate(Sender: TObject);
begin
  Self.RunOnce:=FALSE;
  slInfected:=TStringList.Create();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//run update
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btUpdateClick(Sender: TObject);
begin
  lbinfectedcount.caption:='0';
  Panel1.Visible:=TRUE;
  Animate1.Active:=TRUE;

  mmLog.Clear();

  DosCom.CommandLine:=sBaseexe + ' /update';

  self.addlog('running :'+ExtractFilename(DosCom.Commandline));
  slInfected.Clear();
  DosCom.Execute();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//Scan
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btScanClick(Sender: TObject);
var
  sCom : longstring;
begin
  lbinfectedcount.caption:='0';
  Panel1.Visible:=TRUE;
  Animate1.Active:=TRUE;

  mmLog.Clear();

  addlog('target : ' + dlbMain.Directory);

  sCom:=' /infext=virus';
  if (cbDisinfect.Checked)         then sCom:=sCom + ' /dis';
  if (cbIgnorePackedFiles.Checked) then sCom:=sCom + ' /nopack';
  if (cbRename.Checked)            then sCom:=sCom + ' /ren';
  if (cbScanArchives.Checked)      then sCom:=sCom + ' /arc';
  if (cbScanBootsector.Checked)    then sCom:=sCom + ' /boot';
  if (cbShowFiles.Checked)         then sCom:=sCom + ' /list';
  if (not cbUseHeuristic.Checked)  then sCom:=sCom + ' /nohed';

  DosCom.CommandLine:=sBaseexe + ' "' + dlbMain.Directory + '"' + sCom;
  self.addlog('command : ' + ExtractFilename(sBaseExe + sCom));
  self.addlog('');
  slInfected.Clear();
  DosCom.Execute();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//Cancel
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btCancelClick(Sender: TObject);
begin
  DosCom.Stop();
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//DOS-Progress
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.DosComNewLine(Sender: TObject; NewLine: String;
  OutputType: TOutputType);
begin
  if (pos(' infected: ',NewLine)>0) then
    begin
      slInfected.Add(NewLine);
      lbinfectedcount.caption:=inttostr(slInfected.Count);
    end;

  self.addlog(NewLine);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//Dos Done
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.DosComTerminated(Sender: TObject);
begin
  if (slInfected.Count > 0) then
    begin
      addlog('--------------------------------------------------------------------------------------------------------------------------------------');
      addlog('INFECTED FILES');
      mmLog.Lines.AddStrings(slInfected);
      addlog('--------------------------------------------------------------------------------------------------------------------------------------');
    end;

  self.addlog('done');
  Animate1.Active:=FALSE;
  Panel1.Visible:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//AddLog
////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.addlog(text : longstring);
begin
  mmLog.Lines.Add(Text);


  //Maimal 10000 Eintraege
  while (mmLog.Lines.count > 10000) do
    begin
      mmLog.Lines.Delete(0);
    end;
end;

end.
