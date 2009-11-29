unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, untUnRar, StdCtrls, CheckLst, ComCtrls, ExtCtrls, DFUnRar,IniFiles,
  FileCtrl,ShellApi;

type
  TfmMain = class(TForm)
    PCMain: TPageControl;
    TSMain: TTabSheet;
    TSOptions: TTabSheet;
    TSPasswords: TTabSheet;
    TSLog: TTabSheet;
    GBArchives: TGroupBox;
    CLBFiles: TCheckListBox;
    GBLog: TGroupBox;
    lbLog: TListBox;
    GBOptions: TGroupBox;
    GBPasswords: TGroupBox;
    LBPasswords: TListBox;
    GBMain: TGroupBox;
    btUnpack: TButton;
    btBreak: TButton;
    PNProgress: TPanel;
    PBUnpack: TProgressBar;
    UnRAR: TDFUnRar;
    lbPW: TLabel;
    lbTargetDir: TLabel;
    btBrowseTarget: TButton;
    edTargetDir: TEdit;
    CBSmartPW: TCheckBox;
    CBSavePW: TCheckBox;
    DGTarget: TSaveDialog;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btUnpackClick(Sender: TObject);
    procedure UnRARError(Sender: TObject; Message: String;
      MessageID: Integer);
    procedure UnRARProgress(Sender: TObject; FilesProcessed, FileCount,
      SizeProcessed, SizeCount: Cardinal);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function GetNextPassword(var password : longstring):boolean;

    function Unpack(rarfile : longstring; targetdir : longstring):boolean;
    procedure MovePW();
    procedure btBrowseTargetClick(Sender: TObject);
    procedure btBreakClick(Sender: TObject);
    procedure TSOptionsShow(Sender: TObject);
    procedure LoadOptions();
    procedure SaveOptions();
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure btClearClick(Sender: TObject);
  private
    { Private-Deklarationen }
    bRunOnce   : Boolean;
    sBaseDir   : Longstring;
    bBusy      : Boolean;

    Procedure RunOnce();
    Procedure AddLog(text : Longstring);
  public
    { Public-Deklarationen }
  end;

const
  PASSFILE = 'passwords.txt';
  FILEFILE = 'files.lst';
  RARDLL   = 'unrar.dll';

var
  fmMain: TfmMain;


implementation

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.FormCreate(Sender: TObject);
begin
  Self.bRunOnce:=FALSE;
  DragAcceptFiles(Self.Handle, True);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.FormActivate(Sender: TObject);
begin
  if (Self.bRunOnce=FALSE) then
    begin
      Self.RunOnce();
    end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.RunOnce();
begin
  Self.bRunOnce:=TRUE;

  //Eigenes Verzeichnis holen
  sBaseDir:=ExtractFilePath(ParamStr(0))+'\';

  //Dateien
  if (fileexists(sBaseDir + FILEFILE)=TRUE) then
    begin
      CLBFiles.Items.LoadFromFile(sBaseDir + FILEFILE);
    end;


  //Passwörter?
  if (fileexists(sBaseDir + PASSFILE)=TRUE) then
    begin
      AddLog('loading passwords');
      LBPasswords.Items.LoadFromFile(sBaseDir + PASSFILE);
      AddLog(IntToStr(LBPasswords.Items.Count) + ' words found');
    end
  else
    begin
      AddLog('passwordfile "'+PASSFILE+'" not found');
    end;

  //unrar.dll?
  if (fileexists(sBaseDir + RARDLL)=TRUE) then
    begin
      AddLog(RARDLL+' found');
    end
  else
    begin
      AddLog(RARDLL+' not found');
    end;

  LoadOptions();
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.AddLog(text : longstring);
begin
  LBLog.ItemIndex:=LBLog.Items.Add(Text);
end;


////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btUnpackClick(Sender: TObject);
var
  u32Index : unsigned32;
  sOutDir  : Longstring;
begin
  bBusy:=TRUE;
  btUnpack.Visible:=FALSE;

  //Alle Haken löschen
  u32Index:=0;
  while (u32Index < unsigned32(CLBFiles.Count)) do
    begin
      CLBFiles.Checked[u32Index]:=FALSE;
      inc(u32Index);
    end;

  //Alle Archive entpacken
  u32Index:=0;
  while (u32Index < unsigned32(CLBFiles.Count)) AND (bBusy = TRUE) do
    begin
      //Dateicounter setzen
      CLBFiles.ItemIndex:=u32Index;

      //DArchive da?
      if (fileexists(CLBFiles.Items[u32Index])=TRUE) then
        begin
          //Alle notwendigen Werte setzen
          sOutDir:=ExtractFileName(CLBFiles.Items[u32Index]);
          sOutDir:=ChangeFileExt(sOutDir,'');

          //Entpacken
          if (Unpack(CLBFiles.Items[u32Index],edTargetDir.Text + '\' + sOutDir)=TRUE) then
            begin
              CLBFiles.Checked[u32Index]:=TRUE;
              AddLog(sOutDir + ' unpacked');
            end
          else
            begin
              AddLog(sOutDir + ' corrupt or no password found');
            end;
        end;

      inc(u32Index);
    end;

  bBusy:=FALSE;
  btUnpack.Visible:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.UnRARProgress(Sender: TObject; FilesProcessed, FileCount,
  SizeProcessed, SizeCount: Cardinal);
begin
  PBUnpack.Max:=signed32(FileCount);
  PBUnpack.Position:=signed32(FilesProcessed);
  Application.ProcessMessages();
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.UnRARError(Sender: TObject; Message: String;
  MessageID: Integer);
begin
  AddLog(Message);
end;


////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  DragAcceptFiles(fmMain.Handle, False);
  SaveOptions();
end;

////////////////////////////////////////////////////////////////////////////////
//Das aktuelle Kennwort ganz nach vorne schieben und alle anderen Vorkommen
//löschen
procedure TfmMain.MovePW();
var
  sTemp   : LongString;
  s32Temp : signed32;
begin
  sTemp:=UnRAR.Password;

  if (sTemp <> '') AND (CBSmartPW.Checked=TRUE) then
    begin
      s32Temp:=LBPasswords.Items.IndexOf(sTemp);
      while (s32Temp >= 0) do
        begin
          LBPasswords.Items.Delete(s32Temp);
          s32Temp:=LBPasswords.Items.IndexOf(sTemp);
        end;
      LBPasswords.Items.Insert(0,UnRAR.Password);
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Archiv entpacken
function TfmMain.Unpack(rarfile : longstring; targetdir : longstring):boolean;
var
  sPass : Longstring;
begin
  UnRAR.Password:='';
  UnRAR.FileName := rarfile;
  UnRAR.Directory:= targetdir;
  result:=TRUE;

  LBPasswords.ItemIndex:=0;

  //Kennwort checken
  while ( UnRAR.Test() = FALSE ) AND (result = TRUE) AND (bBusy = TRUE)do
    begin
      //Das nächste Kennwort lesen
      if ( GetNextPassword(sPass) = TRUE ) then
        begin
          UnRAR.Password:=sPass;
          LBPW.Caption:='PW: '+IntToStr(LBPasswords.ItemIndex);
        end
      else
        begin
          result:=FALSE;
        end;
      Application.ProcessMessages();
    end;

  //Nun haben wir das Kennwort oder ''
  if (result=TRUE) AND (bBusy = TRUE) then
    begin
      UnRAR.Extract();
      MovePW();
    end;

end;

////////////////////////////////////////////////////////////////////////////////
//Das nächste Kennwort oder '' liefern
function TfmMain.GetNextPassword(var password : longstring):Boolean;
begin
  if ( LBPasswords.ItemIndex < LBPasswords.Items.Count) then
    begin
      password:=LBPasswords.Items[LBPasswords.ItemIndex];
      LBPasswords.ItemIndex:=LBPasswords.ItemIndex + 1;
      result:=TRUE;
    end
  else
    begin
      password:='';
      result:=FALSE;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btBrowseTargetClick(Sender: TObject);
begin
  with DGTarget do
    begin
      FileName:='output';
//          InitialDir:=sBaseDir;
      if Execute then
        begin
          edTargetDir.Text:=ExtractFilePath(Filename);
        end;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btBreakClick(Sender: TObject);
begin
  bBusy:=FALSE;
  UnRAR.StopProcessing:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.TSOptionsShow(Sender: TObject);
begin
  TSOptions.Visible:=not bBusy;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.LoadOptions();
var
  Ini : TIniFile;
begin
  Ini:=TIniFile.Create(ParamStr(0)+'.ini');

  edTargetDir.Text  :=Ini.ReadString('Options','TargetDir',sBaseDir + 'unpacked');
  CBSavePW.Checked  :=Ini.ReadBool('OPTIONS','SavePW' ,TRUE);
  CBSmartPW.Checked :=Ini.ReadBool('OPTIONS','SmartPW',TRUE);

  Ini.Free();
end;


////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.SaveOptions();
var
  Ini : TIniFile;
begin
  //Optionen abspeichern
  Ini:=TIniFile.Create(ParamStr(0)+'.ini');

  Ini.WriteBool('OPTIONS','SavePW' ,CBSavePW.Checked);
  Ini.WriteBool('OPTIONS','SmartPW',CBSmartPW.Checked);
  Ini.WriteString('OPTIONS','TargetDir',edTargetDir.Text);

  Ini.Free();

  //Passwortliste abspeichern
  if (CBSavePW.Checked=TRUE) then
    begin
      LBPasswords.Items.SaveToFile(sBaseDir + PASSFILE);
    end;

  //Dateiliste abspeichern
  CLBFiles.Items.SaveToFile(sBaseDir + FILEFILE);

end;

procedure TfmMain.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;               // drop handle
  DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
  I: Integer;                 // loops thru all dropped files
  DropPoint: TPoint;          // point where files dropped
begin
  inherited;
  // Store drop handle from the message
  DropH := Msg.Drop;
  try
    // Get count of files dropped
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    // Get name of each file dropped and process it
    for I := 0 to Pred(DroppedFileCount) do
    begin
      // get length of file name
      FileNameLength := DragQueryFile(DropH, I, nil, 0);
      // create string large enough to store file
      // (Delphi allows for #0 terminating character automatically)
      SetLength(FileName, FileNameLength);
      // get the file name
      DragQueryFile(DropH, I, PChar(FileName), FileNameLength + 1);
      // process file name (application specific)
      if (LowerCase(ExtractFileExt(Filename))='.rar') then
        begin
          CLBFiles.Items.Add(Filename);
        end;
    end;
    // Optional: Get point at which files were dropped
    DragQueryPoint(DropH, DropPoint);
    // ... do something with drop point here
  finally
    // Tidy up - release the drop handle
    // don't use DropH again after this
    DragFinish(DropH);
  end;
  // Note we handled message
  Msg.Result := 0;
end;

procedure TfmMain.btClearClick(Sender: TObject);
begin
  CLBFiles.Clear();
end;

end.
