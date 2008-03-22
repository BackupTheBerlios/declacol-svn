unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_filefunctions, CheckLst, ComCtrls;

type
  TForm1 = class(TForm)
    dgOpen: TOpenDialog;
    dgSave: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    gbInput: TGroupBox;
    lbFiles: TCheckListBox;
    GroupBox1: TGroupBox;
    edJoinTarget: TEdit;
    btBrowseJoinTarget: TButton;
    btJoin: TButton;
    btSplit: TButton;
    edJoinSource: TEdit;
    btBrowseJoinSource: TButton;
    GroupBox2: TGroupBox;
    edSplitSource: TEdit;
    btBrowseSplitSource: TButton;
    GroupBox3: TGroupBox;
    cbSize: TComboBox;
    Label1: TLabel;
    GroupBox4: TGroupBox;
    edSplitTarget: TEdit;
    btBrowseSplitTarget: TButton;
    procedure btBrowseJoinSourceClick(Sender: TObject);
    procedure btBrowseJoinTargetClick(Sender: TObject);
    procedure btJoinClick(Sender: TObject);
    procedure cbSizeChange(Sender: TObject);
    procedure btSplitClick(Sender: TObject);
    procedure btBrowseSplitSourceClick(Sender: TObject);
    procedure btBrowseSplitTargetClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btBrowseJoinSourceClick(Sender: TObject);
var
  slFiles : TStringList;
begin
  with (dgOpen) do
    begin
      if (Execute) then
        begin
          edJoinSource.Text:=Filename;
          //Und alle weiteren Dateien suchen
          slFiles:=FileScan( ExtractFilePath(Filename),
                             ChangeFileExt(ExtractFilename(Filename),'*'),
                             '',
                             0);
          slFiles.Sort();
          //Und alle Datein anzeigen
          lbFiles.Clear();
          while (slFiles.Count > 0) do
            begin
              lbFiles.Checked[lbFiles.Items.Add(ExtractFilename(slFiles[0]))]:=True;

              slFiles.Delete(0);
            end;
          //Ausgabevorschlag machen
          edJoinTarget.Text:=ChangeFileExt(Filename,'');
        end;
    end;
end;

procedure TForm1.btBrowseSplitSourceClick(Sender: TObject);
begin
  with (dgOpen) do
    begin
      if (Execute) then
        begin
          edSplitSource.Text:=Filename;
          edSplitTarget.Text:=Filename;
        end;
    end;
//
end;


procedure TForm1.btBrowseJoinTargetClick(Sender: TObject);
begin
  with (dgSave) do
    begin
      if (Execute) then
        begin
          edJoinTarget.Text:=Filename;
        end;
    end;
end;

procedure TForm1.btBrowseSplitTargetClick(Sender: TObject);
begin
  with (dgSave) do
    begin
      if (Execute) then
        begin
          edSplitTarget.Text:=Filename;
        end;
    end;
end;


procedure TForm1.cbSizeChange(Sender: TObject);
begin
  if (StrToFloatDef(cbSize.Text,-1)<0) then
    begin
      ShowMessage('enter a real size');
      cbSize.Text:=cbSize.Items[0];
    end;
end;

procedure TForm1.btJoinClick(Sender: TObject);
var
  u32Index : unsigned32;
  rTarget  : TFileStream;
  rSource  : TFileStream;
begin
  u32Index:=0;

  try
    rTarget:=TFileStream.Create(edJoinTarget.Text,fmOpenWrite or fmCreate);

    //Alle Datei zusammenfügen
    while (u32Index < unsigned32(lbFiles.Count)) do
      begin
        lbFiles.ItemIndex:=u32Index;
        //Nur angehakte Dateien zusammenführen
        if (lbFiles.Checked[u32Index]) then
          begin
            rSource:=TFileStream.Create(lbFiles.Items[u32Index],fmOpenRead);
            Application.ProcessMessages();

            rTarget.CopyFrom(rSource,rSource.Size);

            rSource.Free();
          end;

        inc(u32Index);
      end;
    rTarget.Free();
  except
    //Trotz Ausnahme aufräumen
    if (Assigned(rTarget)) then rTarget.Free();
    if (Assigned(rSource)) then rSource.Free();
  end;
  ShowMessage('done');
end;

procedure TForm1.btSplitClick(Sender: TObject);
var
  rTarget  : TFileStream;
  rSource  : TFileStream;

  u32Loop  : unsigned32;
  u32Size  : unsigned32;
  u32Mod   : unsigned32;
  u32Index : unsigned32;
begin
  try
    rSource:=TFileStream.Create(edSplitSource.Text,fmOpenRead);

    //Anzahl der durchläufe
    u32Size:=trunc( StrToFloatDef(cbSize.Text,1.44) * 1024 * 1024 );
    u32Loop:=rSource.Size div u32Size;
    u32Mod :=rSource.Size mod u32Size;
    u32Index:=0;

    while (u32Loop > 0) do
      begin
        rTarget:=TFileStream.Create(edSplitTarget.Text+Format('.%.3d',[u32Index]),fmOpenWrite or fmCreate);

        rTarget.CopyFrom(rSource,u32Size);

        rTarget.Free();

        inc (u32Index);
        dec (u32Loop);
      end;

    //Und der Rest
    rTarget:=TFileStream.Create(edSplitTarget.Text+Format('.%.3d',[u32Index]),fmOpenWrite or fmCreate);

    rTarget.CopyFrom(rSource,u32Mod);
    rTarget.Free();

    rSource.Free();

  except
    //Trotz Ausnahme aufräumen
    if (Assigned(rTarget)) then rTarget.Free();
    if (Assigned(rSource)) then rSource.Free();
  end;
  ShowMessage('done');
end;
end.
