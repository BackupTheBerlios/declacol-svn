unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_filefunctions, CheckLst;

type
  TForm1 = class(TForm)
    gbInput: TGroupBox;
    edSource: TEdit;
    btBrowseSource: TButton;
    dgOpen: TOpenDialog;
    lbFiles: TCheckListBox;
    GroupBox1: TGroupBox;
    edTarget: TEdit;
    btBrowseTarget: TButton;
    dgSave: TSaveDialog;
    btJoin: TButton;
    procedure btBrowseSourceClick(Sender: TObject);
    procedure btBrowseTargetClick(Sender: TObject);
    procedure btJoinClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btBrowseSourceClick(Sender: TObject);
var
  slFiles : TStringList;
begin
  with (dgOpen) do
    begin
      if (Execute) then
        begin
          edSource.Text:=Filename;
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
          edTarget.Text:=ChangeFileExt(Filename,'');
        end;
    end;
end;

procedure TForm1.btBrowseTargetClick(Sender: TObject);
begin
  with (dgSave) do
    begin
      if (Execute) then
        begin
          edTarget.Text:=Filename;          
        end;
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
    rTarget:=TFileStream.Create(edTarget.Text,fmOpenWrite or fmCreate);

    //Alle Datei zusammenfügen
    while (u32Index < unsigned32(lbFiles.Count)) do
      begin
        lbFiles.ItemIndex:=u32Index;
        rSource:=TFileStream.Create(lbFiles.Items[u32Index],fmOpenRead);
        Application.ProcessMessages();

        rTarget.CopyFrom(rSource,rSource.Size);

        rSource.Free();

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

end.
