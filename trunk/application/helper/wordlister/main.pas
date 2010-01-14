unit main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,IniFiles;

type
  TForm1 = class(TForm)
    dgAddFile: TOpenDialog;
    gbStats: TGroupBox;
    GroupBox1: TGroupBox;
    Button1: TButton;
    btUpper: TButton;
    bTLower: TButton;
    Button4: TButton;
    Label1: TLabel;
    lbWordCount: TLabel;
    Timer1: TTimer;
    lbLog: TListBox;
    btUnique: TButton;
    pbProgress: TProgressBar;
    dgSaveFile: TSaveDialog;
    btClear: TButton;
    Label2: TLabel;
    lbFile: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bTLowerClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btUpperClick(Sender: TObject);
    procedure btUniqueClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  Words : THashedStringList;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Words:=THashedStringlist.Create();
end;

//Eine neue Wortliste der internen Liste zufügen
procedure TForm1.Button1Click(Sender: TObject);
var
  NewWords : TStringlist;
begin
  with (dgAddFile) do
    begin
      if (Execute=TRUE) then
        begin
          //Um den Ladevorgang zu beschleunigen
          //laden wir in eine temporär Liste und
          //fügen dann erst zu
          NewWords:=TStringList.Create();

          LbLog.Items.Add('loading file');
          Application.ProcessMessages();

          NewWords.LoadFromFile(Filename);

          lbLog.ItemIndex:=lbLog.Items.Add(IntToStr(NewWords.Count)+' words found');
          Application.ProcessMessages();
          Words.AddStrings(NewWords);

          lbLog.ItemIndex:=LbLog.Items.Add('done');

          NewWords.Free();

          lbFile.Caption:=ExtractFilename(Filename);
        end;
    end;
  lbWordCount.Caption:=IntToStr(Words.Count);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  Words.Free();
end;

//Alles in Kleinschrift wandeln
//Quick'n'Dirty
procedure TForm1.bTLowerClick(Sender: TObject);
var
  index : unsigned32;
  Delay : unsigned8;
begin
  pbProgress.Max := Words.Count;
  Delay:=0;

  lbLog.ItemIndex:=lbLog.Items.Add('changing case');
  if (Words.Count > 0) then
    begin
      for Index:=0 to Words.Count - 1 do
        begin
          Words[Index]:=LowerCase(Words[Index]);

          //Fortschrittsbalken
          inc(Delay);
          if (Delay=0) then
            begin
              pbProgress.Position:=Index;
              Application.ProcessMessages();
            end;
        end;
    end;
  lbLog.ItemIndex:=lbLog.Items.Add('done');
  pbProgress.Position:=0;
end;

//Alles in Grossschrift wandeln
//Quick'n'Dirty
procedure TForm1.btUpperClick(Sender: TObject);
var
  index : unsigned32;
  Delay : unsigned8;
begin
  pbProgress.Max := Words.Count;
  Delay:=0;

  lbLog.ItemIndex:=lbLog.Items.Add('changing case');
  if (Words.Count > 0) then
    begin
      for Index:=0 to Words.Count - 1 do
        begin
          Words[Index]:=UpperCase(Words[Index]);

          //Fortschrittsbalken
          inc(Delay);
          if (Delay=0) then
            begin
              pbProgress.Position:=Index;
              Application.ProcessMessages();
            end;

        end;
    end;
  lbLog.ItemIndex:=lbLog.Items.Add('done');
  pbProgress.Position:=0;
end;


//Doppelte Worte löschen
//Dazu sortieren wir die Liste erst
//und entfernen dann identische Worte die aufeinander folgen
procedure TForm1.btUniqueClick(Sender: TObject);
var
  Index   : unsigned32;
  Delay   : unsigned8;
  Removed : unsigned32;
begin

  pbProgress.Max := Words.Count;
  Delay:=0;
  Removed:=0;

  lbLog.ItemIndex:=lbLog.Items.Add('removing dupes');

  Words.Sort();

  if (Words.Count > 1) then
    begin
      Index:=0;
      while (Index < unsigned32(Words.Count - 1)) do
        begin
          if (Words[Index] = Words[Index+1]) then
            begin
              Words.Delete(Index + 1);
              inc(Removed);
            end;

          inc(Delay);
          if (Delay=0) then
            begin
              pbProgress.Max := Words.Count;
              pbProgress.Position:=Index;
              Application.ProcessMessages();
            end;

          inc(Index);
        end;
    end;
  lbLog.ItemIndex:=lbLog.Items.Add(IntToStr(Removed)+' words removed');

  pbProgress.Position:=0;
end;

procedure TForm1.btClearClick(Sender: TObject);
begin
  Words.Clear();
end;

//Die Liste abspeichern
procedure TForm1.Button4Click(Sender: TObject);
begin
  with (dgSaveFile) do
    begin
      Filename:=dgAddFile.Filename;
      if (Execute = TRUE) then
        begin
          Words.SaveToFile(Filename);
        end;
    end;
end;



//Die Anzeigen updaten
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  lbWordCount.Caption:=IntToStr(Words.Count);
  Application.ProcessMessages();
end;

end.
