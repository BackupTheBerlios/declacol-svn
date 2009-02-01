unit main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_strings;

type
  TForm1 = class(TForm)
    cbInput: TComboBox;
    edOutput: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
    function extractchar(var input : longstring; position : unsigned32):longstring;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//Einen Buchstaben aus einem String holen und diesen ausschneiden
function TForm1.extractchar(var input : longstring; position : unsigned32):longstring;
begin
  if (position > 0) and (position < unsigned32(length(input)) ) then
    begin
      result:=input[position];
      delete(input,position,1);
    end
  else
    begin
      result:='';
    end;
end;

//Die Worte verwürfeln
procedure TForm1.Button1Click(Sender: TObject);
var
  Words    : TStringlist;
  u32Index : unsigned32;
  sTemp    : Longstring;
begin
  Words:=TStringlist.Create();
  string_explode(cbInput.Text,' ',Words,TRUE);

  edOutput.Text:='';

  while (Words.Count > 0) do
    begin
      //Ein Wort mischen
      sTemp:=Words[0];
      u32Index:=Length(sTemp);

      if ( u32Index > 0 ) then
        begin
          //Erster Buchstabe bleibt gleich
          edOutput.Text:=edOutput.Text + extractchar(sTemp,1);;

          while ( Length (sTemp) > 1 ) do
            begin
              edOutput.Text:=edOutput.Text + extractchar(sTemp, Random(Length(sTemp)) );
            end;

          //Der letzte Buchstabe bleibt gleich
          edOutput.Text:=edOutput.Text + sTemp[1] + ' ';
        end;
      Words.Delete(0);
    end;
  Words.Free();
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Self.Height:=115;
  Self.cbInput.Width:=Self.Width  - 20;
  Self.edOutput.Width:=Self.Width - 20;
end;

end.
