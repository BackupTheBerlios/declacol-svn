unit unit_failure;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmFailure = class(TForm)
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmFailure: TfmFailure;

implementation

{$R *.dfm}

procedure TfmFailure.Button1Click(Sender: TObject);
begin
  fmFailure.Hide();
end;

end.
