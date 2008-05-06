unit progress;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls;

type
  TfmProgress = class(TForm)
    lbInfo: TLabel;
    pbProgress: TProgressBar;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure doprogress(Data:unsigned32; Text:Longstring);
  end;

var
  fmProgress: TfmProgress;

implementation
procedure TfmProgress.doprogress(Data:unsigned32;Text : Longstring);
begin
     lbInfo.Caption:=Text;
     pbProgress.Position:=Data;
     Application.ProcessMessages();
end;

{$R *.DFM}

end.
