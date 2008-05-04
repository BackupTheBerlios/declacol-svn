unit progress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls;

type
  TFM_PausenClown = class(TForm)
    Panel1: TPanel;
    PB_PausenClown: TProgressBar;
    TI_PausenClown: TTimer;
    LB_Status: TLabel;
    procedure TI_PausenClownTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FM_PausenClown: TFM_PausenClown;

implementation

{$R *.DFM}

procedure TFM_PausenClown.TI_PausenClownTimer(Sender: TObject);
const
     iPos : Integer = 1;
     iDif : integer = 1;
begin
     if (iPos = 100) or (iPos=0) then iDif:=-iDif;
     iPos:=iPos+iDif;
     PB_PausenClown.Position:=iPos;
end;

procedure TFM_PausenClown.FormShow(Sender: TObject);
begin
     TI_PausenClown.Enabled:=TRUE;
end;

procedure TFM_PausenClown.FormHide(Sender: TObject);
begin
     TI_PausenClown.Enabled:=FALSE;
end;

end.
