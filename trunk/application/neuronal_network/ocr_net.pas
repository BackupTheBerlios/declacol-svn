unit ocr_net;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFM_Net = class(TForm)
    Image1: TImage;
    Button1: TButton;
    procedure FormPaint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FM_Net: TFM_Net;

implementation

uses ocr_main;

{$R *.DFM}

procedure TFM_Net.FormPaint(Sender: TObject);
begin
     Image1.Left:=0;
     Image1.Top:=0;
     Image1.Width:=Self.ClientWidth;
     Image1.Height:=Self.ClientHeight;
end;

procedure TFM_Net.FormActivate(Sender: TObject);
begin
     Neuro.PaintNetwork(Image1.Canvas);
     Image1.Invalidate;
end;

procedure TFM_Net.Button1Click(Sender: TObject);
begin
     Neuro.PaintNetwork(Image1.Canvas);
     Image1.Invalidate;
end;

end.
