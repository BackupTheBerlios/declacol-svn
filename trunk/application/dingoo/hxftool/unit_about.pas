unit unit_about;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,unit_bumpmapping;

type
  TfmAbout = class(TForm)
    imBumper: TImage;
    tiAnimation: TTimer;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure imBumperClick(Sender: TObject);
    procedure tiAnimationTimer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmAbout: TfmAbout;
  bLoaded : boolean = FALSE;

  x : single = 0.1;
  y : single = 1.2;

implementation

{$R *.dfm}

procedure TfmAbout.FormActivate(Sender: TObject);
begin
  tiAnimation.Enabled:=TRUE;

  if (not bLoaded) then
    begin
      Bump_Init (imBumper.Picture.Bitmap,1,2.6,3);
      bLoaded:=TRUE;
    end;
end;

procedure TfmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tiAnimation.Enabled:=FALSE;
end;

procedure TfmAbout.imBumperClick(Sender: TObject);
begin
  tiAnimation.Enabled:=FALSE;
  Self.Close;
end;

procedure TfmAbout.tiAnimationTimer(Sender: TObject);
var
  xpos : integer;
  ypos : integer;
begin
  x:=x + 0.04;
  y:=y + 0.06;

  if ( x > 2 * pi ) then x:= 0;
  if ( y > 2 * pi ) then y:= 0;

  xpos:=trunc(sin(x) * imBumper.Picture.Bitmap.Width  / 2 + imBumper.Picture.Bitmap.Width / 2);
  ypos:=trunc(sin(y) * imBumper.Picture.Bitmap.Height / 2 + imBumper.Picture.Bitmap.Height / 2);




  Bump_Do(imBumper.Picture.Bitmap,xpos,ypos);
end;

end.
