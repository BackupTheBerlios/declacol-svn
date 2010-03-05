unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus;

type
  TfmMain = class(TForm)
    PopupMenu1: TPopupMenu;
    Exit1: TMenuItem;
    Black1: TMenuItem;
    White1: TMenuItem;
    Silver1: TMenuItem;
    Red1: TMenuItem;
    Blue1: TMenuItem;
    Green1: TMenuItem;
    N1: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Black1Click(Sender: TObject);
    procedure Blue1Click(Sender: TObject);
    procedure Green1Click(Sender: TObject);
    procedure Red1Click(Sender: TObject);
    procedure Silver1Click(Sender: TObject);
    procedure White1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormActivate(Sender: TObject);
begin
  SetWindowPos(Self.Handle,HWND_BOTTOM,0,0,Screen.Width,Screen.Height,SWP_NOACTIVATE);
end;

procedure TfmMain.Exit1Click(Sender: TObject);
begin
  Self.Close();
end;

procedure TfmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then Self.FormActivate(Self);
end;

procedure TfmMain.Black1Click(Sender: TObject);
begin
  fmMain.Color:=clBlack;
end;

procedure TfmMain.Blue1Click(Sender: TObject);
begin
  fmMain.Color:=clBlue;
end;

procedure TfmMain.Green1Click(Sender: TObject);
begin
  fmMain.Color:=clGreen;
end;

procedure TfmMain.Red1Click(Sender: TObject);
begin
  fmMain.Color:=clRed;
end;

procedure TfmMain.Silver1Click(Sender: TObject);
begin
  fmMain.Color:=clSilver;
end;

procedure TfmMain.White1Click(Sender: TObject);
begin
  fmMain.Color:=clWhite;
end;

end.
