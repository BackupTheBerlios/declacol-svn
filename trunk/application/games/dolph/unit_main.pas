unit unit_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  class_monster,
  class_map,
  const_weapon,
  const_armor,
   StdCtrls, ComCtrls
  ;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  m1   : TMonster;
  m2   : TMonster;
implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  m1:=TMonster.create('honk',FALSE);
  m2:=TMonster.create('bopfer',FALSE);

  m1.Attributes.Weapon:=Weapons[WEAPON_SWORD]^;

  m1.Attributes.u32Health:=10;
  m2.Attributes.u32Health:=10;

  m1.Attributes.u32MaxAttack:=90;
  m1.Attributes.u32Attack:=90;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  m1.attack(@m2);

  memo1.Lines.Add(m1.action);
  memo1.Lines.Add(m2.action);
end;

end.
