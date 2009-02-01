unit unit_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,
  class_monster,
  class_monstermaker,

  class_map,
  const_weapon,
  const_armor,
   StdCtrls, ComCtrls
  ;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  mm   : TMonstermaker;
  m1   : TMonster;
  m2   : TMonster;
implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin

     m1.callback(gettickcount(),@m2);
     m2.callback(gettickcount(),@m1);

     memo1.Lines.Add(m1.action);
     memo1.Lines.Add(m2.action);

     m1.sAction:='';
     m2.sAction:='';

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     mm:=TMonsterMaker.Create();

     m1:=mm.createmonster(Bear,nil);
     m2:=mm.createmonster(Zombie,nil);

     mm.free();
end;

end.
