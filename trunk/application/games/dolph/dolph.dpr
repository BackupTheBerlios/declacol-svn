program dolph;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  class_monster,
  class_renderengine,
  class_map,
  const_weapon,
  const_magic,
  const_armor,
  const_inventory,
  unit_types,
  unit_navigation;

var
  render : TRender;
  u32index : integer;

//Alles im Spiel initialisieren
function InitGame():boolean;
begin
  render:=TRender.create(80,20);

  result:=TRUE;
end;


//Die Hauptschleife
begin
  { TODO -oUser -cConsole Main : Hier Code einfügen }
  InitGame();

  for u32Index:=0 to 100000 do
    begin
      render.setpixel(random(Render.xSize),random(Render.ySize),random(255),TEXT_WHITE);
      render.flush();
    end;

  readln;
end.
