unit class_monstermaker;
////////////////////////////////////////////////////////////////////////////////
///
/// Eine mehr oder weniger universelle Monsterklasse die eine rudimentäre
/// KI implementiert
///
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs,windows,sysutils,
     unit_types,
     class_map,
     class_monster,
     const_magic,
     const_weapon,
     const_armor,
     const_inventory;

type TMonsterType = (Player,Human,Zombie,Ghul,Bear,Viewer,Skeleton,DarkKnight);

////////////////////////////////////////////////////////////////////////////////
type PMonsterMaker = ^TMonsterMaker;
     TMonsterMaker = class(TObject)
  protected
  private
    //Einen Namen machen     
    function createname():longstring;

  public
    constructor create();

    function createmonster(MonsterType : TMonsterType;Map:pMap):TMonster;
end;

implementation

////////////////////////////////////////////////////////////////////////////////
//Die Monsterklasse
////////////////////////////////////////////////////////////////////////////////
constructor TMonsterMaker.create();
begin
     randomize();
end;

//Ein Standardmonster bauen
function TMonsterMaker.createmonster(MonsterType : TMonsterType;Map:pMap):TMonster;
begin
     case (MonsterType) of
          Player : begin
                        result:=TMonster.Create('Player');
                        result.Attributes.Attack         := AttributeValueRND(40,20);
                        result.Attributes.Strength       := AttributeValueRND(40,20);
                        result.Attributes.Moral          := AttributeValue(0,0);
                        result.Attributes.Health         := AttributeValueRND(10,20);
                        result.Attributes.Speed          := AttributeValue(50,50);
                        result.Attributes.Magic          := AttributeValueRND(10,10);

                        result.Attributes.HealthUp       := AttributeValue(10,10);
                        result.Attributes.MagicUp        := AttributeValue(10,10);

                        result.Attributes.Hear           := AttributeValue(8,8);
                        result.Attributes.See            := AttributeValue(10,10);
                        result.Attributes.Smell          := AttributeValue(1,1);

                        result.Attributes.bBeserk        := FALSE;
                        result.Attributes.bDead          := FALSE;
                        result.Attributes.bAI            := TRUE;
                        result.Attributes.Aggression     := AttributeValue(0,0);

                        result.Attributes.Spells         := [MAGIC_HEAL];
                        result.Attributes.Weapon         := weapons[WEAPON_SWORD]^;
                        result.Attributes.Armor          := armors[ARMOR_NONE]^;

                        result.Inventory.Weapons[0]      := weapons[WEAPON_HANDS]^;
                        result.Inventory.Weapons[1]      := weapons[WEAPON_LONGBOW]^;
                        result.Inventory.Weapons[2]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[3]      := weapons[WEAPON_NONE]^;

                        result.Inventory.Potions         := AttributeValue(1,10);
                        result.Inventory.Gold            := AttributeValue(random(100) + 10,60000);
                   end;
          //Langsam, Schwert und Bogen, wenig Htpoints
          Zombie : begin
                        result:=TMonster.Create('Zombie');
                        result.Attributes.Attack         := AttributeValueRND(30,10);
                        result.Attributes.Strength       := AttributeValueRND(10,10);
                        result.Attributes.Moral          := AttributeValue(0,0);
                        result.Attributes.Health         := AttributeValueRND(5,5);
                        result.Attributes.Speed          := AttributeValue(30,30);
                        result.Attributes.Magic          := AttributeValue(0,0);

                        result.Attributes.HealthUp       := AttributeValue(0,0);
                        result.Attributes.MagicUp        := AttributeValue(0,0);

                        result.Attributes.Hear           := AttributeValue(3,3);
                        result.Attributes.See            := AttributeValue(5,5);
                        result.Attributes.Smell          := AttributeValue(1,1);

                        result.Attributes.bBeserk        := FALSE;
                        result.Attributes.bDead          := FALSE;
                        result.Attributes.bAI            := TRUE;
                        result.Attributes.Aggression     := AttributeValue(80,80);

                        result.Attributes.Spells         := [];
                        result.Attributes.Weapon         := weapons[WEAPON_HANDS]^;
                        result.Attributes.Armor          := armors[ARMOR_NONE]^;

                        result.Inventory.Weapons[0]      := weapons[WEAPON_DAGGER]^;
                        result.Inventory.Weapons[1]      := weapons[WEAPON_BOW]^;
                        result.Inventory.Weapons[2]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[3]      := weapons[WEAPON_NONE]^;

                        result.Inventory.Potions         := AttributeValue(0,0);
                        result.Inventory.Gold            := AttributeValue(random(50) + 10,60000);
                   end;
          //Langsam, Biss und Klauen, viele Hitpoints
          Bear : begin
                        result:=TMonster.Create('Bear');
                        result.Attributes.Attack         := AttributeValueRND(20,20);
                        result.Attributes.Strength       := AttributeValueRND(40,20);
                        result.Attributes.Moral          := AttributeValue(4,4);
                        result.Attributes.Health         := AttributeValueRND(10,10);
                        result.Attributes.Speed          := AttributeValue(10,10);
                        result.Attributes.Magic          := AttributeValue(0,0);

                        result.Attributes.HealthUp       := AttributeValue(0,0);
                        result.Attributes.MagicUp        := AttributeValue(0,0);

                        result.Attributes.Hear           := AttributeValue(3,3);
                        result.Attributes.See            := AttributeValue(5,5);
                        result.Attributes.Smell          := AttributeValue(1,1);

                        result.Attributes.bBeserk        := FALSE;
                        result.Attributes.bDead          := FALSE;
                        result.Attributes.bAI            := TRUE;
                        result.Attributes.Aggression     := AttributeValue(80,80);

                        result.Attributes.Spells         := [];
                        result.Attributes.Weapon         := weapons[WEAPON_BITE]^;
                        result.Attributes.Armor          := armors[ARMOR_NONE]^;

                        result.Inventory.Weapons[0]      := weapons[WEAPON_CLAWS]^;
                        result.Inventory.Weapons[1]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[2]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[3]      := weapons[WEAPON_NONE]^;

                        result.Inventory.Potions         := AttributeValue(0,0);
                        result.Inventory.Gold            := AttributeValue(0,0);
                   end;
          //Viele Hitpoint, keine Waffen, flieht immer
          Viewer : begin
                        result:=TMonster.Create('Bear');
                        result.Attributes.Attack         := AttributeValue(0,0);
                        result.Attributes.Strength       := AttributeValue(0,0);
                        result.Attributes.Moral          := AttributeValue(1000,1000);
                        result.Attributes.Health         := AttributeValue(100,100);
                        result.Attributes.Speed          := AttributeValue(60,60);
                        result.Attributes.Magic          := AttributeValue(0,0);

                        result.Attributes.HealthUp       := AttributeValue(20,20);
                        result.Attributes.MagicUp        := AttributeValue(0,0);

                        result.Attributes.Hear           := AttributeValue(10,10);
                        result.Attributes.See            := AttributeValue(10,10);
                        result.Attributes.Smell          := AttributeValue(10,10);

                        result.Attributes.bBeserk        := FALSE;
                        result.Attributes.bDead          := FALSE;
                        result.Attributes.bAI            := TRUE;
                        result.Attributes.Aggression     := AttributeValue(0,0);

                        result.Attributes.Spells         := [];
                        result.Attributes.Weapon         := weapons[WEAPON_NONE]^;
                        result.Attributes.Armor          := armors[ARMOR_NONE]^;

                        result.Inventory.Weapons[0]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[1]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[2]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[3]      := weapons[WEAPON_NONE]^;

                        result.Inventory.Potions         := AttributeValue(0,0);
                        result.Inventory.Gold            := AttributeValue(1000,1000);
                   end;
          //Langsam, Klauen, viele Hitpoints
          Ghul : begin
                        result:=TMonster.Create('Ghul');
                        result.Attributes.Attack         := AttributeValueRND(30,20);
                        result.Attributes.Strength       := AttributeValueRND(30,20);
                        result.Attributes.Moral          := AttributeValue(0,0);
                        result.Attributes.Health         := AttributeValueRND(10,10);
                        result.Attributes.Speed          := AttributeValue(40,40);
                        result.Attributes.Magic          := AttributeValue(0,0);

                        result.Attributes.HealthUp       := AttributeValue(20,0);
                        result.Attributes.MagicUp        := AttributeValue(0,0);

                        result.Attributes.Hear           := AttributeValue(3,3);
                        result.Attributes.See            := AttributeValue(5,5);
                        result.Attributes.Smell          := AttributeValue(4,4);

                        result.Attributes.bBeserk        := FALSE;
                        result.Attributes.bDead          := FALSE;
                        result.Attributes.bAI            := TRUE;
                        result.Attributes.Aggression     := AttributeValue(50,50);

                        result.Attributes.Spells         := [];
                        result.Attributes.Weapon         := weapons[WEAPON_CLAWS]^;
                        result.Attributes.Armor          := armors[ARMOR_NONE]^;

                        result.Inventory.Weapons[0]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[1]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[2]      := weapons[WEAPON_NONE]^;
                        result.Inventory.Weapons[3]      := weapons[WEAPON_NONE]^;

                        result.Inventory.Potions         := AttributeValue(0,0);
                        result.Inventory.Gold            := AttributeValue(random(50) + 10,60000);
                   end;

     end;
end;



//Einen Monsternamen erzeugen
function TMonsterMaker.createname():longstring;
begin
end;

end.
