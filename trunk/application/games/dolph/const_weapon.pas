unit const_weapon;
////////////////////////////////////////////////////////////////////////////////
/// Definition der Waffen
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,
     unit_types;

const
//Alle WaffenIDs
  WEAPON_NONE     = 0;
  WEAPON_HANDS    = 1;
  WEAPON_DAGGER   = 2;
  WEAPON_SWORD    = 3;
  WEAPON_LONGSWORD= 4;
  WEAPON_BOW      = 5;
  WEAPON_LONGBOW  = 6;

  WEAPON_CLAWS    = 7;
  WEAPON_BITE     = 8;

  //Rekord für Waffenwerte
type  pWeapon = ^TWeapon;
      TWeapon = record
  Name   : Longstring;    //Name der Waffe
  Range  : unsigned32;    //Reichweite 1 für Nahkampf
  Damage : unsigned32;    //Maximaler Schaden den die Waffe verursacht
  Ammo   : TAttributeValue;    //Wieviel Schuß
end;

//Die verfügbaren Waffen
var
  Weapons : array of pWeapon;


implementation

procedure addweapon(id:unsigned32;name:longstring;range:unsigned32;damage:unsigned32;ammo:unsigned32);
begin
  if (id >= unsigned32(Length(Weapons))) then
    begin
      SetLength(Weapons,id+1);
    end;

  new(Weapons[id]);
  Weapons[id]^.Name    :=Name;
  Weapons[id]^.Range   :=Range;
  Weapons[id]^.Damage  :=Damage;
  Weapons[id]^.Ammo    :=AttributeValue(Ammo,Ammo);
end;

initialization

//Beim Systemstart alle Waffen anlegen
//        ID                 Name       Range Damage Ammo
addweapon(WEAPON_NONE      ,'None'     ,0    ,0     ,0);
addweapon(WEAPON_HANDS     ,'Hands'    ,1    ,3     ,High(unsigned32));
addweapon(WEAPON_DAGGER    ,'Dagger'   ,1    ,4     ,High(unsigned32));
addweapon(WEAPON_SWORD     ,'Sword'    ,1    ,5     ,High(unsigned32));
addweapon(WEAPON_LONGSWORD ,'LongSword',1    ,7     ,High(unsigned32));
addweapon(WEAPON_BOW       ,'Bow'      ,5    ,5     ,100);
addweapon(WEAPON_LONGBOW   ,'LongBow'  ,7    ,5     ,100);
addweapon(WEAPON_CLAWS     ,'Claws'    ,1    ,2     ,High(unsigned32));
addweapon(WEAPON_BITE      ,'Bite'     ,1    ,3     ,High(unsigned32));
end.
