unit class_monster;
////////////////////////////////////////////////////////////////////////////////
///
/// Eine mehr oder weniger universelle Monsterklasse die eine rudimentäre
/// KI implementiert                                                                       
///
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs,windows,
     unit_types,
    class_map,
    const_magic,
    const_weapon,
    const_armor,
    const_inventory;

//Eigenschaften eines Objektes
type TAttributes = record
    //Position auf der Map
    Position    : TPosition;

    //Letzter Timestamp
    u32LastTick : unsigned32;

    //Maximale Werte der Attribute
    u32MaxAttack   : unsigned32;   //Trefferwahrscheinlichkeit [%]
    u32MaxMoral    : unsigned32;   //Ist Health < Moral flüchtet das Monster
    u32MaxHealth   : unsigned32;   //aktuelle Hitpoints
    u32MaxStrength : unsigned32;   //Wird auf den Schaden der Waffe addiert
    u32MaxSpeed    : unsigned32;   //Alle "Speed" Ticks reagiert das Monster
    u32MaxMagic    : unsigned32;   //Anzahl der erlaubten Magieverwendungen

    //Aktuelle Attribute
    u32Attack   : unsigned32;
    u32Moral    : unsigned32;
    u32Health   : unsigned32;
    u32Strength : unsigned32;
    u32Speed    : unsigned32;
    u32Magic    : unsigned32;

    //Die Heilungsfaktoren      //Alle x Ticks wird er entsprechende Wert
    u32HealthUp : unsigned32;   //um eins erhöht
    u32MagicUp  : unsigned32;

    //Reaktionsschwellen
    u32Hear     : unsigned32;   //Hörschwelle, ab der auf Töne reagiert wird
    u32Smell    : unsigned32;   //Riechschwelle
    u32See      : unsigned32;   //Sehweite

    //Zustände
    bBeserk     : Boolean;      //Kreatur läuft Amok
    bDead       : Boolean;      //Kreatur ist tot oder unbelebt

    //Alle gelernten Zaubersprüche
    Spells      : TSpells;

    //Inventory
    Weapon      : TWeapon;      //Aktuelle Waffe
    Armor       : TArmor;       //Aktuelle Rüstung
end;


////////////////////////////////////////////////////////////////////////////////
type PMonster = ^TMonster;
     TMonster = class(TObject)
    //Die Attribute des Monsters
    Attributes  : TAttributes;
    Inventory   : TInventory;

    //Soll das Monster eine KI benutzen?
    bUseAI      : Boolean;
  protected
  private
    //Alle Daten löschen
    procedure reset();

    //Automatische Heilung durchführen    
    procedure doheal(u32timeslice:unsigned32);
    //KI
    procedure doai  (u32timeslice:unsigned32;Player:pMonster;Map:pMap);
    //Hilfmethode um eine Waffe zu wechseln
    function  changeweapon(NewWeapon : TWeapon):TWeapon;
    //Auf die beste Waffe wechseln
    procedure  usebestweapon(Player:pMonster; Map:pMap);

  public
    constructor create(bUseAI : Boolean);

    //Welchen Schaden verursacht das Monster bei einem Angriff
    function getdamage():unsigned32;

    //Monster nimmt Schaden
    procedure setdamage(value:unsigned32);

    //Timeslice Callback
    procedure callback(u32ticks:unsigned32; Player:PMonster; Map:pMap);

end;

implementation
uses unit_navigation;

////////////////////////////////////////////////////////////////////////////////
//Die Monsterklasse
////////////////////////////////////////////////////////////////////////////////
constructor TMonster.create(bUseAI : Boolean);
begin
  //AI aktivieren ?
  Self.bUseAI:=bUseAI;
  //Alle Arrays initialisieren
  Self.reset();

  randomize();
end;

////////////////////////////////////////////////////////////////////////////////
//Alle Werte mit Standard füllen
procedure TMonster.reset();
begin
  Self.Attributes.Position.XPos:=0;
  Self.Attributes.Position.YPos:=0;

  Self.Attributes.u32MaxAttack:=0;
  Self.Attributes.u32MaxMoral:=0;
  Self.Attributes.u32MaxHealth:=0;
  Self.Attributes.u32MaxStrength:=0;
  Self.Attributes.u32MaxSpeed:=65535;
  Self.Attributes.u32MaxMagic:=0;

  Self.Attributes.u32HealthUp:=0;
  Self.Attributes.u32MagicUp:=0;

  Self.Attributes.u32Hear:=0;
  Self.Attributes.u32Smell:=0;
  Self.Attributes.u32See:=0;

  Self.Attributes.bBeserk:=FALSE;
  Self.Attributes.bDead:=FALSE;

  //Am Anfang haben wir nur die Hände
  Self.Attributes.Weapon.Name  :='Hands';
  Self.Attributes.Weapon.Range :=1;
  Self.Attributes.Weapon.Damage:=1;

  //Das Inventar leeren
  Self.Inventory.Weapons[0]:=Weapons[WEAPON_HANDS]^;
  Self.Inventory.Weapons[1]:=Weapons[WEAPON_NONE]^;
  Self.Inventory.Weapons[2]:=Weapons[WEAPON_NONE]^;
  Self.Inventory.Weapons[3]:=Weapons[WEAPON_NONE]^;

  Self.Inventory.Armor[0]:=Armors[ARMOR_NONE]^;
  Self.Inventory.Armor[1]:=Armors[ARMOR_NONE]^;
  Self.Inventory.Armor[2]:=Armors[ARMOR_NONE]^;

  Self.Inventory.Gold:=0;
  Self.Inventory.Food:=0;
  Self.Inventory.Potions:=0;
end;

////////////////////////////////////////////////////////////////////////////////
//Timeslice Callback
//Erhält den aktuellen Timestamp, das Spielerobjekt und die Karte
procedure TMonster.callback(u32ticks:unsigned32; Player:PMonster; Map:PMap);
begin
  if (u32Ticks > Self.Attributes.u32LastTick) then
    begin
      //Über die Zeit heilen
      Self.DoHeal( u32Ticks - Self.Attributes.u32LastTick );

      //Agieren
      if (Self.bUseAI = TRUE) then
        begin
          Self.DoAI( u32Ticks - Self.Attributes.u32LastTick ,Player,Map);
        end;

      //Die Zeitscheibe merken
      Self.Attributes.u32LastTick:=u32Ticks;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//Die beste Waffe für die aktuelle Situation wählen
procedure  TMonster.usebestweapon(Player:pMonster; Map:pMap);
var
  u32index : unsigned32;
  u32range : unsigned32;
begin
  //Abstand zum Spiele (0=Spieler kann nicht gesehen werden)
  u32Range:=getdistance(addr(Self),Player,Map);

  //Wir können den Spieler nicht sehen?
  //Dann brauchen wir die Waffe auch nicht zu wechseln
  if (u32Range > 0) then
    begin
      //Die Waffe nehmen, die noch Munition hat und am besten auf den Abstand passt
      u32index:=0;
      while (u32index < unsigned32(Length(Self.Inventory.Weapons))) do
        begin
          if ( ( u32Range > Self.Attributes.Weapon.Range ) AND //Abstand zum Feind zu groß?
               ( Self.Inventory.Weapons[u32Index].Range >= u32range ) AND //Andere Waffe mehr Reichweite?
               ( Self.Inventory.Weapons[u32Index].Ammo > 0) //Neue noch Munition?
             )
             OR
             ( ( u32Range <= Self.Attributes.Weapon.Range ) AND //Reichweite OK
               ( Self.Inventory.Weapons[u32Index].Damage > Self.Attributes.Weapon.Damage ) AND //Aber mehr Schaden
               ( Self.Inventory.Weapons[u32Index].Ammo > 0) //Neue noch Munition?
             )
             OR
             ( ( Self.Attributes.Weapon.Ammo = 0 ) AND //Aktuelle Waffe keine Munition mehr?
               ( Self.Inventory.Weapons[u32Index].Ammo > 0) //Neue noch Munition?
             )

             then
             begin
              //Dann Waffe tauschen
              Self.Inventory.Weapons[u32Index]:=Self.changeweapon(Self.Inventory.Weapons[u32Index]);
             end;
        end;
    end;
end;


////////////////////////////////////////////////////////////////////////////////
//Die Waffe wechseln (die alte Waffe wird zurückgegeben)
function TMonster.changeweapon(NewWeapon : TWeapon):TWeapon;
begin
  result:=Self.Attributes.Weapon;
  Self.Attributes.Weapon:=NewWeapon;
end;

////////////////////////////////////////////////////////////////////////////////
//Ermitteln, ob das Monster getroffen hat und welchen Schaden es macht
function TMonster.getdamage():unsigned32;
begin
  //Getroffen ?
  if ( unsigned32( random(100) ) < Self.Attributes.u32Attack) then
    begin
      //Schaden ist der Waffenschaden + Stärke
      result:= unsigned32( random( Self.Attributes.Weapon.Damage ))  + Self.Attributes.u32Strength;
    end
  else
    begin
      //Daneben
      result:=0;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//Monster nimmt Schaden
procedure TMonster.setdamage(value:unsigned32);
begin
  //Tot?
  if (value > Self.Attributes.u32Health) then
    begin
      Self.Attributes.u32Health:=0;
      Self.Attributes.bDead:=TRUE;
    end
  else
    begin
      dec(Self.Attributes.u32Health,value);
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//Monster heilt über die Zeit
procedure TMonster.doheal(u32timeslice:unsigned32);
begin
  //Die Gesundheit
  if ( u32Timeslice > Self.Attributes.u32HealthUp ) and
     ( Self.Attributes.u32MaxHealth > Self.Attributes.u32Health )  then
     begin
      inc(Self.Attributes.u32Health);
     end;

  //Die Magiepunkte
  if ( u32Timeslice > Self.Attributes.u32MagicUp ) and
     ( Self.Attributes.u32MaxMagic > Self.Attributes.u32Magic )  then
     begin
      inc(Self.Attributes.u32Magic);
     end;
end;

////////////////////////////////////////////////////////////////////////////////
//Die KI des Monsters
procedure TMonster.doai (u32timeslice:unsigned32;Player:pMonster;Map:pMap);
begin
  //Auf jeden Fall die beste Waffe nehmen
  Self.usebestweapon(Player,Map);
end;


end.
