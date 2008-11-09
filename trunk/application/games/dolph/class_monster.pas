unit class_monster;
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
     const_magic,
     const_weapon,
     const_armor,
     const_inventory;

//Eigenschaften eines Objektes
type TAttributes = record
    //Name
    Name        : Longstring;

    //Position auf der Map
    Position    : TPosition;

    //Letzter Timestamp
    u32LastTick : unsigned32;

    //Maximale Werte der Attribute
    u32MaxAttack   : unsigned32;   //Trefferwahrscheinlichkeit [%]
    u32MaxStrength : unsigned32;   //Wird auf den Schaden der Waffe addiert [%]

    u32MaxMoral    : unsigned32;   //Ist Health < Moral flüchtet das Monster
    u32MaxHealth   : unsigned32;   //aktuelle Hitpoints
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

    sAction     : longstring;
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
    //Eine Aktionsbeschreibung zufügen
    procedure addaction(Text:Longstring);

  public
    constructor create(Name:longstring;bUseAI : Boolean);

    //Einen Angriff auf Monster ausführen
    procedure attack(Monster:pMonster);

    //Uns selbst bewegen (Um TPosition)
    procedure move(rMove : TPosition);

    //Welchen Schaden verursacht das Monster bei einem Angriff
    function getdamage(Monster:pMonster):unsigned32;

    //Monster nimmt Schaden
    procedure setdamage(value:unsigned32);

    //Timeslice Callback
    procedure callback(u32ticks:unsigned32; Player:PMonster; Map:pMap);

    //Ein String der beschreibt, was das Monster tut
    property action : longstring read saction;

end;

implementation
uses unit_navigation;

////////////////////////////////////////////////////////////////////////////////
//Die Monsterklasse
////////////////////////////////////////////////////////////////////////////////
constructor TMonster.create(Name:Longstring;bUseAI : Boolean);
begin
  //AI aktivieren ?
  Self.bUseAI:=bUseAI;
  //Alle Arrays initialisieren
  Self.Attributes.Name:=Name;
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

  //Am Anfang haben wir nur die Hände und nichts an
  Self.Attributes.Weapon:=Weapons[WEAPON_HANDS]^;
  Self.Attributes.Armor:=Armors[ARMOR_NONE]^;

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
//Einen Actionsstring zufügen
procedure TMonster.addaction(Text:Longstring);
begin
  Self.sAction:=Self.Attributes.Name + ' ' + Text;
end;

////////////////////////////////////////////////////////////////////////////////
//Die beste Waffe für die aktuelle Situation wählen
procedure  TMonster.usebestweapon(Player:pMonster; Map:pMap);
var
  u32index : unsigned32;
  u32range : unsigned32;
  sOldName : longstring;
begin
  sOldName:=Self.Attributes.Weapon.Name;

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

    if (Self.Attributes.Weapon.Name <> sOldName) then
      begin
        Self.addaction('changes weapon to '+Self.Attributes.Weapon.Name);
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
//Einen Angriff auf Monster ausführen
procedure TMonster.attack(Monster:pMonster);
var
  u32damage : unsigned32;
begin
  //Noch Munition
  if (Self.Attributes.Weapon.Ammo > 0) then
    begin
      //Dem anderen Monster schaden zufügen
      u32damage:=Self.getdamage(Monster);
      if (u32damage > 0) then
        begin
          Monster^.setdamage(u32damage);
          Self.addaction('hits '+Monster^.Attributes.Name+' inflicting '+IntToStr(u32damage)+' points of damage');
        end
      else
        begin
          Self.addaction('misses '+Monster^.Attributes.Name);
        end;
      //Einen Schuß abziehen
      dec (Self.Attributes.Weapon.Ammo);
    end
  else
    begin
        Self.addaction('has no ammo to attack');
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//Uns selbst bewegen (Um TPosition)
procedure TMonster.move(rMove : TPosition);
begin
  Self.Attributes.Position.XPos:=rMove.XPos;
  Self.Attributes.Position.YPos:=rMove.YPos;
end;


////////////////////////////////////////////////////////////////////////////////
//Ermitteln, ob das Monster getroffen hat und welchen Schaden es macht
function TMonster.getdamage(Monster:pMonster):unsigned32;
var
  u32dice1 : unsigned32;
begin
  //Würfeln
  u32dice1:=unsigned32(random(101));

  //Getroffen ?
  if ( u32dice1 < Self.Attributes.u32Attack ) //Trefferwurf OK
     then
    begin
      //Schaden ist der Waffenschaden + (StärkeProzent / 10)
      result:= unsigned32( random( Self.Attributes.Weapon.Damage )) + ( Self.Attributes.u32Strength DIV 10 );

      //Rüstungsprozente des Monsters abziehen
      result:= (result * (100 - Monster.Attributes.Armor.Damage)) div 100;
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
  if (Self.Attributes.bDead = FALSE) then
    begin
      if (value > Self.Attributes.u32Health) then
        begin
          Self.Attributes.u32Health:=0;
          Self.Attributes.bDead:=TRUE;
          Self.addaction('dies');
        end
      else
        begin
          dec(Self.Attributes.u32Health,value);
        end;
    end
  else
    begin
      Self.addaction('is already dead');
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
var
  rMove      : TPosition;
  u32range   : unsigned32;
begin
  //Auf jeden Fall die beste Waffe nehmen
  Self.usebestweapon(Player,Map);

  //Haben wir noch genügend Mut?
  if ( Self.Attributes.u32Health < Self.Attributes.u32Moral ) then
    begin
      //Fluchtkurs bestimmen
      rMove:=getescape(@Self,Player,Map);

      //Können wir flüchten ?
      if ( ( rMove.XPos <> 0 ) AND ( rMove.YPos <> 0 ) ) then
        begin
          //Dann weg hier
          Self.addaction('flees');
          Self.move(rMove);
        end
      else
        begin
          Self.Attributes.bBeserk:=TRUE;

          //Wir können nicht weg, dann greifen wir an
          u32range:=getsight(@Self,Player,Map);
          if ( u32range <= Self.Attributes.Weapon.Range ) then
            begin
              Self.addaction('attacks suicidally');
              Self.attack(player);
            end
          else
            begin
              //Zu weit weg, hinlaufen
              rMove:=getapproach(@Self,Player,Map);
              Self.Move(rMove);
              Self.addaction('approaches suicidally');
            end;
        end;
    end
  else
    begin
      //Wir haben noch genügend Mut
      u32range:=getsight(@Self,Player,Map);
      //Unsere Waffe erlaubt einen höheren Abstand?
      if (u32range < Self.Attributes.Weapon.Range) then
        begin
          //Dann Abstand gewinnen
          rMove:=getescape(@Self,Player,Map);
          Self.Move(rMove);
        end
      else
        begin
          //Ansonsten greifen wir an
          Self.attack(player);
        end;
    end;

  {
 TYPICAL AI
            If damage > morale
               if can-run-away-from-player
                  run-away-from-player
               else if can-attack-player
                  attack-player
            else if too-far-from-player
               AND can-attack-player
               AND can-move-toward-player
                   if  random < charge-probability
                       move-toward-player     
                   else attack-player
            else if too-close-to-character
               AND can-attack-player
               AND can-move-away-from-player
                   if random < retreat-probability
                      move-away-from-player
                   else attack-player
            else if can-attack-player
               attack-player
            else if too-far-from-player 
               AND can-move-toward-player
     move-toward-player
            else if too-close-to-player
               AND can-move-away-from-player
                   move-away-from-player
            else stand-still
  }
end;


end.
