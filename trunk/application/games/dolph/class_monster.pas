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
    Attack         : TAttributeValue; //Trefferwahrscheinlichkeit [%]
    Strength       : TAttributeValue; //Wird auf den Schaden der Waffe addiert [%]
    Moral          : TAttributeValue; //Ist Health < Moral flüchtet das Monster
    Health         : TAttributeValue; //Lebenspunkte
    Speed          : TAttributeValue; //Alle "Speed" Ticks reagiert das Monster
    Magic          : TAttributeValue; //Anzahl der erlaubten Magieverwendungen

    //Die Heilungsfaktoren
    HealthUp       : TAttributeValue; //Alle x Ticks wird er entsprechende Wert
    MagicUp        : TAttributeValue;

    //Reaktionsschwellen
    Hear           : TAttributeValue;   //Hörschwelle, ab der auf Töne reagiert wird
    Smell          : TAttributeValue;   //Riechschwelle
    See            : TAttributeValue;   //Sehweite

    //Zustände
    bBeserk        : Boolean;      //Kreatur läuft Amok
    bDead          : Boolean;      //Kreatur ist tot oder unbelebt
    bAI            : Boolean;      //Künstliche Intelligenz

    //AI-Helper
    Aggression     : TAttributeValue;

    //Alle gelernten Zaubersprüche
    Spells         : TSpells;

    //Inventory
    Weapon         : TWeapon;      //Aktuelle Waffe
    Armor          : TArmor;       //Rüstungsklasse
end;


////////////////////////////////////////////////////////////////////////////////
type PMonster = ^TMonster;
     TMonster = class(TObject)
    //Die Attribute des Monsters
    Attributes  : TAttributes;
    Inventory   : TInventory;

    //Link zur interner Karte des Systems
    rMap        : pMap;

    //Soll das Monster eine KI benutzen?
    sAction     : longstring;

  protected
    //Getter Setter
    procedure sethealth(value : unsigned32);
    function  gethealth():unsigned32;
    procedure setmagic(value : unsigned32);
    function  getmagic():unsigned32;

  private
    //Alle Daten löschen
    procedure reset();

    //Automatische Heilung durchführen
    procedure doheal(u32timeslice:unsigned32);
    //Sterben
    procedure dodie();
    //KI
    procedure doai  (u32timeslice:unsigned32;Player:pMonster);
    //Hilfmethode um eine Waffe zu wechseln
    //Eine AI-Heilung durchführen
    procedure tryheal();
    function  changeweapon(NewWeapon : TWeapon):TWeapon;
    //Auf die beste Waffe wechseln
    procedure  usebestweapon(Player:pMonster);
    //Eine Aktionsbeschreibung zufügen
    procedure addaction(Text:Longstring);

  public
    constructor create(Name:longstring);

    //Einen Angriff auf Monster ausführen
    procedure attack(Monster:pMonster);

    //Uns selbst bewegen (Um TPosition)
    procedure move(rMove : TPosition);

    //Welchen Schaden verursacht das Monster bei einem Angriff
    function getdamage(Monster:pMonster):unsigned32;

    //Monster nimmt Schaden
    procedure setdamage(value:unsigned32);

    //Timeslice Callback
    procedure callback(u32ticks:unsigned32; Player:PMonster);

    //Ein String der beschreibt, was das Monster tut
    property action : longstring read saction;

    property health : unsigned32 read gethealth write sethealth;
    property magic  : unsigned32 read getmagic  write setmagic;

    property map    : pMap       read rMap      write rMap; 

end;

implementation
uses unit_navigation;

////////////////////////////////////////////////////////////////////////////////
//Die Monsterklasse
////////////////////////////////////////////////////////////////////////////////
constructor TMonster.create(Name:Longstring);
begin
  //Alle Arrays initialisieren
  Self.Attributes.Name:=Name;
  Self.reset();

  randomize();
end;

////////////////////////////////////////////////////////////////////////////////
//Alle Werte mit Standard füllen
procedure TMonster.reset();
begin
  Self.Map:=nil;
     
  Self.Attributes.Position.XPos:=0;
  Self.Attributes.Position.YPos:=0;

  Self.Attributes.Attack    :=AttributeValue(0,0);
  Self.Attributes.Strength  :=AttributeValue(0,0);
  Self.Attributes.Moral     :=AttributeValue(0,0);
  Self.Attributes.Health    :=AttributeValue(0,0);
  Self.Attributes.Speed     :=AttributeValue(0,0);
  Self.Attributes.Magic     :=AttributeValue(0,0);
  Self.Attributes.MagicUp   :=AttributeValue(0,0);
  Self.Attributes.HealthUp  :=AttributeValue(0,0);
  Self.Attributes.Hear      :=AttributeValue(0,0);
  Self.Attributes.See       :=AttributeValue(0,0);
  Self.Attributes.Smell     :=AttributeValue(0,0);
  Self.Attributes.bBeserk   :=FALSE;
  Self.Attributes.bDead     :=FALSE;
  Self.Attributes.Aggression:=AttributeValue(0,0);

  //Am Anfang haben wir nur die Hände und nichts an
  Self.Attributes.Weapon    :=Weapons[WEAPON_HANDS]^;

  //Das Inventar leeren
  Self.Inventory.Weapons[0] :=Weapons[WEAPON_HANDS]^;
  Self.Inventory.Weapons[1] :=Weapons[WEAPON_NONE]^;
  Self.Inventory.Weapons[2] :=Weapons[WEAPON_NONE]^;
  Self.Inventory.Weapons[3] :=Weapons[WEAPON_NONE]^;

  Self.Attributes.Armor:=Armors[ARMOR_NONE]^;

  Self.Inventory.Gold       :=AttributeValue(0,0);
  Self.Inventory.Food       :=AttributeValue(0,0);
  Self.Inventory.Potions    :=AttributeValue(0,0);
end;

////////////////////////////////////////////////////////////////////////////////
//Timeslice Callback
//Erhält den aktuellen Timestamp, das Spielerobjekt und die Karte
procedure TMonster.callback(u32ticks:unsigned32; Player:PMonster);
begin
  //Status löschen
  if (u32Ticks > Self.Attributes.u32LastTick) then
    begin
      //Über die Zeit heilen
      Self.DoHeal( u32Ticks - Self.Attributes.u32LastTick );

      //Agieren
      if (Self.Attributes.bAI = TRUE) and (Self.Attributes.bDead = FALSE) then
        begin
          Self.DoAI( u32Ticks - Self.Attributes.u32LastTick ,Player);
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
procedure  TMonster.usebestweapon(Player:pMonster);
var
  u32index : unsigned32;
  u32range : unsigned32;
  sOldName : longstring;
begin
  sOldName:=Self.Attributes.Weapon.Name;

  //Abstand zum Spiele (0=Spieler kann nicht gesehen werden)
  u32Range:=getdistance(addr(Self),Player,Self.Map);

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
               ( Self.Inventory.Weapons[u32Index].Ammo.Value > 0) //Neue noch Munition?
             )
             OR
             ( ( u32Range <= Self.Attributes.Weapon.Range ) AND //Reichweite OK
               ( Self.Inventory.Weapons[u32Index].Damage > Self.Attributes.Weapon.Damage ) AND //Aber mehr Schaden
               ( Self.Inventory.Weapons[u32Index].Ammo.Value > 0) //Neue noch Munition?
             )
             OR
             ( ( Self.Attributes.Weapon.Ammo.Value = 0 ) AND //Aktuelle Waffe keine Munition mehr?
               ( Self.Inventory.Weapons[u32Index].Ammo.Value > 0) //Neue noch Munition?
             )

             then
             begin
              //Dann Waffe tauschen
              Self.Inventory.Weapons[u32Index]:=Self.changeweapon(Self.Inventory.Weapons[u32Index]);
             end;
          //Nächste Waffe
          inc(u32index);
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
//Selbsttätig heilen
procedure TMonster.tryheal();
var
   u32temp : unsigned32;
begin
     if (Self.Attributes.Health.Value < (Self.Attributes.Health.MaxValue div 3)) then
        begin
             //Magie ?
             if (Self.Attributes.Magic.Value > 0)  AND
                (MAGIC_HEAL in Self.Attributes.Spells)
                then
                begin
                     //Ein Magipunkt runter
                     dec(Self.Attributes.Magic.Value);
                     //Heilung auswürfeln
                     u32temp:=random(magics[MAGIC_HEAL].Damage) + 1;
                     //Und setzen
                     Self.sethealth( Self.gethealth() + u32temp );
                end
             else
                begin
                     if (Self.Inventory.Potions.Value > 0) then
                        begin
                             //TODO Heiltrank benutzen
                        end;
                end;
        end; 
end;


////////////////////////////////////////////////////////////////////////////////
//Einen Angriff auf Monster ausführen
procedure TMonster.attack(Monster:pMonster);
var
  u32damage : unsigned32;
begin
  //Zu weit weg?   
  if (Self.Attributes.Weapon.Range >= getdistance(@Self,Monster,Self.Map)) then
     begin
       //Noch Munition
       if (Self.Attributes.Weapon.Ammo.Value > 0) then
          begin
               //Dem anderen Monster Schaden zufügen
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
               dec (Self.Attributes.Weapon.Ammo.Value);
          end
       else
          begin
               Self.addaction('has no more ammo');
          end;
     end
  else
     begin
          Self.addaction('attacks but ' + Monster^.Attributes.Name + ' is out of range');
     end;
end;

////////////////////////////////////////////////////////////////////////////////
//Uns selbst bewegen (Um TPosition)
procedure TMonster.move(rMove : TPosition);
begin
  Self.Attributes.Position.XPos:=Self.Attributes.Position.XPos + rMove.XPos;
  Self.Attributes.Position.YPos:=Self.Attributes.Position.YPos + rMove.YPos;
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
  if ( u32dice1 < Self.Attributes.Attack.Value ) //Trefferwurf OK
     then
    begin
      //Schaden ist der Waffenschaden + (StärkeProzent / 10)
      result:= unsigned32( random( Self.Attributes.Weapon.Damage )) + ( Self.Attributes.Strength.Value DIV 10 );

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
      if (value > Self.Attributes.Health.Value) then
        begin
          //Wir sind tot
          Self.Attributes.Health.Value:=0;
          Self.Attributes.bDead:=TRUE;
          Self.addaction('dies');
        end
      else
        begin
          //Wir verlieren nur Lebenspunkte   
          dec(Self.Attributes.Health.Value,value);
        end;
    end
  else
    begin
      //Wir sind schon tot   
      Self.addaction('is already dead');
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//Monster heilt über die Zeit
procedure TMonster.doheal(u32timeslice:unsigned32);
begin
  //Die Gesundheit
  if ( u32Timeslice > Self.Attributes.HealthUp.Value ) AND
     ( Self.Attributes.Healthup.Value > 0) then
     begin
          Self.SetHealth( Self.GetHealth() + 1);
     end;

  //Die Magiepunkte
  if ( u32Timeslice > Self.Attributes.MagicUp.Value ) AND
     ( Self.Attributes.MagicUp.Value > 0) then
     begin
          Self.SetMagic( Self.GetMagic() + 1);
     end;
end;

////////////////////////////////////////////////////////////////////////////////
//Monster ist gestorben
procedure TMonster.dodie();
begin
     if (Self.Attributes.bDead=FALSE) then
        begin
             Self.Attributes.bDead:=TRUE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Die KI des Monsters
procedure TMonster.doai (u32timeslice:unsigned32;Player:pMonster);
var
  rMove      : TPosition;
  u32range   : unsigned32;
begin
  //Auf jeden Fall die beste Waffe nehmen
  Self.usebestweapon(Player);

  //Haben wir noch genügend Mut?
  if ( Self.Attributes.Health.Value < Self.Attributes.Moral.Value ) then
    begin
      //Fluchtkurs bestimmen
      rMove:=getescape(@Self,Player,Self.Map);

      //Können wir flüchten ?
      if ( ( rMove.XPos <> 0 ) AND ( rMove.YPos <> 0 ) ) then
        begin
          //Dann weg hier
          Self.addaction('flees');
          Self.move(rMove);

          //Und heilen
          Self.tryheal();
        end
      else
        begin
          Self.Attributes.bBeserk:=TRUE;

          //Wir können nicht weg, dann greifen wir an
          u32range:=getsight(@Self,Player,Self.Map);
          if ( u32range <= Self.Attributes.Weapon.Range ) then
            begin
              Self.addaction('attacks suicidically');
              Self.attack(player);
            end
          else
            begin
              //Zu weit weg, hinlaufen
              rMove:=getapproach(@Self,Player,Self.Map);
              Self.Move(rMove);
              Self.addaction('approaches fearless');
            end;
        end;
    end
  else
    begin
      //Wir haben noch genügend Mut
      u32range:=getsight(@Self,Player,Self.Map);
      //Unsere Waffe erlaubt einen höheren Abstand?
      if (u32range < Self.Attributes.Weapon.Range) AND (random(100) > Self.Attributes.Aggression.Value) then
        begin
          //Dann Abstand gewinnen
          rMove:=getescape(@Self,Player,Self.Map);
          Self.addaction('steps back');
          Self.Move(rMove);
        end
      else
        begin
          if (u32range <= Self.Attributes.Weapon.Range) then
             begin
                  //Ansonsten greifen wir an
                  Self.attack(player);
             end
          else
             begin
                  //Näher ran um angreifen zu können
                  rMove:=getapproach(@Self,Player,Self.Map);
                  Self.Move(rMove);
                  Self.addaction('steps forward');
             end;

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

////////////////////////////////////////////////////////////////////////////////
//Alle Getter/Setter ab hier
procedure TMonster.sethealth(value : unsigned32);
begin
     if (Value > Self.Attributes.Health.MaxValue) then
        begin
             Value:=Self.Attributes.Health.MaxValue;
        end;

     if (Self.Attributes.Health.Value <> Value) then
        begin
             Self.addaction('regains '+IntToStr( Value - Self.Attributes.Health.Value)+' healthpoints');
             Self.Attributes.Health.Value:=Value;
        end;
end;

function  TMonster.gethealth():unsigned32;
begin
     result:=Self.Attributes.Health.Value;
end;

procedure TMonster.setmagic(value : unsigned32);
begin
     if (Value > Self.Attributes.Magic.MaxValue) then
        begin
             Value:=Self.Attributes.Magic.MaxValue;
        end;

     if (Self.Attributes.Magic.Value <> Value) then
        begin
             Self.addaction('regains '+IntToStr( Value - Self.Attributes.Magic.Value)+' magicpoints');
             Self.Attributes.Magic.Value:=Value;
        end;
end;
function  TMonster.getmagic():unsigned32;
begin
     result:=Self.Attributes.Magic.Value;
end;


end.
