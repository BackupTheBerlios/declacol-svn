unit const_magic;
////////////////////////////////////////////////////////////////////////////////
/// Definition der Magie
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs;

const
    //Alle magischen Fähigkeiten
    MAGIC_HEAL     = 1;
    MAGIC_TELEPORT = 2;
    MAGIC_SHOOT    = 3;

//Set aller Zaubersprüche
type TSpells = set of MAGIC_HEAL..MAGIC_SHOOT;

//Type der Zauberspruchtypen
type pMagic = ^TMagic;
     TMagic = record
     Name    : longstring;
     Damage  : unsigned32;
     Range   : unsigned32;
end;

var
    magics : array of pMagic;


implementation
    
procedure addspell(id:unsigned32;name:longstring;damage:unsigned32;range:unsigned32);
begin
  if (id >= unsigned32(Length(magics))) then
    begin
      SetLength(magics,id+1);
    end;

  new(magics[id]);
  magics[id]^.Name    :=Name;
  magics[id]^.Damage  :=Damage;
  magics[id]^.Range   :=Range;
end;

initialization

//Beim Systemstart alle Waffen anlegen
//        ID                 Name     Value  Value2
addspell(MAGIC_HEAL       ,'Heal'     ,10,   0);  //Value = HP
addspell(MAGIC_TELEPORT   ,'Teleport' ,50,   0);  //Value = Distance
addspell(MAGIC_SHOOT      ,'Shoot'    ,10,   5);  //Value = Range, Value2=Damage

end.
