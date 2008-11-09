unit const_armor;
////////////////////////////////////////////////////////////////////////////////
/// Definition der R�stungen
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs;

const
  ARMOR_NONE  = 0;
  ARMOR_SHIRT = 1;
  ARMOR_PLATE = 2;

//R�stungstyp
type pArmor = ^TArmor;
     TArmor = record
  Name   : Longstring;    //Name der R�stung
  Damage : unsigned32;    //Schaden den die R�stung absorbiert
end;

//Die verf�gbaren Waffen
var
  Armors : array of pArmor;

implementation
procedure addarmor(id:unsigned32;name:longstring;damage:unsigned32);
begin
  if (id >= unsigned32(Length(armors))) then
    begin
      SetLength(armors,id+1);
    end;

  new(Armors[id]);
  Armors[id]^.Name  :=Name;
  Armors[id]^.Damage:=Damage;
end;

initialization
//       ID              Name       Damage
addarmor(ARMOR_NONE    ,'None'     ,1);
addarmor(ARMOR_SHIRT   ,'Shirt'    ,2);
addarmor(ARMOR_PLATE   ,'Plate'    ,4);
end.
