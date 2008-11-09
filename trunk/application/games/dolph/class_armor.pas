unit class_armor;

interface
uses unit_typedefs;


//Rüstungstyp
type pArmor = ^TArmor;
     TArmor = record
  Name   : Longstring;    //Name der Rüstung
  Damage : unsigned32;    //Schaden den die Rüstung absorbiert
end;

implementation
end.
