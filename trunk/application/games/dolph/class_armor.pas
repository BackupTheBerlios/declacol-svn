unit class_armor;

interface
uses unit_typedefs;


//R�stungstyp
type pArmor = ^TArmor;
     TArmor = record
  Name   : Longstring;    //Name der R�stung
  Damage : unsigned32;    //Schaden den die R�stung absorbiert
end;

implementation
end.
