unit const_inventory;
////////////////////////////////////////////////////////////////////////////////
/// Definition des Inventars mit alle Objekten
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs,
     unit_types,
     const_weapon,
     const_magic,
     const_armor;

//Das Inventar
type  pInventory = ^TInventory;
      TInventory = record
  Weapons : array[0..3] of TWeapon;
  Gold    : TAttributeValue;
  Food    : TAttributeValue;
  Potions : TAttributeValue;
end;

implementation
end.
