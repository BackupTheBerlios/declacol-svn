unit const_inventory;
////////////////////////////////////////////////////////////////////////////////
/// Definition des Inventars mit alle Objekten
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs,
     const_weapon,
     const_magic,
     const_armor;

//Das Inventar
type  pInventory = ^TInventory;
      TInventory = record
  Weapons : array[0..3] of TWeapon;
  Armor   : array[0..3] of TArmor;
  Gold    : unsigned32;
  Food    : unsigned32;
  Potions : unsigned32;
end;

implementation
end.
