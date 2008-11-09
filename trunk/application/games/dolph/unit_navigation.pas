unit unit_navigation;
////////////////////////////////////////////////////////////////////////////////
/// Hilfsfunktionen um Monster-Monster-Beziehungen zu bestimmen
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs,
     class_monster,
     class_map;

//Den Abstand zwischen zwei Monstern holen
//0 bedeutet, die Monster1 kann Monster2 nicht sehen
function getdistance(Monster1:pMonster; Monster2:pMonster; Map:pMap):unsigned32;


implementation


function getdistance(Monster1:pMonster; Monster2:pMonster; Map:pMap):unsigned32;
begin
  result:=random(100);
end;

end.
