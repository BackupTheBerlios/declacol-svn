unit unit_navigation;
////////////////////////////////////////////////////////////////////////////////
/// Hilfsfunktionen um Monster-Monster-Beziehungen zu bestimmen
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs,
     unit_types,
     class_monster,
     class_map;

//Den Abstand zwischen zwei Monstern holen
function getdistance(Monster1:pMonster; Monster2:pMonster; Map:pMap):unsigned32;

//Den Sichtabstand zwischen zwei Monstern holen
//0 bedeutet, die Monster1 kann Monster2 nicht sehen
function getsight(Monster1:pMonster; Monster2:pMonster; Map:pMap):unsigned32;

//Einen Fluchtkurs einschlagen wenn möglich
function getescape(Monster1:pMonster; Monster2:pMonster; Map:pMap):TPosition;

//Einen Angriffskurs einschlagen wenn möglich
function getapproach(Monster1:pMonster; Monster2:pMonster; Map:pMap):TPosition;

implementation


function getdistance(Monster1:pMonster; Monster2:pMonster; Map:pMap):unsigned32;
begin
  result:=random(100);
end;

function getsight(Monster1:pMonster; Monster2:pMonster; Map:pMap):unsigned32;
begin
  result:=random(100);
end;

function getescape(Monster1:pMonster; Monster2:pMonster; Map:pMap):TPosition;
begin
  result.XPos:=random(2)-1;
  result.YPos:=random(2)-1;
end;


function getapproach(Monster1:pMonster; Monster2:pMonster; Map:pMap):TPosition;
begin
  result.XPos:=random(2)-1;
  result.YPos:=random(2)-1;
end;

end.
