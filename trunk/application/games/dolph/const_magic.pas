unit const_magic;
////////////////////////////////////////////////////////////////////////////////
/// Definition der Magie
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs;

const
    //Alle magischen Fähigkeiten
    MAGIC_MIN      = 0;
    MAGIC_HEAL     = 1;
    MAGIC_TELEPORT = 2;
    MAGIC_SHOOT    = 3;
    MAGIC_MAX      = 100;

//Set aller Zaubersprüche
type TSpells = set of MAGIC_MIN..MAGIC_MAX;



implementation

end.
