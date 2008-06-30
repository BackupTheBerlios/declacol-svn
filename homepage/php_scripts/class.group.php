<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Gruppen Klasse
///
/// Jedes Benutzerobjekt enthält ein Array mit den IDs der zur Gruppe
/// gehörigen Module. Ist keine ID für eine angeforderte ID gesetzt,
/// so werden die Standardwerte des Userstatus übernommen
///
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_GROUP","class_group");
define ("CLASS_GROUP_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_GROUP,"version ".CLASS_GROUP_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("GROUP_NONE"    ,"None");
define ("GROUP_GUEST"    ,"Guest");
define ("GROUP_USER"     ,"Standard User");
define ("GROUP_XUSER"    ,"Extended User");
define ("GROUP_ADMIN"    ,"Administrator");

//Hier liegen alle Mitglieder der Gruppe
define("DB_GROUPS_MEMBERS", DB_GROUPS."_members");

//Hier liegen alle Objekte, die einer Gruppe angehören
define("DB_GROUPS_OBJECTS",DB_GROUPS."_objects");


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class group
    {
    //Öffentliche Eigenschaftem
    var $id       = FALSE;
    var $name      = FALSE;

    //Private Eigenschaften

    //Eigenschaften der Gruppe
    var $readable = FALSE;
    var $writable = FALSE;
    var $admin      = FALSE;
    var $enabled  = TRUE;

    //Frei vergebbare Eigenschaften
    //Können bei einem User mit
    //user->has_param1($objectid) abgefragt werden
    //Damit können z.B innerhalb eines Moduls die
    //Zugriffe weiter verfeinert werden.

    //Die Parameter sollte wie folgt benutzt werden
    //HasParam1 = Darf Einträge anlegen
    //HasParam2 = Darf Einträge bearbeiten
    //HasParam3 = Darf Einträge löschen

    var $param1      = FALSE;
    var $param2      = FALSE;
    var $param3      = FALSE;
    var $param4      = FALSE;
    var $param5      = FALSE;    
    var $param6      = FALSE;    
    var $param7      = FALSE;    


    //Mitglieder
    var $objects  = array();
    var $user     = array();

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function group($name)
        {
        //Alle Werte auf Default setzen
        $this->setdefault();

        //Und die Gruppendefinition laden
        $this->open($name);
        }

    //Destruktor
    function destroy()
        {
        $this->close();
        }

    //Gruppe öffnen
    function open($name)
        {
        global $mysql;
        global $debug;

        //Vorgabe setzen
        $this->setdefault();    

        //Gruppe mit dem Namen $name anfordern
        $query = "SELECT * FROM ".DB_GROUPS." WHERE name='".$name."'";
        $result= $mysql->query($query);

        //Wenn wir was finden, dann die Eigenschaften setzen
        if (is_array($result))
            {
            $this->id      =$result[0]["id"];
            $this->name    =$result[0]["name"];
            
            //Die Zugriffsrechte lesen
            $this->readable=(boolean) $result[0]["readable"];
            $this->writable=(boolean) $result[0]["writable"];
            $this->admin   =(boolean) $result[0]["admin"];
            $this->enabled =(boolean) $result[0]["enabled"];

            //Und die sekundären Attribute
            $this->param1  =(boolean) $result[0]["param1"];
            $this->param2  =(boolean) $result[0]["param2"];
            $this->param3  =(boolean) $result[0]["param3"];
            $this->param4  =(boolean) $result[0]["param4"];
            $this->param5  =(boolean) $result[0]["param5"];
            $this->param6  =(boolean) $result[0]["param6"];
            $this->param7  =(boolean) $result[0]["param7"];

            $result=TRUE;
            }
        else
            {
            //Wenn wir nichts finden, debug-meldung machen
            $debug->add(CLASS_GROUP," group ".$name." not found");

            $this->name=$name;
            $this->id=crypt_create_hash($name);

            $result=FALSE;
            }

        //Und alle Mitgliedsmodule laden
        $query = "SELECT object_id FROM ".DB_GROUPS_OBJECTS." WHERE group_id='".$this->id."'";
        $result= $mysql->query($query);

        //Was gefunden ?
        if (is_array($result))
            {
            //Die ModulIDs speichern
            foreach ($result as $object)
                {
                $this->objects[$object["object_id"]]=TRUE;
                }
            }        

        //Und alle Mitglieder laden
        $query = "SELECT user_id FROM ".DB_GROUPS_MEMBERS." WHERE group_id='".$this->id."'";
        $result= $mysql->query($query);

        //Was gefunden ?
        if (is_array($result))
            {
            //Dan die User-IDs speichern
            foreach ($result as $member)
                {
                $this->user[$member["user_id"]]=TRUE;
                }
            }        

        return($result);
        }

    //Gruppen verwerfen
    function close()
        {
        $this->setdefault();
        }

    //Defaults setzen
    function setdefault()
        {
        $this->id        = crypt_create_unique_id();
        $this->name      = "unknown";
        $this->readable  = FALSE;
        $this->writable  = FALSE;
        $this->admin     = FALSE;
        $this->enabled   = TRUE;
        $this->objects   = array();
        $this->user      = array();
    
        //Diese Parameter sind frei belegbar
        $this->param1    = FALSE;
        $this->param2    = FALSE;
        $this->param3    = FALSE;
        $this->param4    = FALSE;
        $this->param5    = FALSE;
        $this->param6    = FALSE;
        $this->param7    = FALSE;
        }
    
    //Wird hier nicht benutzt
    function install()
        {
        }
    
    //Die Gruppe aus der Datenbank entfernen
    function uninstall()
        {
        global $mysql;

        //Alle aktuellen Werte in die Datenbank einspielen
        //Die alten Datensätze löschen
        $mysql->query("DELETE FROM ".DB_GROUPS." WHERE id='".$this->id."'");
        $mysql->query("DELETE FROM ".DB_GROUPS_MEMBERS." WHERE group_id='".$this->id."'");
        $mysql->query("DELETE FROM ".DB_GROUPS_OBJECTS." WHERE group_id='".$this->id."'");
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Die Gruppe und all ihre Daten in die Datenbank einspielen
    function save()
        {
        global $mysql;

        //Alte Daten löschen
        $this->uninstall();

        //Und die neuen einspielen
        //Die Gruppendefinition
        $query ="INSERT INTO ".DB_GROUPS." (id,name,readable,writable,admin,enabled,param1,param2,param3,param4,param5,param6,param7) VALUES(";
        $query.="'".$this->id."',";
        $query.="'".$this->name."',";
        $query.=(int)$this->readable.",";
        $query.=(int)$this->writable.",";
        $query.=(int)$this->admin.",";
        $query.=(int)$this->enabled.",";
        $query.=(int)$this->param1.",";
        $query.=(int)$this->param2.",";
        $query.=(int)$this->param3.",";
        $query.=(int)$this->param4.",";
        $query.=(int)$this->param5.",";
        $query.=(int)$this->param6.",";
        $query.=(int)$this->param7.")";
        $mysql->query($query);


        //Alle zugehörigen Objekte
        if (is_array($this->objects))
            {
            foreach ($this->objects as $objectid => $value)
                {
                $query="INSERT INTO ".DB_GROUPS_OBJECTS." (object_id,group_id) VALUES('".$objectid."','".$this->id."')";
                $mysql->query($query);
                }
            }

        //Alle zugehörigen Mitglieder
        if (is_array($this->user))
            {
            foreach ($this->user as $userid => $value)
                {
                $query="INSERT INTO ".DB_GROUPS_MEMBERS." (user_id,group_id) VALUES('".$userid."','".$this->id."')";
                $mysql->query($query);
                }
            }
        }

    //Einen User der Gruppe hinzufügen
    //Wir benutzen die UserID im Mitglieder-Array als Index, damit können
    //keine Doppeleinträge entstehen
    function add_user($userid)
        {
        $this->user[$userid]=TRUE;
        }

    //Einen User aus der Gruppe entfernen
    function remove_user($userid)
        {
        if (array_key_exists($userid,$this->user))
            {
            unset($this->user[$userid]);
            }
        }

    //Eine Object der Gruppe zufügen
    //Wir benutzen die ObjectID im Mitglieder-Array als Index, damit können
    //keine Doppeleinträge entstehen
    function add_object($objectid)
        {
        $this->objects[$objectid]=TRUE;
        }

    //Ein Object aus der Gruppe entfernen
    function remove_object($objectid)
        {
        if (array_key_exists($objectid,$this->objects))
            {
            unset($this->objects[$objectid]);
            }
        }
    }

</script>