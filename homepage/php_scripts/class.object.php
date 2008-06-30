<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Objekt Klasse
///
/// Alle angezeigten Objekte leiten sich von dieser Klasse ab.
/// Plugins und Module werden mit dieser Klasse direkt abgebildet
///
/// Um ein Modul/Plugin darzustellen wir dessen Code DIREKT
/// inkludiert, daher sollte man bei der Erstellung von Modulen/Plugins
/// etwas Vorsicht walten lassen
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_OBJECT","class_object");
define ("CLASS_OBJECT_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_OBJECT,"version ".CLASS_OBJECT_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("PLUGIN_LEFT" ,1);
define ("PLUGIN_RIGHT",2);

define ("PLUGIN_VIEW_STANDARD",0);
define ("PLUGIN_VIEW_ADMIN"      ,1);

define ("MODULE_VIEW_STANDARD",0);
define ("MODULE_VIEW_ADMIN"      ,1);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class myobject
    {
    //Öffentliche Eigenschaftem
    var $id       =FALSE;    //ID des Plugins
    var $objectid =FALSE;     //ID des PluginTyps
    var $name     =FALSE;    //Name des Objektes
    var $filename =FALSE;    //Dateinamen ohne _init / _edit / _out
    var $filepath =FALSE;     //Pfad zum Objekt
    var $dbname   =FALSE;    //Name der zugeordneten Datenbank
    var $enabled  =FALSE;     //Plugin aktiv ?
    var $unique      =FALSE;     //Plugin kann nur einmal installiert werden
    var $visible  =FALSE;    //Wird das Plugin angezeigt ?
    var $showtitle=FALSE;     //Den Objekt-Titel anzeigen ?
    var $status   =FALSE;     //Status während der Installation
    var $copyright=FALSE;     //Copyright des Plugins
    var    $info      =FALSE;     //Kurzer Infotext
    var $version  =FALSE;     //Plugin-Version
    var $hpos     =FALSE;    //Position [links/rechts]
    var $vpos     =FALSE;    //Position [oben-unten]
                             //gestartet wird es dennoch
                             //z.B. für Usertracker

    var $param1   =FALSE;    //4 Parameter, die frei benutzt
    var $param2   =FALSE;    //werden können
    var $param3   =FALSE;    //ist store=true werden
    var $param4   =FALSE;    //die Parameter nach dem Ende des Plugins
    var $store    =FALSE;    //gespeichert
    
    var $hits     =0;        //Wie oft wurde die Seite aufgerufen
    var $installed=0;        //Wann wurde sie installiert

    var $type      =OBJECT_NONE;
    //Private Eigenschaften


    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function myobject()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        $this->close();
        }

    //Ein Object mit der ID $id aus der Datenbank laden
    //und die Eigenschaften entsprechend setzen
    function open($id)
        {
        global $mysql;
        global $debug;

        //Die Daten des Plugins holen wir aus der Datenbank
        $query="SELECT * FROM ".DB_OBJECTS." WHERE id='".$id."'";
        $query=$mysql->query($query);

        //Was gefunden ?
        if (is_array($query))
            {
            $this->setdefault();

            //Hier ordnen wir dann die Daten zu
            $this->id        =    $query[0]["id"];
            $this->objectid    =    $query[0]["object_id"];
            $this->name        =    $query[0]["name"];
            $this->filename    =    $query[0]["filename"];
            $this->version    =    $query[0]["version"];
            $this->dbname    =    $query[0]["dbname"];
            $this->enabled    =    $query[0]["enabled"];
            $this->visible    =    $query[0]["visible"];
            $this->showtitle=    $query[0]["showtitle"];
            $this->hpos        =    $query[0]["hpos"];
            $this->vpos        =    $query[0]["vpos"];
            $this->copyright=    "";
            $this->info        =    "";

            //Unique-Flag setzen
            //Uniqze heist, kann nur einmal installiert werden.
            //Sind beide IDs gleich, so ist das Object unique;
            $this->unique    =    ($this->id==$this->objectid);

            $this->param1    =    $query[0]["param1"];
            $this->param2    =    $query[0]["param2"];
            $this->param3    =    $query[0]["param3"];
            $this->param4    =    $query[0]["param4"];

            $this->type      =   $query[0]["type"];

            $this->hits      =   $query[0]["counter"];
            $this->installed =   $query[0]["installed"];


            //Den Pfad bestimmen
            switch ($this->type)
                {
                case (OBJECT_PLUGIN) : $this->filepath=DIR_PLUGINS;    break;
                case (OBJECT_MODULE) : $this->filepath=DIR_MODULES;    break;
                default                 : $this->filepath="";            break;
                }

            $result=TRUE;
            }
        else
            {
            $debug->add(CLASS_OBJECT,"plugin ".$id." not found");

            $result=FALSE;
            }
        //Fertig
        return($result);
        }

    //Verbindung schließen
    //Keine Funktione
    function close()
        {
        }

    //Defaults setzen
    function setdefault()
        {
        $this-> id       =FALSE;
        $this-> objectid =FALSE;
        $this-> name     =FALSE;
        $this-> filename =FALSE;
        $this-> dbname   =FALSE;
        $this-> enabled  =TRUE;
        $this-> showtitle=TRUE;
        $this-> visible  =TRUE;
        $this-> unique     =FALSE;
        $this-> status   =FALSE;
        $this-> hpos     =PLUGIN_RIGHT;
        $this-> vpos     =255;
        $this-> copyright=FALSE;
        $this-> info     =FALSE;
        $this-> version  =FALSE;
        $this-> param1   =FALSE;
        $this-> param2   =FALSE;
        $this-> param3   =FALSE;
        $this-> param4   =FALSE;
        $this-> store    =FALSE;
        $this-> type     =OBJECT_NONE;
        $this-> hits     =0;
        $this-> installed=0;
        }

    //Datenbank erzeugen
    //Wird hier nicht benutzt
    function install()
        {
        }

    //Datenbank zerstören
    //Wird hier nicht benutzt
    function uninstall()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Die IDs des Plugins neu bestimmen
    //Die ObjectID ist ein HASH-Wert aus Name und Version
    //Gleiche ObjectID == Gleicher Objekt
    //Die ID ist eine Unique-ID und bei jeder Erzeugung anders
    //Diese ID wird zur Addressierung in der Datenbank benutzt
    //Bei Unique-Objecten sind Beide IDs == ObjectID
    function calculate_ids()
        {
        //ID dieses Plugin-Typs bestimmen
        //Hash aus Name und Version
        $this->objectid=crypt_create_hash($this->name.$this->version);


        //Wenn das Plugin das Attribut unique hat, ist die unique-id gleich der plugin-id
        //Ansonsten erzeugen wir einfach ein
        if ($this->unique)
            {
            $this->id=$this->objectid;
            }
        else
            {
            //Die Unique-ID des Plugins bestimmen
            $this->id=crypt_create_unique_id();
            }

        return(TRUE);
        }

    //Den Datenbanknamen eines Plugins erzeugen
    function calculate_dbname()
        {
        //Wenn kein Name vorgegeben ist, erzeugen wir einen zufälligen
        if ($this->dbname!="")
            {
            $this->dbname=DB_OBJECTS."_".$this->dbname;
            }
        else
            {
            $this->dbname=DB_OBJECTS."_".crypt_create_unique_id();
            }

        return($this->dbname);
        }

    //Die Parameter des Objects laden
    function _load_parameter()
        {
        //MySQL-Zugriff holen
        global $mysql;
        global $debug;

        //Abfrage macehn
        $query="SELECT param1,param2,param3,param4 FROM ".DB_OBJECTS." WHERE id='".$this->id."'";
        $query=$mysql->query($query);

        //Und die Parameter setzen, wenn wir erfolgreich waren
        if (is_array($query))
            {
            $this->param1    =    $query[0]["param1"];
            $this->param2    =    $query[0]["param2"];
            $this->param3    =    $query[0]["param3"];
            $this->param4    =    $query[0]["param4"];
            }
        else
            {
            $debug->add(CLASS_OBJECT,"unable to load parameter ");
            }
        }

    //Die Parameter des Objects speichern
    function _save_parameter()
        {
        //MySQL-Zugriff holen
        global $mysql;
        global $debug;

        $query ="UPDATE ".DB_OBJECTS." SET ";
        $query.="param1='".$this->param1."',";
        $query.="param2='".$this->param2."',";
        $query.="param3='".$this->param3."',";
        $query.="param4='".$this->param4."' ";
        $query.="WHERE id='".$this->id."'";

        if ($mysql->query($query)===FALSE)
            {
            $debug->add(CLASS_OBJECT,"unable to save parameter ".$this->id);
            }
        }

    //Das Object-Script laden und darstellen
    function show($context)
        {
        //Alles holen, was die Objekte dürfen
        global $mysql;
        global $session;
        global $html;
        global $design;
        global $debug;
        global $vars;
        global $moduleadmin;
        global $pluginadmin;
        global $user;
        global $mailer;
        global $user;
        global $cron;
        global $cookies;
        global $newsletter;

        //Die Zugriffsdatei auswählen
        $filename=$this->filepath.$this->filename.OBJECT_OUT_PRAEFIX;

        //Im Admin-Edit die Edit-Seite darstellen
        //Aber nur wenn wir im  Modulkontext sind
        if (
            ($session->isedit==$this->id) &&
            ($session->user->has_write($this->id))     &&
            ($context==OBJECT_MODULE)
            )
            {
            $filename=$this->filepath.$this->filename.OBJECT_EDIT_PRAEFIX;
            }

        //Und anzeigen
        if (file_exists($filename))
            {
            //Die Parameter laden
            $this->_load_parameter();

            //Store zurücksetzen
            $this->store=FALSE;

            //Datei inkludieren
            include($filename);

            //Wurden sie verändert, dann speichern wir sie hier ab
            if ($this->store)
                {
                $this->_save_parameter();
                }
                
                
            //Nur Module mitzählen
            if ($this->type==OBJECT_MODULE)
                {
                $mysql->query("UPDATE ".DB_OBJECTS." set counter=counter+1 WHERE id='".$this->id."'");
                }
            }
        else
            {
            //Fehler ausgeben
            $html->text("Unable to open ".$filename,20);
            $debug->add(CLASS_OBJECT,"unable to open ".$filename);
            }
        }
    }

</script>