<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// ObjectAdmin Klasse
///
/// Dient einzig der Verwaltung der Objekte in der Datenbank
///
/// In dieser Klasse wird die Darstellung und Enumeration durchgeführt.
/// Auch die Einblendung der Admin-Boxen (Up/Down/bearbeien etc.) wird
/// Hier abgehandelt. Nur in dieser Klasse wird ein Unterschied zwischen
/// Modul und Plugin bei der Darstellung gemacht. Ansonsten werden die
/// beiden Typen immer durch class.object repräsententiert.
/// Auch deren Daten werden in der gleichen Datenbank gespeichert.
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_OBJECTADMIN","class_objectadmin");
define ("CLASS_OBJECTADMIN_VERSION","0.2");
if (isset($debug)) $debug->add(CLASS_OBJECTADMIN,"version ".CLASS_OBJECTADMIN_VERSION);



//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class objectadmin
    {
    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function objectadmin()
        {
        global $vars;

        //Alle Werte auf Default setzen
        $this->setdefault();

        //Und evtl. geschickte Kommandos verarbeiten
        $this->process_plugincommands($vars);
        $this->process_modulecommands($vars);
        }

    //Destruktor
    function destroy()
        {
        }

    //Verbindungsaufbau
    //Wird hier nicht benutzen
    function open()
        {
        }

    //Verbindnug schließen
    //Wird hier nicht benutzen
    function close()
        {
        }

    //Defaults
    function setdefault()
        {
        $this->active=FALSE;
        $this->internal_index=0;
        }

    //Datenbank erzeugen
    function install()
        {
        //Datenbankzugriff holen
        global $mysql;

        //Abfrage aufbauen
        $query="CREATE TABLE IF NOT EXISTS ".DB_OBJECTS." (id char(64),object_id char(64), name char(64),filename char(128),version char(16) , dbname char(64), enabled integer(32),showtitle integer(32),visible integer(32), hpos integer(32), vpos integer(32), param1 char(128), param2 char(128), param3 char(128), param4 char(128),type integer(32),counter integer(32),installed integer(32), PRIMARY KEY (id))";

        //Und eine neue Datenbank anlegen
        $result=$mysql->query($query);

        return($result);
        }

    //Datenbank zerstören
    function uninstall()
        {
        //Datenbankzugriff holen
        global $mysql;

        //Abfrage aufbauen
        $query="DROP TABLE IF EXISTS ".DB_OBJECTS;

        //Und eine neue Datenbank anlegen
        $result=$mysql->query($query);

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Das Install-Array der objectdatei_init.php mit Standardwerten füllen
    function _init_install_array(&$result)
        {
        $result=array();

        $result["name"]          ="";                //Name des Objektes
        $result["version"]       ="";                //Version des Objektes
        $result["copyright"]     ="";                //Programmierer
        $result["info"]          ="";                //Kurzer Infotext zum Plugin
        $result["hpos"]          =PLUGIN_LEFT;        //Horizontale Position [PLUGIN_LEFT/PLUGIN_RIGHT]
        $result["vpos"]          =255;                //Vertikale Position 0(oben) bis 255 (unten)
        $result["filename"]      ="";                //Dateiname des Objektes ohne _edit oder so
        $result["showtitle"]     =FALSE;                //Wird der Plugin-Titel angezeigt
        $result["visible"]       =FALSE;                //Wird das Plugin unsichtbar ausgeführt ?
        $result["unique"]        =FALSE;                //Wird das Plugin unsichtbar ausgeführt ?
        $result["dbname"]        ="";                //Tabellenname, welchen das Plugin nutzen kann. Leer bedeutet automatische erzeugung
        $result["dbvalue"]       ="";                //Create Table Query z.B. (id char(64), name char(64), PRIMARY KEY(id))
        $result["dbqueries"]     =array();            //Weiter Queries die direkt ausgeführt werden

        $result["param1"]        ="";
        $result["param2"]        ="";
        $result["param3"]        ="";
        $result["param4"]        ="";
        }

    //////////////////////////////////////////////////////////////////////////
    //Ein Objektscript mit dem Name $filename laden
    //Als Ergebnis wird ein Object zurückgegeben, welches die Eigenschaften
    //der Init-Datei abbildet
    function _load_object($filename)
        {
        $result=FALSE;

        if (file_exists($filename))
            {
            //Install-Array initialisieren
            $init_array=FALSE;
            $this->_init_install_array($init_array);

            //Datei holen
            include($filename);

            //Neues Plugin-Objekt erzeugen
            $result= new myobject();

            //Damit enthält unser Array alle Daten
            //Und wir schieben sie in unser neues Objekt
            $result-> name     =$init_array["name"];
            $result-> filename =$init_array["filename"];
            $result-> dbname   =$init_array["dbname"];
            $result-> version  =$init_array["version"];
            $result-> copyright=$init_array["copyright"];
            $result-> info     =$init_array["info"];
            $result-> enabled  =TRUE;
            $result-> showtitle=$init_array["showtitle"];
            $result-> visible  =$init_array["visible"];
            $result-> unique   =$init_array["unique"];
            $result-> status   =FALSE;
            $result-> hpos     =$init_array["hpos"];
            $result-> vpos     =$init_array["vpos"];

            //Und noch die Paramter
            $result-> param1   =$init_array["param1"];
            $result-> param2   =$init_array["param2"];
            $result-> param3   =$init_array["param3"];
            $result-> param4   =$init_array["param4"];
            $result-> type     =OBJECT_NONE;

            //Nun erzeugen wir noch die ID und den Datenbanknamen (wenn keiner vorgegeben ist)
            $result->calculate_ids();
            $result->calculate_dbname();

            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Alle verfügbaren Objekt-Scripte des Types (OBJECT_TYPE) holen holen
    function enumerate_templates($type)
        {
        //Alle Dateien holen
        //Je nach Anfrage
        switch ($type)
            {
            case (OBJECT_PLUGIN)  : $objects=file_scan(DIR_PLUGINS,array("php"),TRUE,FALSE,FALSE);    break;
            case (OBJECT_MODULE)  : $objects=file_scan(DIR_MODULES,array("php"),TRUE,FALSE,FALSE);    break;
            default               : $objects=array();                                                    break;
            }

        $result=array();

        //Alle Funde durchgehen
        foreach ($objects as $object)
            {
            //Nur die Inits rausholen
            if (string_pos_left($object,OBJECT_INIT_PRAEFIX)!==FALSE)
                {
                //Und nun die Daten holen und speichern
                //Wir bekommen als Antwort entweder FALSE oder ein
                //initialisiertes Plugin-Objekt
                $newobject=$this->_load_object($object);

                if ($newobject!==FALSE)
                    {
                    //Den Type noch richtig setzen
                    $newobject->type=$type;

                    //Und im Ergebnisarray ablegen
                    $result[]=$newobject;
                    }
                }
            }

        //Wenn wir nichts gefunden haben, Fehler melden
        if (count($result)<1)
            {
            return(FALSE);
            }
        else
            {
            //Nach Modulname sortieren (CaseInsensitive)
            $temp=array();
            foreach ($result as $index=>$name)
                {
                $temp[$index]=strtolower($name->name);
                }
            asort($temp);
            $return=array();
            foreach ($temp as $index=>$name)
                {
                $return[$name]=$result[$index];
                }
            unset($result);
            //Ansonsten das Array schicken
            return($return);
            }
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Ein Objekt laden
    function read_installed($name)
        {
        global $mysql;

        //Einfach alle abfragen
        $query ="SELECT id FROM ".DB_OBJECTS." WHERE name='".$name."'";
        $dbresult=$mysql->query($query);

        $result=array();
        if (is_array($dbresult))
            {
            //Und alle gefundenen als Objekte laden
            foreach ($dbresult as $id)
                {
                //Neues Plugin-Objekt erzeugen
                $newobject=new myobject();
                //Und über die Interne Methode laden
                $newobject->open($id["id"]);

                //Dem Ergebnisarray zufügen
                $result[]=$newobject;
                }
            }

        //Wenn wir nichts gefunden haben, Fehler melden
        if (count($result)<1)
            {
            return(FALSE);
            }
        else
            {
            //Ansonsten das Array schicken
            return($result);
            }
        }

        
    //////////////////////////////////////////////////////////////////////////
    //Alle installierten Objekte des Types (OBJECT_TYPE) holen
    function enumerate_installed($type)
        {
        global $mysql;

        //Einfach alle abfragen
        $query ="SELECT id FROM ".DB_OBJECTS." WHERE type=".$type." ORDER by name";
        $dbresult=$mysql->query($query);

        $result=array();
        if (is_array($dbresult))
            {

            //Und alle gefundenen als Objekte laden
            foreach ($dbresult as $id)
                {
                //Neues Plugin-Objekt erzeugen
                $newobject=new myobject();
                //Und über die Interne Methode laden
                $newobject->open($id["id"]);

                //Dem Ergebnisarray zufügen
                $result[]=$newobject;
                }
            }

        //Wenn wir nichts gefunden haben, Fehler melden
        if (count($result)<1)
            {
            return(FALSE);
            }
        else
            {
            //Ansonsten das Array schicken
            return($result);
            }
        }
    //////////////////////////////////////////////////////////////////////////
    //Ein Objekt installieren
    function install_object(&$object)
        {
        //Datenbankzugriff holen
        global $mysql;

        $result=FALSE;

        //Ist das Plugin auch ein Objekt ?
        if (is_object($object))
            {

            //Dann bauen wir die Abfrage zusammen
            $query ="INSERT INTO ".DB_OBJECTS." ";
            $query.="(id,object_id,name,filename,version,dbname,showtitle,enabled,visible,hpos,vpos,param1,param2,param3,param4,type,counter,installed) ";
            $query.=" VALUES(";
            $query.="'".    $object->id."',";
            $query.="'".    $object->objectid."',";
            $query.="'".    $object->name."',";
            $query.="'".    $object->filename."',";
            $query.="'".    $object->version."',";
            $query.="'".    $object->dbname."',";
            $query.=(int)   $object->showtitle.",";
            $query.=(int)   $object->enabled.",";
            $query.=(int)   $object->visible.",";
            $query.=(int)   $object->hpos.",";
            $query.=(int)   $object->vpos.",";
            $query.="'".    $object->param1."',";
            $query.="'".    $object->param2."',";
            $query.="'".    $object->param3."',";
            $query.="'".    $object->param4."',";
            $query.=(int)   $object->type.",";
            $query.="0,";
            $query.=time().")";

            //Und einspielen
            //Da Unique-Plugins immer die gleichen IDs haben, lassen sie sich
            //auch nur einmal einspielen
            $result=$mysql->query($query);

            //Das Objekt in die Gruppe Standardgruppen einordnen
            $mygroup=new group(GROUP_NONE);
            if ($mygroup->open(GROUP_NONE))
                {
                $mygroup->add_object($object->id);
                $mygroup->save();
                }
            if ($mygroup->open(GROUP_GUEST))
                {
                $mygroup->add_object($object->id);
                $mygroup->save();
                }
            if ($mygroup->open(GROUP_USER))
                {
                $mygroup->add_object($object->id);
                $mygroup->save();
                }
            if ($mygroup->open(GROUP_XUSER))
                {
                $mygroup->add_object($object->id);
                $mygroup->save();
                }
            if ($mygroup->open(GROUP_ADMIN))
                {
                $mygroup->add_object($object->id);
                $mygroup->save();
                }
            $mygroup->destroy();

            //Damit sind wir aber noch nicht fertig
            //Wir müssen noch die Tabelle des Plugins erzeugen
            //Also ziehen wir uns das Install-Script des Objektes
            switch ($object->type)
                {
                case (OBJECT_PLUGIN)    : @include(DIR_PLUGINS.$object->filename.OBJECT_INIT_PRAEFIX); break;
                case (OBJECT_MODULE)    : @include(DIR_MODULES.$object->filename.OBJECT_INIT_PRAEFIX); break;
                }

            //Ist ein DB-Value angegeben ?
            if (isset($init_array["dbvalue"]))
                {
                //Dann erzeugen wir die Datenbank
                $query ="CREATE TABLE ".$object->dbname." ";
                $query.="(".$init_array["dbvalue"].")";

                if (DEBUG)
                    {
                    $result=$mysql->query($query);
                    }
                else
                    {
                    $result=$mysql->query($query);
                    }

                //Gibt es noch initialisierungs einträge ?
                if ( (isset($init_array["dbqueries"])) && ($result!==FALSE) )
                    {
                    //Dann führen wir diese aus
                    foreach ($init_array["dbqueries"] as $query)
                            {
                            //Dan Platzhalter durch den DB-Namen ersetzen
                            $query=str_replace("%table%",$object->dbname,$query);

                            //Und ausführen
                            if (DEBUG)
                                {
                                $mysql->query($query);
                                }
                            else
                                {
                                @$mysql->query($query);
                                }
                            }
                    }
                }
            }
        return($result);
        }

    //Ein Object ohne Referenz installieren
    //Typ gibt das Suchverzeichnis vor (Module / Plugin)
    function add_object($filename,$type)
        {
        //Dateinamen bauen
        switch ($type)
            {
            case (OBJECT_PLUGIN) :        $filename=DIR_PLUGINS.$filename.OBJECT_INIT_PRAEFIX;    break;
            case (OBJECT_MODULE) :        $filename=DIR_MODULES.$filename.OBJECT_INIT_PRAEFIX;    break;
            }


        //Ein Objekt ziehen
        $myobject=$this->_load_object($filename);


        //Was gefunden ?
        if ($myobject!=FALSE)
            {
            //Den Typ angeben
            $myobject->type=$type;

            //Dann installieren
            $this->install_object($myobject);

            return($myobject);
            }
        return(FALSE);
        }


    //////////////////////////////////////////////////////////////////////////
    //Ein Object deinstallieren (Aus der Objectreferenz heraus)
    function uninstall_object(&$object)
        {
        global $mysql;

        //Daten des Plugins ziehen
        $query ="SELECT * FROM ".DB_OBJECTS." WHERE (id='".$object->id."') AND (type=".$object->type.")";
        $result=$mysql->query($query);

        //Was gefunden ?
        if (is_array($result))
            {
            //Alle Datenbanken holen, die mit diesem Object zu tun haben
            $tables=$mysql->query("SHOW TABLES LIKE '".$result[0]["dbname"]."%'");

            //Damit werden alle DBs gelöscht die z.B.
            //$this->dbname."_addon";
            //heißen
            //Und alle löschen
            if (is_array($tables))
                {
                foreach ($tables as $table)
                    {
                    $query="DROP TABLE IF EXISTS ".array_pop($table);
                    $mysql->query($query);
                    }
                }


            //Und löschen den Eintrag aus der Liste
            $query="DELETE FROM ".DB_OBJECTS." WHERE id='".$object->id."'";
            $mysql->query($query);
            }
        }

    //Ein Object deinstallieren
    function remove_object($id)
        {
        //Neues Object erzeugen
        $myobject=new myobject();
        $myobject->open($id);

        //Ein installiertes Objekt ?
        if ($myobject->type!==OBJECT_NONE)
            {
            //Dann deinstallieren
            $this->uninstall_object($myobject);
            }
        $myobject->destroy();
        }

    //Die Positionen der Objekte richtig schön anordnen
    function _egalize_vpos($type)
        {
        global $mysql;

        $query="SELECT id,vpos FROM ".DB_OBJECTS." WHERE type=".$type." ORDER BY vpos";
        $result=$mysql->query($query);

        //Und nun alle schön egalisieren
        $vpos=0;
        if (is_array($result))
            {
            foreach ($result as $entry)
                {
                $mysql->query("UPDATE ".DB_OBJECTS." set vpos=".$vpos." WHERE id='".$entry["id"]."'");
                $vpos+=2;
                }
            }
        }

    //Das Objekt mit der ID $id um eine Position nach oben schieben
    function increase_vpos($id)
        {
        global $mysql;

        //Einfach die Wertigkeit um zwei erhöhen
        //Damit überspringen wir den nächsten Eintrag
        $query ="UPDATE ".DB_OBJECTS." SET ";
        $query.="vpos=vpos-3 WHERE id='".$id."'";

        return($mysql->query($query));
        }

    //Das Objekt mit der ID $id um eine Position nach unten schieben
    function decrease_vpos($id)
        {
        global $mysql;

        //Einfach die Wertigkeit um zwei erhöhen
        //Damit überspringen wir den nächsten Eintrag
        $query ="UPDATE ".DB_OBJECTS." SET ";
        $query.="vpos=vpos+3 WHERE id='".$id."'";

        return($mysql->query($query));
        }

    //Die horizontale Position des Objektes mit der ID $id setzen
    //hat nur bei Plugins Auswirkungen
    function set_hpos($id,$hpos)
        {
        global $mysql;

        $query ="UPDATE ".DB_OBJECTS." SET ";
        $query.="hpos=".$hpos." WHERE id='".$id."'";

        return($mysql->query($query));
        }

    //Ein Objekt ein oder Ausschalten
    function set_status($id,$enabled)
        {
        global $mysql;

        $query ="UPDATE ".DB_OBJECTS." SET ";
        $query.="enabled=".(int)$enabled." WHERE id='".$id."'";

        return($mysql->query($query));
        }

    //Den Namen eines Objektes ändern
    function set_name($id,$newname)
        {
        global $mysql;

        //Den Namen filtern
        $newname=string_filter($newname,FILTER_EMAIL);

        $query ="UPDATE ".DB_OBJECTS." SET ";
        $query.="name='".$newname."' WHERE id='".$id."'";

        return($mysql->query($query));
        }

    //Die Adminbefehle der Objektes zeigen
    //Context gibt den Anzeigetyp an.
    //OBJECT_MODULE / OBJECT_PLUGIN
    function show_adminbox(&$object,$context)
        {
        switch ($context)
            {
            case (OBJECT_PLUGIN)    : $this->show_plugin_adminbox($object);    break;
            case (OBJECT_MODULE)    : $this->show_module_adminbox($object);    break;
            }
        }

    //Die Adminbefehle für ein Modul anzeigen
    function show_module_adminbox(&$module)
        {
        global $session;
        global $vars;
        global $html;

        //Stimmen überhaupt die Zugriffsrechte ?
        if ( !$session->user->has_write($module->id))
            {
            return(FALSE);
            }

        //Sind wir im Edit-Modus (dann kein "Bearbeiten" Button)
        if ($session->isedit)
            {
            //Für Plugins nicht anzeigen
            if ($module->type!=OBJECT_PLUGIN)
                {
//                $vars->cmd="";
                $html->div_open("module_admin");
                $html->link_short($session->createlink()," [".LNG_RELOAD."] ","","module_admin");
                $html->link_short($session->createlink($module->id,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)," [".LNG_EXIT."] ","","module_admin");
                $html->div_close();
                }
            }
        else
            {
            //Wenn der User Schreibrechte hat, blenden wir den bearbeiten Button ein
            $html->div_open("module_admin");

            //Und einen Link daraus machen
            $html->link_short($session->createlink("",FALSE,FALSE,FALSE,FALSE,ADMIN_EDIT,$module->id),"[".LNG_EDIT."]","","module_admin");

            $html->div_close();
            }
        return(TRUE);
        }

    //Die Adminbefehle für ein Plugin anzeigen
    function show_plugin_adminbox(&$plugin)
        {
        global $session;
        global $vars;
        global $html;

        //Rechte holen
        $admin=$session->user->has_admin($plugin->id);
        $write=$session->user->has_write($plugin->id);

        //Falsche Zugriffe abfangen
        if ( ! ($admin || $write))
            {
            return(FALSE);
            }


        //Wenn wir Admin sind, dann blenden wir die Adminfunktionen ein
        $html->div_open("plugin_admin");

        //Im Admin-Modus auch deaktivierte Plugins darstellen
        if ($admin)
            {
            if (!$plugin->enabled)
                {
                $html->text($plugin->name);
                $html->nextline();

                //Und einen Link daraus machen
                $html->link_short($session->createlink("",FALSE,FALSE,FALSE,FALSE,ADMIN_PLUGIN_ENABLE,$plugin->id),"[".LNG_ENABLE."]","","");
                }
            else
                {
                $html->link_short($session->createlink("",FALSE,FALSE,FALSE,FALSE,ADMIN_PLUGIN_DISABLE,$plugin->id),"[".LNG_DISABLE."]","","");

                //Unsere Transport-Variablen definieren
                //Und einen Link daraus machen
                $html->link_short($session->createlink("",FALSE,FALSE,FALSE,FALSE,ADMIN_PLUGIN_MOVE_UP,$plugin->id),GFX_ARROW_UP,"","");
                $html->link_short($session->createlink("",FALSE,FALSE,FALSE,FALSE,ADMIN_PLUGIN_MOVE_DOWN,$plugin->id),GFX_ARROW_DOWN,"","");
                $html->link_short($session->createlink("",FALSE,FALSE,FALSE,FALSE,ADMIN_PLUGIN_MOVE_LEFT,$plugin->id),GFX_ARROW_LEFT,"","");
                $html->link_short($session->createlink("",FALSE,FALSE,FALSE,FALSE,ADMIN_PLUGIN_MOVE_RIGHT,$plugin->id),GFX_ARROW_RIGHT,"","");
                $html->nextline();
                }
            }

        //Den Bearbeitenbutton auch für Leute darstellen die Schreibrechte haben
        if ( ($plugin->enabled) && ($write) )
            {
            $html->link_short($session->createlink("",FALSE,FALSE,FALSE,FALSE,ADMIN_EDIT,$plugin->id),"[".LNG_EDIT." ".$plugin->name."]","","");
            }

        $html->div_close();
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Plugin-Nachrichte verarbeiten
    ///Alle Nachrichten, deren ID < ADMIN_PLUGIN_MIN und ID > ADMIN_PLUGIN_MAX
    ///entsprechen werden ignoriert
    function process_plugincommands(&$vars)
        {
        //Erstmal prüfen, ob Überhaupt ein Kommando vorliegt
        if ($vars->admin===FALSE)
            {
            return(FALSE);
            }
        if ($vars->admin > ADMIN_PLUGIN_MAX)
            {
            return(FALSE);
            }
        if ($vars->admin < ADMIN_PLUGIN_MIN)
            {
            return(FALSE);
            }

        //So, da ist also wirklich ein Befehl, dann führen wir ihn aus
        switch ($vars->admin)
            {
            //Hier werden einfach nur die entsprechenden Funktionen aufgerufen
            case (ADMIN_PLUGIN_MOVE_UP)   : $this->increase_vpos($vars->adminid);         break;
            case (ADMIN_PLUGIN_MOVE_DOWN) : $this->decrease_vpos($vars->adminid);         break;
            case (ADMIN_PLUGIN_MOVE_LEFT) : $this->set_hpos($vars->adminid,PLUGIN_LEFT); break;
            case (ADMIN_PLUGIN_MOVE_RIGHT): $this->set_hpos($vars->adminid,PLUGIN_RIGHT);break;
            case (ADMIN_PLUGIN_ENABLE)    : $this->set_status($vars->adminid,TRUE);      break;
            case (ADMIN_PLUGIN_DISABLE)   : $this->set_status($vars->adminid,FALSE);     break;

            //Noch nicht benutzt
            case (ADMIN_PLUGIN_EDIT)      : break;
            }

        //Datenbank hinbiegen
        $this->_egalize_vpos(OBJECT_PLUGIN);
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Module-Nachrichten verarbeiten
    ///Alle Nachrichten, deren ID < ADMIN_MODULE_MIN und ID > ADMIN_MODULE_MAX
    ///entsprechen werden ignoriert
    function process_modulecommands(&$vars)
        {
        //Erstmal prüfen, ob Überhaupt ein Kommando vorliegt
        if ($vars->admin===FALSE)
            {
            return(FALSE);
            }

        if ($vars->admin > ADMIN_MODULE_MAX)
            {
            return(FALSE);
            }
        if ($vars->admin < ADMIN_MODULE_MIN)
            {
            return(FALSE);
            }


        //So, da ist also wirklich ein Befehl, dann führen wir ihn aus
        switch ($vars->admin)
            {
            //Hier werden einfach nur die entsprechenden Funktionen aufgerufen
            case (ADMIN_MODULE_MOVE_UP)   : $this->increase_vpos($vars->adminid);         break;
            case (ADMIN_MODULE_MOVE_DOWN) : $this->decrease_vpos($vars->adminid);         break;
            case (ADMIN_MODULE_ENABLE)    : $this->set_status($vars->adminid,TRUE);      break;
            case (ADMIN_MODULE_DISABLE)   : $this->set_status($vars->adminid,FALSE);     break;

            //Noch nicht benutzt
            case (ADMIN_MODULE_EDIT)      : break;
            }
        //Datenbank hinbiegen
        $this->_egalize_vpos(OBJECT_MODULE);
        return(TRUE);
        }
    }
</script>