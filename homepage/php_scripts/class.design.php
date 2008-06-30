<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Design Klasse
/// Ist für die Ausgabe der HTML-Seite zuständig.
/// Nicht zu verwechseln mit der Template-Klasse, die nur einzelne
/// HTML-Tags ausgibt
///
/// Hier wird das Skinmanagement abgehandelt und die Darstellung von
/// Modulen und Plugins gesteuert.
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_DESIGN","class_design");
define ("CLASS_DESIGN_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_DESIGN,"version ".CLASS_DESIGN_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
//Eine Templatetabelle
define("MYSQL_CREATE_DESIGN_TABLE","CREATE TABLE IF NOT EXISTS %tablename%   (design_id char(64),name char (32),path char (255),active integer(32), PRIMARY KEY (design_id))");
define("MYSQL_ADD_TO_DESIGN_TABLE","INSERT INTO %tablename% (design_id,name,path,active)");

define ("DESIGN_STYLE"  ,"style.css");
define ("DESIGN_HEAD"   ,DIR_TEMPLATES."header.php");
define ("DESIGN_FOOT"     ,DIR_TEMPLATES."footer.php");

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class design
    {
    //Öffentliche Eigenschaftem
    var $name        ="";
    var $path        ="";
    var $skinok      =FALSE;

    //Private Eigenschaften
    var $internal_header="";
    var $internal_footer="";


    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function design()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        $this->close();
        }

    //Verbindungsaufbau
    //Lädt das aktuellen Design (CSS) und fügt deren
    //Definition automatisch in den Header ein.
    function open()
        {
        global $mysql;

        //Simple Abfrage machen
        $query="SELECT * FROM ".DB_DESIGN." WHERE active=1";
        $query=$mysql->query($query);


        //Und den Pfad zurückgeben
        if (is_array($query))
            {
            $result=$query[0]["path"];

            //Die anderen Werte auch noch setzen
            $this->name=$query[0]["name"];
            $this->id  =$query[0]["design_id"];

            $this->skinok=TRUE;
            }
        else
            {
            //Ansonsten keinen richtigen Pfad zurückgeben
            $this->name="none";
            $this->id="none";
            $result="none";
            $this->skinok=FALSE;
            }

        //In einen URL-Pfad wandlen
        $this->path=str_replace(DIR_BASE,URL_BASE,$result);

        //Kopf und Fußteil laden
        $this->initheader();
        $this->initfooter();
        }

    //Verbindungsabbau
    //Wird hier nicht benötigt
    function close()
        {
        }

    //Defaults
    function setdefault()
        {
        //Mit Standardwerten initiieren
        $this->name        ="default";
        $this->id         ="1";
        $this->path        ="/default";
        }

    //Datenbank erzeugen
    function install()
        {
        global $mysql;

        //Alte Datenbank verwerfen
        $this->uninstall();

        //Einfach den Standardquery ausführen
        $query=str_replace("%tablename%",DB_DESIGN,MYSQL_CREATE_DESIGN_TABLE);
        $mysql->query($query);

        //Und nun alle Verfügbaren Skins einlesen
        $skins=dir_scan(DIR_SKINS,TRUE,FALSE,FALSE);

        $stdquery=str_replace("%tablename%",DB_DESIGN,MYSQL_ADD_TO_DESIGN_TABLE);
        foreach($skins as $skin)
            {
            //Daten rausziehen
            $name=string_extractfilename($skin);
            $path=string_add($skin,"/").DESIGN_STYLE;
            $id  =crypt_create_hash($name);


            //Prüfen, ob die Style-Datei existiert
            if (file_exists($path))
                {
                //Und Eintrag machen
                $query=$stdquery.sprintf("VALUES('%s','%s','%s',%u)",$id,$name,$path,("default"==$name?1:0));
                $mysql->query($query);
                }
            }
        }

    //Datenbank zerstören
    function uninstall()
        {
        global $mysql;

        //Alte Datenbank löschen
        $query="DROP TABLE IF EXISTS ".DB_DESIGN;
        $mysql->query($query);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Ein Design aktivieren
    function activate($id)
        {
        global $mysql;

        $result=FALSE;

        //Aktivieren
        $query="UPDATE ".DB_DESIGN." SET active=".YES." WHERE design_id='".$id."'";
        if ($mysql->query($query))
            {
            //Alle anderen deaktivieren
            $query="UPDATE ".DB_DESIGN." SET active=".NO." WHERE design_id<>'".$id."'";
            $result=$mysql->query($query);
            }

        return($result);
        }

    //Alle verfügbaren Designs als Array ausgeben
    function enumerate()
        {
        global $mysql;

        $result=array();
        //Simple Abfrage machen
        $query="SELECT * FROM ".DB_DESIGN." ORDER BY name";
        $query=$mysql->query($query);

        if (is_array($query))
            {
            $result=$query;
            }
        return($result);
        }
    //////////////////////////////////////////////////////////////////////////
    //Den Header laden
    function initheader()
        {
        //Einfach die Datei laden
        if (file_exists(DESIGN_HEAD))
            {
            $this->internal_header=file_get_contents(DESIGN_HEAD);
            }
        else
            {
            $this->internal_header="<html><head><title>".PAGE_TITLE."</title></head><body>\n";
            }

        //Styleinfos einfügen
        if ($this->skinok)
            {
            $this->internal_header=str_replace("%style%",$this->path,$this->internal_header);
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Den Footer laden
    function initfooter()
        {
        //Einfach die Datei laden
        if (file_exists(DESIGN_FOOT))
            {
            $this->internal_footer=file_get_contents(DESIGN_FOOT);
            }
        else
            {
            $this->internal_footer="</body></html>";
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Den Kopf ausgeben
    function writeheader($title="main")
        {
        global $html;
        global $session;

        //Und ausgeben
        $output=str_replace("%title%",$title,$this->internal_header);
        $output=str_replace("%product%",PAGE_TITLE,$output);
        $html->write($output);
        }

    //////////////////////////////////////////////////////////////////////////
    //Den Fuß ausgeben
    function writefooter()
        {
        global $html;

        $html->write(str_replace("%version%","powered by ".PRODUCT_NAME.".".PRODUCT_VERSION,$this->internal_footer));
        }



    //////////////////////////////////////////////////////////////////////////
    //Ausgabe von Plugins und Modulen
    //////////////////////////////////////////////////////////////////////////

    //Ein Plugin mit der ID $id ausgeben
    function writeplugin($id)
        {
        global $html;
        global $debug;
        global $objectadmin;
        global $session;
        global $user;

        //Falsche Zugriff abfangen
        if (!$session->user->has_read($id))
            {
            return(FALSE);
            }

        //Neues Plugin-Objekt erzeugen
        $newobject=new myobject();

        //Und über die Interne Methode laden
        if ($newobject->open($id))
            {
            //Plugin aktiv ?
            if ($newobject->enabled)
                {
                //Plugin sichtbar ?
                if ($newobject->visible)
                    {
                    //Das kpl Plugin
                    $html->div_open("plugin");

                    //Plugin-Titel
                    if ($newobject->showtitle)
                        {
                        $html->div_open("plugin_title");
                        $html->text($newobject->name,"plugin_title");
                        $html->div_close();
                        }

                    //Das eigentliche Plugin
                    $html->div_open("plugin_box");

                    //Anzeigen (Und dabei den Kontext mitgeben)
                    $newobject->show(OBJECT_PLUGIN);

                    //Plugin schließen
                    $html->div_close();

                    //Bottom anzeigen
                    $html->div_open("plugin_bottom");
                    $html->div_close();

                    //Und das Plugin wieder schließen
                    $html->div_close();
                    }
                else
                    {
                    //Das Plugin einfach so einblenden
                    $newobject->show(OBJECT_PLUGIN);
                    }
                }
            else
                {
                $debug->add(CLASS_DESIGN," plugin ".$id." not found");
                }

            //Adminbox einblenden
            $objectadmin->show_plugin_adminbox($newobject);

            //Speicher freigeben
            $newobject->destroy();
            }
        unset($newobject);
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////

    //Alle Plugins die an der entsprechenden Position
    //(LINKS/RECHTS) liegen ausgeben
    function writeplugins($position)
        {
        global $mysql;
        global $debug;
        global $session;
        global $html;

        //Probleme abfangen
        if (
            ($position !=PLUGIN_LEFT) &&
            ($position !=PLUGIN_RIGHT)
            )
            {
            $debug->add(CLASS_OBJECTADMIN,"illegal pluginposition");
            return(FALSE);
            }


        //Einfach alle abfragen
        //Je nach Userstatus
        if ($session->user->status==USER_ADMIN)
            {
            $query ="SELECT id,vpos FROM ".DB_OBJECTS." WHERE (hpos=".$position.") AND (type=".OBJECT_PLUGIN.") ORDER BY vpos";
            }
        else
            {
            $query ="SELECT id,vpos FROM ".DB_OBJECTS." WHERE (hpos=".$position.") AND (type=".OBJECT_PLUGIN.") AND (enabled=".YES.") ORDER BY vpos";
            }

        $dbresult=$mysql->query($query);

        if (is_array($dbresult))
            {
            //Plugin-Frame
            $position==PLUGIN_LEFT?$html->div_open("plugin_frame_left"):$html->div_open("plugin_frame_right");

            //Und alle gefundenen Einträge durchgehen
            foreach ($dbresult as $id)
                {
                //Und darstellen
                $this->writeplugin($id["id"]);
                }
            //Plugin-Frame
            $html->div_close();
            }
        return(TRUE);
        }


    //////////////////////////////////////////////////////////////////////////
    //Die Ausgabefunktionen eines Modules
    //////////////////////////////////////////////////////////////////////////

    //Ein Modul mit der ID $id ausgeben
    function writemodule($id)
        {
        global $session;
        global $vars;
        global $mysql;
        global $debug;
        global $userlog;

        //Wenn wir im Admin-Edit sind, die anfrage überschreiben und
        //das entsprechende Admin-Modul anzeigen
        if (
            ($vars->admin > ADMIN_EDIT_MIN) &&
            ($vars->admin < ADMIN_EDIT_MAX ) &&
            ($session->user->status==USER_ADMIN)
           )
            {
            $this->writemodule_admin($vars->admin);
            return(TRUE);
            }

        //Im Adminmodus überschreiben wir die Seitenvorgaben
        if ($session->isedit!==FALSE)
            {
            $id=$session->isedit;

            //Das angespochene Object suchen
            $query="SELECT * FROM ".DB_OBJECTS." WHERE (id='".$id."') AND (enabled=".YES.")";
            }
        else
            {
            //Das angespochene Modul suchen
            $query="SELECT * FROM ".DB_OBJECTS." WHERE (id='".$id."') AND (enabled=".YES.") AND (type=".OBJECT_MODULE.")";
            }
        $dbresult=$mysql->query($query);

        //Nix gefunden ?
        //Dann den ersten Eintrag suchen
        if (!is_array($dbresult))
            {
            $query="SELECT * FROM ".DB_OBJECTS." WHERE (enabled=".YES.") AND (type=".OBJECT_MODULE.") ORDER BY vpos";

            $dbresult=$mysql->query($query);
            }

        //Jetzt was gefunden ?
        if (is_array($dbresult))
            {
            //Neue ID setzen
            $id=$dbresult[0]["id"];

            //Nur für User mit passenden Rechten anpassen
            if ($session->user->has_read($id))
                {
                //Neues Modul-Objekt erzeugen
                $newobject=new myobject();

                //Und über die Interne Methode laden
                if ($newobject->open($id))

                //Darstellen
                $this->writemodule_raw($newobject);

                //Logging
                if (BOTS_LOG || !$session->user->bot)
                    {
                    $userlog->add($session->user->name." -> ".$newobject->name);
                    }

                //Und wieder zerstören
                $newobject->destroy();
                unset($newobject);
                }
            }
        return(TRUE);
        }

    //Ein Administrator-Module anzeigen
    function writemodule_admin($cmd)
        {
        //Alle Klassen freigeben, auf die das Modul Zugriff haben soll
        global $session;
        global $html;
        global $vars;
        global $user;
        global $objectadmin;
        global $groupadmin;
        global $design;
        global $cookies;
        global $newsletter;

        if ( ($cmd > ADMIN_EDIT_MIN) && ($cmd < ADMIN_EDIT_MAX) )
            {
            //Die Dateien sind fest verdrahtet, um Mißbrauch vorzubeugen
            $html->div_open("main");
            $html->div_open("module_frame");
            $html->div_open("module");
            $html->div_open("module_box");

            switch ($cmd)
                {
                case (ADMIN_EDIT_GROUPS)      : include(DIR_ADMIN."admin_group.php");       break;
                case (ADMIN_EDIT_USER)        : include(DIR_ADMIN."admin_user.php");        break;
                case (ADMIN_EDIT_MODULES)     : include(DIR_ADMIN."admin_modules.php");     break;
                case (ADMIN_EDIT_PLUGINS)     : include(DIR_ADMIN."admin_plugins.php");     break;
                case (ADMIN_EDIT_LOGS)        : include(DIR_ADMIN."admin_logs.php");        break;
                case (ADMIN_EDIT_CRON)        : include(DIR_ADMIN."admin_cron.php");        break;
                case (ADMIN_EDIT_LANGUAGE)    : include(DIR_ADMIN."admin_language.php");    break;
                case (ADMIN_EDIT_SKINS)       : include(DIR_ADMIN."admin_skin.php");        break;
                case (ADMIN_EDIT_MAIL)        : include(DIR_ADMIN."admin_mail.php");        break;
                case (ADMIN_EDIT_STATUS)      : include(DIR_ADMIN."admin_status.php");      break;
                case (ADMIN_EDIT_NEWSLETTER)  : include(DIR_ADMIN."admin_newsletter.php");  break;
                }

            $html->div_close();
            $html->div_close();
            $html->div_close();
            $html->div_close();

            return(TRUE);
            }
        return (FALSE);
        }

    //Ein Module direkt (Ohne DIV-Rahmen) ausgeben
    //dabei wird nicht die ID sondern das Modul direkt als Referenz übergeben
    function writemodule_raw(&$module)
        {
        global $session;
        global $html;
        global $vars;
        global $objectadmin;
        global $newsletter;

        //Hoffentlich haben wir jetzt was
        $html->div_open("main");
        if (is_object($module))
            {

            //Module-Frame
            $html->div_open("module_frame");

            //Das kpl Modul
            $html->div_open("module");

            //Modul-Titel
            $html->div_open("module_title");
            $html->text($module->name,"module_title");

            //Editmodus anzeigen
            if ($session->isedit==$module->id)
                {
                $html->text(" [Edit] ","small");
                }
            else
                {
                //Nur eingeloggte Besucher können abonnieren
                if ($session->user->status>USER_NONE)
                    {
                    //Abo möglich machen
                    if ($newsletter->check($module->id,$session->user->id)==FALSE)
                        {
                        //hat schon ein Abo
                        $html->link_short($session->createlink("","","","","",NEWSLETTER_REGISTER)," [ ".LNG_GETNEWSLETTER." ]" );
                        }
                    else
                        {
                        //Hat noch kein Abo
                        $html->link_short($session->createlink("","","","","",NEWSLETTER_REMOVE)," [ ".LNG_CLRNEWSLETTER." ]" );
                        }
                    }
                }

            //Adminbox einblenden
            $objectadmin->show_module_adminbox($module);

            $html->div_close();

            //Das eigentliche Module
            $html->div_open("module_box");

            //Anzeigen
            $module->show(OBJECT_MODULE);

            //Modul schließen
            $html->div_close();

            //Bottom anzeigen
            $html->div_open("module_bottom");
            $html->div_close();

            //Und das Module wieder schließen
            $html->div_close();

            //Module-Frame
            $html->div_close();
            }
        //Main-Frame
        $html->div_close();
        }
    }

</script>