<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Diese Klasse verwaltet die Newsletters
///
/// Jedes Modul kann einfach darauf zugreifen und User registrieren
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_NEWSLETTER","class_newsletter");
define ("CLASS_NEWSLETTER_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_NEWSLETTER,"version ".CLASS_NEWSLETTER_VERSION);

define ("DB_NEWSLETTER",DB_PREFIX."newsletter");

define ("NEWSLETTER_REGISTER"  ,312521231);
define ("NEWSLETTER_REMOVE"    ,245123113);

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class newsletter
    {
    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function newsletter()
        {
        global $session;
        global $vars;
        
        //Alle Werte auf Default setzen
        $this->setdefault();
        
        //Und die Datenbank erzwingen
        $this->install();
        }

    //Destruktor
    function destroy()
        {
        }

    //Verbindungsaufbau
    function open()
        {
        }

    //Verbindungsabbau
    function close()
        {
        }
    
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        }
    
    //Datenbank und alles andere erzeugen
    function install()
        {
        //Datenbankzugriff holen
        global $mysql;
        
        //Abfrage aufbauen
        $query="CREATE TABLE IF NOT EXISTS ".DB_NEWSLETTER." (id int(11) NOT NULL AUTO_INCREMENT , module char(64), userid char(64), PRIMARY KEY (id))";

        //Und eine neue Datenbank anlegen
        $result=$mysql->query($query);

        return($result);
        }
    
    //Datenbank und alles andere zerstören
    function uninstall()
        {
        //Datenbankzugriff holen
        global $mysql;
        
        //Abfrage aufbauen
        $query="DROP TABLE IF EXISTS mytable";

        //Und eine neue Datenbank anlegen
        $result=$mysql->query($query);

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////
    function process()
        {
        global $session;
        global $vars;
        //Nur angemeldete Benuzter dürfen abonnieren
        if ($session->user->status > USER_NONE)
            {
            switch ($vars->admin)
                {
                case NEWSLETTER_REGISTER       : $this->add   ($vars->page,$session->user->id); break;
                case NEWSLETTER_REMOVE         : $this->remove($vars->page,$session->user->id); break;
                default                        : break;
                }
            }
         }


    //Prüfen, ob der User schon registriert ist
    function check($moduleid,$userid)
        {
        global $mysql;
        
        $result=$mysql->query("SELECT * FROM ".DB_NEWSLETTER." WHERE (module='".$moduleid."') AND (userid='".$userid."')");

        return(is_array($result));
        }
    

    //////////////////////////////////////////////////////////////////////////
    //Einen neuen User registrieren
    function add($moduleid,$userid)
        {
        global $mysql;
        
        if (!$this->check($moduleid,$userid))
            {
            $mysql->doquery("INSERT INTO ".DB_NEWSLETTER." (module,userid) VALUES(?,?)",array($moduleid,$userid));
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Alle registrierten User ausgeben
    function enum($moduleid=ID_NONE)
        {
        global $user;
        global $mysql;
        
        $result=array();

        //Alles aus der Tabelle ziehen
        $query="SELECT module,userid,
                (SELECT name FROM ".DB_USER."    s WHERE s.id=n.userid) as username,
                (SELECT name FROM ".DB_OBJECTS." o WHERE o.id=n.module) as modulename
                FROM ".DB_NEWSLETTER." n";
        if ($moduleid!=ID_NONE)
            {
            $query.=" WHERE module='".$moduleid."'";
            }
        $query.=" ORDER BY modulename, username";

        $dbresult=$mysql->query($query);
            
        if (is_array($dbresult))
            {
            //Und alle Daten schön in ein Array ablegen
            foreach ($dbresult as $entry)
                {
                $result[$entry["module"]]["name"]  =$entry["modulename"];
                $result[$entry["module"]]["user"][]=$user->read($entry["userid"]);
                }
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Einen neuen User entfernen
    function remove($moduleid,$userid)
        {
        global $mysql;

        $mysql->doquery("DELETE FROM ".DB_NEWSLETTER." WHERE (module='".$moduleid."') AND (userid='".$userid."')");
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine EMail an alle registrierten User loswerden
    function send($moduleid,$text)
        {
        global $mysql;

        //Nur bei aktiviertem Mailsupport machen
        if (MAIL_SUPPORT)
            {
            global $mailer;

            $result=$mysql->query("SELECT userid,(SELECT email FROM ".DB_USER." u WHERE u.id=n.userid)as email,(SELECT name FROM ".DB_OBJECTS." o WHERE o.id=n.module) as modulename FROM ".DB_NEWSLETTER." n WHERE module='".$moduleid."'");

            if (is_array($result))
                {
                foreach ($result as $entry)
                    {
                    $mail ="Newsletter from ".PAGE_TITLE."\r\n";
                    $mail.=$entry["modulename"]." : ";
                    $mail.=trim($text);
                    $mail.="\r\n try";
                    $mail.="\r\n  ".URL_BASE."?page=".$moduleid."";
                    $mail.="\r\n for more info";
                    $mail.="\r\n";
                    $mailer->add($entry["email"],"Newsletter from ".PAGE_TITLE,$mail,PRODUCT_NAME." Newsletter");
                    }
                }
            }
        }
    }
</script>