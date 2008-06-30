<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Session Klasse
///
/// Verwaltet alle Vorgänge, die mit Sessions zu tun haben.
/// Ist eine Session erzeugt, so wird in ihr auch direkt ein User
/// angelegt. Es kann keinen User auf der Webseite geben, dem keine
/// Session zugeordnet ist.
///
/// Um auf die Daten eines Users zuzugreifen (Name EMail etc)
/// kann man über die Session-Klasse gehen.
///
/// z.B
/// $session->user->name;
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_SESSION","class_session");
define ("CLASS_SESSION_VERSION","0.1");
if (isset($debug)) $debug->add(CLASS_SESSION,"version ".CLASS_SESSION_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define("MYSQL_CREATE_SESSION_TABLE","CREATE TABLE IF NOT EXISTS %tablename%   (session_id char(64),user_id  char (64),name char(64),created integer(32),lastaction integer(32),page_id char(64), PRIMARY KEY (session_id))"); 
define("MYSQL_ADD_TO_SESSION_TABLE","INSERT INTO %tablename% (session_id,user_id,name,created,lastaction)");


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class session
    {
    //Öffentliche Eigenschaftem
    var $id          ="none";                //ID der aktuellen Session
    var $user        =FALSE;                 //Userobjekt
    var $ip          ="0.0.0.0";             //IP-Adresse
    var $count       =0;                     //Anzahl der Sessions
    var $countable   =TRUE;                  //Session neu ?
    var $created     =0;                     //Wann wurde die Session erzeugt
    var $lastaction  =0;                     //Zeitpunkt der letzten Aktion
    var $lastpage    =FALSE;                 //Zuletzt besuchte Seite
    var $isedit      =FALSE;                 //Sind wir im Editiermodus ?

    var $buffer      =FALSE;                 //Buffer für Userdaten
    
    //Private Eigenschaften


    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function session()
        {
        global $vars;

        //Alle Werte auf Default setzen
        $this->setdefault();

        //Sind wir im Editmodus ?
        if ($vars->admin==ADMIN_EDIT)
            {
            //Dann die Seite speichern
            $this->isedit=$vars->adminid;
            }
        }

    //Destruktor
    function destroy()
        {
        $this->close();
        }

    //Loader (wird hier nicht benutzt)
    function open()
        {
        }

    //Alles schließen und Timeouts killen
    function close()
        {
        //Datenbankzugriff holen
        global $mysql;

        //Alte Einträge entfernen

        //Im Cookie-Login nur Sessions löschen, deren Startzeitpunkt
        //seit zwei Wochen abgelaufen ist
        if (COOKIE_LOGIN)
            {
            $query="DELETE FROM ".DB_SESSION." WHERE created < ".(time()  - SESSION_LIFETIME);
            }
        else
            {
            //Im Normalmodus Sessions löschen, die inaktiv sind
            $query="DELETE FROM ".DB_SESSION." WHERE lastaction < ".(time()-SESSION_TIMEOUT);
            }
        $mysql->query($query);
        
        //Wenn der Buffer gesetzt ist,
        //speichern wir deren Inhalt ab
        if ($this->buffer!==FALSE)
            {
            string_write(serialize($this->buffer),DIR_TEMP.$this->id);
            }
        }

    //Defaults
    function setdefault()
        {
        //Globale Variablen holen
        global $vars;

        //Als erstes setzen wir die Sessiondaten
        $this->id=crypt_create_unique_id();
        $this->user=new user();
        $this->ip=$vars->ip;
        $this->created=time();
        $this->lastaction=time();
        $this->lastpage=$vars->page;
        }
    
    //Datenbank erzeugen
    function install()
        {
        //Datenbankzugriff holen
        global $mysql;
        
        //Und einfach den Standardquery ausführen
        $query=str_replace("%tablename%",DB_SESSION,MYSQL_CREATE_SESSION_TABLE);

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
        $query="DROP TABLE IF EXISTS ".DB_SESSION;

        //Und Abfrage machen
        $result=$mysql->query($query);

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////

    //Eine Session mit den aktuellen Werten erzeugen
    //Als Parameter werden die aktuellen Eigenschaft benutzt
    function _add()
        {
        global $mysql;

        $query=str_replace("%tablename%",DB_SESSION,MYSQL_ADD_TO_SESSION_TABLE);
        $query.=sprintf("VALUES('%s','%s','%s','%d','%d')",$this->id,$this->user->id,$this->user->name,time(),time());
        $mysql->query($query);
        }

    //////////////////////////////////////////////////////////////////////////

    //Eine Session rauswerfen
    function _remove($sessionid)
        {
        global $mysql;
        global $cookies;

        $query="DELETE FROM ".DB_SESSION." WHERE session_id='".$sessionid."'";

        //Cookie rauswerfen
        if (COOKIE_LOGIN)
            {
            $cookies->remove("autologin");
            }

        $mysql->query($query);
        }

    //////////////////////////////////////////////////////////////////////////

    //Interne Funktion zur ermittlung der Userdaten
    //Status,Name,EMail etc
    function _getuserdata($userid)
        {
        //Userverwaltung holen
        global $user;

        $myuser=$user->read($userid);

        //Ein User gefunden ?
        if ($myuser!==FALSE)
            {
            //Die alten Daten löschen
            $this->user->destroy();

            //Dann veröffentlichen
            $this->user=$myuser;
            
            //LogIn-Status setzen
            $this->user->loggedin=TRUE;

            //Und freimachmen
            $myuser->destroy();
            unset($myuser);    
            }
        }
    
    //////////////////////////////////////////////////////////////////////////
    //Eine Session initieren (oder wiederaufnehmen wenn eine Session mit der
    //entsprechenden ID existiert)
    function add($sessionid)
        {
        //Datenbankzugriff holen
        global $mysql;
        global $debug;
        global $vars;
        global $cookies;

        $result=FALSE;

        //Wenn wir im AutoLogIn sind, dann über die SessionID einloggen
        if ((COOKIE_LOGIN) && $cookies->exists("autologin"))
            {
            //Session aus dem Cookie lesen
            $sessionid=string_filter($cookies->read("autologin"),FILTER_ALPHANUM);
            }

        //Unsere Session suchen
        $query ="SELECT * FROM ".DB_SESSION." WHERE session_id='".$sessionid."'";            
        $query=$mysql->query($query);

        //Gefunden ?
        if (is_array($query))
            {
            //Dann die Daten setzen
            $this->id               = $query[0]["session_id"];
            $this->created          = $query[0]["created"];
            $this->countable        = FALSE;

            //Und Userdaten lesen
            $this->_getuserdata($query[0]["user_id"]);

            //Die ID ziehen wir aus der Sessionliste, damit bei einem USER_NONE
            //nicht ständig die ID wechselt (wird bei jeder neuen Erzeugung ausgewürfelt)
            $this->user->id=$query["0"]["user_id"];

            //Session refreshen
            $query="UPDATE ".DB_SESSION." SET lastaction=".time().", page_id='".$this->lastpage."' WHERE session_id='".$this->id."'";
            $mysql->query($query);
            }
        else
            {
            //Neue Session machen
            $this->_add();
            //Neue Session anzeigen
            $this->countable=TRUE;
            }
        //Gruppenrechte laden
        $this->user->setgroups();

        //Anzahl der Sessions zählen
        $this->count=$mysql->query("SELECT count(session_id)as anzahl FROM ".DB_SESSION);
        $this->count=$this->count[0]["anzahl"];

        //Debug-Info
        $debug->add(CLASS_SESSION,"session id     :".$this->id);
        $debug->add(CLASS_SESSION,"session name   :".$this->user->name);
        $debug->add(CLASS_SESSION,"session userid :".$this->user->id);

        //Zwei Wochen im vorraus einloggen
        $cookies->add("autologin",$this->id,time() + 2 * SECS_PER_WEEK);

        //Wenn es einen Buffer gibt, holen wir diesen
        if (is_readable(DIR_TEMP.$this->id))
            {
            $this->buffer=unserialize(string_read(DIR_TEMP.$this->id));
            }
        else
            {
            $this->buffer=FALSE;
            }

        return($result);
        }    
        
    //////////////////////////////////////////////////////////////////////////
    //Session erhöhen und die Rechte des entsprechenden Benutzers setzen
    function elevate($username,$password)
        {
        //Userverwaltung holen
        global $user;
        global $userlog;

        //Auf ein Login checken
        $myuser=$user->login($username,$password);
    
        //Login OK
        if (is_object($myuser))
            {
            //Alten User rauswerfen
            $this->user->destroy();

            //Neuen User übernehmen
            $this->user=$myuser;

            //Und aufräumen
            $myuser->destroy();

            //Die Alte Session rauswerfen
            $this->_remove($this->id);
            //Und neu erzeugen
            $this->_add();

            //So, jetzt setzen wir das loggedin-flag
            $this->user->loggedin=TRUE;

            //Logging
            $userlog->add("user login ".$this->user->name);
            }
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Session erniedrigen zu nicht eingeloggt
    function degrade()
        {
        global $userlog;

        //Alte Session entfernen
        $this->_remove($this->id);

        //Aktuellen User einfach mit Standardwerten
        //initialisieren
        $this->user->setdefault();

        //Logging
        $userlog->add("user logout ".$this->user->name);

        //Und neu anlegen
        $this->_add();
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Einen link mit der aktuellen SessionID ausgeben
    //Und alle aktuellen Paramter anhängen
    function getlink()
        {
        global $vars;    

        //Sessionlink
        $result=URL_BASE."?session=".$this->id;

        //Und die Parameter hintendran
        $result.=$vars->getparam();

        return($result);
        }
        
    //Einen Link mit den gewünschten Daten zusammenbauen
    //Alle nicht übergebenen Daten werden mit den aktuellen Werten gefüllt
    function createlink($page="",$cmd="",$cmdid="",$subcmd="",$subcmdid="",$admin="",$adminid="",$sort="",$sortorder="")
        {
        global $vars;

        //Bei einem Robot vergeben wir keine Session ID
        //Intern wird diese geführt, aber nicht verbreitet
        $result=URL_BASE.( $this->user->bot!=TRUE ? "?session=".$this->id : "?r=1" );

        if ($page!==FALSE) if ($page!=="") $result.="&amp;page=".$page; else $result.="&amp;page=".$vars->page;
        if ($cmd!==FALSE)  if ($cmd!=="") $result.="&amp;cmd=".$cmd; else $result.="&amp;cmd=".$vars->cmd;
        if ($cmdid!==FALSE)  if ($cmdid!=="") $result.="&amp;cmdid=".$cmdid; else $result.="&amp;cmdid=".$vars->cmdid;
        if ($subcmd!==FALSE)  if ($subcmd!=="") $result.="&amp;subcmd=".$subcmd; else $result.="&amp;subcmd=".$vars->subcmd;
        if ($subcmdid!==FALSE)  if ($subcmdid!=="") $result.="&amp;subcmdid=".$subcmdid; else $result.="&amp;subcmdid=".$vars->subcmdid;
        if ($admin!==FALSE)  if ($admin!=="") $result.="&amp;admin=".$admin; else $result.="&amp;admin=".$vars->admin;
        if ($adminid!==FALSE)  if ($adminid!=="") $result.="&amp;adminid=".$adminid; else $result.="&amp;adminid=".$vars->adminid;
        if ($sort!==FALSE)  if ($sort!=="") $result.="&amp;sort=".$sort; else $result.="&amp;sort=".$vars->sort;
        if ($sortorder!==FALSE)  if ($sortorder!=="") $result.="&amp;sortorder=".$sortorder; else $result.="&amp;sortorder=".$vars->sortorder;

        //Evtl. Spaces zur Sicherheit ersetzen
        return(str_replace(" ","%20",$result));
        }

    //Einen Link nur für Sortierung erzeugen. Alle anderen Werte bleiben unangetastet.
    //Post verfällt
    function createsortlink($sort="",$sortorder="")
        {
        global $vars;
        
        //Nur Benutze Einträge übernehmen, um die URLs kurz zu halten
        if ($vars->page=="")    $page=FALSE;    else $page="";
        if ($vars->cmd=="")     $cmd=FALSE;     else $cmd="";
        if ($vars->cmdid=="")   $cmdid=FALSE;   else $cmdid="";
        if ($vars->subcmd=="")  $subcmd=FALSE;  else $subcmd="";
        if ($vars->subcmdid=="")$subcmdid=FALSE;else $subcmdid="";
        if ($vars->admin=="")   $admin=FALSE;   else $admin="";
        if ($vars->adminid=="") $adminid=FALSE; else $adminid="";

        return($this->createlink($page,$cmd,$cmdid,$subcmd,$subcmdid,$admin,$adminid,$sort,$sortorder));
        }


    //////////////////////////////////////////////////////////////////////////
    //Einen link mit der aktuellen SessionID ausgeben
    function getpurelink()
        {
        //Sessionlink
        $result=URL_BASE."?session=".$this->id;

        return($result);
        }
    }

</script>