<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// User Klasse
///
/// Repräsentiert einen User mit all seinen Eigenschaften
/// Wird von Useradmin und Session benutzt
/// Diese Klassen enthält quasi nur Eigenschaften, um das Userhandling
/// zu vereinfachen.
///
/// Über die "has"-Methoden können Berechtigungen zu beliebigen Objekten
/// abgefragt werden. Diese Berechtigungen sind Userstatus- und Gruppen-
/// abhängig.
///
/// Der Aufruf erfolgt mit der ID des gewünschten Objektes und gibt TRUE
/// oder FALSE zurück.
///
/// Die Eigenschaften Param1-Param7 entsprechen den Gruppenflags und können
/// beliebig vergeben werden. Sie werden nicht intern benutzt.
/// Module oder Plugins können frei darüber verfügen.
///
///z.B.
///
/// Abfragen, ob der aktuelle Benutzer Schreibrechte für das aktuell
/// angezeigte Modul hat. $this-> ist (aus einem Objekt) aufgerufen
/// immer die Klasse des aktuellen Objektes (Modul/Plugin)
///
/// if ($session->user->has_write($this->id))
///    {
///    //Benuter darf schreiben
///    }
///
/// if ($session->user->has_param1($this->id))
///    {
///    //Benutzer hat Eigenschaft Param1=TRUE
///    }
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_USER","class_user");
define ("CLASS_USER_VERSION","0.1");
if (isset($debug)) $debug->add(CLASS_USER,"version ".CLASS_USER_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
//Verschiedene User-Zustände
define ("USER_ENABLED"   , 1);
define ("USER_DISABLED"  , 0);
define ("USER_NOT_FOUND" ,-1);

define ("USER_NONE"        , 0);
define ("USER_GUEST"     ,1);
define ("USER_USER"         ,2);
define ("USER_XUSER"     ,4);
define ("USER_ADMIN"     ,8);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class user
    {
    //Öffentliche Eigenschaftem
    var $id        ="";
    var $name      ="";
    var $username  ="";
    var $email     ="";
    var $lastlogin =0;
    var $loggedin  =FALSE;
    var $status    =USER_NOT_FOUND;
    var $statusstr ="";
    var $telephone ="";
    var $dir       ="";
    var $bot       ="";
    var $agent     ="";

    //Private Eigenschaften

    //Zugriffseigenschaften
    //In den Arrays werden je nach Zugriffrechten die zugelassenen ObjectIDs
    //gepspeichert. D.h. Objecte mit nur Lesezugriff erscheinen nur in der
    //Gruppe readgroup
    var $internal_readgroup     =array();
    var $internal_writegroup    =array();
    var $internal_admingroup    =array();

    //Diese Gruppen verhalten sich wie die Zugriffsgruppen, allerdings haben
    //sie keinen Einfluß auf die Benutzeroberfläche sondern dienen einzig
    //zur Verfeinerung der Rechteverteilung in Modulen.
    //Damit kann ein Modulprogrammierer je nach Parameterzustand des Users
    //Optionen freischalten oder ähnliches
    var $internal_param1group    =array();
    var $internal_param2group    =array();
    var $internal_param3group    =array();
    var $internal_param4group    =array();
    var $internal_param5group    =array();
    var $internal_param6group    =array();
    var $internal_param7group    =array();

    //Die Vorgabewerte, falls ein Object nicht in einer der Rechtegruppen
    //gefunden wurde
    var $internal_readdefault    =FALSE;
    var $internal_writedefault    =FALSE;
    var $internal_admindefault    =FALSE;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function user()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    //Wird hier nicht benutzt
    function destroy()
        {
        $this->close();
        unset($this);
        }

    //Verbindungsaufbau
    //Wird hier nicht benutzt
    function open()
        {
        }

    //Alles schließen
    //Wird hier nicht benutzt
    function close()
        {
        }

    //Defaults setzen
    function setdefault()
        {
        $this->id        =crypt_create_unique_md5_id();
        $this->name      =LNG_VISITOR;
        $this->username  ="none";
        $this->email     ="abc@def.gh";
        $this->lastlogin =0;
        $this->status    =USER_NONE;
        $this->loggedin  =FALSE;
        $this->bot       =FALSE;
        @$this->agent     =$_SERVER["HTTP_USER_AGENT"];

        $this->calc_statusstr();
        //Und die Standardgruppen laden
        $this->setgroups();
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
    //Den Userstatus in einen String wandeln
    function calc_statusstr()
        {
        switch ($this->status)
            {
            case (USER_NONE)    : $this->statusstr="none";        break;
            case (USER_GUEST)   : $this->statusstr="guest";       break;
            case (USER_USER)    : $this->statusstr="user";        break;
            case (USER_XUSER)   : $this->statusstr="xuser";       break;
            case (USER_ADMIN)   : $this->statusstr="admin";       break;

            default     : $this -> statusstr="none";              break;
            }
        //Verzeicbnis setzen
        $this->dir=DIR_USER.$this->username."_".md5($this->id)."/";
        }

    //Die Userdaten als Text ausgeben (Hauptsächlich zu Debugzwecken)
    function write()
        {
        global $html;
        $html->pre_open("");
        $html->write("ID        :".$this->id."\n");
        $html->write("NAME      :".$this->name."\n");
        $html->write("LOGIN     :".$this->username."\n");
        $html->write("EMAIL     :".$this->email."\n");
        $html->write("TELEPHONE :".$this->telephone."\n");
        $html->write("LOGGEDIN  :".string_bool2str($this->loggedin)."\n");
        $html->write("LASTLOGIN :".date(DATE_LONG,$this->lastlogin)."\n");
        $html->write("STATUS    :".$this->statusstr."\n");
        $html->pre_close();
        }

    //Einem UserObject seine Gruppen zuordnen
    //Die Defaultgruppe wird aus seinem Grunduserstatus gelesen
    //Alle weiteren Rechte aus der Tabelle DB_GROUPS_MEMBERS
    function setgroups()
        {
        //Die alten zuordnungen löschen
        $this->internal_readgroup    =array();
        $this->internal_writegroup    =array();
        $this->internal_admingroup    =array();

        //Die Standardrechte des Users laden
        switch ($this->status)
            {
            //ID , READ, WRITE , ADMIN , SETASDEFAULT
            case (USER_NONE)    :  $this->addgroup(GROUP_NONE    ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE);    break;
            case (USER_GUEST)    :  $this->addgroup(GROUP_GUEST    ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE);    break;
            case (USER_USER)    :  $this->addgroup(GROUP_USER    ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE);    break;
            case (USER_XUSER)    :  $this->addgroup(GROUP_XUSER    ,TRUE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE);    break;
            case (USER_ADMIN)    :  $this->addgroup(GROUP_ADMIN    ,TRUE, TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE);    break;
            }
        }

    //Dem User eine Gruppe zufügen (Nicht in der Datenbank, sondern nur als Rechtetabelle)
    function addgroup($id,$readable,$writable,$admin,$param1=FALSE,$param2=FALSE,$param3=FALSE,$param4=FALSE,$param5=FALSE,$param6=FALSE,$param7=FALSE,$default=FALSE)
        {
        //Sollen die Werte als Default gelten ?
        if ($default)
            {
            $this->internal_readdefault =$readable;
            $this->internal_writedefault=$writable;
            $this->internal_admindefault=$admin;
            }
        else
            {
            //Einfach die übergebene ID den entsprechenden Gruppen zuordnen
            if ($readable)  $this->internal_readgroup[$id]=TRUE;
            if ($writable)  $this->internal_writegroup[$id]=TRUE;
            if ($admin)        $this->internal_admingroup[$id]=TRUE;
            }

        //Verteilung der Parameter auf die Gruppen
        //Für Parameter gibt es keine Defaulteinstellung
        if ($param1)  $this->internal_param1group[$id]=TRUE;
        if ($param2)  $this->internal_param2group[$id]=TRUE;
        if ($param3)  $this->internal_param3group[$id]=TRUE;
        if ($param4)  $this->internal_param4group[$id]=TRUE;
        if ($param5)  $this->internal_param5group[$id]=TRUE;
        if ($param6)  $this->internal_param6group[$id]=TRUE;
        if ($param7)  $this->internal_param7group[$id]=TRUE;
        }

    //Hat der User leserechte für das Modul mit der ID $id ?
    function has_read($id)
        {
        if (array_key_exists($id,$this->internal_readgroup)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return($this->internal_readdefault);
            }
        }

    //Hat der User schreibrechte für das Modul mit der ID $id ?
    function has_write($id)
        {
        if (array_key_exists($id,$this->internal_writegroup)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return($this->internal_writedefault);
            }
        }

    //Hat der User schreibrechte für das Modul mit der ID $id ?
    function has_admin($id)
        {
        if (array_key_exists($id,$this->internal_admingroup)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return($this->internal_admindefault);
            }
        }

    //Hat der User die Eigenschaft Param1 ?
    function has_param1($id)
        {
        if (array_key_exists($id,$this->internal_param1group)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return(FALSE);
            }
        }

    //Hat der User die Eigenschaft Param2 ?
    function has_param2($id)
        {
        if (array_key_exists($id,$this->internal_param2group)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return(FALSE);
            }
        }

    //Hat der User die Eigenschaft Param3 ?
    function has_param3($id)
        {
        if (array_key_exists($id,$this->internal_param3group)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return(FALSE);
            }
        }

    //Hat der User die Eigenschaft Param4 ?
    function has_param4($id)
        {
        if (array_key_exists($id,$this->internal_param4group)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return(FALSE);
            }
        }

    //Hat der User die Eigenschaft Param5 ?
    function has_param5($id)
        {
        if (array_key_exists($id,$this->internal_param5group)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return(FALSE);
            }
        }

    //Hat der User die Eigenschaft Param6 ?
    function has_param6($id)
        {
        if (array_key_exists($id,$this->internal_param6group)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return(FALSE);
            }
        }

    //Hat der User die Eigenschaft Param7 ?
    function has_param7($id)
        {
        if (array_key_exists($id,$this->internal_param7group)!==FALSE)
            {
            return(TRUE);
            }
        else
            {
            return(FALSE);
            }
        }
    }

</script>