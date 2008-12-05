<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Klasse zum einfachen Zugriff auf Subversion-Repositories
///
//////////////////////////////////////////////////////////////////////////
require_once("class.easyxml.php");
require_once("class.ini.php");

//Versioninfo speichern
define ("CLASS_SVNCONNECT","class_svnconnect");
define ("CLASS_SVNCONNECT_VERSION","0.03");
if (isset($debug)) $debug->add(CLASS_SVNCONNECT,"version ".CLASS_SVNCONNECT_VERSION);

//Ein Paar vordefinierte Befehle
define ("_SVN_CMD_RECURSE","list --xml --recursive --username=%username% --password=%password% --non-interactive \x22%svnpath%\x22");
define ("_SVN_CMD_LIST"   ,"list --xml --username=%username% --password=%password% --non-interactive \x22%svnpath%\x22");
define ("_SVN_CMD_LOAD"   ,"export --username=%username% --password=%password% --non-interactive  \x22%svnpath%\x22 \x22%tempfile%\x22");
define ("_SVN_CMD_MKDIR"  ,"mkdir --username=%username% --password=%password% --non-interactive  --message=\"autocreated\" \x22%svnpath%\x22");

//Zugriffsrechte
define ("SVN_HAS_NONE"   ,0);
define ("SVN_HAS_READ"   ,1);
define ("SVN_HAS_WRITE"  ,2);

//Kommentar über dem IniFile
define ("SVN_COPYRIGHT"  ,"# file created using ".CLASS_SVNCONNECT." v".CLASS_SVNCONNECT_VERSION);


//////////////////////////////////////////////////////////////////////////
/// Administrationsklasse
/// Sie benötigt den "echten" Pfad zu einem Repository um die
/// Konfigurationsdateien zu lesen
//////////////////////////////////////////////////////////////////////////
class svnadmin
    {
    //Öffentliche Eigenschaften
    var $path        = "/usr/svn/";
    var $anon_access = "none";
    var $auth_access = "write";

    //Hier werden die Daten der User abgelegt
    var $userdata    = array();
    var $usergroups  = array();
    var $pathgroups  = array();

    //Interne Daten
    var $passfile    = "";
    var $groupfile   = "";


    function svnadmin()
        {
        $this->setdefaults();
        }
        
    function destroy()
        {
        unset($this);
        }
        
    function setdefaults()
        {
        $this->path        = "/usr/svn/";
        $this->anon_access = "none";
        $this->auth_access = "none";
        $this->userdata    = array();
        $this->usergroups  = array();
        $this->pathgroups  = array();
        $this->passfile    = "";
        $this->groupfile   = "";
        }

    function open()
        {
        $result=FALSE;
        
        //Slash am Ende erzwingen
        $this->path=string_add($this->path,"/");
        
        //Als allererstes laden wir die Konfiguration des Repositories, um an die
        //User und Gruppendateien zu kommen
        $config=new ini();
        $config->autoconvert=FALSE;
        if ($config->open($this->path."conf/svnserve.conf"))
            {
            //Die Zugriffsmodi lesen
            $this->anon_access =$config->read_value("general","anon-access","?");
            $this->auth_access =$config->read_value("general","auth-access","?");
            $this->passfile    =$config->read_value("general","password-db","?");
            $this->groupfile   =$config->read_value("general","authz-db","?");
            
            //Evtl. Traversen rausnehmen
            $this->passfile  =realpath($this->path."conf/".$this->passfile);
            $this->groupfile =realpath($this->path."conf/".$this->groupfile);

            //Auf Unixformat bringen
            $this->passfile =str_replace("\\","/",$this->passfile);
            $this->groupfile=str_replace("\\","/",$this->groupfile);

            $this->_loaduser($this->passfile);
            $this->_loadgroups($this->groupfile);

            $result=TRUE;
            }

        $config->destroy();
        
        return ($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Die Daten parsen und abspeichern
    function flush()
        {
        global $html;
        
        //Die Userdatei schreiben
        $out=new ini();
        $out->header=array(SVN_COPYRIGHT);
        $out->autoconvert=FALSE;
        foreach ($this->userdata as $name=>$pass)
            {
            $out->write_value("users",$name,$pass);
            }
        $out->flush($this->passfile);
        $out->destroy();
        
        //Die Gruppen sind etwas schwieriger, da hier nicht nur Benutzer sondern auch Gruppen gemischt werden
        $out=new ini();
        $out->header=array(SVN_COPYRIGHT);
        $out->autoconvert=FALSE;
        
        //Die Benutzergruppen
        foreach ($this->usergroups as $group=>$members)
            {
            //Alle Mitglieder durchgehen und nur solche aufnehmen, die
            //auch als Benutzer angelegt sind
            $groupmembers=array();
            foreach($members as $user)
                {
                $user=trim($user);
                if ($this->userexists($user))
                    {
                    $groupmembers[]=$user;
                    }
                }
            //Wenn noch Mitglieder übrig sind, übernehmen wir diese
            if (count($groupmembers) > 0)
                {
                $out->write_value("groups",$group,implode(",",$groupmembers));
                }
            else
                {
                $out->write_value("groups",$group,"none");
                }
            }

        //Und nun die Pfadfreigaben
        foreach ($this->pathgroups as $path=>$groups)
            {
            //Immer einen Slash erzwingen
            if (substr($path,0,1)!="/") $path="/".$path;

            //Nun gehen wir alle Freigaben durch und Prüfen die zugehörigkeiten
            foreach ($groups as $member=>$rights)
                {
                if ($this->groupexists($member))
                    {
                    $out->write_value($path,"@".$member,$this->rightstostring($rights));
                    }
                if ($this->userexists($member))
                    {
                    $out->write_value($path,$member,$this->rightstostring($rights));
                    }
                }
            }
        $out->flush($this->groupfile);
        $out->destroy();
        }

    //Alles wieder freimachen
    function close()
        {
        $this->setdefaults();
        }
    
    //////////////////////////////////////////////////////////////////////////
    ///Verwaltungsfunktionen
    //////////////////////////////////////////////////////////////////////////
    //Einen User zu einer PfadGruppe zufügen. Wenn die Gruppe noch nicht
    //existiert, wird sie erzeugt
    function addpathmember($path,$member,$rights)
        {
        $result=FALSE;
        $path  =string_filter($path,FILTER_URL);
        $rights=string_filter($rights,FILTER_NUMBER);

        //Ein User ?
        if ($this->userexists($member))
            {
            $this->pathgroups[$path][$member]=$rights;
            $result=TRUE;
            }

        //Eine Gruppe ?
        if ($this->groupexists($member))
            {
            $this->pathgroups[$path]["@".$member]=$rights;

            $result=TRUE;
            }

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine Mitglied aus einer PfadGruppe entfernen. Ist die Gruppe leer, wird sie
    //automatisch beim nächsten Speichern entfernt
    function removepathmember($path,$member)
        {
        $result=FALSE;

        //Ein User ?
        if (isset($this->pathgroups[$path][$member])==TRUE)
            {
            unset($this->pathgroups[$path][$member]);
            $result=TRUE;
            }

        //Eine Gruppe ?
        $member="@".$member;
        if (isset($this->pathgroups[$path][$member])==TRUE)
            {
            unset($this->pathgroups[$path][$member]);
            $result=TRUE;
            }

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Einen User zu einer BenutzerGruppe zufügen. Wenn die Gruppe noch nicht
    //existiert, wird sie erzeugt
    function addgroupmember($group,$user)
        {
        $result=FALSE;
        if ($this->userexists($user) == TRUE)
            {
            $this->usergroups[$group][$user]=$user;

            $result=TRUE;
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Einen User aus einer Gruppe entfernen. Ist die Gruppe leer, wird sie
    //automatisch beim nächsten Speichern entfernt
    function removegroupmember($group,$user)
        {
        $result=FALSE;
        if ( isset($this->usergroups[$group][$user]) )
            {
            unset($this->usergroups[$group][$user]);
            $result=TRUE;
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Einen Benutzer zufügen. Zur Sicherheit lassen wir nur Usernamen zu,
    //die weder als User noch als Gruppe schon existieren
    function adduser($username,$password)
        {
        $result=FALSE;
        if ( ($this->userexists ($username) == FALSE) &&
             ($this->groupexists($username) == FALSE))
            {
            $this->userdata[$username]=$password;
            $result=TRUE;
            }
        return($result);
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Einen Benutzer entfernen (Wird beim Abspeichern automatisch aus
    //allen Gruppen etc mitentfernt
    function removeuser($username)
        {
        $result=FALSE;
        if ($this->userexists($username) != FALSE)
            {
            unset($this->userdata[$username]);
            $result=TRUE;
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Ein neues Kennwort setzen
    function changepassword($user,$oldpassword,$newpassword)
        {
        $result=FALSE;
        //User OK
        if ($this->userexists($user)==TRUE)
            {
            //Altes Kennwort OK
            if ($this->userdata[$user]==$oldpassword)
                {
                //Dann alles OK
                $this->userdata[$user]=$newpassword;
                $result=TRUE;
                }
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine Gruppe zufügen
    function addgroup($group)
        {
        $result=FALSE;
        if ( ($this->userexists ($group) == FALSE) &&
             ($this->groupexists($group) == FALSE))
            {
            $this->usergroups[$group]=array();
            $result=TRUE;
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine kpl. Gruppe entfernen
    function removegroup($group)
        {
        $result=FALSE;
        if ($this->groupexists($group) != FALSE)
            {
            unset($this->usergroups[$group]);
            $result=TRUE;
            }
            
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine Gruppe zufügen
    function addpath($path)
        {
        if (substr($path,0,1) != "/") $path="/".$path;
        
        $result=FALSE;
        if (isset($pathgroups[$path])==FALSE)
            {

            $this->pathgroups[$path]["*"]=SVN_HAS_NONE;
            $result=TRUE;
            }
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine kpl. Gruppe entfernen
    function removepath($path)
        {
        $result=FALSE;
        if ( isset($this->pathgroups[$path])== TRUE )
            {
            unset($this->pathgroups[$path]);
            $result=TRUE;
            }

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Prüfen, ob ein User valide ist
    function uservalid($name,$password)
        {
        $result=FALSE;
        if (isset($this->userdata[$name])==TRUE)
            {
            if ($this->userdata[$name]==$password)
                {
                $result=TRUE;
                }
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Prüfen, ob ein Benutzer existiert
    function userexists($name)
        {
        //Wildcard behandeln wir extra
        if ($name=="*")
            {
            $result=TRUE;
            }
        else
            {
            $result=isset($this->userdata[$name]);
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Prüfen, ob eine Gruppe existiert
    function groupexists(&$name)
        {
        //Wenn ein Steuerzeichen mitkommt entfernen wir dieses
        if ($name!="")
            {
            if ($name[0]=="@")
                {
                $name=substr($name,1,strlen($name));
                }
            }
        return(isset($this->usergroups[$name]));
        }

    function rightstostring($rights)
        {
        $result="";
        if ( ($rights & SVN_HAS_READ) !=0) $result.="r";
        if ( ($rights & SVN_HAS_WRITE)!=0) $result.="w";
        if ($result=="") $result=" ";

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Den Userfile laden und das Array entsprechend füllen.
    function _loaduser($filename)
        {
        $users=new ini();
        $users->autoconvert=FALSE;

        if ($users->open($filename) != FALSE)
            {
            $this->userdata=$users->read_values("users");
            }
        $users->destroy();
        }
    
    
    //////////////////////////////////////////////////////////////////////////
    //Den Userfile laden und das Array entsprechend füllen.
    function _loadgroups($filename)
        {
        global $html;
        
        $groups=new ini();
        $groups->autoconvert=FALSE;

        if ($groups->open($filename) != FALSE)
            {
            //Wenn es Gruppen gibt, initialisieren wir das Grouparray
            //Leider lässt Subversion auch Zugriff auf Pfade für User zu,
            //die keiner Gruppe angehören
            $result=$groups->read_values("groups");
            if (is_array($result))
                {
                foreach($result as $group=>$users)
                    {
                    //Zur späteren Erleichterung der verwaltung
                    //Setzen wir im Array den Index und den Inhalt gleich
                    $members=explode(",",$users);
                    foreach ($members as $member)
                        {
                        $this->usergroups[$group][$member]=$member;
                        }
                    
                    }
                }
                
            //Weiterhin liegen in diese Datei die einzelnen Rechte für Pfade
            //Dazu lesen wir einfach alle Sectionen und und entfernen die
            //Gruppensection. Ein @ vor einem Namen definiert eine Gruppe,
            //die diesem Pfad zugeordnet ist. * mein alle User
            $result=$groups->read_sections();
            if (is_array($result)!=FALSE)
                {
                foreach ($result as $userpath)
                    {
                    if (strtolower($userpath)!="groups")
                        {
                        $rights=$groups->read_values($userpath);
                        
                        //Nun haben wir alle Pfade mit ihren Zuordnungen als
                        //Array vorliegen. Um weiterhin leichter damit arbeiten
                        //zu können, wandeln wir die Klartextrechte in Flags um
                        if (is_array($rights)!=FALSE)
                            {
                            foreach ($rights as $user=>$mode)
                                {
                                $mymode=SVN_HAS_NONE;
                                $mode=strtolower($mode);
                                if (strpos($mode,"r")!==FALSE) $mymode|=SVN_HAS_READ;
                                if (strpos($mode,"w")!==FALSE) $mymode|=SVN_HAS_WRITE;
                                
                                $this->pathgroups[$userpath][$user]=$mymode;
                                }
                            }
                        }
                    }
                }
            }
        $groups->destroy();
        }
    }


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse zum Browsen eines Repositories
//////////////////////////////////////////////////////////////////////////
class svnbrowser
    {
    //Öffentliche Eigenschaftem
    var $path   = "svn://myrepository";
    var $bin    = "svn.exe";
    var $user   = "username";
    var $pass   = "password";
    var $tempdir= FALSE;
    var $tempfile=FALSE;
    
    var $files      = array();
    var $directories= array();

    var $xml        =FALSE;
    
    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function svnbrowser()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        
        //Die XML-Klasse initialisieren
        $this->xml= new xml();

        //Gibt es ein temporäres Verzeichnis ?
        if (defined("DIR_TEMP"))
            {
            $this->tempdir=DIR_TEMP;
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->xml->destroy();
        }

    //////////////////////////////////////////////////////////////////////////
    //Verbindungsaufbau
    function open()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    //Verbindungsabbau
    function close()
        {
        }
    
    //////////////////////////////////////////////////////////////////////////
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->files       = array();
        $this->directories = array();
        $this->bin    = "svn.exe";
        $this->user   = "";
        $this->ass    = "";
        }
    
    //////////////////////////////////////////////////////////////////////////
    //Datenbank und alles andere erzeugen
    function install()
        {
        return(TRUE);
        }
    
    //////////////////////////////////////////////////////////////////////////
    //Datenbank und alles andere zerstören
    function uninstall()
        {
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////
    //Eine Datei laden und in ein temporäres Verzeichnis ablegen
    function load()
        {
        //Haben wir ein temporäres verzeichnis ?
        if (!is_dir($this->tempdir))
            {
            die ("tempdir needed");
            }
            
        //Immer mit einem Slash enden
        $this->tempdir=string_add($this->tempdir,"/");
        //Einen temporären Dateinamen erzeugen
        $this->tempfile=$this->tempdir.md5(time().rand(0,65535));
        
        $this->exec_subversion(_SVN_CMD_LOAD);
        
        return (file_exists($this->tempfile));
        }

    /////////////////////////////////////////////////////////////////////////
    //Die Übersicht über den aktuellen Pfad holen und in den Ausgabearrays ablegen
    function read()
        {
        //Alte Ergebnisse löschen
        $this->directories=array();
        $this->files=array();
        
        $result = $this->exec_subversion(_SVN_CMD_LIST);

        //Das Ergebnis parsen
        $this->xml->import($result);
        
        $index=0;
        $node=$this->xml->findfirst("entry");
        while ($node !== FALSE)
            {
            //Daten aus den Knoten holen
            $namenode=$this->xml->searchbyname("name",$node);
            if ($namenode!==FALSE)
                {
                $sizenode  =$this->xml->searchbyname("size",$node);
                $commitnode=$this->xml->searchbyname("commit",$node);
                $authornode=$this->xml->searchbyname("author",$node);
                $datenode  =$this->xml->searchbyname("date",$node);
                
                //Ist die Größe definiert, sind wir eine Datei, ansonsten ein Verzeichnis
                if ($sizenode!==FALSE)
                    {
                    $this->files[$index]["size"]=$sizenode->tree->value;
                    $this->files[$index]["name"]=$namenode->tree->value;
                    $this->files[$index]["commit"]=$commitnode->tree->value;
                    $this->files[$index]["author"]=$authornode->tree->value;
                    $this->files[$index]["date"]=$this->convert_date($datenode->tree->value);
                    }
                else
                    {
                    $this->directories[$index]["size"]=0;
                    $this->directories[$index]["name"]=$namenode->tree->value;
                    $this->directories[$index]["commit"]=$commitnode->tree->value;
                    $this->directories[$index]["author"]=$authornode->tree->value;
                    $this->directories[$index]["date"]=$this->convert_date($datenode->tree->value);
                    }
                $index++;
                }
            $node=$this->xml->findnext($node);
            }
        }


    /////////////////////////////////////////////////////////////////////////
    //Ein Verzeichnis erzeugen
    function createdir($dirname)
        {
        //Den Befehl ganz normal aufbauen
        $cmd=$this->bin." ".$this->process_command(_SVN_CMD_MKDIR);
        
        //Und dann für alle Zwischenpfade den CreateBefehl aufrufen, um auch
        //mehrstufige Verzeichnisse automatisch zu erzeugen
        $dirs=explode("/",$dirname);

        $base=string_add($this->path,"/");
        $sub=$base;
        foreach ($dirs as $directory)
            {
            if ($directory!="")
                {
                extend_runtime(10);
                
                $sub.=string_add($directory,"/");
                //Den Pfad immer weiter aufbauen
                $newcmd=str_replace($this->path,$sub,$cmd);
                shell_exec($newcmd);
                }
            }
        }

    /////////////////////////////////////////////////////////////////////////
    //Den Externen Subversionprozess aufrufen
    function exec_subversion($cmd)
        {
        $cmd=$this->bin." ".$this->process_command($cmd);
        return(@shell_exec($cmd));
        }

    /////////////////////////////////////////////////////////////////////////
    //Einen Befehl um die Schlüsselwörter ertweitern
    function process_command($command)
        {
        $result=str_replace("%username%",$this->user,$command);
        $result=str_replace("%password%",$this->pass,$result);
        $result=str_replace("%svnpath%" ,$this->path,$result);
        $result=str_replace("%temppath%",$this->tempdir,$result);
        $result=str_replace("%tempfile%",$this->tempfile,$result);
        return($result);
        }

    /////////////////////////////////////////////////////////////////////////
    //Aus dem Zeitstempel einen Unix-Timestamp machen
    function convert_date($input)
        {
        //Datum und Uhrzeit rauszwirbeln
        $result=array();
        if (preg_match_all("#([0-9\-]{10}).*([0-9\:]{8})#",$input,$result)>0)
            {
            reset($result);
            $date=next($result);
            $time=next($result);
            //Und aus den Werten einen Timestamp machen
            $input=strtotime(reset($date)." ".reset($time));
            }
        else
            {
            $input=0;
            }

        return($input);
        }
    }
</script>