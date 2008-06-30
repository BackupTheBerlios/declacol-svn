<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// User-Verwaltungs Klasse
///
/// Verwaltet alles was mit Usern zu tun hat
/// Über diese Klasse werden alle Änderungen des Userstatus,
/// des Usernamens etc abgehandelt.
///
//////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("CLASS_USERADMIN","class_useradmin");
define ("CLASS_USERADMIN_VERSION","0.1");
if (isset($debug)) $debug->add(CLASS_USERADMIN,"version ".CLASS_USERADMIN_VERSION);

//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
//Eine Benutzertabelle
define("MYSQL_CREATE_USER_TABLE","CREATE TABLE IF NOT EXISTS %tablename% (id char(64), name char(64),username char(64),hash char(64),email char(64),telephone char(64),lastlogin integer(32) UNSIGNED,userstatus integer(32), PRIMARY KEY(id))");
define("MYSQL_ADD_TO_USER_TABLE"   ,"INSERT INTO %tablename% (id,name,username,hash,email,telephone,lastlogin,userstatus)");


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class useradmin
    {
    //Öffentliche Eigenschaftem
    var $user                =FALSE;

    //Konstruktor
    function useradmin()
        {
        //User-Klasse instanzieren
        $this->user=new user();
        }

    //Destruktor
    function destroy()
        {
        }

    //Verbindnug öffnen
    //Wird hier nicht benutzt
    function open()
        {
        }

    //Verbindnug schließen
    //Wird hier nicht benutzt
    function close()
        {
        }

    //Defaults
    function setdefaults()
        {
        $this->active=FALSE;
        $this->internal_index=0;
        }

    //Datenbank erzeugen
    function install()
        {
        //Datenbankzugriff holen
        global $mysql;

        //In die Standardabfrage den richtigen Namen einkopieren
        $query=str_replace("%tablename%",DB_USER,MYSQL_CREATE_USER_TABLE);

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
        $query="DROP TABLE IF EXISTS ".DB_USER;

        //Und die Abfrag machen
        $result=$mysql->query($query);

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////

    //Den Hashwert für die Datenbank berechnen
    function _hash($username,$password)
        {
        return(crypt_create_sha1_hash($username.$password));
        }

    //Die ID eines Users bestimmen
    function _id($username)
        {
        return(crypt_create_sha1_hash($username));
        }

    //Eingaben filtern (Nur EMail-RFC-Zeichen sind zugelassen)
    function _filter($input)
        {
        return(string_filter($input,FILTER_EMAIL));
        }

    //Einen User einfügen
    //Ein neuer User wird immer mit minimalen Rechten und deaktiviert angelegt
    //Als Rückgabe kommt entweder FALSE oder das UserObject
    function add($name,$username,$password,$email,$telephone=0)
        {
        //Datenbankzugriff holen
        global $mysql;

        //Eingaben filtern
        $name     =string_filter ($name     ,FILTER_EMAIL);
        $username =string_filter ($username ,FILTER_PASSWORD);
        $password =$this->_filter($password ,FILTER_PASSWORD);
        $email    =$this->_filter($email    ,FILTER_EMAIL);
        $telephone=$this->_filter($telephone,FILTER_EMAIL);

        //Login-Hash erzeugen
        $hash=$this->_hash($username,$password);

        //User-ID erzeugen
        $id=$this->_id($username);

        //In die Standardabfrage den richtigen Namen einkopieren
        $query=str_replace("%tablename%",DB_USER,MYSQL_ADD_TO_USER_TABLE);

        //Values anhängen
        $query.=sprintf(" VALUES('%s','%s','%s','%s','%s','%s','%u','%u')",$id,$name,$username,$hash,$email,$telephone,0,USER_DISABLED);

        //Und einspielen
        $result=@$mysql->query($query);

        //Und ergebnis zurückgeben
        if ($result!==FALSE)
            {
            //Ein Userverzeichnis anlegen
            mkdir(DIR_USER.$id);
            
            
            return($this->read($id));
            }
        else
            {
            return(FALSE);
            }
        }

    //Einen User entfernen
    function remove($id)
        {
        //Datenbankzugriff holen
        global $mysql;

        $id    =string_filter($id,FILTER_ALPHANUM);

        $query="DELETE FROM ".DB_USER." WHERE id='".$id."'";

        //Und einspielen
        $result=$mysql->query($query);

        //Und ergebnis zurückgeben
        return($result);
        }

    //Daten eines Users lesen (Mit Test auf User und Passwort)
    //Das Ergebnis ist entweder FALSE oder ein User-Objekt
    function login($username,$password)
        {
        //Datenbankzugriff holen
        global $mysql;

        //Eingaben filtern
        $username=string_filter($username  ,FILTER_EMAIL);
        $password=$this->_filter($password ,FILTER_PASSWORD);

        //Fehler annehmen
        $result=FALSE;

        //Login-Hash erzeugen
        $hash=$this->_hash($username,$password);

        //Abfrage machen
        $query="SELECT * FROM ".DB_USER." WHERE (hash='".$hash."') AND (userstatus<>".USER_DISABLED.")";

        //Und los
        $query=$mysql->query($query);

        //Was gefunden ?
        if (is_array($query))
            {
            //Nur ein Ergebnis ?
            if (count($query)==1)
                {
                //Dann alle Werte in den lokalen Kontext übernehmen
                $result=new user();
                $result->id           =$query[0]["id"];
                $result->name         =$query[0]["name"];
                $result->username     =$query[0]["username"];
                $result->email        =$query[0]["email"];
                $result->telephone    =$query[0]["telephone"];
                $result->lastlogin    =$query[0]["lastlogin"];
                $result->status       =$query[0]["userstatus"];

                //Und Lastlogin setzen
                $query="UPDATE ".DB_USER." SET lastlogin=".time()." WHERE id='".$result->id."'";
                $mysql->query($query);

                //Nun laden wir den User noch mit seinen Gruppenrechten
                $result->setgroups();

                //Und den Statusstring noch bestimmen
                $result->calc_statusstr();
                }
            }
        return($result);
        }

    //Daten eines Users lesen (Ohne Test auf User und Passwort)
    //Das Ergebnis ist entweder FALSE oder ein User-Objekt
    function read($userid)
        {
        //Datenbankzugriff holen
        global $mysql;

        //Fehler annehmen
        $result=FALSE;

        //Abfrage machen
        $query="SELECT * FROM ".DB_USER." WHERE id='".$userid."'";

        //Und los
        $query=$mysql->query($query);

        //Was gefunden ?
        if (is_array($query))
            {
            //Nur ein Ergebnis ?
            if (count($query)==1)
                {
                //Dann alle Werte in den lokalen Kontext übernehmen
                $result=new user();
                $result->id         =$query[0]["id"];
                $result->name       =$query[0]["name"];
                $result->username   =$query[0]["username"];
                $result->email      =$query[0]["email"];
                $result->telephone  =$query[0]["telephone"];
                $result->lastlogin  =$query[0]["lastlogin"];
                $result->status     =$query[0]["userstatus"];

                //Und den Statusstring noch bestimmen
                $result->calc_statusstr();
                }
            }
        return($result);
        }

    //Den User über eine MD5(ID) lesen
    //sehr nützlich, um in der ausgelieferten Seite keine UserIDs sondern nur
    //deren MD5-Hash mitzuschicken
    function read_md5($md5id)
        {
        //Datenbankzugriff holen
        global $mysql;

        //Fehler annehmen
        $result=FALSE;

        //Injection abfangen
        $md5id=string_filter($md5id,FILTER_ALPHANUM);

        //Abfrage machen
        $query="SELECT id FROM ".DB_USER." WHERE MD5(id)='".$md5id."'";

        //Und los
        $query=$mysql->query($query);

        //Was gefunden ?
        if (is_array($query))
            {
            $result=$this->read($query[0]["id"]);
            }
        return($result);
        }
        

    //Das Kennwort eines Users ändern
    function setpass($id,$username,$newpassword)
        {
        //Datenbankzugriff holen
        global $mysql;

        //Eingaben filtern
        $id            =string_filter($id            ,FILTER_ALPHANUM);
        $username   =string_filter($username    ,FILTER_EMAIL);
        $newpassword=string_filter ($newpassword,FILTER_PASSWORD);

        //Neuen Login-Hash erzeugen
        $newhash=$this->_hash($username,$newpassword);

        //Query bauen
        $query="UPDATE ".DB_USER." SET hash='".$newhash."' WHERE id='".$id."'";

        //Und einspielen
        return($mysql->query($query));
        }

    //Die Email eines Users ändern
    function setemail($id,$newemail)
        {
        //Datenbankzugriff holen
        global $mysql;

        //Eingaben filtern
        $id            =string_filter($id        ,FILTER_ALPHANUM);
        $newemail   =string_filter($newemail,FILTER_EMAIL);

        //Query bauen
        $query="UPDATE ".DB_USER." SET email='".$newemail."' WHERE id='".$id."'";

        //Und einspielen
        $result=@$mysql->query($query);

        //Und ergebnis zurückgeben
        return($result);
        }

    //Die Telefonnummer eines Users ändern
    function settelephone($id,$newphone)
        {
        //Datenbankzugriff holen
        global $mysql;

        //Eingaben filtern
        $id         =string_filter($id      ,FILTER_ALPHANUM);
        $newphone   =string_filter($newphone,FILTER_EMAIL);

        //Query bauen
        $query="UPDATE ".DB_USER." SET telephone='".$newphone."' WHERE id='".$id."'";

        //Und einspielen
        $result=@$mysql->query($query);

        //Und ergebnis zurückgeben
        return($result);
        }

    //Den Anzeigenamen eines Users ändern
    function setname($id,$name)
        {
        //Datenbankzugriff holen
        global $mysql;

        //Eingabe filtern
        $id        =string_filter($id    ,FILTER_ALPHANUM);
        $name   =string_filter($name,FILTER_EMAIL);

        //Query bauen
        $query="UPDATE ".DB_USER." SET name='".$name."' WHERE id='".$id."'";

        //Und einspielen
        $result=@$mysql->query($query);

        //Und ergebnis zurückgeben
        return($result);
        }

    //Den Status eines Users setzen
    //(Guest,User etc) siehe Konstanten in class.user.php
    function setstatus($id,$userstatus)
        {
        //Datenbankzugriff holen
        global $mysql;
        global $groupadmin;

        $result=FALSE;

        $id        =string_filter($id    ,FILTER_ALPHANUM);


        //Alten Status holen
        $query="SELECT userstatus FROM ".DB_USER." WHERE id='".$id."'";
        $oldstatus=$mysql->query($query);

        //Alten Status gefunden ?
        if (is_array($oldstatus))
            {
            //Dann Status umstellen
            $query="UPDATE ".DB_USER." SET userstatus=".$userstatus." WHERE id='".$id."'";

            //Und einspielen
            $result=$mysql->query($query);

            //Wenn es ein Gruppenobjekt gibt, dann auch die Gruppe setzen
            if (is_object($groupadmin))
                {
                //Die alte Gruppe laden
                $mygroup=new group($groupadmin->statustogroup($oldstatus[0]["userstatus"]));
                //Und uns deregistrieren
                $mygroup->remove_user($id);
                $mygroup->save();
                $mygroup->close();

                //Neue Gruppe laden
                $mygroup->open($groupadmin->statustogroup($userstatus));
                //User registrieren
                $mygroup->add_user($id);
                $mygroup->save();

                //Fertig
                $mygroup->destroy();
                }
            }

        //Und ergebnis zurückgeben
        return($result);
        }

    //Eine Liste aller User holen
    //Rückgabe ist ein Array mit Userobjekten
    function enumerate($active=TRUE)
        {
        //Datenbankzugriff holen
        global $mysql;

        $result=FALSE;

        //Alle Abfragen
        if ($active)
            {
            $query="SELECT * FROM ".DB_USER." ORDER BY name";
            }
        else
            {
            $query="SELECT * FROM ".DB_USER." WHERE userstatus<>0 ORDER BY name";
            }
        $query=$mysql->query($query);

        //Und nun durch alle Ergebnisse durchgehen
        if (is_array($query))
            {
            foreach ($query as $oneuser)
                {
                //Für jeden User ein neues Objekt bauen
                $newuser=new user();
                $newuser->id        =$oneuser["id"];
                $newuser->name      =$oneuser["name"];
                $newuser->username  =$oneuser["username"];
                $newuser->email     =$oneuser["email"];
                $newuser->telephone =$oneuser["telephone"];
                $newuser->lastlogin =$oneuser["lastlogin"];
                $newuser->status    =$oneuser["userstatus"];

                $newuser->calc_statusstr();

                //Und dem Ergebnis zufügen
                $result[]=$newuser;
                //Freigeben
                unset($newuser);
                }
            }

        //Fertig
        return($result);
        }

    //Den Namen eines Users aus der ID holen
    function getname($id)
        {
        global $mysql;

        $dbresult=$mysql->query("SELECT name FROM ".DB_USER." WHERE id='".$id."'");

        if (is_array($dbresult))
            {
            return($dbresult[0]["name"]);
            }
        else
            {
            return(FALSE);
            }
        }

    //Den EMail eines Users aus der ID holen
    function getemail($id)
        {
        global $mysql;

        $dbresult=$mysql->query("SELECT email FROM ".DB_USER." WHERE id='".$id."'");

        if (is_array($dbresult))
            {
            return($dbresult[0]["email"]);
            }
        else
            {
            return(FALSE);
            }
        }

    //Den EMail eines Users aus der ID holen
    function gettelephone($id)
        {
        global $mysql;

        $dbresult=$mysql->query("SELECT telephone FROM ".DB_USER." WHERE id='".$id."'");

        if (is_array($dbresult))
            {
            return($dbresult[0]["telephone"]);
            }
        else
            {
            return(FALSE);
            }
        }
    }
</script>