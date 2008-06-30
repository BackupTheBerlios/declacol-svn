<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Diese Klasse verwaltet Cookies
///
/// Um ein Cookie zu setzen, wird einfach mit add ein Cookie hinzugefügt.
/// Bei der nächsten Aktion wird das das Cookie aus der Datenbank gelesen
/// und abgespeichert. Damit sind auch Module in der Lage, Cookies zu
/// setzen
///
/// Ist buffered=FALSE, so werden Cookies direkt ausgegeben.
/// Dies funktioniert nur im Zusammenspiel mit der Templateklasse,
/// da diese die Ausgaben puffern kann und erst durch ein Flush die
/// Seite ausgibt. Das ist notwendig, da Cookies IMMER vor dem Seiten-
/// quelltext zum Browser übertragen werden müssen.
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_COOKIES","class_cookies");
define ("CLASS_COOKIES_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_COOKIES,"version ".CLASS_COOKIES_VERSION);

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class cookies
    {
    //Werden Cookies gepuffert oder direkt ausgegeben ?
    var $buffered=TRUE;

    var $cookies=array();

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function cookies()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();

        //Und die vorhandenen Cookies einlesen
        if (is_array($_COOKIE))
            {
            foreach ($_COOKIE as $index=>$cookie)
                {
                //Id des Kontrollcookies erzeugen
                $controlid =crypt_create_hash($index);

                //Gibt es ein ControlCookie ?
                if (isset($_COOKIE[$controlid]))
                    {
                    //Stimmen die Daten ?
                    if ($_COOKIE[$controlid]==crypt_create_hash($cookie))
                        {
                        //Alles OK, dann merken
                        $this->cookies[$index]=$cookie;
                        }
                    }
                }
            //Und um Mißbrauch vorzubeugen das Cookie-array
            //löschen
            $_COOKIE=array();
            }
        }

    //Destruktor
    function destroy()
        {
        unset($this);
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
        $this->buffered=TRUE;
        $this->cookies=array();
        }
    
    //Datenbank und alles andere erzeugen
    function install()
        {
        //Datenbankzugriff holen
        global $mysql;
        
        //Abfrage aufbauen
        $query="CREATE TABLE IF NOT EXISTS ".DB_COOKIES." (id int(32) AUTO_INCREMENT NOT NULL , name char(64),path char(64), cookie_value longtext,expire int(32), PRIMARY KEY (id))";

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
        $query="DROP TABLE IF EXISTS ".DB_COOKIES;

        //Und eine neue Datenbank anlegen
        $result=$mysql->query($query);

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Ein Cookie in die Pipe hängen
    //Wenn wir im gepufferten Modus sind, werden Cookies in der Datenbank gespeichert
    //Ansonsten werden sie direkt ausgegeben
    function add($name,$value,$expire=0,$path="/")
        {
        global $mysql;

        if ($this->buffered)
            {
            $query="INSERT INTO ".DB_COOKIES." (name,cookie_value,path,expire) VALUES('".$name."','".$value."','".$path."',".$expire.")";
            $mysql->query($query);
            }
        else
            {
            $this->_setcookie($name,$value,$expire,$path);
            }
        }

    //Ein Cookie entfernen
    function remove($name,$path="/")
        {
        global $mysql;

        if ($this->buffered)
            {
            $query="DELETE FROM ".DB_COOKIES." where name='".$name."'";
            $mysql->query($query);
            }
        else
            {
            $this->_delcookie($name,$path);
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Interne Funktion zur Ausgabe von Cookies
    //zu jedem Cookie wird ein "Identifier" erzeugt, der sicherstellt, daß der
    //User das bei Ihm abgelegte Cookie nicht manipuliert hat
    function _setcookie($name,$value,$expire=0,$path="/")
        {
        //Einmal das normale Cookie setzen
        setcookie($name,$value,$expire,$path);

        //Und einmal das Controlcookie
        setcookie(crypt_create_hash($name),crypt_create_hash($value),$expire,$path);
        }

    //Ein Cookie entfernen
    function _delcookie($name,$path="/")
        {
        //Einmal das normale Cookie setzen
        setcookie($name,"",time()+SECS_PER_WEEK,$path);

        //Und einmal das Controlcookie
        setcookie(crypt_create_hash($name),"",time()+SECS_PER_WEEK,$path);
        }

    //////////////////////////////////////////////////////////////////////////
    //Alle Cookies löschen
    function clear()
        {
        global $mysql;

        return($mysql->query("DELETE FROM ".DB_COOKIES));
        }

    //////////////////////////////////////////////////////////////////////////
    //Alle Cookies in der Datenbank ausgeben
    function flush()
        {
        global $mysql;
        //Alle Cookies holen
        $result=$mysql->query("SELECT * FROM ".DB_COOKIES);
        
        if (is_array($result))
            {
            foreach ($result as $cookie)
                {
                $this->_setcookie($cookie["name"],$cookie["cookie_value"],$cookie["expire"],$cookie["path"]);
                }
            }
        //Und alle rauswerfen
        $this->clear();
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Auf ein Cookie prüfen
    function exists($name)
        {
        return(isset($this->cookies[$name]));
        }

    //////////////////////////////////////////////////////////////////////////
    //Den Inhalt eines Cookies lesen
    function read($name)
        {
        if ($this->exists($name))
            {
            return($this->cookies[$name]);
            }
        else
            {
            return(FALSE);
            }
        }
    }
</script>