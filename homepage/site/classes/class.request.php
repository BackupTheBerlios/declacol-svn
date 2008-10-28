<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Request behandelt alle Datenübertragungen (inclusive cookies)
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Eigentliche Klasse
class request
    {
    var $requests   = array();
    var $filebuffer = array();
    var $cookies    = FALSE;
    
    //Alle zugelassenen Variablennamen
    var $allowed_requests = array( "cmd", "cmdid", "folder", "details", "admin", "name", "selected","id","file","page","rpc",
                                   "data0","data1","data2","data3","data4","data5","data6","data7","data8","data9",
                                 );

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function request()
        {
        //Alle Übertragenen Daten übernehmen
        $this->filebuffer = $_FILES;

        $this->initrequests();
        $this->initcookies();

        //Alle öffentlichen Arrays löschen, um Schindluder zu vermeiden
        $_POST  = array();
        $_GET   = array();
        $_FILE  = array();
        $_COOKIE= array();
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "request";      //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "request";    //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";             //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;              //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;             //Wird eine Datenbank benötigt
        $result[CLASS_INDEX_CLEANUP]   = FALSE;              //Soll die Datenbank initialisiert werden ?
        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;              //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;              //Soll die Datenbank komprimiert werden (gz)
        $result[CLASS_INDEX_RUNLEVEL]  = 1;                //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }

    //Hier können bei der Installation Daten in die Registry geschrieben werden
    function preset(&$registry)
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alle Requests lesen und nur erlaubte zulassen
    function initrequests()
        {
        foreach ($_REQUEST as $name => $value)
            {
            if (in_array($name,$this->allowed_requests)==TRUE)
                {
                $this->requests[$name]=$value;
                }
            }
        }
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Requests verarbeiten
    function getrequest($name,$default,$filtertype)
        {
        $result=$default;
        if (isset($this->requests[$name])==TRUE)
            {
            $result=string_filter ( $this->requests[$name],$filtertype );
            }

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Cookies lesen und nur signierte zulassen
    function initcookies()
        {
        //Und die vorhandenen Cookies einlesen
        if (is_array($_COOKIE))
            {
            foreach ($_COOKIE as $index=>$cookie)
                {
                //Id des Kontrollcookies erzeugen
                $controlid =callmethod("crypt","hash",$index);

                //Gibt es ein ControlCookie ?
                if (isset($_COOKIE[$controlid]))
                    {
                    //Stimmen die Daten ?
                    if ($_COOKIE[$controlid]==callmethod("crypt","hash",$cookie))
                        {
                        //Alles OK, dann merken
                        $this->cookies[$index]=$cookie;
                        }
                    }
                }
            }
        $_COOKIE=array();
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Inhalt eines Cookies lesen
    function getcookie($name,$default)
        {
        if (isset($this->cookies[$name])==TRUE)
            {
            return($this->cookies[$name]);
            }
        else
            {
            return($default);
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein Cookie setzen
    function setcookie($name,$value,$expire=0,$path="/")
        {
        //Einmal das normale Cookie setzen
        setcookie($name,$value,$expire,$path);

        //Und einmal das Controlcookie
        setcookie( callmethod("crypt","hash",$name), callmethod("crypt","hash",$value),$expire,$path);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein Cookie entfernen
    function delcookie($name,$path="/")
        {
        //Einmal das normale Cookie setzen
        $this->setcookie($name,"-",(CURRENT_TIME - 1),$path);
        }
    }
</script>