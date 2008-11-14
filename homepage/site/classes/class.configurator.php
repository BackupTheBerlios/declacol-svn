<script language="PHP">
/*
 _|    _|            _|                              _|                _|
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|

(c) 2008 Borg@sven-of-nine.de
*/
//////////////////////////////////////////////////////////////////////////
///
/// Configurator Klasse
///
/// Hilfsklasse zur Erstellung und Manipulation von Config-Dateien in PHP
///
/// Die Klasse l‰dt eine Datei und extrahiert daraus alle Defines, die
/// danach im Array $this->config zur Verf¸gung stehen.
/// Diese lassen sich beliebig ¸ber die vorgegebenen Methoden
/// manipulieren. Mit einem $this->flush wird die Konfiguration wieder
/// geschrieben. ‹bergibt man Flush einen Dateinamen, so werden die
/// Daten in eine neue Datei geschrieben
///
//////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("CONFIG_SCRIPT_START","<script language=\x22php\x22>");
define ("CONFIG_SCRIPT_END"  ,"<script>");

if (!defined("LINEFEED"))
    {
    define ("LINEFEED","\r\n");
    }

//Wo stehen die Kommentare
define ("CONFIG_PADDING"    ,80);

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class configurator
    {
    //Public
    var $config                =array();

    //Protected
    var $internal_filename    =FALSE;
    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function configurator()
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
    function open($filename)
        {
        $result=FALSE;

        //Fehler abfangen
        if (!file_exists($filename))
            {
            return($result);
            }

        //Datei ˆffnen
        $fp=fopen($filename,"rb");
        //Handle OK ?
        if ($fp!==FALSE)
            {
            //Name merken
            $this->internal_filename=$filename;

            //Nach vorne gehen
            fseek($fp,0,0);
            //Dann alle Zeilen lesen
            while (!feof($fp))
                {
                //Zeile holen
                $line=fgets($fp);

                //Was gelesen
                if ($line!==FALSE)
                    {
                    //Mit Kommentar
                    //define ("FOO","bar") //Foo=bar
                    if (preg_match("/^define[ \t]*\([ \t]*\"([a-zA-Z0-9_]*)\"[ \t]*,[ \t]*(.*)[ \t]*\);[ \t]*#[#]*(.*)/",$line,$result)==1)
                        {

                        //Den String auf seinen Type analysieren und entsprechend typisiert zur¸ckgeben
                        $result[2]=$this->_retype($result[2]);

                        //Und den Wert abspeichern
                        $this->config[$result[1]]["value"]  =$result[2];
                        $this->config[$result[1]]["comment"]=trim($result[3]);
                        }
                    else
                        {
                        //Ohne Kommentar ?
                        //define ("FOO","bar");
                        if (preg_match("/^define[ \t]*\([ \t]*\"([a-zA-Z0-9_]*)\"[ \t]*,[ \t]*(.*)\);/",$line,$result)==1)
                            {
                            //Den String auf seinen Type analysieren und entsprechend typisiert zur¸ckgeben
                            $result[2]=$this->_retype($result[2]);

                            //Und den Wert abspeichern
                            $this->config[$result[1]]["value"]  =$result[2];
                            $this->config[$result[1]]["comment"]="";
                            }
                        }
                    }
                }
            fclose($fp);
            }
        else
            {
            echo "unable to open file";
            }
        return ($result);
        }

    //Die Konfigurationsdatei schreiben
    function flush($filename="")
        {
        //Wenn kein Name ¸bergeben wurde, dann den der geˆffneten Datei nehmen
        if ($filename=="")
            {
            $filename=$this->internal_filename;
            }

        $fp=fopen($filename,"w+");
        if ($fp!==FALSE)
            {
            //Kopf
            fwrite($fp,"<script language=\x22php\x22>".LINEFEED);
            foreach ($this->config as $constant => $value)
                {
                //Eine Zeile bauen
                $line=$this->_build($constant,$value["value"],$value["comment"]);
                //Und schreiben
                fwrite($fp,$line.LINEFEED);
                }
            //Fuﬂ
            fwrite($fp,"</script>".LINEFEED);
            fclose($fp);
            }
        }

    //Verbindungsabbau
    function close()
        {
        $this->setdefault();
        }

    //Defaults
    function setdefault()
        {
        $this->internal_filename        =FALSE;
        }

    //Datenbank erzeugen
    //Wird hier nicht benutzt
    function install()
        {
        return(TRUE);
        }

    //Datenbank zerstˆren
    //Wird hier nicht benutzt
    function uninstall()
        {
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Einen Konstantenausdruck bauen
    function _build ($constant,$value,$comment="")
        {
        //Je nach Var-Type Anf¸hrungsstriche oder nicht
        $const= "define (";
        $const.=str_pad("\x22".$constant."\x22,",32," ",STR_PAD_RIGHT);

        //Eine Zahl ?
        switch (gettype($value))
            {
            case ("integer")    :    $line=$const.$value.");";            break;
            case ("boolean")    :    if ($value==TRUE)
                                        {
                                        $line=$const."TRUE);";
                                        }
                                    else
                                        {
                                        $line=$const."FALSE);";
                                        }
                                    break;
            //Vorgabe ist mit Anf¸hrungsstrichen
            default :                $line=$const."\x22".$value."\x22);";    break;
            }

        //Auf die vorgegebene L‰nge padden
        //Und einen Kommentar anh‰ngen
        if ($comment!="")
            {
            $line=str_pad($line,CONFIG_PADDING," ",STR_PAD_RIGHT)."#".$comment;
            }

        return($line);
        }

    //Einen String auf seinen Typ analysieren und entsprechend zur¸ckgeben
    function _retype($input)
        {
        //In Anf¸hrungsstrichen ist ein String
        $result=array();
        if (preg_match ("/\"(.*)\"/",$input,$result)==1)
            {
            //Den String ohne Anf¸hrungszeichen zur¸ckgeben
            return($result[1]);
            }

        //Keine Anf¸hrungszeichen bleiben eigentlich nur noch Zahlen und Bools ¸brig

        //Bool ?
        $temp=strtolower($input);
        if (strpos($temp,"false")!==FALSE)
            {
            return (FALSE);
            }
        if (strpos($temp,"true")!==FALSE)
            {
            return (TRUE);
            }

        //Nix zugeordnet dann eine Zahl
        return((integer)$input);
        }

    //Nach einer Konstante in der Config suchen
    //Setzt Value auf den Konstantenwert (wenn Sie gefunden wird) und gibt entweder FALSE
    //oder den Konstantenindex zur¸ck
    function _find ($constant,&$value)
        {
        //Gibt es die Konstante ¸berhaupt ?
        if (!isset($this->config[$constant]))
            {
            //Dann schon hier abbrechen
            $result=FALSE;
            }
        else
            {
            //Wert holen
            $value=$this->config[$constant]["value"];
            $result=TRUE;
            }
        return($result);
        }


    //Einen Konstanten-Eintrag zuf¸gen
    function add($constant,$value,$comment="")
        {
        //Wert speichern
        $this->config[$constant]["value"]    =$value;
        //Kommentar speichern
        $this->config[$constant]["comment"]    =$comment;
        return (TRUE);
        }

    //Alle Eintr‰ge auflisten
    function enumerate()
        {
        return(array_keys($this->config));
        }

    //Den Wert einer Konstante holen und bei Erfolg TRUE ansonsten FALSE melden
    function read($constant,&$value)
        {
        //Einfach das interne FIND kapseln
        return($this->_find($constant,$value));
        }
    }
</script>