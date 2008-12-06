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
/// Die Klasse lädt eine Datei und extrahiert daraus alle Defines, die
/// danach im Array $this->config zur Verfügung stehen.
/// Diese lassen sich beliebig über die vorgegebenen Methoden
/// manipulieren. Mit einem $this->flush wird die Konfiguration wieder
/// geschrieben. Übergibt man Flush einen Dateinamen, so werden die
/// Daten in eine neue Datei geschrieben
///
//////////////////////////////////////////////////////////////////////////
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

        //Datei öffnen
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

                        //Den String auf seinen Type analysieren und entsprechend typisiert zurückgeben
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
                            //Den String auf seinen Type analysieren und entsprechend typisiert zurückgeben
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
        //Wenn kein Name übergeben wurde, dann den der geöffneten Datei nehmen
        if ($filename=="")
            {
            $filename=$this->internal_filename;
            }

        $output="<script language=\x22php\x22>".LINEFEED;
        foreach ($this->config as $constant => $value)
          {
          //Eine Zeile bauen
          $output.=$this->_build($constant,$value["value"],$value["comment"]).LINEFEED;
          }
        $output.="</script>";
        file_put_contents($filename,$output);
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

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Einen Konstantenausdruck bauen
    function _build ($constant,$value,$comment="")
        {
        //Je nach Var-Type Anführungsstriche oder nicht
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
            //Vorgabe ist mit Anführungsstrichen
            default :                $line=$const."\x22".$value."\x22);";    break;
            }

        //Auf die vorgegebene Länge padden
        //Und einen Kommentar anhängen
        if ($comment!="")
            {
            $line=str_pad($line,CONFIG_PADDING," ",STR_PAD_RIGHT)."#".$comment;
            }

        return($line);
        }

    //Einen String auf seinen Typ analysieren und entsprechend zurückgeben
    function _retype($input)
        {
        //In Anführungsstrichen ist ein String
        $result=array();
        if (preg_match ("/\"(.*)\"/",$input,$result)==1)
            {
            //Den String ohne Anführungszeichen zurückgeben
            return($result[1]);
            }

        //Keine Anführungszeichen bleiben eigentlich nur noch Zahlen und Bools übrig

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
    //oder den Konstantenindex zurück
    function _find ($constant,&$value)
        {
        //Gibt es die Konstante überhaupt ?
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


    //Einen Konstanten-Eintrag zufügen
    function add($constant,$value,$comment="")
        {
        //Wert speichern
        $this->config[$constant]["value"]    =$value;
        //Kommentar speichern
        $this->config[$constant]["comment"]    =$comment;
        return (TRUE);
        }

    //Alle Einträge auflisten
    function enumerate()
        {
        return(array_keys($this->config));
        }

    //Den Wert oder Default zurückgeben
    function read($constant,$default)
        {
        //Einfach das interne FIND kapseln
        $result=FALSE;
        if ( $this->_find($constant,$result)==FALSE )
          {
          $result=$default;
          }
        }
    }
</script>