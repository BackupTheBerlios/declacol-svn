<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Ini Klasse
///
/// Zum einfachen Zugriff auf Ini-Dateien
/// Die Funktionalität beschränkt sich auf die Standardaufgaben einer
/// Ini-Datei. Kommentaren werden beim speichern verworfen.
///
///
//////////////////////////////////////////////////////////////////////////
require_once(DIR_CLASS."class.profiler.php");

//Versioninfo speichern
define ("CLASS_INI","class_ini");
define ("CLASS_INI_VERSION","0.02");
if (isset($debug)) $debug->add(CLASS_INI,"version ".CLASS_INI_VERSION);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class ini
    {
    //Öffentliche Eigenschaften
    var $ini                =array();
    var $path               ="";
    //Jede Ini erzeugt für sich selbst eine eindeutige ID
    var $id                 =FALSE;
    
    //Ein Array mit Zeilen, welches an den Kopf der Datei gesetzt wird.
    //Kann z.B. Kommentare etc. enthalten
    var $header             =array();
    
    //Sectionsnamen automatisch in Großbuchstaben konvertieren ?
    var $autoconvert        = TRUE;

    //Private Eigenschaften
    var $internal_name        ="";


    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function ini()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        unset($this);
        }

    //Laden der Ini-Datei
    function open($filename)
        {
        $result=FALSE;

        //Alte Ini löschen
        $this->close();

        //Datei öffnen
        if (file_exists($filename)==TRUE)
            {
            //Und direkt einlesen
            $result=$this->_parse_ini($filename);

            //Wenn alles OK ist,
            //dann merken wir uns den Dateinamen
            $this->internal_name=$filename;
            $this->path=$filename;
            //Und machen eine ID
            $this->id=sha1($this->path);
            }
        return($result);
        }

    //Verbindung schließen
    function close()
        {
        unset($this->ini);
        $this->setdefault();
        }

    //Defaults
    function setdefault()
        {
        $this->ini=array();
        $this->internal_name=FALSE;
        $this->path=FALSE;
        }

    //Datenbank erzeugen
    //Wird hier nicht benötigt
    function install()
        {
        }

    //Datenbank zerstören
    //Wird hier nicht benötigt
    function uninstall()
        {
        }

    //Die Ini-Daten in eine Datei schreiben. Wird kein Datenname übergeben,
    //so wird der Name der geöffneten Ini benutzt
    function flush($filename="")
        {
        //Wenn kein Dateiname übergeben wurde, dann benutzen wir den alten
        if ($filename=="")
            {
            $filename=$this->internal_name;
            }

        //Datei öffnen
        $fp=fopen($filename,"w+");
        if ($fp!==FALSE)
            {
            //Header
            if (is_array($this->header))
                {
                foreach ($this->header as $outline)
                    {
                    fwrite($fp,$outline."\r\n");
                    }
                }

            //Und alles schreiben
            foreach ($this->ini as $sectionname => $section)
                {
                //Die Section
                fwrite($fp,"[".$sectionname."]\r\n");

                //Die Werte
                foreach ($section as $key => $value)
                    {
                    //Der Key
                    fwrite($fp,$key."=".$value."\r\n");
                    }
                //Leerzeile
                fwrite($fp,"\r\n");
                }
            fclose($fp);
            }
        }


    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////


    //Eine Ini-Datei laden und in ein assoziatives Array parsen
    function _parse_ini($filename)
        {
        //Fehler annehmen
        $result=FALSE;

        //Datei laden
        $input=file($filename);

        //Standardsektion vorgeben
        $section="UNKNOWN";

        //Nun gehen wir einfach alle Zeilen durch und analysieren sie
        foreach ($input as $line)
            {
            //Cleanen
            $line=trim($line);

            //Zeile enthält Daten ?
            if (!$this->_is_comment($line))
                {
                //Wenn die Zeile Daten enthält, ist eine Datei geladen
                //Und damit alles gut gegangen. Ob Daten in der Ini sind,
                //Ist nicht unser Problem
                $result=TRUE;

                //Sind wir am Beginn einer neuen Sektion
                if ( ($newsection=trim($this->_is_section($line)))!=FALSE )
                    {
                    $section=($this->autoconvert==TRUE?trim(strtoupper($newsection)):trim($newsection));
                    }
                else
                    {
                    //Am Gleichheitszeichen zerlegen
                    $pos=strpos($line,"=");
                    if ($pos!==FALSE)
                        {
                        $left =substr($line,0,$pos);
                        $right=substr($line,$pos+1,strlen($line));

                        //Evlt. den Keynamen konvertieren, wenn es gewünscht ist
                        if ($this->autoconvert==TRUE) $left=strtolower($left);
                        $this->ini[$section][$left]=$right;
                        }
                    }
                }
            }
        return($result);
        }
        
    //Die Ini nach Sektionen sortieren
    function sort()
        {
        ksort($this->ini);
        }

    //Einen Wert zufügen oder überschreiben
    function write_value($section,$key,$value)
        {
        //Sektion werden konvertiert wenn es gewünscht ist
        if ($this->autoconvert==TRUE) $section=strtoupper($section);

        $this->ini[$section][$key]=$value;
        }

    //Einen Wert lesen. Wird der Wert nicht gefunden, so wird $default zurückgegeben
    function read_value($section,$key,$default)
        {
        //Sektion und Keys werden konvertiert wenn es gewünscht ist
        if ($this->autoconvert==TRUE)
            {
            $section=strtoupper($section);
            $key    =strtolower($key);
            }

        if (isset($this->ini[$section][$key]))
            {
            return($this->ini[$section][$key]);
            }
        else
            {
            return($default);
            }
        }

    //Alle Werte einer Sektion lesen
    //Rückgabe als Array oder FALSE
    function read_values($section)
        {
        //Sektion werden evtl. konvertiert
        if ($this->autoconvert==TRUE) $section=strtoupper($section);

        if (isset($this->ini[$section]))
            {
            return($this->ini[$section]);
            }
        else
            {
            return(FALSE);
            }
        }

    //Alle Keys einer Sektion lesen
    //Rückgabe als Array oder FALSE
    function read_keys($section)
        {
        //Sektion werden evtl. konvertiert
        if ($this->autoconvert==TRUE) $section=strtoupper($section);

        if (isset($this->ini[$section]))
            {
            return(array_keys($this->ini[$section]));
            }
        else
            {
            return(FALSE);
            }
        }

    //Alle Sektionen lesen
    //Rückgabe als Array oder FALSE
    function read_sections()
        {
        return(array_keys($this->ini));
        }

    //Prüfen, ob eine Section existiert (TRUE/FALSE)
    function section_exists($section)
        {
        //Sektion werden evtl. konvertiert
        if ($this->autoconvert==TRUE) $section=strtoupper($section);

        return (isset($this->ini[$section]));
        }

    //Prüfen, ob ein Key existiert (TRUE/FALSE)
    function key_exists($section,$key)
        {
        //Sektion werden evtl. konvertiert
        if ($this->autoconvert==TRUE)
            {
            $section=strtoupper($section);
            $key    =strtolower($key);
            }

        return (isset($this->ini[$section][$key]));
        }

    //Einen Wert entfernen
    function remove_value($section,$key)
        {
        //Sektion werden evtl. konvertiert
        if ($this->autoconvert==TRUE)
            {
            $section=strtoupper($section);
            $key    =strtolower($key);
            }

        if (isset($this->ini[$section][$key]))
            {
            unset($this->ini[$section][$key]);
            }
        }

    //Eine Sektion entfernen
    function remove_section($section)
        {
        //Sektion werden evtl. konvertiert
        if ($this->autoconvert==TRUE) $section=strtoupper($section);

        if (isset($this->ini[$section]))
            {
            unset($this->ini[$section]);
            }
        }


    //////////////////////////////////////////////////////////////////////////
    /// Hilfsfunktionen
    //////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////

    //Prüfen, ob die Zeile ein Kommentar ist
    function _is_comment($line)
        {
        //Einfach mit einem Regulären Ausdruck checken
        if (preg_match("/^[;#]]/",$line,$result)!=FALSE)
            {
            return(TRUE);
            }
        //Und leere Zeilen nehmen wir auch gleich raus
        if ($line=="")
            {
            return(TRUE);
            }
        return(FALSE);
        }

    //Prüfen, ob die Zeile eine Section einleitet
    function _is_section($line)
        {
        //Einfach mit einem Regulären Ausdruck checken
        $result=array();
        if (preg_match("#\[([\w\W]+?)\]#",$line,$result)!=FALSE)
            {
            //Und den Namen extrahieren
            return($result[1]);
            }
        return(FALSE);
        }
    }
</script>