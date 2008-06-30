<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Debugklasse
///
/// bietet eine einfache Mˆglichkeit, um Debug-Informationen unter PHP
/// zu speichern. Die Debug-Datei kann entweder angezeigt oder
/// direkt abgespeichert werden.
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_DEBUG","class_group");
define ("CLASS_DEBUG_VERSION","0.2");
if (isset($debug)) $debug->add(CLASS_DEBUG,"version ".CLASS_DEBUG_VERSION);

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class debug
    {
    //÷ffentliche Eigenschaftem

    //Private Eigenschaften
    var $internal_buffer=array();


    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function debug()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        }

    //Verbindung ˆffnen
    //Wird hier nicht benˆtigt
    function open()
        {
        return(TRUE);
        }

    //Verbindung schlieﬂen
    //Wird hier nicht benˆtigt
    function close()
        {
        return(TRUE);
        }

    //Defaultwerte setzen
    //Und Debug-Kopf schreiben
    function setdefault()
        {
        //Den Puffer initialisieren
        $this->internal_buffer=array();

        //Und als Erstes die Startzeit ausgeben
        $this->add("class_debug","created ".date("H:i:s d.m.Y",time()));
        $this->add("class_debug","version ".CLASS_DEBUG_VERSION);
        }

    //Datenbank erzeugen
    //Wird hier nicht benˆtigt
    function install()
        {
        return(TRUE);
        }

    //Datenbank zerstˆren
    //Wird hier nicht benˆtigt
    function uninstall()
        {
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    //Einen Debugeintrag hinzuf¸gen
    //Modul ist dabei der KLassen oder Bibliotheksnamen
    //Text der zu speichernde Text
    function add($modul,$text)
        {
        //Dann einfach im Array speichern
        if (DEBUG) $this->internal_buffer[$modul][]=$text;
        }

    ////////////////////////////////////////////////////////////////////////////////
    //Den Logpuffer als HTML ausgeben
    function flush()
        {
        global $session;
        
        echo "<pre>";
        if (isset($session))
            {
            print_r($session);
            }
        
        print_r($this->internal_buffer);
        echo "</pre>";
        }
    ////////////////////////////////////////////////////////////////////////////////
    //Den Logpuffer in die Datei $filename schreiben
    function save()
        {
        //Datei ˆffnen
        @$fh=fopen($this->internal_logfile,"a+");
        //OK?
        if ($fh!=FALSE)
            {
            //Alle Zeilen abspeichern
            foreach ($this->internal_buffer as $line)
                {
                //Und Zeile schreiben
                fwriteln(trim($line));
                }
            //Datei schlieﬂen
            fclose($fh);
            }
        }
    }
</script>