<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Log Klasse
///
/// Um Logs zu erzeugen.
/// Wird die Klasse mit dem Parameter buffered=TRUE
/// erzeugt, so werden die Log-Eintr‰ge erst mit dem Befehl Flush()
/// geschrieben. Andernfalls wird mit jedem Add() dieser Eintrag
/// automatisch in die Datei gesichert.
/// Im normalen Betrieb ist der bufferedmodus vorzuziehen,
/// da er die geringere Last erzeugt
///
/// Diese klasse benˆtigt die lib.web.php, um IP und Useragent aufzulˆsen
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_LOGGING","class_log");
define ("CLASS_LOGGING_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_LOGGING,"version ".CLASS_LOGGING_VERSION);



//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class logging
    {
    //÷ffentliche Eigenschaftem

    //Private Eigenschaften
    var $internal_logfile;
    var $internal_bufferedlog;
    var $internal_buffer=array();


    //Konstruktor
    function logging($logfile,$buffered=FALSE)
        {
        //Einfach die Variablen speichern
        $this->internal_logfile=$logfile;
        $this->internal_bufferedlog=$buffered;

        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        }

    //Verbindungsaufbau
    //wird hier nicht benutzt
    function open()
        {
        }
        
    //Verbindung schlieﬂen und den Buffer (wenn Buffered ein ist) schreiben
    function close()
        {
        //Evtl. den Buffer schreiben
        $this->flush();

        //Array leeren
        $this->internal_buffer=array();
        }

    //Defaults
    function setdefault()
        {
        //Den internen Puffer initialisieren,
        //wenn wir im gepufferten Modus sind
        if ($this->internal_bufferedlog)
            {
            $this->internal_buffer=array();
            }
        }
    
    //Datenbank erzeugen
    //wird hier nicht benutzt
    function install()
        {
        }
    
    //Datenbank zerstˆren
    //wird hier nicht benutzt
    function uninstall()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Eine Zeile in die Logdatei schreiben
    function _write($line)
        {
        //Datei ˆffnen
        @$fh=fopen($this->internal_logfile,"a+");
        //OK?
        if ($fh!=FALSE)
            {
            //Und Zeile schreiben
            fwrite($fh,trim($line)."\n");

            //Datei schlieﬂen
            fclose($fh);
            }
        }
        
    ////////////////////////////////////////////////////////////////////////////////
    //Eigentliche Logfunktion
    ////////////////////////////////////////////////////////////////////////////////

    //Einen Logeintrag zuf¸gen
    function add($text)
        {
        global $_SERVER;
        global $session;

        //Ein paar Daten des Users holen
        $remote=web_get_ip();
        $agent=web_get_useragent();

        //Eine ID f¸r diese Verbindung machen
        if (is_object($session))
            {
            //Die SessionID mitreinnehmen, wenn es eine gibt
            $id=sprintf("%x",crc32($remote.$agent.$session->id));
            }
        else
            {
            $id=sprintf("%x",crc32($remote.$agent));
            }

        //Zeile zusammenbauen
        $line =date("Y.m.d H:i:s",time());
        $line.="|".$id;
        $line.="|".$remote;
        $line.="|".$agent;
        $line.="|".$text;

        //Sind wir im gepufferten Modus ?
        if ($this->internal_bufferedlog)
            {
            //Dann einfach im Array speichern
            $this->internal_buffer[]=$line;
            }
        else
            {
            //Ansonsten speichern wir es direkt in die Datei
            $this->_write($line);
            }    
        }
        
    ////////////////////////////////////////////////////////////////////////////////
    //Bei Buffered Logging den Logpuffer schreiben
    function flush()
        {
        //Wenn wir im buffered Mode sind, schreiben
        //Wie das Array in die Datei
        if ($this->internal_bufferedlog)
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
                    fwrite($fh,trim($line)."\n");
                    }
            //Datei schlieﬂen
            fclose($fh);
                }
            }
        }
    }

</script>