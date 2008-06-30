<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Viewer Klasse
///
/// zur Anzeige diverser Dateiformate
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_VIEWER","class_viewer");
define ("CLASS_VIEWER_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_VIEWER,"version ".CLASS_VIEWER_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("FILETYPE_TXT"        ,1);
define ("FILETYPE_ZIP"        ,2);
define ("FILETYPE_PDF"        ,3);



//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class viewer
    {
    //Erlaubte Dateitypen
    var $allowed=array("txt","log","zip");

    //Zu benutzender CSS-Style (Wenn die HTML-Klasse verfügbar ist)
    var $style="";

    //Das für die aktuelle Datei vorgesehene Anzeigemodul
    var $internal_module=FALSE;
    var $internal_file  =FALSE;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function viewer()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        }

    //Auf Fehler checken und 
    function open($filename)
        {
        $result=FALSE;
        //Prüfen, was für eine Datei das ist
        $this->setdefault();
        if (file_exists($filename))
            {
            //Anhand der Dateiendung den Typ angeben
            switch (string_extractfileext(strtolower($filename)))
                {
                //ZIP ?
                case ("zip")    :    $this->internal_module=FILETYPE_ZIP;    break;

                //Wenn nix gefunden wird, immer als Text ausgeben
                default         :    $this->internal_module=FILETYPE_TXT;    break;
                }

            //Dateinamen merken
            //und in kanonischen umwandeln
            $this->internal_file=realpath($filename);
            $result=TRUE;
            }

        return($result);
        }
        
    //Verbindungsabbau
    function close()
        {
        }

    //Defaults setzen
    function setdefault()
        {
        $this->allowed=array("txt","log","zip");
        $this->internal_module=FALSE;
        $this->internal_file  =FALSE;
        }
    
    //Datenbank erzeugen
    //Wird hier nicht benötigt
    function install()
        {
        return(TRUE);
        }
    
    //Datenbank zerstören
    //Wird hier nicht benötigt
    function uninstall()
        {
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    //Ausgabe der Datei initiieren
    function flush()    
        {
        //Über den Dateityp das modul auswählen
        switch ($this->internal_module)
            {
            case (FILETYPE_TXT)    : $this->show_text($this->internal_file);    break;
            case (FILETYPE_ZIP)    : $this->show_zip ($this->internal_file);    break;
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine Textdatei ausgeben
    function show_text($filename)
        {
        //Datei in ein Array ziehen
        $filedata=file($filename);

        //Was geladen ?
        if (is_array($filedata))
            {
            //Dann ausgeben
            $this->_show_text($filedata);
            }

        //Speicher freigeben
        unset($filedata);
        }    

    //Die Anzeige eines Textarrays
    function _show_text(&$filedata)
        {
        global $html;

        //Templateengine verfügbar ?
        if (is_object($html))
            {
            $this->_show_text_html($filedata);
            }
        else
            {
            $this->_show_text_raw($filedata);
            }
        }

    //Textausgabe ohne HTML-Templates
    function _show_text_raw(&$filedata)
        {
        foreach ($filedata as $line)
            {
            //Zeilen cleanen und ausgeben
            echo htmlentities(trim($line));
            }
        }

    //Textausgabe mit HTML-Templates
    function _show_text_html(&$filedata)
        {
        global $html;


        //Dann Zeile für Zeile ausgeben
        $html->table_open("",$this->style);

        foreach ($filedata as $line)
            {
            $html->row_open("");

            //Zeilen cleanen
            $line=htmlentities(trim($line));

            //Trennzeichen ?
            $cells=explode("|",$line);

            //Alles ausgeben
            foreach($cells as $cell)
                {
                $html->cell($cell);
                }
            $html->row_close();
            }
        $html->table_close();
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine ZipDatei ausgeben
    function show_zip($filename)
        {
        //Zip-Modul verfügbar ?
        if (!function_exists("zip_open"))
            {
            $this->_show_text(array("no zip functions available install php_zip.so"));
            return(FALSE);
            }

        //Zip-Datei öffnen
        $zip = @zip_open($filename);

        //Überschriften
        $output=array("name|filesize|compressed size|method");

        //Und los        
        if ($zip!=FALSE)
            {
            while ($zip_entry = zip_read($zip))
                {
                //Eine Zeile zusammensetzen
                $output[]=zip_entry_name($zip_entry)."|".zip_entry_filesize($zip_entry)."|".zip_entry_compressedsize($zip_entry)."|".zip_entry_compressionmethod($zip_entry);

/*                //Inhalt lesen
                if (zip_entry_open($zip, $zip_entry, "r"))
                    {
                    $buf = zip_entry_read($zip_entry, zip_entry_filesize($zip_entry));
                    echo "$buf\n";
                    zip_entry_close($zip_entry);
                    }
*/                }
               zip_close($zip);
            }
        //Über das Textmodul ausgeben
        $this->_show_text($output);
        unset($output);
        return(TRUE);
        }
    }
</script>