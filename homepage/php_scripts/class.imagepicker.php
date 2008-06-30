<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Imageselect Klasse
/// Erzeugt eine Tabelle mit Thumbnails die angeklickt werden können
/// Als POST-Variable wird $_POST["select"] mit dem Dateipfad gesetzt
///
/// Der Picker funktioniert nur mit Browsern, die Forms kpl. unterstützen
/// FF Opera etc. (Nicht IE6);
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_IMAGEPICKER","class_imagepicker");
define ("CLASS_IMAGEPICKER_VERSION","0.2");
if (isset($debug)) $debug->add(CLASS_IMAGEPICKER,"version ".CLASS_IMAGEPICKER_VERSION);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class imagepicker
    {
    //Öffentliche Eigenschaftem
    var $_allowed_images=array("jpg","png","gif");

    //Private Eigenschaften
    var $internal_path=FALSE;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function imagepicker()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        }

    //Öffnen des Dateipfades
    //Es wird noch nichts dargestellt
    function open($path)
        {
        global $debug;

        //Ist der Pfad ein Verzeichnis
        if (!is_dir($path))
            {
            $debug->add(CLASS_IMAGEPICKER,"unable to open ".$path);
            $this->internal_path=FALSE;
            return(FALSE);
            }

        //Den Pfad merken
        $this->internal_path=$path;
        return(TRUE);
        }

    //Destruktor
    function close()
        {
        }

    //Defaults
    function setdefault()
        {
        $this->internal_path=FALSE;
        }

    //Datenbank erzeugen
    //Wird hier nicht benutzt
    function install()
        {
        }

    //Datenbank zerstören
    //Wird hier nicht benutzt
    function uninstall()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Eine Liste aller Grafiken zurückgeben
    //Es wird nichts ausgegeben sondern nur ein Array mit allen
    //Grafik-Pfaden zurückgegeben
    function enumerate()
        {
        global $debug;

        if ($this->internal_path==FALSE)
            {
            $debug->add(CLASS_IMAGEPICKER,"unable to open ".$this->internal_path);
            return(FALSE);
            }

        //Und nun einfach alle Grafiken durchgehen
        $files=file_scan($this->internal_path,$this->_allowed_images,TRUE,FALSE,FALSE);

        //Und zurückgeben
        return($files);
        }

    //Eine Grafikübersicht erzeugen (Als Tabelle)
    //Colcount ist die Anzahl der Spalten. Die Zeilenzahl wird automatisch berechnet
    function write($style,$colcount)
        {
        global $html;


        //HTML-Objekt geladen ?
        if (!is_object($html))
            {
            return(FALSE);
            }

        //Unsere Grafiken holen
        $files=$this->enumerate();
        //Fehler abfangen
        if ($files===FALSE)
            {
            return(FALSE);
            }

        //Ein Hidden Tag einschreiben, um per Javascript schlechte Browser kompatible zu machen
        $html->data_hidden("selected","?");

        //Ausrechnen wir groß unsere Tabelle sein muß
        $count=count($files);
        //Auf den passenden Teiler bringen
        $rest=(round($count/$colcount)*$colcount) + $colcount;
        //Und die Zeilenzahl bestimmen
        $rowcount=$rest/$colcount;

        //Alles über die Template-Engine
        $html->table_open("","");
        $cell=reset($files);
        $counter=0;
        //Alle Zeilen
        for ($rows=0; $rows < $rowcount; $rows++)
            {
            //Zeile öffnen
            $html->row_open("nohover");
            for ($cols=0; $cols < $colcount; $cols++)
                {
                //Zelle öffnen
                $html->cell_open("");
                //Daten im Array gefunden ?
                if ($cell!==FALSE)
                    {
                    //Dateinamen gegen URL tauschen
                    $cell=str_replace(DIR_BASE,URL_BASE,$cell);

                    //Dann ausgeben
                    $html->button_image($cell,"a".$counter,base64_encode($cell),$style);

                    //Und nächstes Datum anvisieren
                    $cell=next($files);
                    }
                //Zelle schließen
                $html->cell_close($style);
                $counter++;
                }
            //Zeile schließen
            $html->row_close($style);
            }
        //Tabelle schließen
        $html->table_close();
        return (FALSE);
        }
    }

</script>