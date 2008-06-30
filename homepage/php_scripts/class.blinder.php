<script language="PHP">
//////////////////////////////////////////////////////////////////////////////
/// Unimatrix
/// Die Skinning und Template-Klasse
///
/// folgende Schl�sselw�rter sind vordefiniert und k�nnen nicht �berschrieben
/// werden
/// %unimatrix_version% => Version der Klasse
/// %unimatrix_time%    => Laufzeit von der Erzeugung bis zur Ausgabe in Sekunden
/// %unimatrix_file%    => zur Ausgabe benutzter Templatefile
/// %unimatrix_keys%    => alle Schl�sselw�rter zum Zeitpunkt der Ausgabe
///
/// Spezialtags
/// %include:filename%
/// f�gt die Datei filename an der entsprechenden Stelle ein
/// Includetags m�ssen mit ->filter_include zugelassen werden
/// URL-Includes m�ssen mit ->url_includes=TRUE zugelassen werden
///
///
///
/// Die Ausgabe kann gefiltert werden, indem die entsprechenden Flags gesetzt sind
/// ->filter_url_replace   =TRUE  ersetzt URLs durch einen Link
/// ->filter_flat_tags     =TRUE  entfernt Zeilenumbr�che hinter TAGs
/// ->filter_strip_comments=TRUE  entfernt HTML-Kommentare
/// ->filter_trim          =TRUE  trim die Ausgabe (Achtung bei PRE Tags)
/// ->filter_include       =TRUE  Schaltet den Spezialtag %include:filename% ein
///
//////////////////////////////////////////////////////////////////////////////



//BEISPIEL :
/*
//Template erzeugen
$template=new unimatrix();

//Vorlagendatei angeben
$template->template="tpl.css-main.php";

//Alle Schl�sselw�rter extrahieren und in das array $template->replace[schl�sselwort]=>Ersetzung parsen
//Ist nicht unbeding n�tig, erleichtert aber den �berblick und die Fehlersuche
//Dabei wird gleichzeitig die Vorlagendatei geladen und gepuffert.
$template->extract();

//Ein eigenes Schl�sselwort zuf�gen
//Ersetzt %dogosch% in der Vorlage durch den Text "dogoschreplace"
//Es k�nnen auch mehrere Ersetzungen als Array �bergeben werden.
// $template->add(array(keyword=>replace,....),"");
$template->add("dogosch","dogoschreplace");

//Direkte Addressierung der Schl�sselw�rter
$template->replace["page_title"]="Seitentitel";
$template->replace["plugins_left"]="Linke PluginBox";
$template->replace["plugins_right"]="Rechte PluginBox";

$template->replace["module_title"]="Titel des Moduls";
$template->replace["module_contents"]="Die ist der Inhalt des Moduls";
$template->replace["module_bottom"]="Fu� des Moduls";

$template->replace["page_css"]="/_templates/skins/default/style.css";

//Ausgeben
$template->flush();
//print_r($template);
*/

//////////////////////////////////////////////////////////////////////////////
class unimatrix
    {
    //Name der HTML-Head-Datei
    var $head          ="";

    //Name der Vorlagendatei
    var $template      ="";

    //Pfad zu den Vorlagendateien
    var $template_path ="";
    
    //Sollen die Felder ein Formular dargestellt werden
    var $asform        =FALSE;
    
    //Array mit dem Replacement der Inhalte
    //Der Index ist das Keyword und der Wert die Ersetzung
    var $replace=array();

    //Fehler ausgeben ?
    var $quiet                = FALSE;
    
    //Auch includes �ber URLs zulassen ?
    var $url_includes         = TRUE;

    //Flags f�r einzelne Filtermodule
    var $filter_url_replace   = TRUE;      //alle URLs durch Links ersetzen
    var $filter_flat_tags     = FALSE;     //Zeilenumr�che hinter Tags entfernen
    var $filter_trim          = TRUE;      //Alles Zeilen trimmen
    var $filter_strip_comment = TRUE;      //Kommentare entfernen
    var $filter_include       = TRUE;      //Erlaubt / Verbietet einf�gen externer Dateien

    //Interne Variablen
    
    //Puffer f�r die Vorlage
    var $internal_buffer ="";
    //Ausgabepuffer
    var $internal_output ="";

    //Runtime
    var $internal_runtime=0;

    //Unsere Version
    var $internal_version=0.3;

    //////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function unimatrix()
        {
        $this->internal_runtime=time();
        $this->replace=array();
        $this->internal_buffer ="";
        }

    //////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->template        ="";
        $this->template_path   ="";
        $this->replace=array();
        $this->internal_buffer ="";
        $this->internal_output ="";
        }

    //////////////////////////////////////////////////////////////////////////////
    //Ausgabe
    function flush($direct=TRUE)
        {
        //Alles parsen
        $this->parse();
        //Und einfach ausgeben
        if ($direct)
            {
            echo $this->out();
            return("");
            }
        else
            {
            return($this->out());
            }
        }
    
    //////////////////////////////////////////////////////////////////////////////
    //Die Templatedatei laden
    function read()
        {
        $tpl="";
        //Pfad bauen
        $myfile=$this->template_path . $this->template;
        //Und los
        if (file_exists($myfile))
            {
            //Den Kopf lesen
            $head=$this->template_path.$this->head;
            if (is_file($head))
                {
                $tpl=file_get_contents($head);
                }

            //Kpl. Datei lesen
            $tpl.=file_get_contents($myfile);
            }
        //Ver�ffentlichen
        $this->internal_buffer=$tpl;
        unset ($tpl);
        }

    //////////////////////////////////////////////////////////////////////////////
    //Den Puffer l�schen
    function clear()
        {
        $this->internal_buffer="";
        }

    //////////////////////////////////////////////////////////////////////////////
    //Die Templatedatei laden und alle Schl�sselw�rter extrahieren
    function extract()
        {
        $this->clear();
        $this->read();
        
        $result=array();
        if (preg_match_all("/\\%([^\\%\\s]+?)\\%/",$this->internal_buffer,$result)>0)
            {
            //Doppelte Raus
            $result=array_unique($result[1]);
            //Inhalte l�schen und Namen als Key setzen
            $entry=reset($result);
            while ($entry !==FALSE)
                {
                $this->add($entry,"");
                $entry=next($result);
                }
            }
        }

    //////////////////////////////////////////////////////////////////////////////
    //Dazu wird die Templatedatei geladen und alles in den Keyword/Value-Arrays ersetzt
    //Interne Funktion um die Patterns aufzuarbeiten
    function _parse(&$item,$key)
        {
        $item="�%".$item."%�";
        }
        
    function _parse_asform(&$item,$key)
        {
        //if ($item!="")
//            {
            $item="<input name=\x22".$key."\x22 value=\x22".$item."\x22 size=\x22".(strlen($item)+4)."\x22/>";
//            }
        }
        
    //Alle vordefinierten Schl�sselw�rter setzen
    function _setunimatrix()
        {
        //Runtime mitbestimmen
        $this->add("unimatrix_time",CURRENT_DATE-$this->internal_runtime);

        //Version erzwingen
        $this->add("unimatrix_version",$this->internal_version);

        //Templatedatei erzwingen
        $this->add("unimatrix_file",$this->template);
        
        //Alle Schl�sselw�rter
        $this->add("unimatrix_keys","void");
        ksort($this->replace);
        $this->add("unimatrix_keys","</br>\n".implode("</br>\n",array_keys($this->replace)));
        }

    //Die eigentliche Parserfunktion
    function parse()
        {
        $result=FALSE;
        //Wenn es noch keinen internen Inhalt gibt, dann Template laden
        if ($this->internal_buffer=="")
            {
            $this->read();
            }

        //Haben wir was ?
        if ($this->internal_buffer!="")
            {
            $tpl=$this->internal_buffer;

            //Vordefinierte Schl�sselw�rter setzen
            $this->_setunimatrix();

            //Externe Dateien nachladen ?
            if ($this->filter_include)       $tpl = $this->_filter_include      ($tpl);

            //Die Patterns holen
            $pattern=array_keys($this->replace);

            //Auf RegExp anpassen
            array_walk($pattern,array($this,"_parse"));
            
            //Soll ein Formular erzeugt werden ?
            $replace=$this->replace;
            if ($this->asform)
                {
                array_walk($replace,array($this,"_parse_asform"));
                }

            //Und nun einfach mit RegEx ersetzen
            $tpl=(preg_replace($pattern,$replace,$tpl));

            //Die Filter ansetzen
            if ($this->filter_url_replace)   $tpl = $this->_filter_url          ($tpl);
            if ($this->filter_strip_comment) $tpl = $this->_filter_strip_comment($tpl);
            if ($this->filter_trim)          $tpl = $this->_filter_trim         ($tpl);
            if ($this->filter_flat_tags)     $tpl = $this->_filter_flat_tags    ($tpl);

            //Fertig
            $this->internal_output=$tpl;
            unset($tpl);
            $result=TRUE;
            }
        return($result);
        }
        
    //////////////////////////////////////////////////////////////////////////////
    //Den Buffer ausgeben
    function out()
        {
        return($this->internal_output);
        }

    //////////////////////////////////////////////////////////////////////////////
    //Ein neues Keyword festlegen (ohne die %-Zeichen)
    //Wenn Keyword ein Array ist array[keyword]=value
    //wird das ganze Array auf einmal zugef�gt
    function add($keyword,$value="")
        {
        if (!is_array($keyword))
            {
            $keyword=array($keyword=>$value);
            }
        
        $this->replace=array_merge($this->replace,$keyword);
        }

    //////////////////////////////////////////////////////////////////////////////
    /// Private Funktionen
    //////////////////////////////////////////////////////////////////////////////

    //Filter um externe Dateien einzubinden
    function _filter_include($input)
        {
        //Alle Includetags holen
        $result=array();
        if (preg_match_all("/\\%include:([^\\%\\s]+?)\\%/",$this->internal_buffer,$result)>0)
            {
            //Alle Ergebnisse durchgehen
            foreach (next($result) as $filename)
                {
                //Eindeutiges Keyword erzeugen, um den URL-Parser nicht aus dem Tritt zu bringen
                $keyword=md5($filename);
                
                //Ist der Dateiname eine URL ?
                if (strpos($filename,"http:")!==FALSE)
                     {
                     //URL-Includes zugelassen ?
                     if ($this->url_includes)
                        {
                        $this->add($keyword,file_get_contents($filename));
                        }
                     else
                        {
                        $this->add($keyword,"URL-Includes disabled for ".$filename);
                        }
                     }
                else
                    {
                    //Existiert die Datei
                    if (file_exists($filename))
                        {
                        $this->add($keyword,file_get_contents($filename));
                        }
                    else
                        {
                        //Fehler einf�gen
                        if (!$this->quiet) $this->add($keyword,"<b>File [".$filename."] not found</b>");
                        }
                    }
                //Und die alten includes ersetzen, um zu verhindern das beu URLs der URL-Parser zuschl�gt
                $input=str_replace("%include:".$filename."%","%".$keyword."%",$input);
                }
            }
        return($input);
        }

    //////////////////////////////////////////////////////////////////////////////
    //Filter um alle URLs in einen anklickbaren Link zu wandeln
    function _filter_url($input)
        {
        return(preg_replace("�(\\s)((http|ftp|https)://([^/\\s]+)[^\\s]*)(\\s)�",'$1<a href="$2" target="_blank">$4</a>',$input));
        }
        
    //////////////////////////////////////////////////////////////////////////////
    //Filter um die Zeilen zu trimmen
    function _filter_trim($input)
        {
        //Machen wir mit einem regul�ren Ausdruck
        return(preg_replace("/(?m)^[\\s]*/","",$input));
        }

    //////////////////////////////////////////////////////////////////////////////
    //Filter um Kommentare zu entfernen
    function _filter_strip_comment($input)
        {
        //Machen wir mit einem regul�ren Ausdruck
        return(preg_replace("/\\<!--[^>]+>/","",$input));
        }

    //////////////////////////////////////////////////////////////////////////////
    //Filter um Zeilenumbr�che hinter Tags zu entfernen
    function _filter_flat_tags($input)
        {
        //Machen wir mit einem regul�ren Ausdruck
        return(preg_replace("/((<[^>]+>)\r|\n)/",'$2',$input));
        }
    }
</script>