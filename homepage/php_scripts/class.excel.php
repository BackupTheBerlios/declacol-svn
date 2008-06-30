<script language="php">
//////////////////////////////////////////////////////////////////////////
///
/// Klasse, um CSV und (hoffentlich bald) XLS-Dateien zu erzeugen
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_EXCELWRITER","class_excelwriter");
define ("CLASS_EXCELWRITER_VERSION","0.02");
if (isset($debug)) $debug->add(CLASS_EXCELWRITER,"version ".CLASS_EXCELWRITER_VERSION);

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class xlswriter
    {
    //Öffentliche Eigenschaften
    var $limiter      = ",";        //Limiter für CSV-Export
    var $cellchar     = "\x22";     //Zeiger für Zeichenfolgen beim CSV-Export
    
    var $head         = FALSE;      //Tabelle hat Überschriften ?
    var $headalign    = TRUE;       //Der Index der Zeilen enthält die Überschriften und soll ausgerichtet werden ?
    var $tablestyle   = "";         //Styles für den TableTag
    var $headstyle    = "";         //Styles für die TH-Tag
    var $headrowstyle = "";         //Styles für die Head-Zeile (wenn sie existiert)
    var $rowstyle1    = "";         //Styles für den TR-Tag
    var $rowstyle2    = "";         //Styles für den TR-Tag
    var $cellstyle    = "";         //Styles für den TD-Tag


    //Ausgabepuffer
    var $internal_head       = array();
    var $internal_buffer     = array();
    var $internal_col        = 0;
    var $internal_row        = 0;


    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function xlswriter()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    //Verbindungsaufbau
    //Hat hier keine Funktion
    function open()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    //Verbindungsabbau
    //Hat hier keine Funktion
    function close()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->internal_buffer=array();
        $this->internal_head  =array();
        $this->head=FALSE;
        $this->reset();
        }

    //////////////////////////////////////////////////////////////////////////
    //Datenbank und alles andere erzeugen
    //Hat hier keine Funktion
    function install()
        {
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    //Datenbank und alles andere zerstören
    //Hat hier keine Funktion
    function uninstall()
        {
        return(TRUE);
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Einen Wert zufügen (oder überschreiben)
    //Wird ein Array übergeben wird auch dieses als mehrere Zellen verarbeitet
    function add($value,$row=FALSE,$col=FALSE)
        {
        $isarray=is_array($value);
        
        //Möglichkeiten aufteilen
        if ($row===FALSE)
            {
            if ($isarray)
                {
                //Keine Zeilenangabe und ein Array
                //Dann Zeile einfügen und eins weiterrutschen
                $row=$this->internal_row++;
                }
            else
                {
                //Keine Zeilenangabe und kein Array
                //Dann die aktuelle Zeile nehmen
                $row=$this->internal_row;
                }
            if ($col===FALSE)
                {
                if ($isarray)
                    {
                    //Keine Zeile und keine Spalte und ein Array
                    //Selbsttätig ein Nextline ausführen
                    $this->internal_col=0;
                    }
                else
                    {
                    //Keine Zeile, keine Spalte und kein Array
                    //Selbsttätig eine Spalte weiterrutschen
                    $col=$this->internal_col++;
                    }
                }
            }

        //Wenn der Datensatz ein Array ist, dann fügen wir Ihn als kpl. Zeile ein
        if ($isarray)
            {
            $this->add_line($value,$row);
            }
        else
            {
            //Daten setzen
            $this->internal_buffer[$row][$col]=$value;
            }
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Einen "Linebreak" bei den Koordinaten durchführen
    function nextline()
        {
        $this->internal_row++;
        $this->internal_col=0;
        }

    //////////////////////////////////////////////////////////////////////////
    //Die Koordinaten resetten
    function reset()
        {
        $this->internal_row=0;
        $this->internal_col=0;
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine Zeile zufügen
    function add_line($array,$row=FALSE)
        {
        //Keine Zeile übergeben ?
        if ($row===FALSE)
            {
            //Dem internen Zeilenzeiger folgen
            $this->internal_buffer[$this->internal_row++]=$array;
            }
        else
            {
            //An die adressierte Stelle schieben
            $this->internal_buffer[$row]=$array;
            }
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Die Daten transformieren und als String zurückgeben
    function flush($type="csv")
        {
        //Nach Zeilennummern sortieren
        ksort($this->internal_buffer);

        //Headalign nur bei Head=TRUE zulassen
        $this->headalign=$this->head && $this->headalign;

        //Kopfzeile ?
        if ($this->head)
            {
            //Hilfvariable füllen
            $this->internal_head=reset($this->internal_buffer);
            }

        //Ausgabetyp verarbeiten
        switch ($type)
            {
            case("html")   :   $result=$this->_create_html();                             break;
            case("csv")    :   $result=$this->_create_csv();                              break;
            case("xls")    :   $result=$this->_write($filename,$this->_create_xls());     break;
            default        :   $result=$this->_create_html();                             break;
            }
        return($result);
        }

    //Die Daten im gewünschten Format abspeichern
    function save($filename,$type="csv")
        {
        return($this->_write($filename,$this->flush($type)));
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Einen Datensatz in eine Datei schreiben
    function _write($filename,$data)
        {
        $result=FALSE;
        $fp=fopen($filename,"w");
        if ($fp!==FALSE)
            {
            fwrite($fp,$data);
            fclose($fp);
            $result=TRUE;
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Die Daten als CSV erzeugen und den String zurückgeben
    function _create_csv()
        {
        $result="";

        //Bufferzeiger resetten
        $line=reset($this->internal_buffer);

        //Alle Zeilen direkt verarbeiten
        while ($line!==FALSE)
            {
            //Zeile evtl am Kopf ausrichten
            $line=$this->align_row($line);

            //Und Zellen aufbauen
            $cell=reset($line);
            while ($cell!==FALSE)
                {
                //Zelle einfügen
                $result.=$this->cellchar.$cell.$this->cellchar;
                $cell=next($line);
                
                if ($cell!==FALSE)
                    {
                    $result.=$this->limiter;
                    }
                }
                
            //Nächste Zeile holen
            $line=next($this->internal_buffer);

            //Zeilenumbruch
            $result.="\r\n";
            }
        return($result);
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Die Daten als HTML erzeugen und den String zurückgeben
    function _create_html()
        {
        $result="\r\n<table class=\x22".$this->tablestyle."\x22>\r\n";

        //Wenn nur ein Style angegeben wurde, dann diesen für alle Zeilen verwenden
        if ($this->rowstyle2=="") $this->rowstyle2=$this->rowstyle1;

        //Styles aufarbeiten
        $tr1="<tr";
        $tr1.=($this->rowstyle1!="")?" class=\x22".$this->rowstyle1."\x22>":">";
        $tr2="<tr";
        $tr2.=($this->rowstyle2!="")?" class=\x22".$this->rowstyle2."\x22>":">";
        $trh="<tr";
        $trh.=($this->headrowstyle!="")?" class=\x22".$this->headrowstyle."\x22>":">";
        $td="<td";
        $td.=($this->cellstyle!="")?" class=\x22".$this->cellstyle."\x22>":">";
        $th="<th";
        $th.=($this->headstyle!="")?" class=\x22".$this->headstyle."\x22>":">";

        //Bufferzeiger resetten
        $line=reset($this->internal_buffer);

        //Überschrift ?
        if ($this->head)
            {
            $result.=$trh;
            $cell=reset($this->internal_head);
            while ($cell!==FALSE)
                {
                //Zelle einfügen
                $result.=$th.$cell."</th>";

                $cell=next($this->internal_head);
                }
            //Zeilenumbruch
            $result.="</tr>\r\n";

            //Nächste anspringen, um die Überschrift zu überspringen
            $line=next($this->internal_buffer);
            }

        //Der Rest kommt untendran
        $zebra=FALSE;
        while ($line!==FALSE)
            {
            //Zeilenscwitcher
            $result.=($zebra?$tr1:$tr2);
            $zebra=!$zebra;
            
            //Zeile evtl am Kopf ausrichten
            $line=$this->align_row($line);

            //Und Zellen aufbauen
            $cell=reset($line);
            while ($cell!==FALSE)
                {
                //Zelle einfügen
                $result.=$td.$cell."</td>";
                $cell=next($line);
                }
            //Zeilenumbruch
            $result.="</tr>\r\n";
            
            $line=next($this->internal_buffer);
            }
        $result.="</table>\r\n";

        return($result);
        }



    //////////////////////////////////////////////////////////////////////////
    //Die Daten in ein Excel-Format bringen
    //ToDo
    function _create_xls()
        {
        return("nix");
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Die Daten einer Zeile ggf. nach dem Überschriftenindex ausrichten
    function align_row($rowarray)
        {
        //Indexausrichtung ?
        if ($this->headalign)
            {
            $result=array();
            foreach ($this->internal_head as $index=>$value)
                {
                if (isset($rowarray[$index]))
                    {
                    $result[$index]=$rowarray[$index];
                    }
                else
                    {
                    $result[$index]="";
                    }
                }
            }
        else
            {
            //Ansonsten das Array wie es ist
            $result=$rowarray;
            }
        return($result);
        }
    }
</script>