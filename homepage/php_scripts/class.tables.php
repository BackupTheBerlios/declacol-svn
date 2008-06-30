<script language="PHP">
///////////////////////////////////////////////////////////////////////////////////////
/// Diese Klasse hilft alle Möglichen Arten von Tabellen zu erstellen
///
///
///Beispiel : Ausgabe eine Benutzerliste (NUR ein Beispiel, bitte nicht so übernehmen)
///
///$table= new table();
///
/////Zebratabelle erzeugen
///$table->add_style("tr","color1");
///$table->add_style("tr","color2");
///
/////Unerwünschte Views blind schalten
///$table->add_view("id","");
///$table->add_view("userid","");
///
/////Filter für einzelne Views setzen
///$table->add_view ("changed","d");   //Wir als Timestamp interpretiert
///$table->add_view ("password","64");  //Wird von Base64 nach String konvertiert
///
/////Eine Callbackfunktion einhängen. Diese wird mit einem Array der kompletten Zeilendaten aufgerufen
///$table->add_view ("callbackedit","f");
///
/////Die Tabellentitel setzen.
///$table->add_title("password","Kennwort");
///$table->add_title("changed","Letzte Änderung");
///$table->add_title("username","Benutzername");
///
/////Daten aus SQL-Ziehen
///$dbresult=$mysql->query("SELECT * FROM MYUSERDB ORDER BY username");
///if ($dbresult!=FALSE)
///    {
///    $table->add_array($dbresult);
///    $table->render();
///    }
///else
///    {
///    echo "no data found";
///    }
///
///$table->destroy();
///
///function callbackedit($rowdata)
///    {
///    //Als Beispiel nur die Daten ausgeben. Man könnte natürlich auch ein
///    //Formular einhängen
///    print_r($rowdata);
///    }
///////////////////////////////////////////////////////////////////////////////////////
class table
    {
    //Arrays mit den Daten
    //Jedes array in Data wird als geparster Text
    //und einzelnen Eintrag in der Liste angezeigt.
    //Der Index des Datenarrays wird als Überschrift benutzt,
    //wenn die gewünscht ist
    var $data =array();
    var $title=array();

    //Der Array View bestimmt die Art und Weise der Darstellung
    //Es gibt folgende Parameter
    //D : Langes Datum mit Uhrzeit
    //d : Kurzes Datum ohne Uhrzeit
    //t : Uhrzeit
    //b : Anzeige eines Zahlenwertes als automatisch bestimmer Wert KByte/MByte etc.
    //B : Anzeige eines Zahlenwertes als Balken
    //64: Dekodiert den Übergebenen Wert von Base64 nach String
    //x : Dekodiert den übergebenen Wert von HEX nach String
    //s : geparster String
    //f : Inhalt wird als CalBackFunktionsname interpretiert und mit dem Zeilenarray aufgerufen
    //  : Leer ignoriert alle Zellen mit diesem View
    //i : Inhalt wird als URL eines Bildes interpretiert
    //I : Inhalt wird als URL eines Bildes interpretiert und ein Link auf die Originaldatei gesetzt
    //l : Inhalt wird als URL interpretiert und ein Link gesetzt dargestellt wird der kpl Link
    //L : Inhalt wird als URL interpretiert und ein Link gesetzt dargestellt wird nur der "Dateiname"
    var $view=array();

    //Transponiert die Zellen in Spalten wobei die Callbackfunktionen dennoch den
    //"alten" Zeilenwert bekommen
    var $transform=FALSE;
    //Wieviele Spalten sollen angezeigt werden
    var $columns  =5;
    
    //Interne Kopien der externen Arrays um bei einer Formatierung
    //die Originaldaten nicht zu überschreiben
    var $internal_data     = FALSE;
    var $internal_view     = FALSE;
    var $internal_title    = FALSE;

    //Array, das die Styles für die Tabelle enthält
    var $styles   = array("table"=>array(),
                          "tr"=>array(),
                          "th"=>array(),
                          "td"=>array(),
                          "bar"=>array("color" =>"#b0b0b0",
                                       "height"=>"100%"
                                       )
                          );

    ///////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function tables()
        {
        $this->reset();
        }
        
    ///////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->reset();
        unset($this);
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Alles auf Standard setzen
    function reset()
        {
        $this->styles   = array("table"=>array(),
                                "tr"=>array(),
                                "th"=>array(),
                                "td"=>array());
        $this->transform=FALSE;
        $this->data     = array();
        $this->view     = array();
        $this->internal_data =array();
        $this->internal_view =array();
        $this->internal_title=array();
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Eine kpl. Tabelle einfügen
    function add_array($array)
        {
        if (is_array($array)!=FALSE)
            {
            //Einfach Zeilenweise zufügen
            foreach ($array as $row)
                {
                $this->add_data($row);
                }
            $result=TRUE;
            }
        else
            {
            $result=FALSE;
            }
        //Und den Index der Zeile zurückgeben
        return($result);
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Eine Datenzeile zufügen
    function add_data($row)
        {
        if (is_array($row)==FALSE)
            {
            $this->data[]=array($row);
            }
        else
            {
            $this->data[]=$row;
            }
        //Und den Index der Zeile zurückgeben
        return(count($this->data)-1);
        }
        
    ///////////////////////////////////////////////////////////////////////////////////////
    //Einen Anzeigemodifier zufügen
    function add_view($id,$mode)
        {
        $this->view[$id]=$mode;
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Einen Titeleintrag zufügen
    function add_title($id,$text)
        {
        $this->title[$id]=$text;
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Einen Style zufügen
    function add_style($type,$style,$id=FALSE)
        {
        //Nur die vorgegebenen Einträge zulassen
        if (isset($this->styles[$type])==TRUE)
            {
            if ($id==FALSE) $id=count($this->styles[$type]);

            $this->styles[$type][$id]=$style;
            }
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Die Tabelle anzeigen
    function render()
        {
        //Vorberechnungen machen
        $this->_sync_arrays();

        if ($this->transform==TRUE)
            {
            $this->render_vertical();
            }
        else
            {
            $this->render_standard();
            }
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Standardrendering d.h. Zeilen im Datenarray werden auch so dargestellt
    function render_standard()
        {
        global $html;

        $html->table_open("",reset($this->styles["table"]));
        //Title ausgeben ?
        if ($this->internal_title!=FALSE)
            {
            
            //$html->printr($this->internal_title);
            //$html->printr($this->internal_view);

            $html->row_open("");
            foreach ($this->internal_title as $key=>$head)
                {
                //Nur gewünschte Zellen anzeigen
                if ($this->internal_view[$key] != "_cell_ignore")
                    {
                    $html->head(HTMLScript_Parse($head,$this->styles["th"][$key]));
                    }
                }
            $html->row_close();
            }

        //Daten ausgeben ?
        if ($this->internal_data!=FALSE)
            {
            //Jede Zeile
            $rowstyle=reset($this->styles["tr"]);
            foreach ($this->internal_data as $row)
                {
                $html->row_open($rowstyle);
                //Jede Spalte
                foreach ($row as $key => $cell)
                    {
                    //Je nach View die Renderfunktion aufrufen
                    $this->_cell_render($key,$cell);
                    }
                $html->row_close();

                $rowstyle=next($this->styles["tr"]);
                if ($rowstyle==FALSE)
                    {
                    $rowstyle=reset($this->styles["tr"]);
                    }
                }
            }
        //Fertig
        $html->table_close();
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Die Tabelle vertikal rendern
    //Dazu sollte aber nur eine Zelle pro Zeile sichtbar sein
    //unerwünschte Zellen können mit set_view(SPALTE,"") ausgeschaltet werden.
    function render_vertical()
        {
        global $html;

        $html->table_open("",reset($this->styles["table"]));

        //Daten ausgeben ?
        if ($this->internal_data!=FALSE)
            {
            $cols=$this->columns;
            //Jede Zeile
            $rowstyle=reset($this->styles["tr"]);
            $html->row_open($rowstyle);
            
            foreach ($this->internal_data as $row)
                {
                foreach ($row as $key => $cell)
                    {
                    //Nach der gewünschten Anzahl Spalten umbrechen
                    if ($cols<1)
                        {
                        $html->row_close();
                        //Styles für die Zeilen durchgehen
                        $rowstyle=next($this->styles["tr"]);
                        if ($rowstyle==FALSE)
                            {
                            $rowstyle=reset($this->styles["tr"]);
                            }
                        $html->row_open($rowstyle);
                        $cols=$this->columns;
                        }

                    //Nur Rendern wenn wir auch etwas darstellen
                    if ($this->internal_view[$key]!="_cell_ignore")
                        {
                        $this->_cell_render($key,$cell);
                        $cols--;
                        }
                    }
                }
            //Der Vollständigkeit halber rendern wir die Zeile noch voll
            while ($cols-- > 0)
                {
                $html->cell("");
                }
            $html->row_close();
            }
        //Fertig
        $html->table_close();
        }
        
    ///////////////////////////////////////////////////////////////////////////////////////
    /// Interne Funktionen
    ///////////////////////////////////////////////////////////////////////////////////////
    //Konvertierfunktionen der einzelnen Darstellungsarten
    //Beim Aufruf der Anzeige werden diese Funktionen nur noch Über Funktionszeiger
    //angesprungen
    function _cell_render($key,$cell)
        {
        //Funktionname und CSS-Style holen
        $function=array(&$this,$this->internal_view[$key]);
        $style=$this->styles["td"][$key];
        
        //Renderfunktion aufrufen
        call_user_func($function,$cell,$style);
        }

    function _cell_ignore($data,$style)
        {
        //global $html;
        //$html->cell("DISABLED",$style);
        }

    function _cell_base64($data,$style)
        {
        global $html;
        $html->cell(base64_decode($data));
        }

    function _cell_date_short($data,$style)
        {
        global $html;
        $html->cell(date("d.m.Y",$data),$style);
        }

    function _cell_date_long($data,$style)
        {
        global $html;
        $html->cell(date("H:i:s d.m.Y",$data),$style);
        }
        
    function _cell_time($data,$style)
        {
        global $html;
        $html->cell(date("H:i:s",$data),$style);
        }
        
    function _cell_string($data,$style)
        {
        global $html;
        $html->cell(HTMLScript_Parse($data),$style);
        }

    function _cell_image($data,$style)
        {
        global $html;
        $html->cell(HTMLScript_Parse("[IMG=".$data."]"),$style);
        }

    function _cell_imagelink($data,$style)
        {
        global $html;
        $html->cell(HTMLScript_Parse("[LINK=".$data."][IMG=".$data."][/LINK]"),$style);
        }

    function _cell_link($data,$style)
        {
        global $html;
        $html->cell(HTMLScript_Parse("[LINK=".$data."]".$data."[/LINK]"),$style);
        }

    function _cell_linkshort($data,$style)
        {
        global $html;
        $html->cell(HTMLScript_Parse("[LINK=".$data."]".basename($data)."[/LINK]"),$style);
        }

    function _cell_bytes($data,$style)
        {
        global $html;
        
        $data=intval($data);
        $result=$data;
        
        $kbyte=1024;
        $mbyte=1024 * 1024;
        $gbyte=1024 * 1024 * 1024;

        //Werte in Byteverteilung umrechnen
        if ($data > $kbyte)
            {
            if ($data > $mbyte)
                {
                $result=round($data / $mybte,2)." M";
                }
            else
                {
                $result=round($data / $kbyte,2)." K";
                }
            }
        $html->cell($result."Byte",$style);
        }

    function _cell_bar($data,$style)
        {
        global $html;

        $html->cell_open($style);
        $html->bar($this->styles["bar"]["color"],$this->styles["bar"]["height"],$data,$data);
        $html->cell_close();
        }

    function _cell_callback($data,$style)
        {
        global $html;
        //Durch die Vorverarbeitung des Datenarrays wird als $data eine Array mit folgendem
        //Aufbau übergeben
        //array("row"=>ZeilenAray,"function"=>funktionsname);
        $html->cell_open($style);
        if (is_callable($data["function"]))
            {
            call_user_func($data["function"],$data["row"]);
            }
        $html->cell_close();
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Die Views und Datenarray synchronisieren um die Bearbeitung zu
    //beschleunigen
    function _sync_arrays()
        {
        //Sicherstellen, daß die Daten immer als Zeilenarray vorliegen
        if (is_array(reset($this->data))==FALSE)
            {
            $this->internal_data=array();
            foreach ($this->data as $key=>$row)
                {
                $this->internal_data[$key]=array($row);
                }
            }
        else
            {
            $this->internal_data=$this->data;
            }

        //Alle nicht gesetzten Vieweinträge auf
        //String setzen und die Arrays auffüllen
        $keys=array_keys(reset($this->internal_data));
        $callbackrun=array();
        foreach ($keys as $index)
            {
            $myview=(isset($this->view[$index])!=FALSE)?$this->view[$index]:"s";

            //Nicht das Flag sondern den Funktionzeiger ablegen
            switch ($myview)
                {
                case ("D")  : $this->internal_view[$index]="_cell_date_long";  break;
                case ("d")  : $this->internal_view[$index]="_cell_date_short"; break;
                case ("t")  : $this->internal_view[$index]="_cell_time";       break;
                case ("b")  : $this->internal_view[$index]="_cell_bytes";      break;
                case ("B")  : $this->internal_view[$index]="_cell_bar";        break;
                case ("s")  : $this->internal_view[$index]="_cell_string";     break;
                case ("")   : $this->internal_view[$index]="_cell_ignore";     break;
                case ("i")  : $this->internal_view[$index]="_cell_image";      break;
                case ("I")  : $this->internal_view[$index]="_cell_imagelink";  break;
                case ("l")  : $this->internal_view[$index]="_cell_link";       break;
                case ("L")  : $this->internal_view[$index]="_cell_linkshort";  break;
                case ("64") : $this->internal_view[$index]="_cell_base64";     break;
                case ("f")  : $this->internal_view[$index]="_cell_callback";
                              $callbackrun[]=$index;
                                                                               break;
                default     : $this->internal_view[$index]="_cell_string";     break;
                }
            }
            
        //Wenn ein Callback angefragt ist, müssen wir leider alle Daten anfassen und
        //den Inhalt entsprechend der vorgaben array("row"=>$row,"function="data");
        //setzen
        if (count($callbackrun)>0)
            {
            foreach ($this->internal_data as $index=>$row)
                {
                foreach ($callbackrun as $key)
                    {
                    $this->internal_data[$index][$key]=array("row"=>$row,"function"=>$row[$key]);
                    }
                }
            }
            
        //Die Titelzeile (wenn es eine gibt) nach den Keys in den Zeilen sortieren
        if (count ($this->title) > 0)
            {
            $this->internal_title=array();
            foreach ($keys as $key)
                {
                if (isset($this->title[$key]))
                    {
                    $this->internal_title[$key]=$this->title[$key];
                    }
                else
                    {
                    $this->internal_title[$key]="";
                    }
                }
            }
        else
            {
            $this->internal_title=FALSE;
            }
            
        //Styles aufarbeiten und nicht definierte Werte für Zellen löschen
        foreach ($keys as $key)
            {
            if (isset($this->styles["td"][$key])==FALSE)
                {
                $this->styles["td"][$key]="";
                }
            if (isset($this->styles["th"][$key])==FALSE)
                {
                $this->styles["th"][$key]="";
                }
            }
        }
    }
</script>