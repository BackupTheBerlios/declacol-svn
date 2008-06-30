<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// HTML Klasse
///
/// zur Ausgabe von HTML-Tags.
/// Ist der Bufferedmode gesetzt wird die Ausgabe erst bei einem flush
/// gemacht. Ansonsten werden Ausgaben direkt geschrieben
///
/// Die Kapselung der HTML-Ausgabe in diese Klasse vereinfacht den Umstieg
/// von einer (X)HTML-Version auf eine andere, da nur die Tags hier
/// geändert werden müssen. Der Quelltext sollte keine Tags enthalten.
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_TEMPLATE","class_template");
define ("CLASS_TEMPLATE_VERSION","0.02");
if (isset($debug)) $debug->add(CLASS_TEMPLATE,"version ".CLASS_TEMPLATE_VERSION);

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class template
    {
    //Öffentliche Eigenschaftem
    //Ist die Templateklasse im gepufferten Modus ?
    //ReadOnly
    var $buffered    = FALSE;
    
    var $filter_trim = FALSE;
    var $filter_flat = FALSE;

    //Private Eigenschaften
    var $internal_buffer="";
    var $internal_style ="";
    
    var $formcounter=0;
    var $currentform="";

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function template($buffermode=FALSE)
        {
        //Buffermode merken
        $this->setbuffer($buffermode);
        }

    //Destruktor
    function destroy()
        {
        $this->close();
        }

    //Verbindungsaufbau
    //wird hier nicht benutzt
    function open()
        {
        }

    //Verbindungsabbau
    //wird hier nicht benutzt
    function close()
        {
        }

    //Defaults setzen
    function setdefault()
        {
        $this->internal_buffer="";
        $this->internal_style="";
        $this->buffered=FALSE;
        }

    //Datenbank erzeugen
    //wird hier nicht benutzt
    function install()
        {
        }

    //Datenbank zerstören
    //wird hier nicht benutzt
    function uninstall()
        {
        }
//////////////////////////////////////////////////////////////////////////
/// Interne Methoden
//////////////////////////////////////////////////////////////////////////
    //Property für den Buffermodus setzen (TRUE/FALSE)
    function setbuffer($buffer)
        {
        $this->buffered=(boolean) $buffer;
        }

    //Standardstyle setzen. Wird kein Style bei Ausgabe eines Tags übergeben
    //ist aber ein Standardstyle gesetzt, wird der Standardstyle benutzt
    function setstyle($style)
        {
        $this->internal_style=$style;
        }

    //Den Buffer löschen ohne ihn auszugeben
    function clearbuffer()
       {
       $this->internal_buffer="";
       }

    //Den Buffer ins nichts leiten
    function nilbuffer()
       {
       $this->internal_buffer=FALSE;
       }


    //////////////////////////////////////////////////////////////////////////
    //Interne Stylefunktion zur Aufarbeitung der TAGs
    function _processstyle($style)
        {
        //Wird ein Style übergeben ?
        if ($style!="")
            {
            //Ja, dann umformatieren
            $style ="class=\x22".$style."\x22 ";
            }
        else
            {
            //Nein
            //Ist ein Standardstyle gesetzt ?
            if ($this->internal_style!="")
                {
                //Dann diesen benutzen
                $style ="class=\x22".$this->internal_style."\x22 ";
                }
            }
        //Und das Ergebnis zurückliefern
        return($style);
        }


    //////////////////////////////////////////////////////////////////////////
    //Daten ausgeben (RAW)
    function write($input)
        {
        //Gepufferter Modus ?
        if ($this->buffered)
            {
            //Ausgabe nicht nillen ?
            if ($this->internal_buffer!==FALSE)
                {
                $this->internal_buffer.=$input;
                }
            }
        else
            {
            echo $input;
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Daten ausgeben (Fettgedruckt)
    function bold($input,$style="")
        {
        $style=$this->_processstyle($style);
        $this->write("<b ".$style.">".$input."</b>");
        }

    //////////////////////////////////////////////////////////////////////////
    //Gepufferte Daten ausgeben
    function flush()
        {
        $input=$this->internal_buffer;

        if ($this->filter_trim) $input=preg_replace("/(?m)^[\\s]*/","",$input);
        if ($this->filter_flat) $input=preg_replace("/((<[^>]+>)\r|\n)/",'$2',$input);

        echo ($input);
        $input="";
        $this->internal_buffer="";
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////
    //Alles was mit Feldern zu tun hat
    //////////////////////////////////////////////////////////////////////////

    //Ein P Open
    function para_open($style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Und ausgeben
        $this->write("<p ".$style.">\n");
        }

    //P-Tag close
    function para_close()
        {
        //Und ausgeben
        $this->write("</p>\n");
        }

    //Pre-Tag open
    function pre_open($style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Und ausgeben
        $this->write("<pre ".$style.">\n");
        }

    //Pre-Tag close
    function pre_close()
        {
        //Und ausgeben
        $this->write("</pre>\n");
        }

    //Einen Text in Pre-Tags ausgeben
    function pre_out($text,$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Und ausgeben
        $this->write("<pre ".$style.">\n".$text."\n</pre>\n");
        }

    //Fieldset open
    function field_open($title,$style="",$width=0,$height=0)
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        $size="";
        //Das Fieldset zusammensetzen
        if (($width!=0) || ($height!=0))
            {
            $size ="style=\x22";
            $size.="width:".$width."; ";
            $size.="height:".$height."; ";
            $size.="\x22";
            }

        //Alles ausgeben
        $this->write("<fieldset ".$style.$size.">\n");
        if ($title!="") $this->write("<legend ".$style.">".$title."</legend>\n");
        }

    //Fielset close
    function field_close()
        {
        $this->write("</fieldset>\n");
        }

    //Div-Set open
    function div_open($style="",$width=0,$height=0)
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        $size="";
        //zusammensetzen
        if (($width!=0) || ($height!=0))
            {
            $size ="style=\x22";
            $size.="width:".$width."; ";
            $size.="height:".$height."; ";
            $size.="\x22";
            }

        //Alles ausgeben
        $this->write("<div ".$style.$size.">\n");
        }

    //Div-Set open mit ID die Scripte werden bei MouseOver oder MouseOff ausgeführt
    function div_open_id($style,$id,$onOver=FALSE,$onOut=FALSE)
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        $id="id=\x22".$id."\x22";

        $script="";
        $script.=($onOver===FALSE?"":" OnMouseOver=\x22".$onOver."\x22 ");
        $script.=($onOut ===FALSE?"":" OnMouseOut=\x22".$onOut."\x22 ");

        //Alles ausgeben
        $this->write("<div ".$style.$id.$script.">\n");
        }


    //Div-Close
    function div_close()
        {
        $this->write("</div>\n");
        }

    //////////////////////////////////////////////////////////////////////////
    /// Alles was mit Formularen zu tun hat
    //////////////////////////////////////////////////////////////////////////

    //Formular öffnen
    function form_open($style="",$opennew=FALSE)
        {
        global $session;
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Name zur verarbeitung mit JS merken
        $this->currentform="form".++$this->formcounter;

        if ($opennew)
            {
            $target=" target=\x22".$opennew."\x22 ";
            }
        else
            {
            $target="";
            }

        if (isset($session))
            {
            $this->write("<form action=\x22".$session->getlink()."\x22 method=\x22post\x22 ".$style." name=\x22".$this->currentform."\x22 ".$target.">\n<div>\n");
            }
        else
            {
            $this->write("<form action=\x22./\x22 method=\x22post\x22 ".$style." name=\x22".$this->currentform."\x22 ".$target.">\n<div>\n");
            }
        return($this->currentform);
        }

    //Formular schließen
    function form_close()
        {
        $this->write("</div>\n</form>\n");
        }


    //////////////////////////////////////////////////////////////////////////
    //Dropdownbox anzeigen. Liste ist ein Array der Einträge.
    //Enthält $liste den Wert $selected wird dieser Wert als Voreinstellung
    //gesetzt. Ist Use-Value gesetzt, wird anstatt mit dem Anzeigewert mit dem
    //Valuewert verglichen, was voreingestellt sein soll
    function dropdown($liste,$selected,$name,$style="",$valuearray=array(),$javascript=TRUE,$usevalue=FALSE)
        {
        //Fehler abfangen
        if (!is_array($liste))
            {
            return(FALSE);
            }

        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Wertearray checken
        if (!is_array($valuearray))
            {
            $valuearray=array();
            }

        //Starttag
        if ($javascript)
            {
            $output ="<select ".$style." name=\x22".$name."\x22 onchange=\x22this.form.submit()\x22>\n";
            }
        else
            {
            $output ="<select ".$style." name=\x22".$name."\x22>\n";
            }

        //Alle Einträge durchgehen
        $value=reset($valuearray);
        foreach ($liste as $eintrag)
            {
            //Als Option anlegen
            $output.="<option ";

            //Etwas vorselektieren ?
            if ($usevalue)
                {
                if ($value==$selected)
                    {
                    $output.="selected ";
                    }
                }
            else
                {
                if ($eintrag==$selected)
                    {
                    $output.="selected ";
                    }
                }

            //Ein Wert vorgesehen ?
            if ($value!=FALSE)
                {
                $output.="value=\x22".$value."\x22>";
                }
            else
                {
                $output.="value=\x22".$eintrag."\x22>";
                }

            $output.=$eintrag."</option>\n";

            $value=next($valuearray);
            }
        $output.="</select>\n";

        //Und ausgeben
        $this->write($output);

        return(TRUE);
        }

    //Mehrzeilige auswahlliste
    function selectbox(&$liste,$selected,$name,$lines=1,$style="",$valuearray=array(),$javascript=TRUE)
        {
        //Fehler abfangen
        if (!is_array($liste))
            {
            return(FALSE);
            }

        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Starttag
        if ($javascript)
            {
            $output ="<select ".$style." size=\x22".$lines."\x22name=\x22".$name."\x22 onchange=\x22this.form.submit()\x22>\n";
            }
        else
            {
            $output ="<select ".$style." size=\x22".$lines."\x22name=\x22".$name."\x22>\n";
            }

        if (!is_array($valuearray))
            {
            $valuearray=array();
            }

        //Alle Einträge durchgehen
        $value=reset($valuearray);
        foreach ($liste as $eintrag)
            {
            //Als Option anlegen
            $output.="<option ";

            //Etwas vorselektieren ?
            if ($eintrag==$selected)
                {
                $output.="selected ";
                }
            //Ein Wert vorgesehen ?
            if ($value!=FALSE)
                {
                $output.="value=\x22".$value."\x22>";
                }
            else
                {
                $output.="value=\x22".$eintrag."\x22>";
                }
            $output.=$eintrag."</option>\n";
            //Nächsten Value holen (wenn es Ihn gibt)
            $value=next($valuearray);
            }
        $output.="</select>\n";

        //Und ausgeben
        $this->write($output);
        return(TRUE);
        }

    //Inputbox für Text
    function input_text($name,$value="",$size="",$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Die Felder vorbereiten
        if ($size !=0 ) $size  ="size=\x22".$size."\x22 ";
        if ($value!="") $value ="value=\x22".$value."\x22 ";
        //Und zusammengesetzt ausgeben
        $this->write("<input name=\x22".$name."\x22 ".$style.$size.$value."/>\n");
        }

    //Inputfeld für Text
    function input_textarea($name,$value="",$width="80",$height="10",$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Tags zusammenbauen
        $output ="<textarea cols=\x22".$width."\x22 rows=\x22".$height."\x22 name=\x22".$name."\x22 ".$style.">";
        $output.=$value;
        $output.="</textarea>";

        //Und zusammengesetzt ausgeben
        $this->write($output);
        }

    //Inputbox für Kennwörter
    function input_password($name,$value="",$size="",$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Die Felder vorbereiten
        if ($size !=0 ) $size  ="size=\x22".$size."\x22 ";
        if ($value!="") $value ="value=\x22".$value."\x22 ";
        //Und zusammengesetzt ausgeben
        $this->write("<input name=\x22".$name."\x22 type=\x22password\x22".$style.$size.$value."/>\n");
        }

    //Standardbutton
    function button_standard($title,$name,$value,$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Die Felder vorbereiten
        if ($value!="") $value ="value=\x22".$value."\x22 ";
        //Und zusammengesetzt ausgeben
        $this->write("<button name=\x22".$name."\x22 ".$style.$value.">".$title."</button>\n");
        }

    //Javascriptbutton
    function button_script($title,$name,$script,$params,$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        if (!is_array($params))
            {
            $params=array($params);
            }

        //Parameterausgabe aufbereiten
        $data=reset($params);
        $out=$script."(";
        while ($data!==FALSE)
            {
            //String legen wir in Anführungsstriche
            if (is_string($data))
                {
                $out.="'".$data."'";
                }
            else
                {
                $out.=$data;
                }

            $data=next($params);
            if ($data!==FALSE)
                {
                $out.=",";
                }
            }
        $out.=")";

        //Und zusammengesetzt ausgeben
        $this->write("<button name=\x22".$name."\x22 ".$style." onclick=\x22".$out."\x22>".$title."</button>\n");
        }

    //Submit-Button
    //Ist Confirm!=FALSE wird der Inhalt als Text benutzt und eine Confirmbox eingeblendet
    function button_submit($title,$name,$value,$style="",$confirm=FALSE)
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Die Felder vorbereiten
        if ($value!="") $value ="value=\x22".$value."\x22 ";
        if ($confirm===FALSE)
            {
            $this->write("<button name=\x22".$name."\x22 type=\x22submit\x22".$style.$value.">".$title."</button>\n");
            }
        else
            {
            $this->write("<button name=\x22".$name."\x22 type=\x22submit\x22".$style.$value." onclick=\x22return confirm('".$confirm."')\x22>".$title."</button>\n");
            }
        }

    //Standardbutton mit einer Grafik als Button
    //Ist Confirm!=FALSE wird der Inhalt als Text benutzt und eine Confirmbox eingeblendet
    function button_image($imageurl,$name,$value,$style="",$confirm=FALSE)
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Die Felder vorbereiten
        if ($value!=="") $value ="value=\x22".$value."\x22 ";
        if ($confirm===FALSE)
            {
            $this->write("<input type=\x22image\x22 src=\x22".$imageurl."\x22 name=\x22".$name."\x22 title=\x22".$name."\x22 ".$style.$value."/>\n");
            }
        else
            {
            $this->write("<input type=\x22image\x22 src=\x22".$imageurl."\x22 name=\x22".$name."\x22 title=\x22".$name."\x22 ".$style.$value." onclick=\x22return confirm('".$confirm."')\x22/>\n");
            }
        }

    //Hidden Data
    function data_hidden($name,$value="")
        {
        $this->write(sprintf("<input type=\x22hidden\x22 name=\x22%s\x22 value=\x22%s\x22/>\n",$name,$value));
        }

    //Alle Daten in Postvars als versteckte Daten einfügen
    function data_vars(&$postvars)
        {
        //Alle Felder in Vars als Hidden markieren
        foreach ($postvars->data as $dataname => $data)
            {
            $this->data_hidden($dataname,$data);
            }
        }

    //Eine Checkbox anzeigen
    function checkbox($name,$checked=FALSE,$style="",$javascript=FALSE)
        {
        $style=$this->_processstyle($style);

        //Die Felder vorbereiten
        if ($checked!==FALSE)         $checked   ="checked=\x22checked\x22 ";    else $checked="";
        if ($javascript!==FALSE)    $javascript="onchange=\x22this.form.submit()\x22 ";    else $javascript="";

        $this->write("<input type=\x22checkbox\x22 name=\x22".$name."\x22 ".$checked.$javascript."/>");
        }

    //Einen Radiobutton anzeigen
    function radio($name,$value,$checked=FALSE,$style="",$javascript=FALSE)
        {
        $style=$this->_processstyle($style);

        //Die Felder vorbereiten
        //Die Felder vorbereiten
        if ($checked!==FALSE)         $checked   ="checked=\x22checked\x22 ";    else $checked="";
        if ($javascript!==FALSE)    $javascript="onchange=\x22this.form.submit()\x22 ";    else $javascript="";

        $value="value=\x22".$value."\x22";

        $this->write("<input type=\x22radio\x22 name=\x22".$name."\x22 ".$checked.$javascript." ".$value." />");
        }


    ////////////////////////////////////////////////////////////////////////////////////
    //Alles was mit Tabellen zu tun hat
    ////////////////////////////////////////////////////////////////////////////////////

    ///Ein Array als Tabelle ausgeben
    function table(&$data,$width="",$style="")
        {
        if (!is_array($data))
            {
            return(FALSE);
            }

        //Style verarbeiten
        $style=$this->_processstyle($style);

        //Weite verarbeiten
        if ($width!="") $width ="style=\x22 width:".$width.";\x22 ";

        //Und nun bauen wir die Tabelle zusammen
        $output ="<table ".$style.$width.">\n";
        $output.="<tr class=\x22nohover\x22>";

        //Überschrift
        foreach (reset($data) as $titel => $row)
            {
            $output.="<th>".$titel."</th>";
            }
        $output.="</tr>\n";

        //Inhalte
        foreach ($data as $row)
            {
            $output.="<tr>";
            foreach ($row as $cell)
                {
                $output.="<td>".$cell."</td>";
                }
            $output.="</tr>\n";
            }
        $output.="</table>\n";


        //Und ausgeben
        $this->write($output);
        return(TRUE);
        }

    //Table open
    function table_open($width,$style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);

        //Weite verarbeiten
        if ($width!="") $width ="style=\x22 width:".$width.";\x22 ";

        //Und nun bauen wir die Tabelle zusammen
        $this->write("<table ".$style.$width.">\n");
        }

    //Table close
    function table_close()
        {
        $this->write("</table>\n");
        }

    //TBody Open
    function tbody_open($style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);

        $this->write("<tbody ".$style.">");
        }

    //tbody close
    function tbody_close()
        {
        $this->write("</tbody>\n");
        }


    //Row Open
    function row_open($style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);

        $this->write("<tr ".$style.">");
        }

    //Row close
    function row_close()
        {
        $this->write("</tr>\n");
        }

    //Eine Head-Zelle ausgeben
    function head($text,$style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);
        //Und Tag ausgeben
        $this->write("<th ".$style.">".$text."</th>");
        }

    //Eine Head-Zelle ausgeben
    function head_open($style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);
        //Und Tag ausgeben
        $this->write("<th ".$style.">");
        }

    //Eine Head-Zelle schließen
    function head_close()
        {
        //Und Tag ausgeben
        $this->write("</th>");
        }


    //Eine Zelle direkt ausgeben
    function cell($text,$style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);
        //Und Tag ausgeben
        $this->write("<td ".$style.">".$text."</td>");
        }

    //Zellen open
    function cell_open($style)
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);

        $this->write("<td ".$style.">");
        }

    //Zelle close
    function cell_close()
        {
        $this->write("</td>");
        }

    //////////////////////////////////////////////////////////////////////////
    ///Alles was mit Listen zu tun hat
    //////////////////////////////////////////////////////////////////////////
    //List open
    function list_open($width,$style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);

        //Weite verarbeiten
        if ($width!="") $width ="style=\x22 width:".$width.";\x22 ";

        //Und nun bauen wir die Tabelle zusammen
        $this->write("<ul ".$style.$width.">\n");
        }

    //List close
    function list_close()
        {
        $this->write("</ul>\n");
        }

    //Item Open
    function item_open($style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);

        $this->write("<li ".$style.">");
        }

    //Item close
    function item_close()
        {
        $this->write("</li>\n");
        }

    //Ein Item direkt ausgeben
    function item($text,$style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);
        //Und Tag ausgeben
        $this->write("<li ".$style.">".$text."</li>");
        }

    //Aus einem Array eine Itemliste erzeugen
    function list_create($inputarray,$style="",$usekeys=FALSE)
        {
        $this->list_open($style);
        foreach ($inputarray as $key=>$value)
            {
            $this->write("<li>");
            if ($usekeys) $this->bold($key."  : ");
            $this->printr($value);
            $this->write("</li>");
            }
        $this->list_close();
        }

    //////////////////////////////////////////////////////////////////////////
    ///Alles was mit Links zu tun hat
    //////////////////////////////////////////////////////////////////////////

    //Link ausgeben
    function link_short($url,$title,$target="",$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Und alle anderen
        $intitle ="title=\x22".$title."\x22 ";
        if ($target!="") $target="target=\x22".$target."\x22 ";

        //URL cleanen
        //Spaces ersetzen
        $url=str_replace(" ","%20",$url);

        $this->write("<a href=\x22".$url."\x22 ".$target.$intitle.$style.">".$title."</a>");
        }

    //Link open
    function link_open($url,$title="",$target="",$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Und alle anderen
        if ($title !="") $title ="title=\x22".$title."\x22 ";
        if ($target!="") $target="target=\x22".$target."\x22 ";

        $this->write("<a href=\x22".$url."\x22 ".$target.$title.$style.">");
        }

    //Link close
    function link_close()
        {
        $this->write("</a>\n");
        }

    function link_script($script,$text)
        {
        $this->write("[ <a href=\x22#\x22 OnClick=\x22".$script."\x22>".$text."</a> ] ");
        }

    function link_over($scriptover,$scriptout,$text)
        {
        $this->write("<a href=\x22#\x22 OnMouseOver=\x22".$scriptover."\x22 OnMouseOut=\x22".$scriptout."\x22>".$text."</a>");
        }


    //////////////////////////////////////////////////////////////////////////
    //Javascript hilfen
    //////////////////////////////////////////////////////////////////////////
    function check_all($form="",$text="")
        {
        //Wenn kein Formular übergeben wurde, addressieren wir das aktuelle
        //Formular
        if ($form=="")
            {
            $form=$this->currentform;
            }
        
        if ($text=="")
            {
            $text=LNG_CHECK_ALL;
            }

        $this->link_script("checkAll(".$form.")",$text);
        }

    function uncheck_all($form="",$text="")
        {
        //Wenn kein Formular übergeben wurde, addressieren wir das aktuelle
        //Formular
        if ($form=="")
            {
            $form=$this->currentform;
            }

        if ($text=="")
            {
            $text=LNG_UNCHECK_ALL;
            }

        $this->link_script("uncheckAll(".$form.")",$text);
        }

    function invert_all($form="",$text="")
        {
        //Wenn kein Formular übergeben wurde, addressieren wir das aktuelle
        //Formular
        if ($form=="")
            {
            $form=$this->currentform;
            }

        if ($text=="")
            {
            $text=LNG_INVERT_ALL;
            }

        $this->link_script("invertAll(".$form.")",$text);
        }

    function runscript($name,$params)
        {
        //Der Einfachheit halber imer ein Array erzwingen
        if (!is_array($params))
            {
            $params=array($params);
            }
            
        //Parameterausgabe aufbereiten
        $data=reset($params);
        $out="";
        while ($data!==FALSE)
            {
            //String legen wir in Anführungsstriche
            if (is_string($data))
                {
                $out="\x22".$data."\x22";
                }
            else
                {
                $out.=$data;
                }

            $data=next($params);
            if ($data!==FALSE)
                {
                $out.=",";
                }
            }
        $this->write("<script language=\"javascript\">".$name."(".$out.")</script>\r\n");
        }


    //////////////////////////////////////////////////////////////////////////
    //Sonstige
    //////////////////////////////////////////////////////////////////////////

    //Nächste Zeile
    function nextline($count=1)
        {
        while ($count >0)
            {
            $this->write("<br/>\n");

            $count--;
            }
        }

    //Leerzeichen
    function space($count=1)
        {
        $buff=GFX_SPACE;
        while ($count>1)
            {
            $buff.=GFX_SPACE;
            $count--;
            }
        $this->write($buff);
        }

    //horizontale Linie
    function line()
        {
        $this->write("<hr/>");
        }

    //Einen Forschrittsbalken darstellen
    function bar($color,$height,$width,$text="bar")
        {
        //Wenn keine Prozente angegeben sind, PX annehmen
        if (string_pos_left($height,"%")===FALSE)
            {
            $height=string_add($height,"px");
            }
        if (string_pos_left($width,"%")===FALSE)
            {
            $width=string_add($width,"px");
            }

        $output="<hr style=\x22width : ".$width."; height : ".$height."; background-color:".$color."; border : none; text-align:left;\x22 title=\x22".$text."\x22/>\n";
        $this->write($output);
        }

    //Einen Prozent-Forschrittsbalken darstellen (mit Farbverlauf)
    function bar_percent($color1,$color2,$percent,$width,$text="bar")
        {
        //Ein "-" ist ca. 30% breit 100% hoch
        $size=round(0.05*$width);



        //Zwanzig Klötze machen
        $percent*=2;
        //Erst die bunten
        $this->write("<font style=\x22 font-size : ".$size."; padding : 0px; cursor : default; margin : 0px;\x22 title=\x22".$text."\x22>\n");
        for ($count=0; $count < $percent; $count+=10)
            {
            //Farbe an der aktuellen Position holen
            $color=colorgradient(0,100,$count,$color1,$color2);

            //Und einen Block ausgeben
            $this->write("<font style=\x22 background-color : ".$color.";\x22>&emsp;</font>");
            }

        //Und dann die durchsichtigen
        $this->write("<font style=\x22 background-color : transparent;\x22>");
        for ($count=$percent; $count < 200; $count+=10)
            {
            $this->write("&emsp;");
            }
        $this->write("</font>\n");
        $this->write("</font>\n");
        }


    //////////////////////////////////////////////////////////////////////////
    //Ein bild einfügen
    function image($source,$name,$style="",$width=0,$height=0)
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Größe bauen
        $size="";
        //zusammensetzen
        if ($width!=0)
            {
            $size.="width:".$width."; ";
            }
        if ($height!=0)
            {
            $size.="height:".$height."; ";
            }
        if ($size!="")
            {
            $size="style=\x22".$size."\x22";
            }


        //ALt automatisch einfügen
        if ($name=="") $name="image";

        //Und den Tag ausgeben
        $this->write("<img src=\x22".$source."\x22 ".$style." alt=\x22".$name."\x22 title=\x22".$name."\x22 ".$size."/>\n");
        }

    ////////////////////////////////////////////////////////////////////////////////////
    ///Switchable Image für ID
    function image_switch($source,$name,$id,$style="")
        {
        //Style verarbeiten
        $style =$this->_processstyle($style);

        //Und den Tag ausgeben
        $this->write("<img src=\x22".$source."\x22 ".$style." alt=\x22".$name."\x22 title=\x22".$name."\x22 onClick=\x22showhidestuff('".$id."')\x22 style=\x22cursor:pointer;\x22/>\n");
        }


    ////////////////////////////////////////////////////////////////////////////////////
    ///Text ausgeben
    function text($text,$size=0,$style="")
        {
        //Style verarbeiten
        $style=$this->_processstyle($style);


        if ($size!=0)
            {
            $size="style=\x22 font-size:".$size."\x22 ";
            }
        else
            {
            $size="";
            }

        if ( ($style=="") && ($size=="") )
            {
            $this->write($text);
            }
        else
            {
            $this->write("<font ".$style.$size.">".$text."</font>");
            }
        }

    //Einen geparsten Text ausgeben
    function text_parsed($text)
        {
        //Einfach durch den HTML-Parser schieben
        $this->write(HTMLScript_Parse($text));
        }

    //Print_r für die HTML-Ausgabe umleiten
    function printr($object)
        {
        $this->pre_open("");
        $this->write(print_r($object,TRUE));
        $this->pre_close();
        }


    //////////////////////////////////////////////////////////////////////////
    //Zusammengesetzte Funktionen
    //////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////
    //Eine LoginBox ausgeben
    function loginbox($style="")
        {
        //Formularstart
        $this->form_open();

        //Zwei boxen
        $this->text             (LNG_USER);
        $this->nextline();
        $this->input_text    ("username","",12,$style);
        $this->nextline();
        $this->text             (LNG_PASSWORD,0,$style);
        $this->nextline();
        $this->input_password("password","",12,$style);

        $this->nextline(2);

        //Und ein Button drunter
        $this->button_submit(LNG_LOGIN,"login","login",$style);

        //Formularende
        $this->form_close();
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine Logoutbox ausgeben
    function logoutbox($style="")
        {
        $this->form_open();
        $this->data_hidden("logout","logout");
        $this->button_submit(LNG_LOGOUT,"logout","logout",$style);
        $this->form_close();
        }

    ////////////////////////////////////////////////////////////////////////////////////
    ///Userstatus ausgeben
    function userstatus($user)
        {
        //Text zusammenbauen
        $this->write($user->name);
        $this->nextline();
        $this->write(LNG_LASTLOGIN);
        $this->nextline();
        $this->write(date(DATE_LONG_BROKEN,$user->lastlogin));
        $this->nextline();
        }

    ////////////////////////////////////////////////////////////////////////////////////
    ///Text hinter einem Icon verbergen
    function icontooltip($icon,$text)
        {
        //Über die JavaScriptBibliothek aufbauen
        ShowIconToolTip($icon,$text,600);
        }

    ////////////////////////////////////////////////////////////////////////////////////
    ///Den Script-Helptext als Hilfeicon anzeigen
    function scripthelp()
        {
        $this->icontooltip(URL_ICONS."icon_help.png",HLP_HTMLSCRIPT);
        }

    //Eine HTML-Datei includen
    function read($filename)
        {
        $this->write(@file_get_contents($filename));
        }

    //Einen Upload zulassen
    function upload($style="")
        {
        global $session;
        
        if (isset($session))
            {
            $this->write("<form action=\x22".$session->getlink()."\x22 method=\x22post\x22 enctype=\x22multipart/form-data\x22 ".$style." name=\x22upload\x22>\n<div>\n");
            }
        else
            {
            $this->write("<form action=\x22./\x22 method=\x22post\x22 enctype=\x22multipart/form-data\x22".$style." name=\x22uploadfile\x22>\n<div>\n");
            }
            
        $this->write("<input type=\x22file\x22 size=\x2260\x22 name=\x22upload\x22>");
        $this->nextline();
        $this->data_hidden("phpupload","1");
        $this->button_submit("Upload","a","a","small");
        $this->write("</div></form>");
        }

    //Eine Liste mit allen aktuellen User zeigen
    //Als Rückgabe erhält man den MD5 der UserID
    function userlist($name,$default,$javascript=FALSE,$all=FALSE)
        {
        global $user;

        //Alle User holen
        $myuser=$user->enumerate($all);

        //Emails extrahieren
        $names=array("");
        $addr =array("12345");
        foreach ($myuser as $oneuser)
            {
            $names[]=$oneuser->name;
            $addr[] =md5($oneuser->id);

            //Und speicher freigeben
            $oneuser->destroy();
            }
        unset($myuser);

        //Und ein Dropdown daraus machen
        $this->dropdown($names,$default,$name,"",$addr,$javascript);
        }


    //Ein Captcha erzeugen
    function captcha($text="",$template=IMG_CAPTCHA)
        {

        //Wenn kein Text vorgegeben wurde, einen erzeugen
        if ($text=="")
            {
            $text=strtoupper(crypt_create_password(6));
            
            //U und V sind schwer zu unterscheiden, daher nehmen wir sie raus
            $text=str_replace("V","T",$text);
            $text=str_replace("U","Z",$text);
            }

        $id=crypt_create_hash($text);

        //Grafik-Support ?
        if (GD_SUPPORT)
            {
            //Bild laden
            $img_handle = imagecreatefrompng($template);
            //Breite des Textes
            $width=( imagesx($img_handle)- ( imagefontwidth(5)*strlen($text) )  ) >> 1;

            //Text rein
            imagestring($img_handle,5,$width,2,$text,imagecolorallocate($img_handle, 255, 255, 255));

            //Abspeichern
            $file=md5($text).".png";
            imagepng($img_handle,DIR_IMG_TEMP.$file);

            $this->image(URL_IMG_TEMP.$file,"eatthis");
            
            imagedestroy($img_handle);
            }
        else
            {
            $this->write($text);
            }

        //Und die HTML-Teile einfügen
        $this->data_hidden("capid",$id);
        $this->input_text("capval","",10);
        }



    function toggle_progress()
        {
        $this->write("<script language=\x22javascript\x22>");
        
        $this->write("</script>");
        }

//////////////////////////////////////////////////////////////////////////
/// Testfunktion
//////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    //Testfunktion
    //Hat hier keine Funktion
    function test_sub($style)
        {
        //Alle Templates durchtesten
        $this->pre_out("test-text mit style small",$style);

        $this->write("fieldset ohne größenangaben");
        $this->field_open("test",$style);
        $this->write("im fieldset");
        $this->field_close();

        $this->write("fieldset mit größenangaben");
        $this->field_open("test",$style,"70px","70px");
        $this->write("im fieldset 70x70");
        $this->field_close();

        $this->write("fieldset mit größenangaben");
        $this->field_open("test",$style,"70px","70px");
        $this->write("im fieldset 70x70");
        $this->field_close();

        $this->write("div ohne größenangaben");
        $this->div_open("test",$style);
        $this->write(TEXT_BLIND);
        $this->div_close();

        $this->write("div mit größenangaben");
        $this->div_open("test",$style,"70px","70px");
        $this->write("im div 70x70");
        $this->div_close();

        $this->write("formulare");
        $this->form_open();
        $this->dropdown(array("eintrag1","default","eintrag3"),"default","dropdown",$style);
        $this->button_standard("Button","mybutton","myvalue",$style);
        $this->button_submit  ("Submit","mybutton","myvalue",$style);

        $this->nextline();

        $this->input_text("meintext","vordefinierter text",30,"tiny");
        $this->nextline();
        $this->input_password("meinpasswort","vordefinierter text",30,$style);
        $this->nextline();

        $this->form_close();

        $this->write("loginbox");
        $this->field_open("login",$style,"100px","100px");
        $this->loginbox("small");
        $this->field_close();

        $this->write("link");
        $this->link_open("http://www.google.de","alternativ-text","",$style);
        $this->write("honk");
        $this->link_close();

        $this->write("ein bild");

        $this->link_open("http://www.google.de","ab nach google","_blank",$style);
        $this->image("http://www.google.de/intl/de_de/images/logo.gif","Google-Logo",$style);
        $this->link_close();

        //Tabelle
        for ($x=1; $x <= 10; $x++)
            {
            for ($y=1; $y <= 10; $y++)
                {
                $data[$x][$y]=crypt_create_password(6);
                }
            }
        $this->table($data,"100%");

        }

    function test ()
        {
        //Einmal mit normalen Style
        $this->test_sub("");
        }
    }

</script>