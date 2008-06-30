<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// WYSIWYG-HTML-Editor
///
///
///Prototypen
///
///constructor            //Initialisiert alle Daten mit Defaults
///destructor           //Entlädt alle wichtigen Speicherbereiche etc.
///open                 //Lädt notwendiges oder baut Verbindungen auf
///install()            //Erzeugt eine Datenbank (wenn für Klasse notwendig)
///uninstall()            //Verwirft die Datenbank der Klasse
///setdefault()            //Setzt alle internen Werte auf default
///close()                //Schließt alle Verbindungen
///
///Nameskonventionen
///
///add(mixed)            //Fügt Daten zu
///remove(mixed)        //Entfernt Daten
///clean(mixed)            //Räumt auf
///flush(mixed)            //Buffer ausgeben
///write(mixed)            //Daten ausgeben
///save(mixed)            //Daten in eine Datei schreiben
///load(mixed)            //Daten aus einer Datei lesen
///enumerate(mixed)        //Objekte als array ausgeben (z.B. alle User)
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_WYSIWYG_EDIT","class_wysiwyg_editor");
define ("CLASS_WYSIWYG_EDIT_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_WYSIWYG_EDIT,"version ".CLASS_WYSIWYG_EDIT_VERSION);

//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("MYCLASS_DEF1" ,1);
define ("MYCLASS_DEF2" ,2);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class wysiwyg_editor
    {
    //Öffentliche Eigenschaftem
    var $submiturl           ="./";     //Wohin sollen die Daten übertragen werden
    var $text                ="";       //Eingegebener Text
    var $readonly            =TRUE;     //Editor readonly ?
    var $width               =80;       //Weite des Eingabefensters
    var $height              =20;       //Höhe des Eingabefensters
    var $maxlen              =4096;     //Maximale Länge des Textes
    var $minlen              =0;        //Minmale Länge des Textes
    var $id                  =0;        //ID (Für Formularzugriff notwendig)
    var $alert               ="must be longer than %minlen% and shorter than %maxlen%";       //Text der bei fehlerhafter Eingabe angezeigt wird
    var $preset              ="";       //Vorgegebener Text
    var $buttonname          ="";       //Text auf dem Submitbutton
    var $addform             =FALSE;    //Sollen die Formulartags mitbenutzt werden ?

    //Flags für die Optionen
    var $option_font         =TRUE;
    var $option_img          =TRUE;
    var $option_link         =TRUE;
    var $option_color        =TRUE;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function wysiwyg_editor()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        
        //ID ist einfach eine Zufallszahl
        //Mit dieser ID wird das HTML-Formular markiert.
        //Da wohl kaum jemand mehr als 10 Eingaben nutzen wird,
        //ist das Risiko einer Kollision recht gering
        $this->id="wysedit_".(rand(0,65535));
        }

    //Destruktor
    function destroy()
        {
        }

    //Verbindungsaufbau
    function open()
        {
        }

    //Verbindungsabbau
    function close()
        {
        }
    
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->submiturl        ="./";
        $this->text             ="";
        $this->readonly         =FALSE;
        $this->width            =80;
        $this->height           =20;
        $this->maxlen           =4096;
        $this->minlen           =0;
        $this->preset           ="";
        $this->buttonname       ="submit";
        $this->alert="Ihre Eingabe muss mindestens %minlen% und darf maximal %maxlen% lang sein.";
        }
    
    //Datenbank und alles andere erzeugen
    function install()
        {
        return(TRUE);
        }
    
    //Datenbank und alles andere zerstören
    function uninstall()
        {
        return(FALSE);
        }

    //Den Code für das Javascript ausgeben
    function getscripts()
        {
        return($this->_createjavascript());
        }
        
    //Die Optionsbuttons ausgeben
    function getoptions()
        {
        return($this->_createbuttons());
        }

    //Den Editor ausgeben
    function geteditor()
        {
        return($this->_createinputarea($this->addform));
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Hier werden nach einnander die Einzelnen Teile zusammengesetzt
    function get()
        {
        $result="";
        $result.=$this->_createjavascript();
        $result.=$this->_createbuttons();
        $result.="<br/>";
        $result.=$this->_createinputarea(TRUE);
        return($result);
        }

    function _createbuttons()
        {
        $bt="";
        //Alle Fontbuttons einfügen
        if ($this->option_font)
            {
            $bt.=$this->addframebutton("B","[B]","[/B]");
            $bt.=$this->addframebutton("S","[S]","[/S]");
            $bt.=$this->addframebutton("L","[L]","[/L]");
            $bt.=$this->addframebutton("pre","[P]","[/P]");
            $bt.=$this->addframebutton("frm","[F]","[/F]");
            }

        //Farbauswahl
        if ($this->option_color)
            {
            $bt.=$this->addframebutton("blue","[BLUE]","[/BLUE]");
            $bt.=$this->addframebutton("red","[RED]","[/RED]");
            $bt.=$this->addframebutton("green","[GREEN]","[/GREEN]");
            $bt.=$this->addframebutton("black","[BLACK]","[/BLACK]");
            $bt.=$this->addframebutton("white","[WHITE]","[/WHITE]");
            }

        //Alle Imagebuttons einfügen
        if ($this->option_img)
            {
            $bt.=$this->addinsertbutton("Img","[IMG=url]");
            $bt.=$this->addinsertbutton("Img-L","[LIMG=url]");
            $bt.=$this->addinsertbutton("Img-R","[RIMG=url]");
            }

        //Links
        if ($this->option_link)
            {
            $bt.=$this->addinsertbutton("Link (internal)","[!LINK=url]Name[/LINK]");
            $bt.=$this->addinsertbutton("Link (external)","[LINK=url]Name[/LINK]");
            }

        //Sonderzeichen
        $bt.="<br/>";
        $bt.=$this->addinsertbutton("_","\\r\\n[HR]\\r\\n");
        $bt.=$this->addinsertbutton("&diams;","&diams;");
        $bt.=$this->addinsertbutton("&hearts;","&hearts;");
        $bt.=$this->addinsertbutton("&hearts;","&hearts;");
        
        return($bt);
        }


    function _createinputarea($includeform)
        {
        $frm="";
        if ($includeform)
            {
            $frm.="<form enctype=\"multipart/form-data\" ".
                  "action=\"".$this->submiturl."\" ".
                  "method=\"post\" ".
                  "onsubmit=\"return ValidateWysiwyg(this)\">";
            }

        $frm.="<textarea class=\"wysi_edit\" id=\"".$this->id."\" rows=\"".$this->height."\" cols=\"".$this->width."\">".$this->preset."</textarea>\n";

        if ($includeform)
            {
            $frm.="<br/><button type=\"submit\" name=\"".$this->buttonname."\" class=\"wysi_submit\"/>".$this->buttonname."</button>\n".
                  "</form>\n";
            }
        return($frm);
        }

    //Die Javascript-Funktionen einbetten
    function _createjavascript()
        {
        //Message initialisieren
        $message=str_replace("%minlen%",$this->minlen,$this->alert);
        $message=str_replace("%maxlen%",$this->maxlen,$message);

        //Alle Funktionen im String puffern
        $js="<script language=\"JavaScript\" type=\"text/javascript\">\n";

        //Länge des Eingegebenen Textes prüfen
        $js.="function ValidateWysiwyg(form)\n".
             "{\n".
             " var result = true;\n".
             " if (form.".$this->id.".value.length  < ".$this->minlen.") result=false;\n".
             " if (form.".$this->id.".value.length  > ".$this->maxlen.") result=false;\n".
             " if (result == false)\n".
             "  {\n".
             "  alert('".$message."');\n".
             "  }\n".
             " return result;\n".
             "}\n";
             
        //Eine Zeichenfolge am Cursor einfügen
        $js.="function InsertAtCursor(myField,myValue)\n".
             "{\n".
             " //IE support\n".
             " if (document.selection)\n".
             "  {\n".
             "  myField.focus();\n".
             "  sel = document.selection.createRange();\n".
             "  sel.text = myValue;\n".
             "  }\n".
             " //MOZILLA/NETSCAPE support\n".
             " else if (myField.selectionStart || myField.selectionStart == '0')\n".
             "  {\n".
             "  var startPos = myField.selectionStart;\n".
             "  var endPos = myField.selectionEnd;\n".
             "  myField.value = myField.value.substring(0, startPos)\n".
             "  + myValue\n".
             "  + myField.value.substring(endPos, myField.value.length);\n".
             "  myField.selectionEnd=endPos;".
             "  }".
             " else\n".
             "  {\n".
             "  myField.value += myValue;\n".
             "  }\n".
             " }\n";
        // calling the function
        //<textarea id="mytext" rows="8" cols="70"></textarea>
        //<button onclick="insertAtCursor(document.getElementById('mytext'),'Test text')">Add Test text</button>

        //Eine Markierte Zeichenfolge in Tags fassen
        $js.="function FrameAtCursor(myField,TagOpen,TagClose)\n".
             "{\n".
             " //IE support\n".
             " if (document.selection)\n".
             "  {\n".
             "  myField.focus();\n".
             "  sel = document.selection.createRange();\n".
             "  sel.text = TagOpen + sel.text + TagClose;\n".
             "  }\n".
             " //MOZILLA/NETSCAPE support\n".
             " else if (myField.selectionStart || myField.selectionStart == '0')\n".
             "  {\n".
             "  var startPos = myField.selectionStart;\n".
             "  var endPos   = myField.selectionEnd;\n".
             "  myField.value = myField.value.substring(0, startPos)\n".
             "                  + TagOpen\n".
             "                  + myField.value.substring(startPos,endPos - startPos)\n".
             "                  + TagClose\n".
             "                  + myField.value.substring(endPos, myField.value.length);".
             "  myField.selectionEnd=endPos;".
             "  }".
             " else\n".
             "  {\n".
             "  myField.value += TagOpen;\n".
             "  }\n".
             " }\n";

        $js.="</script>\n";
        return($js);
        }

    //Buttonhilfsfunktionen
    function addinsertbutton($name,$text)
        {
        return("<button onclick=\"InsertAtCursor(document.getElementById('".$this->id."'),'".$text."')\">".$name."</button>\n");
        }

    function addframebutton($name,$open,$close)
        {
        return("<button onclick=\"FrameAtCursor(document.getElementById('".$this->id."'),'".$open."','".$close."')\">".$name."</button>\n");
        }

    }

$edit=new wysiwyg_editor();
$edit->preset="Tach";
$edit->buttonname="Leckmich";
$edit->submiturl="./class.htmleditor.php";
$edit->minlen=0;
$edit->maxlen=255;

echo $edit->get();
echo "<pre>".htmlentities($edit->get());



</script>