<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Styler für CSS-Dateien.
/// Liest Vorgaben aus einer Ini-Datei ein und erzeugt CSS-Dateien
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_CSS_STYLER","class_cssstyler");
define ("CLASS_CSS_STYLER_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_CSS_STYLER,"version ".CLASS_CSS_STYLER_VERSION);

//Ein paar interne defines
//Einrücken in der Ausgabedatei
define ("CSS_PADDING"       ,20);
define ("CSS_MARGIN"        ,5);


//Die Ini-Klasse ggf. nachladen
require_once("class.ini.php");
//Die Mathematik nachladen (für Gaussverteilung)
require_once("lib.math.php");

//Farbfunktionen nachladen (Für RGB-Structs)
require_once("lib.colors.php");


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class css
    {
    var $topcolor = FALSE;
    var $midcolor = FALSE;
    var $basecolor= FALSE;

    //Alle Zugelassenen Events für CSS
    var $internal_events=array("hover");

    //Alle zugelassenen Attribute
    var $internal_attributes=array();

    //Defaultwerte für einige Attribute
    var $internal_defaults=array();

    //Puffer für die Einstellungen
    var $internal_ini  =FALSE;
    

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function css($inifile=FALSE)
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        
        //Ini erzeugen
        $this->internal_ini= new ini();
        $this->internal_ini->open($inifile);
        $this->internal_ini->sort();
        }

    //Destruktor
    function destroy()
        {
        if (is_object($this->internal_ini))
            {
            $this->internal_ini->destroy();
            }
        }

    //Verbindungsaufbau
    function open($inifile)
        {
        }

    //Verbindungsabbau
    //wird hier nicht benötigt
    function close()
        {
        return(TRUE);
        }
    
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        if (is_object($this->internal_ini))
            {
            $this->internal_ini->setdefault();
            $this->midcolor->setrgb(128,128,128);
            $this->basecolor->setrgb(0,0,0);
            $this->topcolor->setrgb(255,255,255);
            }
        else
            {
            $this->internal_ini = new ini();
            $this->topcolor=new rgb(255,255,255);
            $this->midcolor=new rgb(128,128,128);
            $this->basecolor=new rgb(0,0,0);
            }
        $internal_defaults=array();
        }
    
    //Datenbank und alles andere erzeugen
    //wird hier nicht benötigt
    function install()
        {
        return(TRUE);
        }
    
    //Datenbank und alles andere zerstören
    //wird hier nicht benötigt
    function uninstall()
        {
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Die aktuelle CSS-Datei als String zurückgeben
    function get()
        {
        //Shades der aktuellen Basisfarbe erstellen
        $colors=$this->htmlshades($this->midcolor->r,$this->midcolor->g,$this->midcolor->b);

        //Alle Sektionen lesen
        $sections=$this->internal_ini->read_sections();
        
        $margin=str_pad("",CSS_MARGIN," ");
        
        $open =$margin."{\r\n";
        $close=$margin."}\r\n";

        
        $result="";
        foreach ($sections as $section)
            {
            //Für jeden Tag die Keys lesen
            $keys=$this->internal_ini->read_keys($section);
            $result.=strtolower($section)."\r\n".$open;
            
            //Alle Keys durchgehen
            foreach($keys as $key)
                {
                if ($key!="")
                    {
                    $value=$this->internal_ini->read_value($section,$key,"?");
                    if ($value!="?")
                        {
                        if (preg_match("/%colorshade([0-9]+?)%/",$value,$regresult)==1)
                            {
                            $value=$colors[intval($regresult[1])];
                            }

                        $result.=$margin.str_pad($key,CSS_PADDING," ",STR_PAD_RIGHT).": ".$value.";\r\n";
                        }
                    }
                }
            $result.=$close;
            }
        return($result);
        }

    //Den Style abspeichern
    function write($filename)
        {
        $fp=fopen($filename,"w+");
        fputs($fp,$this->get());
        fclose($fp);
        }

    //Einen Styletag zufügen
    //Dieser wird einfach der Ini als Key zugefügt
    function addtag($tagname,$class=FALSE,$event=FALSE)
        {
        //Klasse anhängen ?
        if ($class!==FALSE)
            {
            $tagname=$tagname.".".$class;
            }
            
        //Event anhängen ?
        if ($this->_isevent($event))
            {
            $tagname=$tagname.":".$event;
            }
            
        //Und in die Ini einhängen
        $this->internal_ini->write_value($tagname,"","");

        //Zurück kommt der Tagname
        return($tagname);
        }
        
    //Einem Tagnamen einen Wert zuweisen
    //ist asdefault TRUE, wird dieser Wert in das Defaultarray übernommen und
    //bei leeren Werten für ein gleichnamiges Attribut eingefügt
    function setattribute($tagname,$attribute,$value,$asdefault=FALSE)
        {
        $result=FALSE;
        
        $value=strval($value);
        
        //Default speichern
        if ( $asdefault && ($value!=""))
            {
            $this->internal_defaults[$attribute]=$value;
            }
        
        //Bei einem leeren Wert den Defaultwert laden
        if ( ($value=="") && isset($this->internal_defaults[$attribute]))
            {
            $value=$this->internal_defaults[$attribute];
            }
        
        if ($value!="")
            {
            $result=$this->internal_ini->write_value($tagname,$attribute,$value);
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Interne Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Prüft ob die Eingabe ein zugelassenes Event ist
    function _isevent($event)
        {
        return(array_search(strtolower($event),$this->internal_events)!==FALSE);
        }
    //Prüft ob die Eingabe ein zugelassenes Attribut ist
    function _isattribute($attribute)
        {
        return(array_search(strtolower($attribute),$this->internal_attributes)!==FALSE);
        }

    //Eine Liste mit HTML-Codes für Farbschattierungen erhalten
    function htmlshades($r,$g,$b,$shades=20)
        {
        $result=array();
        
        //Von Schwarz zur Farbe
        for ($index=0; $index < 10;$index++)
            {
            $result[]=colorgradient(0,10,$index,$this->basecolor->html(),colorimplode(array($r,$g,$b)));
            }

        //Und von der Farbe zu Weiß
        for ($index=0; $index < 10;$index++)
            {
            $result[]=colorgradient(0,9,$index,colorimplode(array($r,$g,$b)),$this->topcolor->html());
            }
        return($result);
        }
    }
</script>