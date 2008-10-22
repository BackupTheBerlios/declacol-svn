<script language="php">
////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Interne Templateengine
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///
///
/// {{var:name}}          Es wird der Wert des Eintrages name eingef�gt
///
/// {{include:filename}}  f�gt ein Datei an dieser Stelle ein
/// {{include:url}}       f�gt eine Webseite an dieser Stelle ein
///
/// {{array:name}}        f�gt f�r jeden Wert in Array name die Zeiche zwischen den Arraytags ein
/// <b>{{key}}</b>        und ersetzt key und value durch die entsprechenden Arraydaten
/// => {{val}}<br>
/// {{array}}
///
/// {{bool:name}}         Gibt je nach Bool-Wert von name die Ausgabe aus oder nicht
/// Ausgabe
/// {{bool}}
///
/// {{var:cached}}        Zeitpunkt, wann die Datei gecached wurde
/// {{var:renderversion}} Version der Renderengine
/// {{var:renderengine}}  Name der Renderengine
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");


//Die Regul�ren Ausdr�cke bauen
define ("TEMPLATE_REG_VAR"      ,"@{{var:[a-zA-Z0-9_]+?}}@");
define ("TEMPLATE_REG_SYSTEM"   ,"@{{sys:[a-zA-Z0-9_]+?}}@");
define ("TEMPLATE_REG_INCLUDE"  ,"@{{include:[a-zA-Z0-9\\.\\_\\-\\/]+?}}@");
define ("TEMPLATE_REG_BOOL"     ,"@{{bool:[a-zA-Z0-9_]+?}}[\\w\\W]*?{{bool}}@");
define ("TEMPLATE_REG_ARRAY"    ,"@{{array:[a-zA-Z0-9_]+?}}[\\w\\W]*?{{array}}@");

//Eigentliche Klasse
class unimatrix
    {
    var $basepath     = PATH_TEMPLATES;

    //Array mit allen zu ersetzenden Werten
    var $replace       = array();

    //Soll die Cachingengine benutzt werden
    var $cacheengine  = FALSE;
    var $cachetimeout = 10; //In Sekunden

    //Private Data
    var $_buffer  = "";

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function unimatrix()
        {
        //Vordefinierte Wert setzen
        $result=$this->install();
        $this->assign("cached",date("d.m.Y H:i:s"));
        $this->assign("renderversion",$result[CLASS_INDEX_VERSION]);
        $this->assign("rendername"   ,$result[CLASS_INDEX_NAME]);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zur�ck
    function install()
        {
        $result[CLASS_INDEX_ID]        = "unimatrix";      //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]      = "unimatrix";      //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";            //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;            //Wird eine Registry ben�tigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;            //Wird eine Datenbank ben�tigt
        $result[CLASS_INDEX_CLEANUP]   = FALSE;            //Soll die Datenbank initialisiert werden ?
        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;             //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;            //Soll die Datenbank komprimiert werden (gz)
        $result[CLASS_INDEX_RUNLEVEL]  = 10;               //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einem Variablennamen einen Wert zuweisen
    function assign($varname,$value)
        {
        $this->replace[$varname]=$value;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Checken, ob eine Seite gecached ist
    function iscached($page)
        {
        $result=FALSE;
        if ($this->cacheengine!=FALSE)
            {
            $result=$this->cacheengine->iscached($this->basepath.$page);
            }
        return($result);
        }


    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Templatedatei laden und parsen
    function render($templatefile)
        {
        $result=FALSE;
        
        //Datei lesen
        if (file_exists($this->basepath.$templatefile) == TRUE)
            {
            if ($this->cacheengine!=FALSE)
                {
                //Cache holen
                $this->_buffer = $this->cacheengine->load($this->basepath.$templatefile);

                //Noch kein Cache vorhanden ?
                if ($this->_buffer == FALSE)
                    {
                    //Dann rendern und anlegen
                    $this->_render($templatefile);
                    $this->cacheengine->save($this->basepath.$templatefile,$this->cachetimeout,$this->_buffer);
                    }
                }
            else
                {
                $this->_render($templatefile);
                }
            }
        else
            {
            $result=FALSE;
            }

        echo $this->_buffer;
            
        return($result);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Seite rendern (der Buffer mu� schon gef�llt sein)
    function _render($templatefile)
        {
        //Seite rendern
        $this->_buffer=file_get_contents($this->basepath.$templatefile);

        //Alle Includes verarbeiten, damit diese mitgeparst werden
        $this->processincludes();

        //Alle Bools, da diese �ber Sichtbarkeit entscheiden
        $this->processboolean();
        $this->processarrays();
        $this->processvars();
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Extrahiert alle include Tokens und f�gt die Dateien ein
    function processincludes()
        {
        //Alle Includes rausholen
        if (preg_match_all(TEMPLATE_REG_INCLUDE,$this->_buffer,$result)>0)
            {
            foreach (reset($result) as $include)
                {
                $file=$this->extractvalue($include);

                if (file_exists($this->basepath.$file)==TRUE)
                    {
                    $this->_buffer = str_replace($include,file_get_contents($this->basepath.$file),$this->_buffer);
                    }
                else
                    {
                    $this->_buffer = str_replace($include,"[<b>".$file." not found</b>]",$this->_buffer);
                    }
                }
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ersetzt alle Token durch ihren Wert
    function processvars()
        {
        if (preg_match_all(TEMPLATE_REG_VAR,$this->_buffer,$result)>0)
            {
            foreach (reset($result) as $include)
                {
                $name=$this->extractvalue($include);
                //Variable existiert ?
                if (isset($this->replace[$name])==TRUE)
                    {
                    $this->_buffer = str_replace($include,$this->replace[$name],$this->_buffer);
                    }
                else
                    {
                    $this->_buffer = str_replace($include,"[<b>".$name." not assigned</b>]",$this->_buffer);
                    }
                }
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ersetzt alle Boolean durch ihren Wert
    function processboolean()
        {
        if (preg_match_all(TEMPLATE_REG_BOOL,$this->_buffer,$result)>0)
            {
            foreach (reset($result) as $include)
                {
                $id=substr($include,0,strpos($include,"}}")+2);
                $value=trim(substr($include,strlen($id),strlen($include) - strlen($id) - strlen("{{bool}}") ) );
                
                //Variable gesetzt ?
                $id=$this->extractvalue($id);
                if (isset($this->replace[$id])==TRUE)
                    {
                    if ($this->replace[$id]==TRUE)
                        {
                        $this->_buffer = str_replace($include,$value,$this->_buffer);
                        }
                    else
                        {
                        $this->_buffer = str_replace($include,"",$this->_buffer);
                        }
                    }
                }
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Verarbeitet alle Arrays
    function processarrays()
        {
        if (preg_match_all(TEMPLATE_REG_ARRAY,$this->_buffer,$result)>0)
            {
            foreach (reset($result) as $include)
                {
                //Alle wichtigen Werte rausziehen
                $id=substr($include,0,strpos($include,"}}")+2);
                $value=trim(substr( $include,strlen($id),strlen($include) - strlen($id) - strlen("{{array}}") ));
                $name=$this->extractvalue($id);

                //Gibt es das Array ?
                if (isset($this->replace[$name])==TRUE)
                    {
                    $newvalue="";
                    //F�r jeden Eintrag im Array die Variablen setzen
                    $count=1;
                    foreach ($this->replace[$name] as $key => $val)
                        {
                        //F�r jedes Paar einen Datensatz anlegen
                        $keyname=$name."_key".$count;
                        $valname=$name."_val".$count;

                        $this->assign($keyname,$key);
                        $this->assign($valname,$val);

                        //Den neuen Eintrag f�r den Buffer aufbauen
                        $temp=str_replace("{{key}}","{{var:".$keyname."}}",$value);
                        $temp=str_replace("{{val}}","{{var:".$valname."}}",$temp);
                        $newvalue.=$temp;

                        $count++;
                        }
                    //Ersetzen
                    $this->_buffer = str_replace($include,$newvalue,$this->_buffer);
                    }
                else
                    {
                    $this->_buffer = str_replace($include,"[<b>".$name." not assigned</b>]",$this->_buffer);
                    }
                }
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Den Wert aus einem Token extrahieren
    function extractvalue($input)
        {
        $input=substr($input,strpos($input,":")+1,strlen($input) );
        $input=substr($input,0,strpos($input,"}}"));
        return($input);
        }
    }
</script>