<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Interne Templateengine
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///
///
/// {{var:name}}          Es wird der Wert des Eintrages name eingef�gt
///
/// {{include:filename}}     f�gt eine Datei an dieser Stelle ein
/// {{varinclude:filename}}  f�gt eine Datei aus dem Variablennamen filename an dieser Stelle ein
/// {{include:url}}          f�gt eine Webseite an dieser Stelle ein
///
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
/// {{sys:nocache}}       Deaktiviert Caching f�r diese Seite
///
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///Beispiel
///Ohne Caching
///$render=new unimatrix();                       //Templater initialisieren
///$render->basepath="Pfad mit meinen Templates"; //Basispfad angeben
///$render->assign("username","Mein Name");       //{{var:username}} wird im Template durch "Mein Name" ersetzt
///$render->render("dateiname.template");         //Seite ausgeben
///
///Mit Caching
///$render=new unimatrix();                       //Templater initialisieren
///$render->cacheengine=new cache();              //Cacheengine klarmachen (Siehe Hinweise unten)
///$render->cachetimeout=3600;                    //Verfallsdatum f�r gepufferte Seiten in Sekunden
///$render->basepath="Pfad mit meinen Templates"; //Basispfad angeben
///if ($render->iscached("dateiname.template") != TRUE) //Mu� die Seite neu erzeugt werden ?
///   {
///   $render->assign("username","Mein Name");       //{{var:username}} wird im Template durch "Mein Name" ersetzt
///   }
///$render->render("dateiname.template");         //Seite ausgeben
///$render->cacheengine->destroy();
///$render->destroy();
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///Hinweise zur Cacheengine
///
///Es kann im Prinzip eine beliebie Engine benutzt werden. Sie mu� lediglich folgende Methoden
///unterst�tzen. Evtl. ist ein Wrapper notwendig.
///
///iscached("dateiname"):boolean;
///load("dateiname"):dateiinhalt;
///save("dateiname","lebensdauer","Dateiinhalt");
///
////////////////////////////////////////////////////////////////////////////////////////////////////

require_once("conf.classes.php");


//Die Regul�ren Ausdr�cke bauen
define ("TEMPLATE_REG_VAR"        ,"@{{var:[a-zA-Z0-9_]+?}}@");
define ("TEMPLATE_REG_SYSTEM"     ,"@{{sys:[a-zA-Z0-9_]+?}}@");
define ("TEMPLATE_REG_INCLUDE"    ,"@{{include:[a-zA-Z0-9\\.\\_\\-\\/]+?}}@");
define ("TEMPLATE_REG_VARINCLUDE" ,"@{{varinclude:[a-zA-Z0-9\\.\\_\\-\\/]+?}}@");
define ("TEMPLATE_REG_BOOL"       ,"@{{bool:[a-zA-Z0-9_]+?}}[\\w\\W]*?{{bool}}@");
define ("TEMPLATE_REG_ARRAY"      ,"@{{array:[a-zA-Z0-9_]+?}}[\\w\\W]*?{{array}}@");

//Eigentliche Klasse
class unimatrix
    {
    var $basepath     = PATH_TEMPLATES;

    //Array mit allen zu ersetzenden Werten
    var $replace       = array();

    //Soll die Cachingengine benutzt werden
    var $cacheengine  = FALSE;
    var $cachetimeout = 3600; //In Sekunden

    //Private Data
    var $_buffer  = "";
    var $_system  = array();

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
        $result[CLASS_INDEX_VERSION]   = "0.2";            //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;            //Wird eine Registry ben�tigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;            //Wird eine Datenbank ben�tigt
        $result[CLASS_INDEX_CLEANUP]   = FALSE;            //Soll die Datenbank initialisiert werden ?
        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;             //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;            //Soll die Datenbank komprimiert werden (gz)
        $result[CLASS_INDEX_RUNLEVEL]  = 11;               //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }

    //Hier k�nnen bei der Installation Daten in die Registry geschrieben werden
    function preset(&$registry)
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Systemoptionen setzen
    function _resetsystem()
      {
      $this->_system=array();

      //Caching ist per default an
      $this->_system["nocache"] = FALSE;
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Zeiger auf This liefern
    function getthis()
      {
      $self=&$this;
      return($self);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einem Variablennamen einen Wert zuweisen
    function assign($varname,$value)
        {
        $this->replace[$varname]=$value;
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Checken, ob eine Seite gecached ist
    function iscached($id)
        {
        $result=FALSE;
        if ($this->cacheengine!=FALSE)
            {
            $result=$this->cacheengine->iscached($id);
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Templatedatei laden und parsen
    //Unter der ID wird die Datei abgelegt und �ber iscached kann gepr�ft werden, ob sie noch aktiv ist
    function render($id,$templatefile)
        {
        $result=FALSE;
        
       //Optionen zur�cksetzen
       $this->_resetsystem();
        
        //Pfad passend setzen
        $templatefile=$this->basepath.$templatefile;
        
        //Datei lesen
        if (file_exists($templatefile) == TRUE)
            {
            //Cache aktiviert ?
            if ($this->cacheengine!=FALSE)
                {
                //Cache holen
                $this->_buffer = $this->cacheengine->load($id);

                //Noch kein Cache vorhanden ?
                if ($this->_buffer == FALSE)
                    {
                    //Dann rendern und anlegen
                    $this->_render($templatefile);
                    
                    //Wenn die Seite caching deaktiviert, dann nicht abspeichern
                    if ( $this->_system["nocache"] != TRUE )
                      {
                      $this->cacheengine->save($id,$this->cachetimeout,$this->_buffer);
                      }
                    }
                }
            else
                {
                $this->_render($templatefile);
                }
            //Als Ergebnis die Seite liefern
            $result=$this->_buffer;
            }

        return($result);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Seite rendern (der Buffer mu� schon gef�llt sein)
    function _render($templatefile)
        {
        //Seite rendern
        $this->_buffer=file_get_contents($templatefile);

        //Alle Includes verarbeiten, damit diese mitgeparst werden
        $this->_buffer=$this->processvarincludes($this->_buffer);
        
        $this->_buffer=$this->processincludes($this->_buffer);

        //Alle Bools, da diese �ber Sichtbarkeit entscheiden
        $this->_buffer=$this->processboolean($this->_buffer);

        //Und danach der Rest
        $this->_buffer=$this->processarrays($this->_buffer);
        $this->_buffer=$this->processvars($this->_buffer);

        //Alle Systembefehle holen
        $this->_buffer=$this->processsys($this->_buffer);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Extrahiert alle include Tokens und f�gt die Dateien ein
    function processvarincludes($input)
        {
        //Alle Includes rausholen
        if (preg_match_all(TEMPLATE_REG_VARINCLUDE,$input,$result)>0)
            {
            foreach (reset($result) as $include)
                {
                $name=$this->extractvalue($include);
                
                //Existiert die Variable ?
                if (isset($this->replace[$name])==TRUE)
                    {
                    $file=$this->replace[$name];
                    }
                else
                    {
                    $file="var : ".$name;
                    }

                if (file_exists($this->basepath.$file)==TRUE)
                    {
                    $input = str_replace($include,file_get_contents($this->basepath.$file),$input);
                    }
                else
                    {
                    $input = str_replace($include,"[<b>".$file." not found</b>]",$input);
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Extrahiert alle include Tokens und f�gt die Dateien ein
    function processincludes($input)
        {
        //Alle Includes rausholen
        if (preg_match_all(TEMPLATE_REG_INCLUDE,$input,$result)>0)
            {
            foreach (reset($result) as $include)
                {
                $file=$this->extractvalue($include);

                if (file_exists($this->basepath.$file)==TRUE)
                    {
                    $input = str_replace($include,file_get_contents($this->basepath.$file),$input);
                    }
                else
                    {
                    $input = str_replace($include,"[<b>".$file." not found</b>]",$input);
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ersetzt alle Token durch ihren Wert
    function processvars($input)
        {
        if (preg_match_all(TEMPLATE_REG_VAR,$input,$result)>0)
            {
            foreach (reset($result) as $include)
                {
                $name=$this->extractvalue($include);
                //Variable existiert ?
                if (isset($this->replace[$name])==TRUE)
                    {
                    $input = str_replace($include,$this->replace[$name],$input);
                    }
                else
                    {
                    $input = str_replace($include,"[<b>".$name." not assigned</b>]",$input);
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ersetzt alle Boolean durch ihren Wert
    function processboolean($input)
        {
        if (preg_match_all(TEMPLATE_REG_BOOL,$input,$result)>0)
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
                        $input = str_replace($include,$value,$input);
                        }
                    else
                        {
                        $input = str_replace($include,"",$input);
                        }
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Verarbeitet alle Arrays
    function processarrays($input)
        {
        if (preg_match_all(TEMPLATE_REG_ARRAY,$input,$result)>0)
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
                    $input = str_replace($include,$newvalue,$input);
                    }
                else
                    {
                    $input = str_replace($include,"[<b>".$name." not assigned</b>]",$input);
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Systembefehle extrahieren und im _system-array setzen
    function processsys($input)
        {
        if (preg_match_all(TEMPLATE_REG_SYSTEM,$input,$result)>0)
            {
            foreach (reset($result) as $include)
                {
                $name=$this->extractvalue($include);
                
                //Zustand merken
                $this->_system[$name]=TRUE;
                
                //Und auftreten ersetzen
                $input = str_replace($include,"",$input);
                }
            }
        return($input);
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