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
/// {{var:name}}          Es wird der Wert des Eintrages name eingefügt
///
/// {{include:filename}}     fügt eine Datei an dieser Stelle ein
/// {{varinclude:filename}}  fügt eine Datei aus dem Variablennamen filename an dieser Stelle ein
/// {{include:url}}          fügt eine Webseite an dieser Stelle ein
///
///
/// {{array:name}}        fügt für jeden Wert in Array name die Zeichen zwischen den Arraytags ein
/// <b>{{key}}</b>        und ersetzt key und value durch die entsprechenden Arraydaten
/// => {{val}}<br>
/// {{name}}
///
/// {{bool:name}}         Gibt je nach Bool-Wert von name die Ausgabe aus oder nicht
/// Ausgabe
/// {{name}}
///
/// {{var:cached}}        Zeitpunkt, wann die Datei gecached wurde
/// {{var:renderversion}} Version der Renderengine
/// {{var:renderengine}}  Name der Renderengine
///
/// {{sys:nocache}}       Deaktiviert Caching für diese Seite
/// {{sys:recursion}}     Aktiviert rekursive Verarbeitung von eingebundenen Dateien
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
///$render->cachetimeout=3600;                    //Verfallsdatum für gepufferte Seiten in Sekunden
///$render->basepath="Pfad mit meinen Templates"; //Basispfad angeben
///if ($render->iscached("dateiname.template") != TRUE) //Muß die Seite neu erzeugt werden ?
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
///Es kann im Prinzip eine beliebie Engine benutzt werden. Sie muß lediglich folgende Methoden
///unterstützen. Evtl. ist ein Wrapper notwendig.
///
///iscached("dateiname"):boolean;
///load("dateiname"):dateiinhalt;
///save("dateiname","lebensdauer","Dateiinhalt");
///
////////////////////////////////////////////////////////////////////////////////////////////////////

require_once("conf.classes.php");

if (defined("PATH_TEMPLATES")==FALSE)
    {
    define ("PATH_TEMPLATES","./templates/");
    }

//Die Regulären Ausdrücke bauen
define ("TEMPLATE_REG_VAR"        ,"@{{var:([a-zA-Z0-9_]+?)}}@");
define ("TEMPLATE_REG_SYSTEM"     ,"@{{sys:([a-zA-Z0-9_]+?)}}@");
define ("TEMPLATE_REG_INCLUDE"    ,"@{{include:([a-zA-Z0-9\\.\\_\\-\\/]+?)}}@");
define ("TEMPLATE_REG_VARINCLUDE" ,"@{{varinclude:([a-zA-Z0-9\\.\\_\\-\\/]+?)}}@");
define ("TEMPLATE_REG_BOOL"       ,"@{{bool:([a-zA-Z0-9_]+?)}}([\\w\\W]*?){{\\1}}@");
define ("TEMPLATE_REG_ARRAY"      ,"@{{array:([a-zA-Z0-9_]+?)}}([\\w\\W]*?){{\\1}}@");

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
    var $_burned  = FALSE;

    //für rpc exportierte funktionen
    var $export      = array();

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
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "unimatrix";      //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]      = "unimatrix";      //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.2";            //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;            //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;            //Wird eine Datenbank benötigt
        $result[CLASS_INDEX_CLEANUP]   = FALSE;            //Soll die Datenbank initialisiert werden ?
        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;             //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;            //Soll die Datenbank komprimiert werden (gz)
        $result[CLASS_INDEX_ENCRYPTED] = FALSE;            //Soll die Datenbank verschlüsselt werden?
        $result[CLASS_INDEX_RUNLEVEL]  = 11;               //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }

    //Hier können bei der Installation Daten in die Registry geschrieben werden
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

      //Rekursion ist per default aus
      $this->_system["recursion"] = FALSE;
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
        if ($this->cacheengine != FALSE)
            {
            $result=$this->cacheengine->iscached($id);
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Templatedatei laden und parsen
    //Unter der ID wird die Datei abgelegt und über iscached kann geprüft werden, ob sie noch aktiv ist
    function render($id,$templatefile)
        {
        $result=FALSE;
        
       //Optionen zurücksetzen
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
    //Die Seite rendern (der Buffer muß schon gefüllt sein)
    function _render($templatefile)
        {
        //Datei laden
        $this->_buffer=file_get_contents($templatefile);

        do
          {
          //
          $this->_burned=FALSE;

          //Alle Includes verarbeiten, damit diese mitgeparst werden
          $this->_buffer=$this->processvarincludes($this->_buffer);
        
          $this->_buffer=$this->processincludes($this->_buffer);

          //Alle Bools, da diese über Sichtbarkeit entscheiden
          $this->_buffer=$this->processboolean($this->_buffer);

          //Und danach der Rest
          $this->_buffer=$this->processarrays($this->_buffer);
          $this->_buffer=$this->processvars($this->_buffer);

          //Alle Systembefehle holen
          $this->_buffer=$this->processsys($this->_buffer);
        
          //Solange parsen, bis alles erledigt ist
          } while ( ($this->_burned==TRUE) && ($this->_system["recursion"]==TRUE) );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Wir lösen einfach das Token auf und ersetzen es durch {{include:variable}}
    function processvarincludes($input)
        {
        $result=array();
        if (preg_match_all(TEMPLATE_REG_VARINCLUDE,$input,$result)>0)
            {
            $replaces=reset($result);
            $vars    =next($result);
            foreach ($vars as $index=>$var)
                {
                //Variable existiert ?
                if (isset($this->replace[$var])==TRUE)
                    {
                    $input = str_replace($replaces[$index],"{{include:".$this->replace[$var]."}}",$input);
                    }
                else
                    {
                    $input = str_replace($replaces[$index],"[<b>".$var." not assigned</b>]",$input);
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Extrahiert alle include Tokens und fügt die Dateien ein
    function processincludes($input)
        {
        $result=array();
        //Alle Includes rausholen
        if (preg_match_all(TEMPLATE_REG_INCLUDE,$input,$result)>0)
            {
            //Ein Replace ist angefordert
            $this->_burned=TRUE;

            $replaces=reset($result);
            $vars    =next($result);
              
            foreach ($vars as $index=>$var)
                {
                if (file_exists($this->basepath.$var)==TRUE)
                    {
                    //Rekursionsvariante
                    //$input = str_replace($replaces[$index],$this->processincludes(file_get_contents($this->basepath.$var)),$input);

                    $input = str_replace($replaces[$index],file_get_contents($this->basepath.$var),$input);
                    }
                else
                    {
                    $input = str_replace($replaces[$index],"[<b>".$var." not found</b>]",$input);
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ersetzt alle Token durch ihren Wert
    function processvars($input)
        {
        $result=array();
        if (preg_match_all(TEMPLATE_REG_VAR,$input,$result)>0)
            {
            //Ein Replace ist angefordert
            $this->_burned=TRUE;
            
            $replaces=reset($result);
            $vars    =next($result);
            foreach ($vars as $index=>$var)
                {
                //Variable existiert ?
                if (isset($this->replace[$var])==TRUE)
                    {
                    $input = str_replace($replaces[$index],$this->replace[$var],$input);
                    }
                else
                    {
                    $input = str_replace($replaces[$index],"[<b>".$var." not assigned</b>]",$input);
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ersetzt alle Boolean durch ihren Wert
    function processboolean($input)
        {
        $result=array();
        if (preg_match_all(TEMPLATE_REG_BOOL,$input,$result)>0)
            {
            //Ein Replace ist angefordert
            $this->_burned=TRUE;
            
            $replaces = reset($result);    //Der kpl. Ausdruck
            $ids      = next($result);     //Die Variable
            $vals     = next($result);     //Der Inhalt

            //Alle Treffer durchgehen
            foreach ($replaces as $index => $replace)
              {
              //Variable gesetzt ?
              $id=$ids[$index];
              if (isset($this->replace[$id])==TRUE)
                    {
                    if ($this->replace[$id]==TRUE)
                        {
                        //Und rekursiv auflösen
                        $input = str_replace($replace,$this->processboolean($vals[$index]),$input);
                        }
                    else
                        {
                        $input = str_replace($replace,"",$input);
                        }
                    }
              else
                  {
                  $input = str_replace($replace,"[[".$id." not assigned]]",$input);
                  }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Verarbeitet alle Arrays
    function processarrays($input)
        {
        $result=array();
        if (preg_match_all(TEMPLATE_REG_ARRAY,$input,$result)>0)
            {
            //Ein Replace ist angefordert
            $this->_burned=TRUE;
            
            $replaces = reset($result);
            $ids      = next ($result);
            $vals     = next ($result);
            
            foreach ($replaces as $index=>$replace)
                {
                $val=$vals[$index];
                $id =$ids[$index];

                //Gibt es das Array ?
                if (isset($this->replace[$id])==TRUE)
                    {
                    if (is_array($this->replace[$id])==TRUE)
                      {
                      //Dann den Replacetext aufbauen
                      $newvalue="";
                      //Für jeden Eintrag im Array die Variablen setzen
                      $count=1;
                      foreach ($this->replace[$id] as $key => $value)
                        {
                        //Für jedes Paar einen Datensatz anlegen
                        $keyname=$id."_key".$count;
                        $valname=$id."_val".$count;

                        $this->assign($keyname,$key);
                        $this->assign($valname,$value);

                        //Den neuen Eintrag für den Buffer aufbauen
                        $temp=str_replace("{{key}}","{{var:".$keyname."}}",$val);
                        $temp=str_replace("{{val}}","{{var:".$valname."}}",$temp);
                        $newvalue.=$temp;

                        $count++;
                        }
                      //Ersetzen
                      $input = str_replace($replace,$newvalue,$input);
                      }
                    else
                      {
                      $input = str_replace($replace,"[<b>".$id." not an array</b>]",$input);
                      }
                    }
                else
                    {
                    $input = str_replace($replace,"[<b>".$id." not assigned</b>]",$input);
                    }
                }
            }
        return($input);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Systembefehle extrahieren und im _system-array setzen
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    function processsys($input)
        {
        $result=array();
        if (preg_match_all(TEMPLATE_REG_SYSTEM,$input,$result)>0)
            {
            $replaces=reset($result);
            $vars    =next ($result);
            foreach ($vars as $index=>$var)
                {
                //Zustand merken
                $this->_system[$var]=TRUE;
                
                //System Variablen werden nicht angezeigt
                $input = str_replace($replaces[$index],"",$input);
                }
            }
        return($input);
        }
    }
</script>