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
/// Klasse um alle anderen Klassen zu laden
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///Der Classloader initialisert automatisch alle mit ClassInst registrierten Klassen
///
///Beispiel
///
///$loader=new classload($pathtoregistry);
///$loader->load("session"); //Startet die Klasse "session";
///$loader->load();          //Startet alle registrierten Klassen in der Reihenfolge ihres runlevels
///
///Hier sind die Klassen durch $CLASSES["klassenname"]->methoden verfügbar
///
///$loader->destroy();       //Alle Klassen zerstören und deren Registries abspeichern
///
///Ale geladenen Klassen bekommen ein Array mit allen geladenen Klassen in die Eigenschaft "uplink"
///eingeblendet.
///Damit erfolgt der Zugriff auf eine Klasse über $this->uplink["klassenname"]->methodenname
///
////////////////////////////////////////////////////////////////////////////////////////////////////

require_once("conf.classes.php");
require_once("class.registry.php");

//Ein paar wichtige Bibliotheken laden
require_once(PATH_LIBS."lib.classinterface.php");
require_once(PATH_LIBS."lib.strings.php");

//Alle Klassen werden hier eingetragen
$CLASSES = array();

//Eigentliche Klasse
class classload
    {
    var $_regpath   = FALSE;
    var $_registry  = FALSE;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function classload($regpath)
        {
        $this->_regpath=$regpath;
        $this->_registry= new registry($this->_regpath."classes.reg",FALSE);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        if ( $this->_registry !== FALSE )
            {
            //Alle geladenen Klassen zerstören
            $this->unload();
            
            //Und den Speicher freigeben
            $this->_registry->destroy();
            unset($this->_registry);
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alle verfügbaren Klassen laden
    function load($classname=FALSE)
        {
        //Wenn ein Klassenname angegeben ist, nur diese Klasse instanzieren
        if ($classname !== FALSE)
            {
            $this->_create($classname);
            }
        else
            {
            //Alle Runlevel holen
            $runlevels=$this->_registry->enum("classes/");

            foreach ($runlevels as $runlevel => $dummy)
                {
                if (DEBUG) callmethod("debug","addlog","classload","entering runlevel [".$runlevel."]");

                $classes=$this->_registry->enum("classes/".$runlevel."/");
                
                //Alle Klassen holen
                foreach ($classes as $key => $data)
                    {
                    if (is_array($data))
                        {
                        $this->_create($runlevel,$key);
                        }
                    }
                }
            }
        if (DEBUG) callmethod("debug","addlog","classload","done");
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alle verfügbaren Klassen laden
    function unload($classname=FALSE)
        {
        //Wenn ein Klassenname angegeben ist, nur diese Klasse instanzieren
        if ($classname !== FALSE)
            {
            $this->_destroy($classname);
            }
        else
            {
            //Alle Runlevel holen
            $runlevels=$this->_registry->enum("classes/");

            //Zerstören geht natürlich umgekehrt
            $runlevels=array_reverse(array_keys($runlevels));

            foreach ($runlevels as $runlevel)
                {
                if (DEBUG) callmethod("debug","addlog","classload","entering runlevel [".$runlevel."]");

                $classes=$this->_registry->enum("classes/".$runlevel."/");

                //Alle Klassen holen
                foreach ($classes as $key => $data)
                    {
                    if (is_array($data))
                        {
                        $this->_destroy($key);
                        }
                    }
                }
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Klasse laden
    function _create($runlevel,$classname)
        {
        global $CLASSES;
        
        if (DEBUG) callmethod("debug","addlog","classload","creating ".$classname." [rl_".$runlevel."]");
        
        $classdata=$this->_registry->enum("classes/".$runlevel."/".$classname);
        
        require_once(PATH_CLASSES.$classdata[CLASS_INDEX_CLASSFILE]);

        if ($classdata[CLASS_INDEX_REGISTRY] == TRUE)
            {
            if (DEBUG) callmethod("debug","addlog","classload","loading registry ".$classdata[CLASS_INDEX_REGISTRYFILE]);

            //ggf. eine Registry erzeugen
            $registry = new registry ( PATH_REGISTRY.$classdata[CLASS_INDEX_REGISTRYFILE],
                                       $classdata[CLASS_INDEX_COMPRESSED]);

            $object = new $classdata[CLASS_INDEX_CLASSNAME]($registry);
            }
        else
            {
            $object = new $classdata[CLASS_INDEX_CLASSNAME]();
            }

        //Objekt global veröffentlichen
        $CLASSES[$classname]=$object;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Klasse zerstören
    function _destroy($classname)
        {
        global $CLASSES;
        
        if (isset($CLASSES[$classname])==TRUE)
            {
            if (DEBUG) callmethod("debug","addlog","classload","destroying ".$classname);

            $CLASSES[$classname]->destroy();
            unset($CLASSES[$classname]);
            }
        }
    }
</script>