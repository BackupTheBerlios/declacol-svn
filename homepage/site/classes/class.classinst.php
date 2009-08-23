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
/// Installiert alle Klassen die eine install-Methode anbieten
/// und registriert die Klassen in der classes.reg
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///Beispiel
///
///$inst = new classinst($registrydirectory);
///$inst->register($classfile,$classname,$classid)  //Eine Klasse registrieren
///$inst->registerall($pathtoclasses);              //Alle verfügbaren Klassen registrieren
///$inst->destroy();
///
///Damit sind alle relevanten Daten der Klassen in die classes.reg gespeichert.
///Mit dem ClassLoader können diese dann automatisch geladen werden
///
///Wichtig
///Jede automatisch zu registrierende Klasse muß folgen Methode besitzen :
///(Die Konstanten liegen in conf.classes.php
///    function install()
///        {
///        $result[CLASS_INDEX_ID]        = "templateid";      //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
///        $result[CLASS_INDEX_NAME]      = "templatename";    //Name der Klasse
///        $result[CLASS_INDEX_VERSION]   = "0.1";             //Version der Klasse
///        $result[CLASS_INDEX_REGISTRY]  = TRUE;              //Wird eine Registry benötigt
///        $result[CLASS_INDEX_DATABASE]  = FALSE;             //Wird eine Datenbank benötigt
///
///        $result[CLASS_INDEX_CLEANUP]   = TRUE;              //Soll die Datenbank initialisiert werden ?
///
///        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;              //Soll die Klasse beim Systemstart geladen werden ?
///
///        $result[CLASS_INDEX_COMPRESSED]= TRUE;              //Soll die Datenbank komprimiert werden (gz)?
///        $result[CLASS_INDEX_ENCRYPTED] = TRUE;              //Soll die Datenbank verschlüsselt werden (gz)?
///
///        $result[CLASS_INDEX_RUNLEVEL]  = 10;                //In welchen Runlevel soll die Klasse geladen werden
///
///        return($result);
///        }
///
///Erwartet die Klasse eine Registry muß der Konstruktor diese als Parameter erwarten 
///function myclass(&$myregistry)
///
////////////////////////////////////////////////////////////////////////////////////////////////////

//Datenbak brauchen wir auf jeden Fall
require_once("class.registry.php");

class classinst
    {
    var $_regpath   = FALSE;
    var $_registry  = FALSE;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function classinst($regpath)
        {
        $this->_regpath=$regpath;
        $this->_registry= new registry($this->_regpath."classes.reg",FALSE,FALSE);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        if ( $this->_registry !== FALSE )
            {
            $this->_registry->flush();
            $this->_registry->destroy();
            unset($this->_registry);
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alle verfügbaren Klassen in einem Verzeichnis installieren
    //Dabei ist es wichtig, da die Klassennamen identisch mit den
    //Dateinamen sind
    function registerall($classpath)
        {
        $classes=scandir($classpath);
        
        if (is_array($classes) == TRUE)
            {
            foreach ($classes as $class)
                {
                if (strpos($class,"class.")===0)
                    {
                    $classname=str_replace("class.","",$class);
                    $classname=str_replace(".php"  ,"",$classname);
                    $classid  =md5($class);
                    $this->register($classpath.$class,$classname,$classid);
                    }
                }
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Klassendatei öffnen und deren Installerdaten ablegen
    function register($classfile,$classname,$classid)
        {
        if (file_exists($classfile)==TRUE)
            {
            //Datei ziehen
            require_once($classfile);
            
            //Und evtl. den Installer aufrufen
            if ( method_exists($classname,"install"))
                {
                //Installerdaten holen
                $result = call_user_func( array($classname,"install") );

                if (is_array($result))
                    {
                    //Rückgabearray konsistent halten
                    $result=$this->checkresult($result);
                    
                    //Hat die Klasse eine eigene ID übergeben ?
                    $classid = (isset($result[CLASS_INDEX_ID])==TRUE?$result[CLASS_INDEX_ID]:$classid);

                    //Den Eintrag in der Klassenregistry initialisieren ?
                    if ($result[CLASS_INDEX_CLEANUP]!=FALSE)
                        {
                        $this->_registry->del("classes/".$result[CLASS_INDEX_RUNLEVEL]."/",$classid);
                        }

                    //Und los
                    $classkey="classes/".$result[CLASS_INDEX_RUNLEVEL]."/".$classid;
                    $this->_registry->write($classkey,CLASS_INDEX_NAME      ,$result[CLASS_INDEX_NAME]);
                    $this->_registry->write($classkey,CLASS_INDEX_VERSION   ,$result[CLASS_INDEX_VERSION]);
                    $this->_registry->write($classkey,CLASS_INDEX_AUTOLOAD  ,$result[CLASS_INDEX_AUTOLOAD]);
                    $this->_registry->write($classkey,CLASS_INDEX_COMPRESSED,$result[CLASS_INDEX_COMPRESSED]);
                    $this->_registry->write($classkey,CLASS_INDEX_ENCRYPTED ,$result[CLASS_INDEX_ENCRYPTED]);

                    $regfile = $classid.".reg";
                    $this->_registry->write($classkey,CLASS_INDEX_REGISTRY    ,$result[CLASS_INDEX_REGISTRY]);
                    $this->_registry->write($classkey,CLASS_INDEX_REGISTRYFILE,$regfile);

                    //Registry einrichten ?
                    if ($result[CLASS_INDEX_REGISTRY] !== FALSE)
                        {
                        $dummy= new registry($this->_regpath.$regfile,
                                             $result[CLASS_INDEX_COMPRESSED],
                                             $result[CLASS_INDEX_ENCRYPTED]);
                        $dummy->write("/",CLASS_INDEX_INSTALLDATE,time());

                        //Registry initialisieren ?
                        if ($result[CLASS_INDEX_CLEANUP]==TRUE)
                           {
                           $dummy->clear();
                           }

                        //Presets schreiben
                        $result = call_user_func( array($classname,"preset"),$dummy );

                        $dummy->flush();
                        $dummy->destroy();
                        }
                    
                    //Datenbank einrichten ?
                    if ($result[CLASS_INDEX_DATABASE] != FALSE)
                        {
                        $this->_registry->write($classkey,CLASS_INDEX_DATABASE    ,TRUE);
                        $this->_registry->write($classkey,CLASS_INDEX_DATABASENAME,$classid);
                        }

                    //Dateidaten mit ablegen
                    $this->_registry->write($classkey,CLASS_INDEX_CLASSNAME,basename($classname));
                    $this->_registry->write($classkey,CLASS_INDEX_CLASSFILE,basename($classfile));
                    $this->_registry->write($classkey,CLASS_INDEX_INSTALLDATE,time());
                    }
                }
            //Abspeichern
            $this->_registry->flush();
            }
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein Resultarray auf Vollständigkeit prüfen und evtl. fehlende Wert mit Defaults nachfüllen
    function checkresult($result)
        {
        $result[CLASS_INDEX_ID]        = $this->_checkresult($result,CLASS_INDEX_ID,time());
        $result[CLASS_INDEX_NAME]      = $this->_checkresult($result,CLASS_INDEX_NAME,"unknown");
        $result[CLASS_INDEX_VERSION]   = $this->_checkresult($result,CLASS_INDEX_VERSION,"0.1");
        $result[CLASS_INDEX_REGISTRY]  = $this->_checkresult($result,CLASS_INDEX_REGISTRY,FALSE);
        $result[CLASS_INDEX_DATABASE]  = $this->_checkresult($result,CLASS_INDEX_DATABASE,FALSE);
        $result[CLASS_INDEX_CLEANUP]   = $this->_checkresult($result,CLASS_INDEX_CLEANUP,FALSE);
        $result[CLASS_INDEX_AUTOLOAD]  = $this->_checkresult($result,CLASS_INDEX_AUTOLOAD,TRUE);
        $result[CLASS_INDEX_COMPRESSED]= $this->_checkresult($result,CLASS_INDEX_COMPRESSED,FALSE);

        return($result);
        }

    function _checkresult($array,$name,$default)
        {
        return ( isset($array[$name])==TRUE?$array[$name]:$default);
        }
    }
</script>