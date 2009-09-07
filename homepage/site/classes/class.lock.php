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
/// Klasse um ein einfaches Locking zu realisieren
///
/// einfach eine Lock mit set setzen und mit clr wieder freigeben
/// mit waitfor kann auf eine resource gewartet werden.
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Eigentliche Klasse
class lock
    {
    //Private
    var $_registry = FALSE;
    var $_lockdb   = array();
    var $_lockfile = PATH_TEMP."lock.db";
    var $_handle   = FALSE;

    //für rpc exportierte funktionen
    var $export      = array();

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function lock()
        {
        $time=(time() + 5);
                        
        while ( ($this->handle = fopen($this->_lockfile,"rwb"))== FALSE ) &
              ( $time < time() )
            {
            sleep(1);
            }
            
        //Datei auf jeden Fall locken
        if ($this->_handle != FALSE)
            {
            flock($this->_handle);
            
            fread($this->_handle);
            
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        if ($this->_handle != FALSE)
            {
            fwrite ($this->_handle,serialize($this->_lockdb));
            fclose($this->_handle);
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "lock";      //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "lock";    //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";             //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;              //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;             //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = TRUE;              //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;              //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= TRUE;              //Soll die Datenbank komprimiert werden (gz)
        $result[CLASS_INDEX_ENCRYPTED] = FALSE;            //Soll die Datenbank verschlüsselt werden?

        $result[CLASS_INDEX_RUNLEVEL]  = 10;                //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }
        
    //Hier können bei der Installation Daten in die Registry geschrieben werden
    function preset(&$registry)
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Zeiger auf This liefern
    function getthis()
      {
      $self=&$this;
      return($self);
      }
    }
</script>
