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
/// Zugriff auf das Dateisystem
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Eigentliche Klasse
class fs
    {
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function fs()
        {
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
        $result[CLASS_INDEX_ID]        = "filesystem";      //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "fs";              //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";             //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;             //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;             //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = TRUE;              //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;              //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;             //Soll die Datenbank komprimiert werden (gz)

        $result[CLASS_INDEX_RUNLEVEL]  = 1;                 //In welchen Runlevel soll die Klasse geladen werden

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