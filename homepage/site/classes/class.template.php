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
/// Beispielklasse,
/// An diese Vorlage sollten sich alle Klassen halten. Insbesonder, wenn diese per automatische
/// Initialisierung durch die Install-Methode verfügbar gemacht werden sollen.
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Eigentliche Klasse
class templateclass
    {
    //Private
    var $_registry = FALSE;

    //für rpc exportierte funktionen
    var $export      = array();

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function template(&$registry)
        {
        //Unsere Registrieung intern ablegen
        $this->_registry=$registry;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        if ( $this->_registry !== FALSE )
            {
            $this->_registry->flush();
            $this->_registry->destroy();
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "templateid";      //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "templatename";    //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";             //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = TRUE;              //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;             //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = TRUE;              //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;              //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= TRUE;              //Soll die Datenbank komprimiert werden (gz)

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