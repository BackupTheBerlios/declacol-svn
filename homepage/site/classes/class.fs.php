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
/// Dateisystem
///
/// Alle Zugriffe auf Dateien sollten hierüber laufen, da die Klasse ein virtuelles
/// Dateisystem vorgaukelt und damit bei Mehrbenutzersystemen Datensicherheit bietet.
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

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Pfadangabe cleanen und validieren
    function _cleanpath($path)
      {
      //Traversen killen
      $result=str_replace("\\","/",$path);  
      $result=str_replace("..","",$result);  
        
      //Darf nur unterhalb des Basispfades liegen
      if (strpos(PATH_BASE,$path)!==0)
        {
        $result=FALSE;
        }
      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei öffnen und eine Handle liefern
    function open($filename,$mode)
      {
      $filename=$this->_cleanpath($filename);
      $result=fopen($filename,$mode);
      return($result);    
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Handle schliessen
    function close($handle)
      {
      if ($handle!==FALSE)
        {
        fclose($handle);
        $result=TRUE;
        }
      else
        {
        $result=FALSE;
        }
      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //In einen Buffer lesen
    function read($handle,&$buffer,$size)
      {
      $result=FALSE;
        
      if ($handle!==FALSE)
        {
        }

      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Buffer schreiben
    function write($handle,$buffer,$size)
      {
      $result=FALSE;
 
      if ($handle!==FALSE)
        {
        }
        
      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei kpl. einlesen
    function readfile($filename)
      {
      $filename=$this->_cleanpath($filename);
      return(file_get_contents($filename));
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei kpl. schreiben
    function writefile($filename,$buffer)
      {
      $filename=$this->_cleanpath($filename);
      return(file_put_contents($filename,$buffer));
      }
    }
</script>