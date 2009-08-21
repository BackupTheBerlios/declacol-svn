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
/// Der Dateiname wir jeweils um das aktuelle Salz (SALT) erweitert
///
/// Alle Zugriffe auf Dateien sollten hierüber laufen, da die Klasse ein virtuelles
/// Dateisystem vorgaukelt und damit bei Mehrbenutzersystemen Datensicherheit bietet.
/// (ToDo)
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//FileMode-Konstanten
define ("OPEN_READ"  ,"rb");
define ("OPEN_WRITE" ,"w+b");
define ("OPEN_APPEND","a+b");

//Eigentliche Klasse
class fs
    {
    var $_pathobfuscator = "";

    //für rpc exportierte funktionen
    var $export      = array();

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function fs()
        {
        //Einfach eine PHP-Konstante holen
        $this->_pathobfuscator=PHP_BINDIR.PHP_SHLIB_SUFFIX;

        //Salz als erweiterung übernehmen
        if (defined("SSALT1")==TRUE)
          {
          $this->_pathobfuscator.=SSALT1;
          }

        //PathSafe machen
        $this->_pathobfuscator="#-".abs(CRC32($this->_pathobfuscator))."-#";
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
      else
        {
        //Erweiterung anhängen
        $result.=$this->_pathobfuscator;
        }
      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei öffnen und eine Handle liefern
    function open($filename,$mode)
      {
      $filename=$this->_cleanpath($filename);
      if (file_exists($filename)==TRUE)
         {
         $result=fopen($filename,$mode);
         }
      else
         {
         $result=FALSE;
         }
      return($result);    
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Handle schliessen
    function close($handle)
      {
      if ($handle!==FALSE)
        {
        fflush($handle);
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
        $buffer=fread($fp,$size);
        }

      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Zeile in einen Buffer lesen
    function readln($handle,&$buffer)
      {
      $result=FALSE;

      if ($handle!==FALSE)
        {
        $buffer=fgets($fp);
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
        fwrite($fp,$buffer,$size);
        }
        
      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei als String kpl. einlesen
    function readstring($filename)
      {
      return($this->readfile($filename));
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei als Array kpl. einlesen
    function readarray($filename)
      {
      $filename=$this->_cleanpath($filename);

      if (file_exists($filename)==TRUE)
         {
         $result=file($filename);
         }
      else
         {
         $result=FALSE;
         }

      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei kpl. einlesen
    function readfile($filename)
      {
      $filename=$this->_cleanpath($filename);

      if (file_exists($filename)==TRUE)
         {
         $result=file_get_contents($filename);
         }
      else
         {
         $result=FALSE;
         }
      
      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei kpl. schreiben
    function writefile($filename,$buffer)
      {
      $filename=$this->_cleanpath($filename);
      
      $result=file_put_contents($filename,$buffer);

      return($result);
      }
    }
</script>