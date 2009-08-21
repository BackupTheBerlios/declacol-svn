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
/// Cacheklasse f�r beliebige Daten
///
/// Einfach die gew�nschten Daten per cache->save(uniqueid,daten,runtime) abspeichern
/// Und sp�ter per cache->get(uniqueid) holen
/// ist die R�ckgabe FALSE m�ssen die Daten neu gepuffert werden
////////////////////////////////////////////////////////////////////////////////////////////////////
///Beispiel
///
/// $pageid="indexpage"
/// $cache=new cache();
/// $cache->cachepath="./cache/";           //Hier werden alle Cachedateien abgelegt
/// if ( $cache->iscached($pageid) != TRUE) //Daten sind noch nicht gepuffert ?
///    {
///    $output=createpage($pageid);         //Hier w�rde der Inhalt erzeugt
///    $cache->save($pageid,3600,$output);  //in den Pufer speichern
///    }
/// else
///    {
///    $output=$cache->load($pageid);       //Aus dem Cache laden
///    }
/// echo $output:                           //Seite ausgeben
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Trennzeichen f�r gecachte Dateinamen
define ("CACHE_FILE_LIMITER" ,"#");

//Eigentliche Klasse
class cache
    {
    var $cachepath  = PATH_CACHE;

    var $_filebuffer  = array();
    var $_lastcleanup = 0;

    //Alle exportierten Funktionen
    var $export = array("clear"=>"clears cache",
                        "updatecachebuffer"=>"removes old cacheentries");

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function cache()
        {
        //Immer den ID-Buffer initialisieren
        $this->_lastcleanup=0;
        $this->updatecachebuffer();
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        unset($this->_filebuffer);
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zur�ck
    function install()
        {
        $result[CLASS_INDEX_ID]        = "cache";      //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]      = "cache";      //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";        //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;        //Wird eine Registry ben�tigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;        //Wird eine Datenbank ben�tigt
        $result[CLASS_INDEX_CLEANUP]   = FALSE;        //Soll die Datenbank initialisiert werden ?
        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;         //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;        //Soll die Datenbank komprimiert werden (gz)
        $result[CLASS_INDEX_RUNLEVEL]  = 2;            //In welchen Runlevel soll die Klasse geladen werden
        return($result);
        }

    //Hier k�nnen bei der Installation Daten in die Registry geschrieben werden
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
    //Cache f�llen
    function save($id,$runtime,$data)
        {
        //Daten ablegen
        $data=gzcompress(serialize($data),1);

        $filename=$this->createfilename($id,$runtime);

        file_put_contents($this->cachepath.$filename,$data);

        if (DEBUG) callmethod("debug","addlog","cache","caching ".$filename);

        return(file_exists($data));
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Den Cache l�schen
    function clear()
        {
        //Bestehende Dateien lesen
        $buffer=scandir($this->cachepath);
        foreach ($buffer as $file)
            {
            if (strpos($file,".") !== 0)
                {
                unlink($this->cachepath.$file);
                }
            }

        if (DEBUG) callmethod("debug","addlog","cache","clear");

        //Statuscache neu initialisieren
        clearstatcache ();
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Daten holen
    function load($id)
        {
        //Immer hashen um b�se Buben drau�en zu halten
        $id=callmethod("crypt","hash",$id);
        
        //Ist die Datei gepuffert ?
        if (isset($this->_filebuffer[$id])==TRUE)
            {
            $result=file_get_contents($this->cachepath.$this->_filebuffer[$id]);

            //Fehler abfangen
            if ($result != "")
                {
                $result=gzuncompress($result);
                $result=unserialize($result);
                }
            if (DEBUG) callmethod("debug","addlog","cache","recall ".$id);
            }
        else
            {
            $result=FALSE;
            }

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Pr�fen, ob eine Datei gepuffert ist
    function iscached($id)
        {
        return ( isset($this->_filebuffer[ callmethod("crypt","hash",$id) ]));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alle verf�gbaren Cachefiles lesen
    function updatecachebuffer()
        {
        //Nur alle f�nf Sekunden den GarbageCollector ausf�hren.
        if ($this->_lastcleanup < CURRENT_TIME)
            {
            $this->_lastcleanup = CURRENT_TIME + 5;
            
            //Alte Daten l�schen
            $this->_filebuffer = array();

            //Bestehende Dateien lesen
            $buffer=scandir($this->cachepath);

            //Und jede Datei indizieren
            foreach ($buffer as $file)
                {
                //Kein versteckten Dateien anfassen
                if ($file[0]!=".")
                    {
                    $data=explode(CACHE_FILE_LIMITER,$file);
            
                    //Valide Daten ablegen
                    if ( end($data) > CURRENT_TIME )
                        {
                        $this->_filebuffer[reset($data)] = $file;
                        }
                    else
                        {
                        //Abgelaufene Dateien entfernen
                        //Hiermit werden auch alte oder irrt�mliche abgelegte Dateien gel�scht.
                        //sehr praktische Angelegenheit
                        if (DEBUG) callmethod("debug","addlog","cache","timeout ".$file);
                        unlink($this->cachepath.$file);
                        }
                    }
                }
            }
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Den Dateinamen f�r eine Bufferdatei erzeugen hinter dem Treenzeichen wird der timestamp,
    //ab wann die Datei veraltet ist angeh�ngt. Als workaround um nicht auf filectime zur�ckzugreifen
    //ist das die einfachste M�glichkeit. filectime ist unter manchen systemen die zeit, wann die datei zum letzten
    //mal ver�ndert wurde
    function createfilename($id,$runtime)
        {
        $id = callmethod("crypt","hash",$id).CACHE_FILE_LIMITER.( time() + $runtime );
        return( $id );
        }
        
    }
</script>