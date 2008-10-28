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
/// Cacheklasse für beliebige Daten
///
/// Einfach die gewünschten Daten per cache->save(uniqueid,daten,runtime) abspeichern
/// Und später per cache->get(uniqueid) holen
/// ist die Rückgabe FALSE müssen die Daten neu gepuffert werden
////////////////////////////////////////////////////////////////////////////////////////////////////
///Beispiel
///
/// $pageid="indexpage"
/// $cache=new cache();
/// $cache->cachepath="./cache/";           //Hier werden alle Cachedateien abgelegt
/// if ( $cache->iscached($page) != TRUE)   //Daten sind noch nicht gepuffert ?
///    {
///    $output=createpage($pageid);         //Hier würde der Inhalt erzeugen
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

//Trennzeichen für gecachte Dateinamen
define ("CACHE_FILE_LIMITER" ,"#");

//Eigentliche Klasse
class cache
    {
    var $cachepath  = PATH_CACHE;

    var $_filebuffer = array();

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function cache()
        {
        //Immer den ID-Buffer initialisieren
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
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "cache";      //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]      = "cache";      //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";        //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;        //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;        //Wird eine Datenbank benötigt
        $result[CLASS_INDEX_CLEANUP]   = FALSE;        //Soll die Datenbank initialisiert werden ?
        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;         //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;        //Soll die Datenbank komprimiert werden (gz)
        $result[CLASS_INDEX_RUNLEVEL]  = 1;            //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }

    //Hier können bei der Installation Daten in die Registry geschrieben werden
    function preset(&$registry)
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Cache füllen
    function save($id,$runtime,$data)
        {
        //Daten ablegen
        $data=gzcompress(serialize($data),1);

        $filename=$this->createfilename($id,$runtime);

        file_put_contents($this->cachepath.$filename,$data);

        return(file_exists($data));
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Den Cache löschen
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
        //Statuscache neu initialisieren
        clearstatcache ();
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Daten holen
    function load($id)
        {
        //Immer MD5 um böse Buben draußen zu halten
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
            }
        else
            {
            $result=FALSE;
            }

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Prüfen, ob eine Datei gepuffert ist
    function iscached($id)
        {
        return ( isset($this->_filebuffer[ callmethod("crypt","hash",$id) ]));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alle verfügbaren Cachefiles lesen
    function updatecachebuffer()
        {
        //Alte Daten löschen
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
                if (end($data) > time() )
                    {
                    $this->_filebuffer[reset($data)] = $file;
                    }
                else
                    {
                    //Abgelaufene Dateien entfernen
                    unlink($this->cachepath.$file);
                    }
                }
            }
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Den Dateinamen für eine Bufferdatei erzeugen
    function createfilename($id,$runtime)
        {
        return( (callmethod("crypt","hash",$id).CACHE_FILE_LIMITER.(time() + $runtime ) ) );
        }
    }
</script>