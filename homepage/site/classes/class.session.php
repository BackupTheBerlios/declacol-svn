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
/// Sessionverwaltung
///
////////////////////////////////////////////////////////////////////////////////////////////////////
if (defined("SESSION_TIMEOUT")==FALSE)
    {
    define ("SESSION_TIMEOUT" , 60 * 15);
    }


//Eigentliche Klasse
class session
    {
    //Public
    var $id       = ID_NONE;
    var $user     = FALSE;
    var $data     = array();
    var $start    = CURRENT_TIME;
    
    //Private
    var $_registry = FALSE;

    //für rpc exportierte funktionen
    var $export      = array();

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function session(&$registry)
        {
        //Unsere Registrierung intern ablegen
        $this->_registry=$registry;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->stop();
        if ( $this->_registry !== FALSE )
            {
            $this->clean();
            $this->_registry->flush();
            unset($this->_registry);
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]       = "session";      //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]     = "session";      //Name der Klasse
        $result[CLASS_INDEX_VERSION]  = "0.1";           //Version der Klasse
        $result[CLASS_INDEX_REGISTRY] = TRUE;            //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE] = FALSE;           //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]  = TRUE;            //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD] = TRUE;           //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_RUNLEVEL] = 5;                 //In welchen Runlevel soll die Klasse geladen werden

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
    //Eine Session starten und die dazugehörige Datei erzeugen
    function start($id=ID_NONE)
        {
        //Zur Sicherheit immer hashen
        $id=$this->createid($id);
        $path="sessions/".$id;
        
        //Wenn es die ID noch nicht gibt, legen wir eine neue an
        if ($this->_registry->exists($path,"userid")==FALSE)
            {
            $this->_registry->write($path,"userid",callmethod("crypt","id"));
            $this->_registry->write($path,"data"  ,array());
            $this->_registry->write($path,"start" ,CURRENT_TIME);
            }
            
        //User lesen
        //ReadByID gibt im Fehlerfall ein Standarduserobjekt ohne Rechte zurück
        $userid=$this->_registry->read($path,"userid",ID_NONE);
        $user=callmethod("user","readbyid",$userid);
        //Bei einem Anonymous eine UserID erzwingen
        $user->id=$userid;

        //Userdaten aus der Registry lesen
        $this->data =$this->_registry->read($path,"data" ,array());
        $this->start=$this->_registry->read($path,"start",CURRENT_TIME);
        
        //Timeout um 15 Minuten verlängern
        $this->_registry->write($path,"timeout",CURRENT_TIME + SESSION_TIMEOUT );
        
        //Daten freigeben
        $this->user=$user;        
        $this->id=$id;

        //und fertig
        return($id);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Session beenden
    function stop()
        {
        $this->user=FALSE;
        $this->data=array();
        $this->id=ID_NONE;
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alte Sessions aus der Datenbank werfen
    function clean()
        {
        $sessions=$this->_registry->enum("sessions");

        foreach ($sessions as $session=>$dummy)
            {
            $time=$this->_registry->read("sessions/".$session,"timeout",0);
            if ($time < CURRENT_TIME)
                {
                $this->_registry->del("sessions/",$session);
                }
            }
        }
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die ID eines Benutzers erzeugen
    //Ist es schon eine ID wird diese direkt zurückgegeben
    function createid($input)
        {
        if ($input===ID_NONE)
            {
            $input=callmethod("crypt","id");
            }
            
        //Mit singlehash wird verhindert, dass eine richtige ID nochmals gehasht wird
        return ( callmethod("crypt","singlehash",$input,"%SID%") );
        }
        
    }
</script>