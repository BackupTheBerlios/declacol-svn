<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2009 Borg@sven-of-nine.de
*/
////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Transparente Kommunikation zwischen Sessions
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");
require_once(PATH_LIBS."/lib.datetime.php");

define ("CLEAN_TIMEOUT"    , 10);
define ("MESSAGE_TIMEOUT"  , 30);


//Eigentliche Klasse
class message
    {
    //für rpc exportierte funktionen
    var $export      = array();

    var $_registry = FALSE;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function message(&$registry)
        {
        //Unsere Registrierung intern ablegen
        $this->_registry=$registry;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->cleanup();
        $this->_registry->flush();
        unset($this->_registry);
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "message";        //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "message";        //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";            //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = TRUE;             //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;            //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = TRUE;             //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;             //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;            //Soll die Datenbank komprimiert werden?
        $result[CLASS_INDEX_ENCRYPTED] = FALSE;            //Soll die Datenbank verschlüsselt werden?

        $result[CLASS_INDEX_RUNLEVEL]  = 3;                 //In welchen Runlevel soll die Klasse geladen werden

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
    //Eine Nachricht an eine Session versenden
    function send ($receiver,$messagetype,$messagedata)
      {
      //Existiert der Empfänger überhaupt?
      if (callmethod("session","exists",$receiver,FALSE)==TRUE)
        {
        //Einen Unique Pfad finden
        do
          {
          $index=usecs() + MESSAGE_TIMEOUT;
          $path="data/".$receiver.":".$messagetype."/".$index;
          }
        while ( $this->_registry->exists($path,"data") == TRUE );

        //Und die Message ablegen
        $this->_registry->write($path,"data",$messagedata);
        }
      }
      
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Nachricht an alle Sessions versenden
    function broadcast($messagetype,$messagedata)
      {
      $sessions=callmethod("session","enum",array());
      
      foreach ($sessions as $session=>$temp)
        {
        $this->send($session,$messagetype,$messagedata);
        }
      }
      
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die nächste Nachricht eines Types aus der Mailbox abholen
    function receive($messagetype,$remove)
      {
      $result=FALSE;  
       
      //Nur für die eigene Session Zugriff zulassen
      $session=getproperty("session","id",ID_NONE);
      if ($session != ID_NONE)
        {
        //Alle Messages des passenden Typs lesen
        $path="data/".$session.":".$messagetype;
        $messages=$this->_registry->enum($path);

        ksort($messages);
        
        //Und die erste Zurückgeben
        reset($messages);
        $key=key($messages);
        $temp=$this->_registry->read($path."/".$key,"data",VOID_MARKER);

        if ($temp !== VOID_MARKER)
          {
          $result=$temp;
          //Und evtl. entfernen
          if ($remove==TRUE)
            {
            $this->_registry->del($path,$key);
            }
          }
        }
      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alte Messages verwerfen
    function cleanup()
      {
      //Nur alle paar Sekunden aufräumen um Überlastung zu vermeiden
      if ($this->_registry->read("/","timeout",0) <> NOW)
        {
        $this->_registry->write("/","timeout",(NOW + CLEAN_TIMEOUT));
        
        //Aufräumen
        $sessions = $this->_registry->enum("data");
        
        //Alle Sessions
        foreach ($sessions as $session=>$temp)
          {
          $messages=$this->_registry->enum("data/".$session);

          
          //Alle Messages
          foreach ($messages as $id=>$temp)
            {
            //Abgelaufene entfernen
            if ( $id < NOW )
              {
              $this->_registry->del("data/".$session,$id);
              }
            }
          }
        }
      }
    }
</script>