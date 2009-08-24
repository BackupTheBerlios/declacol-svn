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
/// Internes Fehlerhandling
///
///Ist eine Konstante DEBUG definiert, hängt sich die Klasse automatisch in das interne PHP-Fehler-
///handling ein und fängt alle Fehlerlevel und Exceptions ab.
///Die Fehler/Exceptions werden mitgeloggt und lesbar ausgegeben.
///Standardmäßig brechen alle Warnungen,Hinweise und Fehler die Verarbeitung ab.
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Eigentliche Klasse
class debug
    {
    var $log         = array();
    
    //Private
    var $active      = FALSE;

    //für rpc exportierte funktionen
    var $export      = array("addlog"=>"add a log entry");

    var $runtime     = 0;
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function debug()
        {
        //Soll im Debugmodus gestartet werden?
        if (defined("DEBUG")==TRUE)
            {
            require_once(PATH_LIBS."lib.xml.php");

            $this->active = DEBUG;

            //Unsere Daten holen
            $data = $this->install();
            define("DEBUG_CLASS",$data[CLASS_INDEX_ID]);

            //Alle eigenen Handler einhängen
            set_exception_handler(array(DEBUG_CLASS,"exception"));
            set_error_handler    (array(DEBUG_CLASS,"error"));
            
            $this->addlog("debug","launched");
            $this->runtime=microtime(TRUE);
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        //Laufzeit merken
        $this->runtime=(microtime(TRUE)-$this->runtime);
        
        if (defined("DEBUG")==TRUE)
            {
            restore_error_handler();
            restore_exception_handler();

            //Wenn eine Debugsession aktiv war scheiben wir eine LogDatei
            if ($this->active==TRUE)
                {
                $this->addlog("debug","runtime ".$this->runtime);
                $this->addlog("debug","ended");

                //Und log als XML schreiben
                file_put_contents(PATH_TEMP."debug_".microtime(TRUE).".log",arraytoxml($this->log));
                }
                
            $this->clearlog();
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "debug";          //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]      = "debugger";       //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";            //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;            //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;            //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = TRUE;             //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;             //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;            //Soll die Datenbank komprimiert werden?
        $result[CLASS_INDEX_ENCRYPTED] = FALSE;            //Soll die Datenbank verschlüsselt werden?
        $result[CLASS_INDEX_RUNLEVEL]  = 0;                //In welchen Runlevel soll die Klasse geladen werden

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
    //Unser eigener Errohandler
    function error($errno, $errstr, $errfile="unknown", $errline="0", $errcontext="undefined")
        {
        $error= "";
        $halt = FALSE;
        switch ($errno)
            {
            case E_USER_NOTICE  :  $error.="USER_NOTICE   [".$errno."] ".$errstr;
                                   $halt=FALSE;
                                   break;
                                   
            case E_USER_WARNING :  $error.="USER_WARNING  [".$errno."] ".$errstr;
                                   $halt=TRUE;
                                   break;
                                   
            case E_USER_ERROR   :  $error.="USER_ERROR    [".$errno."] ".$errstr;
                                   $halt=TRUE;
                                   break;

            case E_NOTICE       :  $error.="SYS_NOTICE    [".$errno."] ".$errstr;
                                   $halt=FALSE;
                                   break;
                                   
            case E_WARNING      :  $error.="SYS_WARNING   [".$errno."] ".$errstr;
                                   $halt=TRUE;
                                   break;
                                   
            case E_ERROR        :  $error.="SYS_ERROR     [".$errno."] ".$errstr;
                                   $halt=TRUE;
                                   break;

            case E_CORE_WARNING :  $error.="CORE_WARNING  [".$errno."] ".$errstr;
                                   $halt=TRUE;
                                   break;
            case E_CORE_ERROR  :  $error.="CORE_NOTICE   [".$errno."] ".$errstr;
                                   $halt=TRUE;
                                   break;

            case E_PARSE        :  $error.="PARSER_ERROR  [".$errno."] ".$errstr;
                                   $halt=TRUE;
                                   break;

            default             :  $error.="UNKNOWN_ERROR [".$errno."] ".$errstr;
                                   $halt=TRUE;
                                   break;
            }

        //Zusatzdaten anhängen
        $error.="\n file    : ".$errfile;
        $error.="\n line    : ".$errline;
        $error.="\n context : ".print_r($errcontext,TRUE);

        //Abspeichern
        callmethod("debug","addlog","system",$error);

        //Handbremse ziehen, wenn ein Fehler aufgetreten ist
        if ($errno != 0)
            {
            callmethod("debug","printlog");
            if ($halt==TRUE)
                {
                callmethod("debug","addlog","system","halted");

                //Log als XML schreiben
                $log=callmethod("debug","getlog");
                file_put_contents(PATH_TEMP."debug_".CURRENT_TIME.".log",arraytoxml( $log ) );

                die("system halted");
                }
            }
        return(TRUE);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Unser eigener Exceptionhandler
    function exception ($exception)
        {
        $error="EXCEPTION    [".$exception->code."] ".$exception->message;
        callmethod("debug","addlog","system",$error);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Log zufügen
    function addlog($module,$text)
        {
        $this->log[$module][]=date("H:i:s")." : ".$text;
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Log ausgeben
    function printlog()
        {
        echo "<pre>";
        print_r( arraytoxml( $this->getlog() ) );
        echo "</pre>";
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Log liefern
    function getlog()
        {
        return($this->log);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Log löschen
    function clearlog()
        {
        $this->log=array();
        }
    }
</script>