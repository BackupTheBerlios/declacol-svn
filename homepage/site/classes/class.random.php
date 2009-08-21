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
/// Zufallsgenerator
///
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Eigentliche Klasse
class random
    {
    //Private
    var $_registry = FALSE;

    //für rpc exportierte funktionen
    var $export      = array("seed"=>"set seed",
                             "getbyte" =>"get a random byte",
                             "getword" =>"get a random word",
                            );
                             
    var $_seed1 = 0;
    var $_seed2 = 0;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function random()
        {
        $this->_seed1=(USALT1 ^ USALT2);
        $this->_seed2=(USALT1 ^ USALT3);
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
        $result[CLASS_INDEX_ID]        = "random";          //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "random";          //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";             //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;             //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;             //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = FALSE;             //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;              //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= TRUE;              //Soll die Datenbank komprimiert werden (gz)

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
      
    function setseed($value)
        {
        $this->_seed1 = ($value ^ USALT1);
        $this->_seed2 = ($value ^ USALT2);

        //Nullen verhindern
        if ($this->_seed1 == 0) $this->_seed1=$value ^ USALT2;
        if ($this->_seed2 == 0) $this->_seed2=$value ^ USALT1;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    function getmax()
        {
        return (0x0fffffff);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    function getbit()
        {
        return ( $this->_get() & 0x00000001 );
        }        
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    function getbyte()
        {
        return ( $this->_get() & 0x000000ff );
        }        
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    function getword()
        {
        return ( $this->_get() & 0x0000ffff );
        }        
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    function get($min,$max)
        {
        $result=$this->_get();
        
        if ($result < $min)
            {
            $result=$min;
            }
        else
            {
            $result = $result % $max;
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Zwei parallele Generatoren, von denen jeweils nur die oberen 16Bit die Werte liefern
    function _get()
        {
        $this->_seed1 = intval($this->_seed1 * 214013 + 2531011);
        $this->_seed2 = intval($this->_seed2 * 214013 + 2531011);
        
        return ( ($this->_seed1 & 0xffff0000) | ( ($this->_seed2 >> 16) & 0x0000ffff) );
        }
    }
</script>