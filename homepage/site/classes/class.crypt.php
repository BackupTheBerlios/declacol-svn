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
/// Zentraler Provider für Verschlüsselung und Hashing
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Flags für die Hashmodi
define ("CRYPT_PROVIDER_MD5"    ,1);
define ("CRYPT_PROVIDER_SHA1"   ,2);
define ("CRYPT_PROVIDER_CUSTOM" ,3);
define ("CRYPT_HASH_PREFIX"     ,"%GUID%");
define ("CRYPT_MAX_RAND"        ,callmethod("random","getmax"));

//Eigentliche Klasse
class crypt
    {
    var $salt     = 5463;
    var $provider = CRYPT_PROVIDER_CUSTOM;
    var $maxrand  = CRYPT_MAX_RAND;

    //Alle exportierten Funktionen
    var $export=array("hash"   =>"calculates hash of dataset",
                      "hash"   =>"encrypts string",
                      "id"     =>"creates unique id",
                      );

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function crypt()
        {
        //Salt setzen
        if (defined("SSALT1") != FALSE)
            {
            $this->salt=SSALT1;
            }
        else
            {
            if (DEBUG) callmethod("debug","addlog","crypt","WARNING no salt defined! fallback to constsalt");
            }
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
        $result[CLASS_INDEX_ID]        = "crypt";      //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "crypt";    //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";             //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;              //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;             //Wird eine Datenbank benötigt
        $result[CLASS_INDEX_CLEANUP]   = FALSE;              //Soll die Datenbank initialisiert werden ?
        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;              //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;            //Soll die Datenbank komprimiert werden?
        $result[CLASS_INDEX_ENCRYPTED] = FALSE;            //Soll die Datenbank verschlüsselt werden?
        $result[CLASS_INDEX_RUNLEVEL]  = 2;                //In welchen Runlevel soll die Klasse geladen werden

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
    //Einen Hashwert erzeugen
    function hash($input)
        {
        switch ($this->provider)
            {
            case CRYPT_PROVIDER_MD5  : $result=$this->_md5   ($input); break;
            case CRYPT_PROVIDER_SHA1 : $result=$this->_sha1  ($input); break;
            default                  : $result=$this->_custom($input); break;
            }
        return($result);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Hashwert für Kennwörter erzeugen
    function passhash($input)
        {
        switch ($this->provider)
            {
            case CRYPT_PROVIDER_MD5  : $result=$this->_md5   ($input); break;
            case CRYPT_PROVIDER_SHA1 : $result=$this->_sha1  ($input); break;
            default                  : $result=$this->_custom($input); break;
            }
            
        //Ist immer ein SHA1
        $result=$this->_sha1($result);
        return($result);
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Hash so erzeugen, das er nicht zweimal gehasht wird.
    //Prefix ist eine Konstante die den Hash identifiziert z.B. %USERID%
    function singlehash($input,$prefix=CRYPT_HASH_PREFIX)
        {
        if (strpos($input,$prefix)===0)
            {
            $result=$input;
            }
        else
            {
            $result=$prefix.$this->hash($input);
            }
        return ($result);
        }
       

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine ID erzeugen
    function id()
        {
        switch ($this->provider)
            {
            case CRYPT_PROVIDER_MD5  : $result=$this->_md5   ($this->_id()); break;
            case CRYPT_PROVIDER_SHA1 : $result=$this->_sha1  ($this->_id()); break;
            default                  : $result=$this->_custom($this->_id()); break;
            }
        return($result);
        }
   
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //"Zufallszahl erzeugen"
    function _id()
        {
        return( $this->_random().time().$this->_random() );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Zufallszahl erzeugen
    function _random($min=0,$max=CRYPT_MAX_RAND)
        {
        return ( callmethod("random","get",$min,$max) );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Verschiedenen Hashfunktionen

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Salted MD5
    function _md5($input)
        {
        return (md5($input.$this->salt.$input));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Salted SHA1
    function _sha1($input)
        {
        return(sha1($input.$this->salt.$input));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Salted Selfmade
    function _custom($input)
        {
        //CRC32 draus machen
        $input=abs(crc32($input.$input.$this->salt.$input));

        //Und nun bilden wir daraus einen simplen String
        $result="";
        while ($input > 0)
            {
            //Nur Buchstaben aus dem ASCII-Satz nehmen
            $result.=chr( ($input % 26) + 97);
            $input=$input >> 2;
            }
        return($result);
        }
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    function encrypt($text,$key)
      {
      return( $this->_xor($text,$key) );
      }
       
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    function decrypt($text,$key)
      {
      return( $this->_xor($text,$key));
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    // Einen String simpel XOR verschlüsseln
    // Kein sehr sicheres Verfahren, aber es funktioniert
    function _xor($text,$key)
        {
        $size=strlen($text);
            
        if ( $size > 0 )
            {
            $rnd=new random();
            $rnd->setseed(crc32($key)); 
        
            for ($index=0;$index < $size; $index++)
                {
                $text[$index]=chr( ord($text[$index]) ^ $rnd->getbyte() );
                }            
            $rnd->destroy();
            }         
        return($text);
        }        
    }
</script>