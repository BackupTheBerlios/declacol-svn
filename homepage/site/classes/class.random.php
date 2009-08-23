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
/// Zufallsgenerator
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
                             
    var $_sbox    = array();
    var $_sindex1 = 0;
    var $_sindex2 = 0;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function random()
        {
        $this->setseed(rand());
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
        $result[CLASS_INDEX_ID]        = "random";         //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "random";         //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";            //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;            //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;            //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = FALSE;            //Soll die Datenbank initialisiert werden ?

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
    //Maximale Zufallszahl zurückgeben
    function getmax()
        {
        return (0x0fffffff);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein Zufallsbit zurückgeben
    function getbit()
        {
        return ( $this->_get() & 0x00000001 );
        }        
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein Zufallsbyte zurückgeben
    function getbyte()
        {
        return ( $this->_get() );
        }        
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein Zufallsword zurückgeben
    function getword()
        {
        return ( ($this->getbyte() << 8) | $this->getbyte() );
        }        
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Zufallsbereich holen
    function get($min,$max)
        {
        //32 Bit holen
        $result=( $this->getword() << 16 ) |
                ( $this->getword());
        
        //Grenzen einhalten
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
    //RC4 SBox initialisieren
    //Initialisierung:
    //Setze k[] = (Schlüssel-Zeichenfolge beliebiger Länge > 0 )
    //Setze s[] = (sBox-Zeichenfolge der Länge 2^n)
    //Für i = 0 bis LängeVon(s)
    //    s[i] = i
    //Setze j = 0
    //Für i = 0 bis LängeVon(s)
    //    j = (j + s[i] + k[i mod LängeVon(k)]) mod LängeVon(s)
    //    vertausche(s[i],s[j])
    function setseed($value)
        {
        //Auf jedenfall immer einen String bauen
        $value=sha1($value);
        $len=strlen($value);

        //SBox vorinitialisieren        
        for ($index=0; $index < 256; $index++)
          {
          $this->_sbox[$index]=$index;
          }
        
        //Das Kennwort einkneten
        $keyindex=0;
        for ($index=0; $index < 256; $index++)
          {
          //Neuen Index bestimmen
          $keyindex = ( $keyindex + $this->_sbox[$index] + $value[$index % $len] ) % 256;
          
          //Bytes vertauschen
          $tmp=$this->_sbox[$keyindex];
          $this->_sbox[$keyindex]=$this->_sbox[$index];
          $this->_sbox[$index]=$tmp;
          }

        //Startindizes setzen
        $this->_sindex1=$this->_sbox[0];
        $this->_sindex2=$this->_sbox[1];
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Zufallsbyte nach RC4 berechnen
    //Zwar sehr langsam, aber dafür sicher
    function _get()
      {
      //Indizees anpassen
      $index1=$this->_sindex1;
      $index2=$this->_sindex2;
       
      $index1 = ($index1 + 1) % 256;
      $index2 = ($index2 + $this->_sbox[$index1]) % 256;

      $this->_sindex1=$index1;
      $this->_sindex2=$index2;
      
      //SBox vertauschen
      $tmp=$this->_sbox[$index1];
      $this->_sbox[$index1]=$this->_sbox[$index2];
      $this->_sbox[$index2]=$tmp;
      
      //Randombyte holen
      $result = $this->_sbox[ ($this->_sbox[$index1] + $this->_sbox[$index2]) % 256 ];
    
      //Fertig      
      return($result);
      }    
    }
</script>