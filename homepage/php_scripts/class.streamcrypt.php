<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Klasse die mit einem PseudoOneTimePad einen Datenstrom verschlüsselt.
/// Hält einen echten Profi kaum auf, aber die 99,9% der Laien
///
//////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("CLASS_STREAMCRYPT","class_streamcrypt");
define ("CLASS_STREAMCRYPT_VERSION","0.05");
if (isset($debug)) $debug->add(CLASS_STREAMCRYPT,"version ".CLASS_STREAMCRYPT_VERSION);

//Benötigt die Zufallsklasse
require_once("class.random.php");

class streamcrypt
    {
    var $internal_rnd = FALSE;
    var $internal_byte= 0x13;

    //Konstruktor
    function streamcrypt ($seed)
        {
        //Den Zufallsgenerator initialisieren
        $this->internal_rnd=new random();
        $this->seed($seed);
        }

    //Destruktor
    function destroy()
        {
        $this->internal_rnd->destroy();
        unset($this->internal_rnd);
        }

    //Den Generator resetten
    function seed($seed)
        {
        //Generator initialisieren
        $this->internal_rnd->seed($seed);
        
        //Startbyte setzen
        $this->internal_byte=$seed & 0xff;
        }

    //Ein Byte verschlüsseln
    function encode($byte)
        {
        //Einmal mit vorhergehenden Byte verXORen
//        $result=$byte ^ $this->internal_byte;

        //Einmal mit dem Generator verXORen
        $result=$byte ^ $this->internal_rnd->byte();

        //Für die nächste Runde Puffern
        $this->internal_byte=$result;
        
        return($result);
        }

    //Ein Byte entschlüsseln
    function decode($byte)
        {
        //Einmal mit vorhergehenden Byte verXORen
//        $result=$byte ^ $this->internal_byte;

        //Einmal mit dem Generator verXORen
        $result=$byte ^ $this->internal_rnd->byte();

        //Für die nächste Runde Puffern
        $this->internal_byte=$byte;

        return($result);
        }
        
        
    //Einen String verschlüsseln
    function encode_string($string)
        {
        $result="";
        
        if ($string!="")
            {
            for ($index=0; $index < strlen($string); $index++)
                {
                $result.= chr ($this->encode(ord($string[$index])));
                }
            }
        return($result);
        }

    //Einen String entschlüsseln
    function decode_string($string)
        {
        $result="";

        if ($string!="")
            {
            for ($index=0; $index < strlen($string); $index++)
                {
                $result.= chr ($this->decode(ord($string[$index])));
                }
            }
        return($result);
        }

    }


$a = new streamcrypt (55);

echo "<pre>";

$a->seed(123);
$crypt= $a->encode_string("test");
print_r($a);


$a->seed(123);
$text = $a->decode_string($crypt);
print_r($a);

echo $crypt."<br>";
echo $text."<br>";




</script>