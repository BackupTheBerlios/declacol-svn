<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Klasse für Zufallszahlen
///
/// Erlaubt den gakapselten Zugriff auf voneinander unabhängige Zufalls-
/// generatoren. Dies kann manchmal sehr nützlich sein. Z.B chiffrierung
/// etc.
///
/// Da PHP keine UnsignedInts kennt, muß selbst auf das Vorzeichen geachtet
/// werden.
///
/// Einfach die Klasse instanzieren und mit den einzelnen Funktionen
/// Werte holen.
/// ->bit();
/// ->byte();
/// ->word();
/// ->longword();
/// ->value(lo,hi)  // lo < rnd < hi
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_RANDOM","class_random");
define ("CLASS_RANDOM_VERSION","0.02");
if (isset($debug)) $debug->add(CLASS_RANDOM,"version ".CLASS_RANDOM_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class random
    {
    //Private Eigenschaften
    //Zufallssamen
    var $internal_seed   =0;

    //States für den Tautsworthe-Generator
    var $internal_state1 = 0;
    var $internal_state2 = 0;
    var $internal_state3 = 0;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function random($seed=FALSE)
        {
        $this->seed($seed);
        }

    //Destruktor
    function destroy()
        {
        unset($this);
        }

    //Verbindungsaufbau
    function open()
        {
        return(TRUE);
        }

    //Verbindungsabbau
    function close()
        {
        return(TRUE);
        }

    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        return(TRUE);
        }

    //Datenbank und alles andere erzeugen
    //Wird hier nicht benutzt
    function install()
        {
        return(TRUE);
        }

    //Datenbank und alles andere zerstören
    //Wird hier nicht benutzt
    function uninstall()
        {
        return(TRUE);
        }

    //Generator initialisieren
    function seed($seed=FALSE)
        {
        if ($seed===FALSE)
            {
            //Mit (quasi)-Zufall seeden
            $this->internal_seed=((int)( microtime() * 1000003 ) << 16 );
            }
        else
            {
            $this->internal_seed=$seed;
            }

        $this->_seed();
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //PRNG seeden
    function _seed()
        {
         //Die Seeds bestimmen
        $this->internal_state1=$this->internal_seed;
        $this->internal_state2=$this->internal_state1 << 3;
        $this->internal_state3=$this->internal_state1 << 7;
        }

    // Gibt einen unsigned 32Bit Zufallswert zurück
    // nach Pierre L'Ecuyer (1995)
    // "Maximally equidistributed combined Tausworthe generators"
    // http://www.iro.umontreal.ca/~lecuyer/
    // benötigt zwingend 32Bit
    function _rnd()
        {
        //Drei Generatoren
        $u32shift              = ( ( $this->internal_state1 << 13) ^ $this->internal_state1 ) >> 19;
        $this->internal_state1 = ( ( $this->internal_state1 & 4294967294 ) << 12 ) ^ $u32shift;

        $u32shift              = ( ( $this->internal_state2 <<  2) ^ $this->internal_state2 ) >> 25;
        $this->internal_state2 = ( ( $this->internal_state2 & 4294967288 ) << 12 ) ^ $u32shift;

        $u32shift              = ( ( $this->internal_state3 <<  3) ^ $this->internal_state3 ) >> 11;
        $this->internal_state3 = ( ( $this->internal_state3 & 4294967280 ) << 12 ) ^ $u32shift;

        //Gesamtergebnis bestimmen
        $result=$this->internal_state1 ^ $this->internal_state2 ^ $this->internal_state3;

        //Fertig
        return((int) $result);
        }

    //Weitere Generatoren
    /*
    
    //Multiplikativer Generator nach
    //Knuth's Seminumerical Algorithms, 3rd Ed., Seiten 106–108
    // x(n+1) = (a * xn) mod m
    // Es gibt für diesen Generator verschiedene Parameter :
    // Borosh-Niederreiter : a = 1812433253 m = 2^32        = 4294967296
    // Fishman18           : a = 62089911   m = 2^31 - 1    = 2147483647
    // Fishman20           : a = 48271      m = 2^31 - 1    = 2147483647
    // L'Ecuyer            : a = 40692      m = 2^31 - 249  = 2147483399
    // Waterman            : a = 1566083941 m = 2^32        = 4294967296
    // Park and Miller     : a = 16807      m = 2^31 - 1    = 2147483647
    // Der SCHLECHTE IBM   : a = 65539      m = 2^31        = 2147483648
    // INMOS Transputer    : a = 1664525    m = 2^32        = 4294967296
    // VAX                 :
    
    // Durch die Mod-Operation ist dieser Generator leider etwas langsam
    function _rnd()
        {
        $this->internal_state1 = ( 40692 * $this->internal_state1 ) % 2147483399;
        return($this->internal_state1);
        }
        
    //Rekursiver Generator zweiter Ordnung nach
    //Knuth in Seminumerical Algorithms, 3rd Ed., Seite 108
    //Die Besonderheit dieses Generators ist die Eigenschaft, nicht nur den direkten
    //sonderen auch den zweiten Vorfahren heranzuziehen
    function _rnd()
        {
        $result= ( 271828183 * $this->internal_state1 ) + ( 314159269 * $this->internal_state2 ) % 2147483647;

        //Werte zweistufig speichern
        $this->internal_state2 = $this->internal_state1;
        $this->internal_state1 = $result;
        
        return ($result);
        }

    //Generator der VAX-Maschinen
    function _rnd()
        {
        $this->internal_state = (69069 * $this->internal_state1 + 1) % 4294967296;
        
        return ($this->internal_state1);
        }

    */

    //Diverse Zufallswerte holen

    //Ein u32 Zufallswert
    function longword()
        {
        return($this->_rnd());
        }

    //Ein u16 Zufallswert
    function word()
        {
        return( ($this->_rnd() >> 16) & 0x0000ffff );
        }

    //Ein u8 Zufallswert
    function byte()
        {
        return( ($this->_rnd() >> 24) & 0x000000ff );
        }

    //Ein u1 Zufallswert
    function bit()
        {
        return( ($this->_rnd() >> 31) & 0x00000001 );
        }

    //einen "beliebigen Wert"
    // lo < rnd < hi
    function value($lo,$hi)
        {
        //Abs-Zahl holen
        $rnd = $this->_rnd() & 0x7fffffff;

        //Interval bestimmen
        $rnd=( $rnd % $hi) + $lo;

        return($rnd);
        }
    }
</script>