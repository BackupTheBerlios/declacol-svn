<pre><script language="php">
//////////////////////////////////////////////////////////////////////////////
//
//Klass zum geheimen Schlüsseltausch und einer sicheren Datenübertragung
//
//////////////////////////////////////////////////////////////////////////////
require_once("class.random.php");

$keyswap=new securesync();

$keyswap->getswapdata();

$swapdata=print_r($keyswap);

$keyswap->destroy();


class securesync
    {
    // 2 < base < (prime - 2)
    var $data=array(
                   "prime"  =>99173,
                   "base"   =>0,
                   "modulo" =>0,
                   "check"  =>0
                   );

    var $random = 0;
    var $key    = 0;

    //Das Zufallsobjekt
    var $rnd    = FALSE;

    function securesync()
        {
        $this->rnd=new random();
        }
        
    function destroy()
        {
        $this->rnd->destroy();
        unset($this);
        }
        
        
    function getswapdata()
        {
        //Neu seeden
        $this->rnd->seed();
        
        //Zufall initialisieren
        $this->random = $this->rnd->value(2,($this->data["prime"]-2));
        
        //Basedata bestimmen
        $this->data["base"]=round( $this->data["prime"] / $this->rnd->value(2,64));

        //Tauschwerte bestimmen
        $this->data["modulo"] = pow($this->data["base"],$this->random);// % $this->data["prime"] );

        //Prüfsumme erzeugen
        $this->data["check"]  = $this->_createcheck($this->data,$this->random);

        //Tauschwerte initialisieren
        $this->swap=serialize($this->data);
        }
        

    //Die Daten aus einem öffentlichen Tausch akzeptieren
    //und den internen Status damit setzen
    function setswapdata($data)
        {
        @$temp=userialize($data);


        
        }



    //Daten sicher aus einem Array extrahieren
    function _extractdata($array,$key,$default)
        {
        if (isset($array[$key]))
            {
            $result=$array[$key];
            }
        else
            {
            $result=$default;
            }
        return($default);
        }
        
    //Eine Checksumme des internen Status erzeugen
    function _createcheck($array,$salt)
        {
        $result = sha1( $this->_extractdata( $array,"prime" ,rand(0,65535) ).
                        $this->_extractdata( $array,"base"  ,rand(0,65535) ).
                        $this->_extractdata( $array,"modulo",rand(0,65535) ).
                        $salt
                       );
        return($result);
        }
    }
</script>