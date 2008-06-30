<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Klasse f�r Stromverschl�sselungen
///
/// Die Umsetzung erfolg �ber ein PseudeOneTimePad
/// Ein Zufallsgenerator wird mit dem "Kennwort" geseeded und liefert
/// danach eine vorhersagbare Sequenz.
///
/// Durch den Zufallsanteil wird ein statistischer Angriff trotz der
/// simplen XOR-Verschl�sselung sinnlos.
/// Da aber der Adressraum des PRNG sehr begrenzt ist, hat ein BruteForce-
/// Angriff gute Chancen.
/// Um Laien mit Cryptool fernzuhalten sollte es aber problemlos reichen
///
/// Nochmal ausdr�cklich : Dies ist KEINE sichere Verschl�sselung sondern
/// nur eine Hilfe um ScriptKiddies und automatische Scanner zu verwirren.
///
//////////////////////////////////////////////////////////////////////////
///Beispiel
///Verschl�sseln
///$cypher=new crypt();
///$cypher->setpass("geheim");
///$coded=$cypher->str_encode("Mein geheime Botschaft");
///$cypher->destroy();
///
///Entschl�sseln
///$cypher=new crypt();
///$cypher->setpass("geheim");
///$text=$cypher->str_decode($coded);
///$cypher->destroy();
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_CRYPT","class_crypt");
define ("CLASS_CRYPT_VERSION","0.02");
if (isset($debug)) $debug->add(CLASS_CRYPT,"version ".CLASS_CRYPT_VERSION);

//////////////////////////////////////////////////////////////////////////
//Die Plattformunabh�ngige Zufallsklasse nachladen
require_once("class.random.php");

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class crypt
    {
    //Private Eigenschaften
    //Zufallssamen
    var $internal_rnd     = FALSE;
    var $internal_buff    = FALSE;
    var $internal_seed    = FALSE;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function crypt($pass=FALSE)
        {
        //Generator initialisieren
        $this->internal_rnd=new random();

        $this->setpass($pass);
        }

    //Destruktor
    function destroy()
        {
        $this->internal_rnd->destroy();
        unset($this);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Die Verschl�sselung resetten
    function reset()
        {
        $this->internal_rnd->seed($this->internal_seed);
        $this->internal_buff=$this->internal_seed;
        }

    //Das Kennwort setzen
    function setpass($pass)
        {
        //Einfach aus dem Kennwort eine Zahl bilden
        $seed=crc32($pass);
        if ($seed==0) $seed=151;

        //Fertig
        $this->internal_seed=$seed;
        $this->reset();
        }

    //Ein Byte verschl�sseln
    function encode($in)
        {
        $in=ord($in);

        //Byte mit dem vorherigen verkn�pfen
        $out=$in  ^ ($this->internal_buff & 0xff);

        //Mit dem Zufallsgenerator verkn�pfen
        $out=$out ^ ($this->internal_rnd->byte());

        //F�r die n�chste Runde speichern
        $this->internal_buff=$in;

        //Fertig
        return(chr($out));
        }
        
    //Ein Byte entschl�sseln
    function decode($in)
        {
        $in=ord($in);
        
        //Mit dem Zufallsgenerator verkn�pfen
        $out=$in  ^ ($this->internal_rnd->byte());

        //Byte mit dem vorherigen verkn�pfen
        $out=$out ^ ($this->internal_buff & 0xff);

        //F�r die n�chste Runde speichern
        $this->internal_buff=$out;

        //Fertig
        return(chr($out));
        }
        
    //Einen String verschl�sseln
    function str_encode($in)
        {
        $result="";
        
        for ($index=0; $index < strlen($in); $index++)
            {
            $result.=$this->encode($in[$index]);
            }
        return($result);
        }

    //Einen String entschl�sseln
    function str_decode($in)
        {
        $result="";

        for ($index=0; $index < strlen($in); $index++)
            {
            $result.=$this->decode($in[$index]);
            }
        return($result);
        }
        
    //Eine Datei mit der Stromverschl�sselung kodieren
    function file_encode($source,$target)
        {
        $result=FALSE;

        //Quelldatei �ffnen
        $fin =fopen($source,"rb");
        if ($fin!=FALSE)
            {
            //Zieldatei �ffnen
            $fout=fopen($target,"wb+");

            if ($fout!=FALSE)
                {
                //Kopieren und dabei den Puffer verschl�sseln
                while (!feof($fin))
                    {
                    $in=fread($fin,CRYPT_BUFFER);

                    $out=$this->str_encode($in);

                    fwrite($fout,$out);
                    }
                fclose($fout);
                $result=TRUE;
                }
            fclose($fin);
            }
        return($result);
        }

    //Eine Datei mit der Stromverschl�sselung dekodieren
    function file_decode($source,$target)
        {
        $result=FALSE;

        //Quelldatei �ffnen
        $fin =fopen($source,"rb");
        if ($fin!=FALSE)
            {
            //Zieldatei �ffnen
            $fout=fopen($target,"wb+");

            if ($fout!=FALSE)
                {
                //Kopieren und dabei den Puffer verschl�sseln
                while (!feof($fin))
                    {
                    $in=fread($fin,CRYPT_BUFFER);

                    $out=$this->str_decode($in);

                    fwrite($fout,$out);
                    }
                fclose($fout);
                $result=TRUE;
                }
            fclose($fin);
            }
        return($result);
        }
    }
</script>