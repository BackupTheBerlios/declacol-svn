<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Klasse die byteweise aus eine Datei liest
///
/// Hierbei wird zur Beschleunigung des Zugriffes immer ein Buffer
/// mitgef�hrt. Erst wenn der Buffre "leer" ist, wird nachgeladen.
/// Gerade bei gro�en Dateien ergibt sich dadurch ein Geschwindigkeits-
/// vorteil.
/// Folgende Methoden werden unterst�tzt
///
/// open("datei"):Boolean  �ffnet die Datei und initialisiert die Klasse
/// getbyte():Char         liest das n�chste Byte oder meldet FALSE bei EOF
/// get(size):string       liefert einen String der L�nge size
/// getline(eol):string    liefert den n�chsten String bis zum eol-Zeichen
/// rewind(Width)          spult um Width Bytes zur�ck (-1 f�r Dateianfang)
/// forward(Width)         spult um Width Bytes vor
/// close()                schlie�t die Datei
///
/// Eigenschaften
/// eof                    EOF-Flag der Datei
/// eol                    Default eol-Zeichen der Klasse
/// size                   Gr��e der ge�ffneten Datei
/// maxbuffer              Gr��e des Lesepuffers
/// filename               Name der ge�ffneten Datei
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_BYTEREADER","class_bytereader");
define ("CLASS_BYTEREADER_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_BYTEREADER,"version ".CLASS_BYTEREADER_VERSION);


$reader=new bytereader();
$reader->maxbuffer=50;

if ($reader->open("test.txt")!=FALSE)
    {
    echo "file open<br>";
    
        while ($reader->eof==FALSE)
            {
            echo $reader->getbyte();
            }
        echo "<hr>";
        $reader->rewind(3);
        echo $reader->get(10);

        echo "<hr>";

        $reader->rewind();
        $reader->forward(3);
        echo $reader->get(10);
        echo "<hr>";
        $reader->rewind();
        
        while ($reader->eof==FALSE)
            {
            echo $reader->getline()."<br/>";
            }
    }

$reader->destroy();

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class bytereader
    {
    //Public
    var $eof               = TRUE;
    var $eol               = "\n";
    var $maxbuffer         = 10240;
    var $filename          = "";
    var $size              = 0;

    //Private
    //Buffer f�r die Bytes
    var $internal_buffer   = "";
    //End of File
    var $internal_eof      = TRUE;
    //End of Buffer
    var $internal_eob      = TRUE;
    //FileHandle
    var $internal_handle   = FALSE;
    //Bytes im Buffer
    var $internal_size     = 0;
    //Zeiger auf da aktuelle Byte
    var $internal_pointer  = 0;
    
    
    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function bytereader($filename="")
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        
        if ($filename!="")
            {
            $this->open($filename);
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->close();
        }

    //////////////////////////////////////////////////////////////////////////
    //Verbindungsaufbau
    function open($filename)
        {
        $this->setdefault();
        
        if (is_readable($filename)==TRUE)
            {
            $this->internal_handle=fopen($filename,"rb");
            if ($this->internal_handle!=FALSE)
                {
                $this->size=filesize($filename);

                $this->filename=$filename;

                //Ganz zum Anfang springen
                $this->rewind();

                //EOF-Flag �bertragen
                $this->eof=feof($this->internal_handle);
                }
            }
        else
            {
            $this->internal_handle=FALSE;
            }

        return($this->internal_handle!=FALSE);
        }

    //////////////////////////////////////////////////////////////////////////
    //Verbindungsabbau
    function close()
        {
        if ($this->internal_handle!=FALSE)
            {
            if (fclose($this->internal_handle)==TRUE)
                {
                $this->internal_handle=FALSE;
                }
            $this->setdefault();
            }
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Defaults
    function setdefault()
        {
        //Zur Sicherheit Handles schlie�en
        $this->close();

        //Und alles zur�cksetzen
        $this->eof               = TRUE;
        $this->filename          = FALSE;
        $this->internal_buffer   = "";
        $this->internal_eof      = TRUE;
        $this->internal_eob      = TRUE;
        $this->internal_pointer  = 0;
        $this->internal_size     = 0;
        $this->internal_handle   = FALSE;
        $this->size              = 0;
        }

    //////////////////////////////////////////////////////////////////////////
    //Datenbank erzeugen
    //Wird hier nicht benutzt
    function install()
        {
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    //Datenbank zerst�ren
    //Wird hier nicht benutzt
    function uninstall()
        {
        return(TRUE);
        }

        
    //////////////////////////////////////////////////////////////////////////
    //Vorspulen
    function forward($width)
        {
        $this->get($width);
        }

    //////////////////////////////////////////////////////////////////////////
    //Zur�ckspulen
    //Negative Zahlen springen direkt an den Anfang der Datei
    function rewind($width=-1)
        {
        //Bei negativen Zahle spulen wir ganz zur�ck
        if ($width<0)
            {
            fseek($this->internal_handle,0,SEEK_SET);
            }
        else
            {
            $offset=ftell($this->internal_handle) -  $width;
            if ($offset < 0) $offset=0;
            fseek($this->internal_handle,$offset,SEEK_SET);
            }

        //Alles OK ?
        if (ftell($this->internal_handle) < $this->size)
            {
            //Buffer invalidieren
            $this->internal_eob=TRUE;
            $this->internal_eof=FALSE;
            $this->eof=FALSE;
            }
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Einen String der L�nge $size holen
    function get($size)
        {
        $result="";
        while ( ($size > 0) && ($this->eof==FALSE))
            {
            $result.=$this->getbyte();
            $size--;
            }
        return($result);
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Einen String bis zu einem Endzeichen lesen
    function getline($eol="")
        {
        $result="";
        $byte="";
        
        //Defaultzeichen nehmen
        if ($eol=="")
            {
            $eol=$this->eol;
            }
            
        //Auf ein Zeichen begrenzen
        $eol=substr($eol,0,1);
        
        //Und einfach �ber den Bytezugriff lesen, bis wir da sind
        while ( ($byte!=$eol) && ($this->eof==FALSE))
            {
            $byte=$this->getbyte();
            $result.=$byte;
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    //Ein Byte lesen
    function getbyte()
        {
        $byte=FALSE;
        
        //Mu� der Buffer gef�llt werden ?
        if ($this->internal_eob==TRUE)
            {
            $this->_fillbuffer();

            $this->internal_eob= ($this->internal_pointer >= $this->internal_size);
            }

        //Ende des Buffers ?
        if ($this->internal_eob==FALSE)
            {
            //Byte holen
            $byte=$this->internal_buffer[$this->internal_pointer];
            
            //N�chste Position
            //Ist das Ende des Buffers erreicht, wird beim n�chsten Fillbuffer
            //nachgeladen
            $this->internal_pointer++;
            
            //EOB-Flag setzen
            $this->internal_eob=($this->internal_pointer >= $this->internal_size);
            }

        //Flags anpassen
        //F�r den Benutzer ist das Ende erreicht, wenn Datei UND Buffer am Ende sind
        $this->eof = ($this->internal_eof && $this->internal_eob);
        
        return($byte);
        }


    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //////////////////////////////////////////////////////////////////////////
    //Den Puffer f�llen
    function _fillbuffer()
        {
        if ($this->internal_handle!=FALSE)
            {
            $this->internal_buffer=fread($this->internal_handle,$this->maxbuffer);
                
            //Neue Buffergr��e setzen
            $this->internal_size=strlen($this->internal_buffer);
                
            //Pointer zur�ck auf Los
            $this->internal_pointer=0;
                
            //EOF ?
            $this->internal_eof=feof($this->internal_handle);
            }
        else
            {
            //Handle Fehler, wir brechen ab
            $this->internal_eof=TRUE;
            $this->internal_eob=TRUE;
            }
        }
    }
</script>