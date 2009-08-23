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
/// Simple Registrierdatenbank
///
////////////////////////////////////////////////////////////////////////////////////////////////////
/// !!Arrays lassen sich nicht speichern!!
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///Beispiel
///
///$reg=new registry ("myreg.reg"); // Öffnet oder erzeugt die myreg.reg
///$reg->write("basispfad/unterverzeichnis/unterverzeichnis/","wertenamen","wert");
///$reg->write("basispfad/unterverzeichnis/unterverzeichnis/","wertenamen1","wert1");
///$reg->write("basispfad/unterverzeichnis/unterverzeichnis/","wertenamen2","wert2");
///
///$wert=$reg->read("basispfad/unterverzeichnis/unterverzeichnis/","wertenamen2","defaultwert");
///
/////Alle Wert in einem Verzeichnis ausgeben
/////Arrays bezeichnen Unterverzeichnisse
///$values=$reg->enum("basispfad/unterverzeichnis/unterverzeichnis/");
///
///$reg->flush();    // Änderungen in die Datei Abspeichern
///$reg->destroy();  // Alles freigeben (Es wird nichts gespeichert!)
///
////////////////////////////////////////////////////////////////////////////////////////////////////
//Mit dem Voidmarker werden leere Verzeichnisse markiert
define ("VOID_MARKER"  ,"%void%");

class registry
    {
    var $readonly   = FALSE;      //Readonly lässt schreiben nur temporär zu
    var $autoclean  = TRUE;       //Räumt die Daten beim Speichern auf
    var $compressed = FALSE;      //Datenbank komprimiert bearbeiten
    var $encrypted  = FALSE;      //Datenbank verschlüsselt bearbeiten
    
    var $_file    = FALSE;
    var $_reg     = FALSE;
    var $_changed = FALSE;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function registry($filename, $compressed, $encrypted)
        {
        $this->compressed=$compressed;
        $this->encrypted =$encrypted;
        
        $this->clear();
        $this->_file=$filename;
        $this->_open($filename);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        unset($this->_reg);
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Zeiger auf This liefern
    function getthis()
        {
        $self=&$this;
        return($self);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alles freigeben
    function clear()
        {
        unset($this->_reg);
        $this->_reg=array();
        $this->_burn(TRUE);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Leere Zweige entfernen
    function clean()
        {
        $this->_clean($this->_reg);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Abspeichern
    function flush($filename=FALSE)
        {
        //Wenn ein Dateiname erzwungen wurde diesen übernehmen
        if ($filename !== FALSE)
            {
            $this->_file=$filename;
            }
        return ($this->_flush());
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Existiert ein Wert oder Pfad ?
    function exists($path,$name)
        {
        $pointer=$this->_getregpointer($path);
        
        return (isset($pointer[$name]));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Wert lesen
    function read($path,$name,$default)
        {
        //Pointer auf unseren Reg-Zweig holen
        $pointer=&$this->_getregpointer($path);

        //Und entweder Wert oder default liefern
        if ( isset($pointer[$name]) === TRUE)
            {
            //Abfangen, wenn aus Versehen ein Pfad addressiert wird
            if ( is_array($pointer[$name]) === FALSE)
                {
                $result=$this->_decodevalue($pointer[$name]);
                }
            else
                {
                $result=$default;
                }
            }
        else
            {
            $result=$default;
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Wert schreiben
    //Da wir im Gegensatz zu einem Ini-File mit zwei Dimensionen arbeiten,
    //ist die Erzeugung eine Pfades notwendig
    function write($path,$name,$value)
        {
        //Pointer auf unseren Reg-Zweig holen
        $pointer=&$this->_getregpointer($path);

        //Daten nur speichern, wenn sie sich geändert haben
        $value=$this->_encodevalue($value);

        if ( isset($pointer[$name]) === FALSE )
            {
            $pointer[$name]=$value;
            $this->_burn(TRUE);
            }
        else
            {
            if ( $value !== $pointer[$name] )
                {
                $pointer[$name]=$value;
                $this->_burn(TRUE);
                }
            }

        //Funktioniert immer
        return (TRUE);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Wert löschen
    function del($path,$name)
        {
        $result=FALSE;
        
        //Pointer auf unseren Reg-Zweig holen
        $pointer=&$this->_getregpointer($path);

        //Und entweder Wert oder default liefern
        if ( isset($pointer[$name]) === TRUE)
            {
            //Abfangen, wenn aus Versehen ein Pfad addressiert wird
            if ( is_array($pointer[$name]) === FALSE)
                {
                unset($pointer[$name]);
                }
            else
                {
                //Den kpl. Eintrag entfernen
                unset($pointer[$name]);
                }
            $this->_burn(TRUE);
            $result=TRUE;
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Alle Werte in einem Pfad lesen
    function enum($path)
        {
        //Pointer auf unseren Reg-Zweig holen
        $pointer=&$this->_getregpointer($path);
        
        $result=array();

//        print_r($pointer);

        //Alles durchgehen
        foreach ($pointer as $key => $value)
            {
            if  ($key !== VOID_MARKER)
                {
                //Werte sind keine Arrays
                if ( is_array($value) === FALSE )
                    {
                    $result[$key]=$this->_decodevalue($value);
                    }
                else
                    {
                    //Unterverzeichnisse als Array markieren
                    $result[$key]=array();
                    }
                }
            }
        ksort($result);

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Pfad in ein Array auflösen
    function &_getregpointer($path)
        {
        //Den Pfad zerlegen und normieren
        $path=($path==""?"/":$path);
        $path=str_replace("//","/",$path);
        $patharray=explode("/",$path);

        //Abschließenden Slash rausnehmen
        if (end($patharray)=="")
            {
            array_pop($patharray);
            }

        //Pfad durchgehen und dabei die notwendigen Zweige erzeugen
        $pointer=&$this->_reg;
        foreach ($patharray as $pathbit)
            {
            //Dummys setzen ?
            if (isset($pointer[$pathbit])==FALSE)
                {
                $pointer[$pathbit][VOID_MARKER]=FALSE;
                }

            //Runtertraversieren
            $pointer=&$pointer[$pathbit];
            }

        //Funktioniert immmer
        return($pointer);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Datenbank laden und in den Speicher ziehen
    function _open($filename)
        {
        if ( ( file_exists($filename) == TRUE) && (is_readable($filename) == TRUE) )
            {
            $this->_reg=$this->_decode(file_get_contents($filename));
            $this->_burn(FALSE);
            }
        else
            {
            //Datei erzwingen
            @file_put_contents($filename,"");
            }

        //Schreibstatus setzen
        $this->readonly=!is_writable($filename);

        //War OK?
        return(file_exists($filename));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Datenbank in eine Datei flushen
    function _flush()
        {
        $result=FALSE;

        //Nur abspeichern, wenn sich etwas geändert hat und wir nicht
        //im ReadOnly-Modus sind
        if ( ($this->readonly == FALSE) && ($this->_changed == TRUE) )
            {
            //Datei erzwingen
            if (file_exists($this->_file)===FALSE)
                {
                @file_put_contents($this->_file,"");
                }
        
            //Daten dumpen
            if (is_writable($this->_file)==TRUE)
                {
                //Vor dem Abspeichern reinigen wir noch
                if ($this->autoclean == TRUE)
                    {
                    $this->clean();
                    }
            
                //Fertig
                file_put_contents($this->_file,$this->_encode($this->_reg));
                //Änderungsstatus zurücksetzen
                $this->_burn(FALSE);
                $result=TRUE;
                }
            }
        else
            {
            //Readonly lassen wir immer zu
            $result=TRUE;
            }

        //Fertig
        return($result);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Leere Zweige in der Registrierung entfernen
    function _clean(&$tree)
        {
        $haschild=FALSE;
        $hasvalue=FALSE;
        
        foreach ($tree as $key => $value)
            {
            //Hat der Knoten ein Kind ?
            if ( is_array($value) == TRUE )
                {
                //Kind ist clean
                if ( $this->_clean($tree[$key]) == TRUE )
                    {
                    //raus damit
                    unset($tree[$key]);
                    $this->_burn(TRUE);
                    }
                else
                    {
                    $haschild=TRUE;
                    }
                
                //Wenn das Kind auch gelöscht werden kann,
                //brauchen wir es nicht zu beachten
                }
            else
                {
                if ($key != VOID_MARKER)
                    {
                    $hasvalue=TRUE;
                    }
                }
            }

        //Kann der Knoten entfernt werden ?
        return ( ( ($haschild || $hasvalue) != TRUE) );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Datenbank als String ablegen
    function _encode($data)
        {
        $data = @serialize($data);
        
        if ( $this->encrypted == TRUE )
            {
            $data=callmethod("crypt","encrypt",$data,SSALT1);
            }

        if ( $this->compressed == TRUE )
            {
            $data=@gzcompress($data,1);
            }

        return ( $data );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Datenbankstring zurückwandeln
    function _decode($data)
        {
        if ( $this->compressed == TRUE )
            {
            $data=@gzuncompress($data);
            }

        if ( $this->encrypted == TRUE )
            {
            $data=callmethod("crypt","decrypt",$data,SSALT1);
            }
        return ( @unserialize( $data ) );
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Werte encodieren
    function _encodevalue($value)
        {
        return( serialize( $value ) );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Werte decodieren
    function _decodevalue($value)
        {
        return( unserialize( $value ) );
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Marker für Änderungen setzen
    function _burn($status)
        {
        $this->_changed=$status;
        }
    }
</script>