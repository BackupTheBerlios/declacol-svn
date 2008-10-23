<script language="PHP">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
//////////////////////////////////////////////////////////////////////////
///
/// Password und Checksum Bibliothek
///
///Prototypen
/// crypt_create_unique_id():string        //Eindeutigen Identifier erzeugen
/// crypt_create_hash($input):string       //Aus einem String einen Hash machen
/// crypt_create_pass_hash($input):string  //Aus einem String einen SHA1-Hash machen
/// crypt_create_password(l�nge):string    //Ein Kennwort erzeugen
///
///Alle Hash- und Verschl�sselungsanforderungen sollten nur �ber diese Aufrufe
///laufen, um die Konsistenz der Daten �ber alle Skripte sicherzustellen.
///
//////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("LIB_CRYPT","lib_crypt");
define ("LIB_CRYPT_VERSION","0.05");
if (isset($debug)) $debug->add(LIB_CRYPT,"version ".LIB_CRYPT_VERSION);

//Modus der ID/Hash-Erzeugung
define ("CRYPT_MODE","generic");   //[generic/md5/sha1]

//Unser Salz aus der Konfiguration holen
if (defined("SALT")==TRUE)
    {
    define ("CRYPT_SALT"    ,SALT);
    }
else
    {
    define ("CRYPT_SALT"    ,123);
    }

//Verschl�sselungskey (nix tolles, aber gegen dumme Filescanner hilft es)
define ("CRYPT_KEY"     ,CRYPT_SALT & 0xff);

//Tempor�res Verzeichnis f�r Dateifunktionen
if (defined("DIR_TEMP")==TRUE)
    {
    define ("CRYPT_TEMP"    ,DIR_TEMP);
    }
else
    {
    define ("CRYPT_TEMP"    ,"./");
    }

//Defaultgr��e des Konverterpuffers
define ("CRYPT_BUFFER"      ,1024);


//Zufallsgenerator sicher initialisieren
mt_srand((double)microtime()*1000000);

//////////////////////////////////////////////////////////////////////////
//Eindeutigen Identifier erzeugen
function crypt_create_unique_id()
    {
    switch (CRYPT_MODE)
        {
        case ("md5")    : $result=crypt_create_unique_md5_id();                 break;
        case ("sha1")   : $result=crypt_create_unique_sha1_id();                break;
        case ("generic"): $result=crypt_create_shorty( crypt_create_digit() );  break;
        default         : $result=crypt_create_shorty( crypt_create_digit() );  break;
        }
    return($result);
    }

//////////////////////////////////////////////////////////////////////////
//Einen Stringhash erzeugen
function crypt_create_hash($input)
    {
    switch (CRYPT_MODE)
        {
        case ("md5")    : $result=crypt_create_sha1_hash($input.CRYPT_SALT);    break;
        case ("sha1")   : $result=crypt_create_md5_hash ($input.CRYPT_SALT);    break;
        case ("generic"): $result=crypt_create_shorty($input.CRYPT_SALT);       break;
        default         : $result=crypt_create_shorty($input.CRYPT_SALT);       break;
        }
    return($result);
    }

//////////////////////////////////////////////////////////////////////////
//Einen Zufallsstring erzeugen
function crypt_create_digit()
    {
    return( mt_rand().time().mt_rand() );
    }

//////////////////////////////////////////////////////////////////////////
//Aus einem String (oder Zahl) eine kurze Buchstabenfolge erzeugen
function crypt_create_shorty($input)
    {
    //CRC32 draus machen
    $input=abs(crc32($input.$input));

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

//////////////////////////////////////////////////////////////////////////
//Eindeutigen Identifier erzeugen
//Gibt es als einzelne Funktion, um Anwendungen zu erlauben, h�here
//Sicherheitsgrade als Default zu nutzen
function crypt_create_unique_sha1_id()
    {
    //Das sollte einigerma�en ausreichen
    return(crypt_create_sha1_hash( crypt_create_digit() ));
    }

//////////////////////////////////////////////////////////////////////////
//Eindeutigen Identifier erzeugen
//Gibt es als einzelne Funktion, um Anwendungen zu erlauben, h�here
//Sicherheitsgrade als Default zu nutzen
function crypt_create_unique_md5_id()
    {
    //Das sollte einigerma�en ausreichen
    return(crypt_create_md5_hash( crypt_create_digit() ));
    }

//////////////////////////////////////////////////////////////////////////
//Einen Stringhash erzeugen
//Gibt es als einzelne Funktion, um Anwendungen zu erlauben, h�here
//Sicherheitsgrade als Default zu nutzen
function crypt_create_sha1_hash($input)
    {
    //Das sollte einigerma�en ausreichen
    return(sha1($input.CRYPT_SALT));
    }

//////////////////////////////////////////////////////////////////////////
//Einen Stringhash erzeugen
//Gibt es als einzelne Funktion, um Anwendungen zu erlauben, h�here
//Sicherheitsgrade als Default zu nutzen
function crypt_create_md5_hash($input)
    {
    //Das sollte einigerma�en ausreichen
    return(md5($input.CRYPT_SALT));
    }

//////////////////////////////////////////////////////////////////////////
//Base64 SHA1 erzeugen (Wie ihn z.B apache benutzt)
//Gibt es als einzelne Funktion, um Anwendungen zu erlauben, h�here
//Sicherheitsgrade als Default zu nutzen
function crypt_create_sha1_base64_hash($input)
    {
    $result=base64_encode(pack("H*",sha1($input)));
    }

    
//////////////////////////////////////////////////////////////////////////
//Ein Kennwort erzeugen
function crypt_create_password($size)
    {
    ///Arrays f�r die Kennworterzeugung
    $_parts=array(
                  array("a","e","i","o","u","y","au","ou","ie","ae","ue","oe","au","eu"),
                  array("b","c","d","f","g","h","j","k","l","m","p","qu","r","s","t","v","w","x","z","ck","st","gh")
                 );

    //Und nun einfach basteln
    $result="";
    $part=rand(0,1);
    //Abwechsend einen "Konsonanten" und eine "Vokal" aneinanderkleben
    while(strlen($result) < $size)
        {
        $result.=$_parts[$part][rand(0,count($_parts[$part])-1)];
        //Zwischen den Teilen wechseln

        $part=$part==1?0:1;
        }
    //Auf die richtige L�nge beschneiden
    return(substr($result,0,$size));
    }
    
    
//Eine Datei verschl�sseln (Simples XOR)
function crypt_encode_file($filename,$key=CRYPT_KEY)
    {
    //Da XOR symetrisch ist, einfach die Decode-Methode aufrufen
    return(crypt_decode_file($filename,$key));
    }
    
//Eine Datei entschl�sseln (Simples XOR)
function crypt_decode_file($filename,$key=CRYPT_KEY)
    {
    //Die Datei ins Tempverzeichnis verschieben
    $tempfile=CRYPT_TEMP.crypt_create_unique_id();
    copy($filename,$tempfile);

    //Konvertieren
    $result=crypt_convert_file($tempfile,$filename,$key);

    //Tempdatei entfernen
    @unlink($tempfile);
    
    return($result);
    }


//Eine Datei entweder verschl�sseln oder entschl�sseln
//Da XOR symetrisch ist, m�ssen wir uns darum keine Sorgen machen
//Nat�rlich bietet das hier keinerlei Schutz vor ernsthaften Angriffen,
//aber Signatursuchen auf dem Webspace laufen damit ins Leere
function crypt_convert_file($source,$target,$key=CRYPT_KEY)
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
                
                $out=crypt_decode($in,$key);

                fwrite($fout,$out);
                }
            fclose($fout);
            $result=TRUE;
            }
        fclose($fin);
        }
    return($result);
    }

    
//Einen String verschl�sseln
function crypt_encode($in,$key=CRYPT_KEY)
    {
    //Da XOR symetrisch ist, rufen wir einfach die decode Methode auf
    return(crypt_decode($in,$key));
    }
    
//Einen String entschl�sseln
function crypt_decode($in,$key=CRYPT_KEY)
    {
    if ($in!="")
        {
        $result="";
        for ($index=0; $index < strlen($in); $index++)
            {
            $result.=chr( ord($in[$index]) ^ $key);
            }
        }
    else
        {
        $result=FALSE;
        }
        
    return($result);
    }

</script>