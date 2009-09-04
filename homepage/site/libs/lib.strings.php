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
/// Bibliothek mit nützlichen Stringfunktionen
///
//////////////////////////////////////////////////////////////////////////

//Alle zugelassene Zeichen
$_all_chars =Array( "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",
                    "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z",
                    "1","2","3","4","5","6","7","8","9","0" );


//////////////////////////////////////////////////////////////////////////
//Definitionen der Stringcleaner
define ("FILTER_NONE"        ,0);        //Alles
define ("FILTER_ALPHA"       ,1);        //Nur Buchstaben
define ("FILTER_NUMBER"      ,2);        //Nur Zahlen
define ("FILTER_ALPHANUM"    ,4);        //Nur Buchstaben und Zahlen
define ("FILTER_SECURE"      ,8);        //Nur Sichere Strings (Kein HTML)
define ("FILTER_URL"         ,16);       //Nur URL-Zeichen
define ("FILTER_EMAIL"       ,32);       //Nur E-Mail-Zeichen
define ("FILTER_BASE64"      ,128);      //Base64-Zeichen
define ("FILTER_DATE"        ,256);      //Datumsangaben
define ("FILTER_PASSWORD"    ,512);      //In einem Kennwort zugelassene Zeichen

//Alle deutsche Spezialzeichen so kodiert das die kodierung der Datei egal ist
//die interne Dartsellung wird so von der Dateikodierung getrennt
define ("STRING_SPECIAL",html_entity_decode("&Uuml;&uuml;&Auml;&auml;&Ouml;&ouml;&szlig;"));


//////////////////////////////////////////////////////////////////////////////////////////
//Einen "sauberen" String erzeugen
//Die Filter werden mit den Konstante FILTER_* eingestellt.
function string_filter($input,$filter)
    {
    //Die einzelnen Filter wählen
    //Default ist Alphanum
    $replace="";
    if ($filter !== FILTER_NONE)
      {
      switch ($filter)
        {
        case FILTER_ALPHA        : $filter="([^a-zA-Z".STRING_SPECIAL." _]*)";                            break;
        case FILTER_NUMBER       : $filter="([^0-9\.,]*)";                                break;
        case FILTER_ALPHANUM     : $filter="([^a-zA-Z0-9".STRING_SPECIAL." ]*)";                        break;
        case FILTER_URL          : $filter="([^a-zA-Z0-9".STRING_SPECIAL."=/_\+\?\&. \*\\\:!#%@\-~]*)";    break;
        case FILTER_EMAIL        : $filter="([^a-zA-Z0-9".STRING_SPECIAL.".,_\-@\+ ]*)";                    break;
        case FILTER_SECURE       : $filter="([^a-zA-Z0-9./\x22_\-\+@\[\]&;!$%\(\)\{\},<>\^#\?\n\\".STRING_SPECIAL.":='€ ~]*)";            break;
        case FILTER_BASE64       : $filter="([^a-zA-Z0-9\+/\=]*)";                        break;
        case FILTER_DATE         : $filter="([^0-9a-zA-Z. ]*)";                        break;
        case FILTER_PASSWORD     : $filter="([^a-zA-Z0-9".STRING_SPECIAL."\+/\]*_@\(\),. :~])";        break;
        default                  : $filter="([^a-zA-Z _]*)";                            break;
        }
      $result=preg_replace($filter,$replace,$input);
      }
    else
      {
      //Ungefiltert
      $result=$input;
      }

    return($result);
    }
    
function string_securepath($input)
    {
    $result=trim(strtolower($input));
    $result=preg_replace("#([^a-z0-9\\.\\-\\:\\/]+)#","_",$result);
    return($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen Text (mehr oder weniger) gefühlvoll auf eine vorgegebene Länge kürzen
function string_shorten($input,$length)
    {
    //Auf Fehler prüfen
    if (string_length($input) < $length)
        {
        return($input);
        }

    //Den Text explode
    $slices=explode(" ",$input);


    //Und los gehts
    $result="";
    $word=reset($slices);
    while (string_length($result) < $length)
        {
        $result.=" ".$word;
        $word=next($slices);
        }

    unset($slices);

    return(trim($result)."...");
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen Zufallsstring der Länge $size erzeugen
function string_random($size)
    {
    global $_all_chars;

    $result="";
    while ($size>0)
        {
        $result.=$_all_chars[rand(0,count($_all_chars)-1)];
        $size--;
        }
    return ($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Bestimmte Strings prüfen

//Ist es eine EMail
function string_is_email($input)
    {
    return(preg_match("/[a-zA-Z0-9\\.\\-]+@[a-zA-Z0-9\\.\\-]+\.[a-zA-Z]{2}/",$input)==1);
    }


//////////////////////////////////////////////////////////////////////////////////////////
//Einen String als Datei abspeichern
function string_write($string,$filename)
    {
    $fp=fopen($filename,"wb");
    if ($fp!=FALSE)
        {
        fwrite($fp,$string);
        fclose($fp);
        }
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen String glesen
function string_read($filename)
    {
    if (is_readable($filename))
        {
        return(file_get_contents($filename));
        }
    else
        {
        return(FALSE);
        }
    }

//////////////////////////////////////////////////////////////////////////////////////////
//ToHex From HEx
function string_to_hex($input)
    {
    return(bin2hex($input));
    }

function hex_to_string($input)
    {
    return(pack("H*",$input));
    }
</script>