<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Bibliothek mit nützlichen Stringfunktionen
///
/// einige der Funktionen sind in aktuelleren PHP-Versionen
/// zwar schon verfügbar, aber aus historischen Gründen hier noch
/// enthalten.
///
///
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("LIB_STRINGS","lib_strings");
define ("LIB_STRINGS_VERSION","0.03");
if (isset($debug)) $debug->add(LIB_STRINGS,"version ".LIB_STRINGS_VERSION);


//Alle zugelassene Zeichen
$_all_chars =Array( "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z",
                    "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z",
                    "1","2","3","4","5","6","7","8","9","0" );


//////////////////////////////////////////////////////////////////////////
//Definitionen der Stringcleaner
define ("FILTER_ALPHA"        ,1);        //Nur Buchstaben
define ("FILTER_NUMBER"        ,2);        //Nur Zahlen
define ("FILTER_ALPHANUM"    ,4);        //Nur Buchstaben und Zahlen
define ("FILTER_SECURE"        ,8);        //Nur Sichere Strings (Kein HTML)
define ("FILTER_URL"        ,16);        //Nur URL-Zeichen
define ("FILTER_EMAIL"        ,32);        //Nur E-Mail-Zeichen
define ("FILTER_BASE64"        ,128);        //Base64-Zeichen
define ("FILTER_DATE"        ,256);        //Datumsangaben
define ("FILTER_PASSWORD"    ,512);        //In einem Kennwort zugelassene Zeichen

//Alle deutsche Spezialzeichen so kodiert das die kodierung der Datei egal ist
//die interne Dartsellung wird so von der Dateikodierung getrennt
define ("STRING_SPECIAL",html_entity_decode("&Uuml;&uuml;&Auml;&auml;&Ouml;&ouml;&szlig;"));

//////////////////////////////////////////////////////////////////////////////////////////
//Den Linken Teil eines Strings der Länge Length zurückliefern
function string_left($input,$length)
    {
    $result=substr($input,0,$length);
    return($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen Teil der Länge length ab dem Index start aus einem String schneiden
function string_mid($input,$start,$length)
    {
    $result=substr($input,$start,$length);
    return($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Den rechten Teil eines Strings der Länge Length zurückliefern
function string_right($input,$length)
    {
    $result=substr($input,strlen($input)-$length,$length);
    return($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Die erste Position von Needle in Haystack zurückliefern (Suche von Links)
function string_pos_left($haystack,$needle)
    {
    //Die Standardfunktion nehmen
    return(strpos($haystack,$needle));
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Die erste Position von Needle in Haystack zurückliefern (Suche von Rechts)
function string_pos_right($haystack,$needle)
    {
    //Die Standardfunktion nehmen
    return(strrpos($haystack,$needle));
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen String anfügen, wenn er noch nicht hintendran ist
function string_add($string,$tail)
    {
    if (string_right($string,strlen($tail))!=$tail)
        {
        return($string.$tail);
        }
    else
        {
        return($string);
        }
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen String entfernen, wenn er hintendran ist
function string_remove($string,$tail)
    {
    if (string_right($string,strlen($tail))==$tail)
        {
        return(string_left($string,strlen($string)-strlen($tail)));
        }
    else
        {
        return($string);
        }
    }



//////////////////////////////////////////////////////////////////////////////////////////
//Stringteile ersetzen
function string_replace($haystack,$needle,$replace)
    {
    return(str_replace($needle,$replace,$haystack));
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen "sauberen" String erzeugen
//Die Filter werden mit den Konstante FILTER_* eingestellt.
function string_filter($input,$filter)
    {
    //Die einzelnen Filter wählen
    //Default ist Alphanum
    $replace="";
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

    return($result);
    }
    
function string_securepath($input)
    {
    $result=trim(strtolower($input));
    $result=preg_replace("#([^a-z0-9\\.\\-\\:\\/]+)#","_",$result);
    return($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Ersetzt mehrfache Zeichen durch ein einzelnes
//Es muß nur ein einzelnes Zeichen übergeben werden.
// $needle=a ersetzt also "aaaa" durch "a"
function string_unique($input,$needle)
    {
    //RegEx-Filter
    $filter="/(".$needle.$needle."*)/";

    //Einfach ersetzen
    $result=preg_replace($filter,$needle,$input);

    return ($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Die HTML-Entities in einem String ersetzen
function string_replace_entities($input)
    {
    //Alle Entities verwursten
    $result=htmlentities($input);

    //Und die Anführungsstriche zubauen
    $result=string_replace($result,"'","&#039;");
    $result=string_replace($result,"\x22","&#022;");

    //Fertig
    return($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Die Entities in einem STring zurückverwandlen
function string_replace_html($input)
    {
    //Erstmal mit der internen Funktion ran
    $result=html_entity_decode($input);

    //Und dann unsere eigenen Entities ersetzen
    $result=string_replace($result,"&#039;","'");
    $result=string_replace($result,"&#022;","\x22");

    //Fertig
    return($result);
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Die Länge eines String zurückgeben
function string_length($input)
    {
    return(strlen($input));
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Den Dateinamen extrahieren
function string_extractfilename($input)
    {
    //Pfadinfos holen
    $pathinfo=pathinfo($input);

    if (isset($pathinfo["basename"]))
        {
        return($pathinfo["basename"]);
        }
    else
        {
        return(FALSE);
        }
    }


//////////////////////////////////////////////////////////////////////////////////////////
//Den Dateipfad extrahieren (Endet immer mit einem Slash
function string_extractfilepath($input)
    {
    //Pfadinfos holen
    $pathinfo=pathinfo($input);

    if (isset($pathinfo["dirname"]))
        {
        return($pathinfo["dirname"]."/");
        }
    else
        {
        return(FALSE);
        }
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Die Dateiextension extrahieren
function string_extractfileext($input)
    {
    //Pfadinfos holen
    $pathinfo=pathinfo($input);

    if (isset($pathinfo["extension"]))
        {
        return($pathinfo["extension"]);
        }
    else
        {
        return(FALSE);
        }
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Boolean in string  "TRUE" / "FALSE" wandeln
function string_bool2str($boolean)
    {
    return($boolean?"TRUE":"FALSE");
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen Strin gin Bool wandeln
function string_str2bool($string)
    {
    $result=FALSE;

    $string=strtolower($string);

    $result|=strpos($string,"on");
    $result|=strpos($string,"true");
    $result|=strpos($string,"yes");

    return( (bool) $result );
    }

//////////////////////////////////////////////////////////////////////////////////////////
//In einem String jeden Wortanfang groß schreiben
function string_word_upcase($input)
    {
    return(preg_replace("/([a-zA-Z])([a-zA-Z]*)/e","strtoupper('\\1').'\\2'",$input));
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Einen Text nach jedem Zeichen mit einem <br/> beglücken
function string_tower($input)
    {
    $result="";
    $count=string_length($input);

    //Fehler abfangen
    if ($count!=0)
        {
        for ($index=0; $index < $count; $index++)
            {
            $result.=$input[$index]."<br />";
            }
        }
    //Fertig
    return ($result);
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
//Den Soundex-Code eines Strings erzeugen
function string_code($string,$clean=FALSE)
    {
    if ($clean!=FALSE)
        {
        //Und flatten
        $string=preg_replace("#[^a-zA-Z0-9]+#","",$string);
        }
    return(soundex($string));
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
//Einen Teil in einem String Hilighten
function string_highlight($stack,$needle)
    {
    if ($needle!="")
        {
        return(str_replace($needle,"[B]".$needle."[/B]",$stack));
        }
    else
        {
        return($stack);
        }
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Den linken Teil eines String bis zum Limiter extrahieren
function string_leftpart($string,$limiter)
    {
    //Wenn es keinen Limiter gibt, geben wir den ganzen String zurück
    $pos=strpos($string,$limiter);
    if ($pos!==FALSE)
        {
        return(substr($string,0,$pos));
        }
    else
        {
        return($string);
        }
    }

//////////////////////////////////////////////////////////////////////////////////////////
//Den Rechten Teil eines String bis zum Limiter extrahieren
function string_rightpart($string,$limiter)
    {
    return(trim(substr($string,strpos($string,$limiter)+1,strlen($string))));
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
function string_tohex($input)
    {
    return(bin2hex($input));
    }

function string_fromhex($input)
    {
    return(pack("H*",$input));
    }


</script>