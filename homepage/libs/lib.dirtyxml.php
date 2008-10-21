<script language="php">
//////////////////////////////////////////////////////////////////////////////
///
/// Quick and Dirty XML-Funktionen
/// Damit lassen sich schnell Arrays in XML-Dateien wandeln und zurück.
/// Einschränkungen :
/// - Keine Properties e.g. <tag abc="property">text</tag> wird zu <tag>text</tag>
/// - Mehrere identische Tags auf der gleichen Ebene können prinzipbedingt
///   nicht in ein assoziatives Array geparst werden.
/// - Es wird keine Fehlerprüfung durchgeführt
//////////////////////////////////////////////////////////////////////////////
define("TAG_OPEN"       ,"<");
define("TAG_CLOSE"      ,">");
define("TAG_END"        ,"/");
define("TAG_NEXTLINE"   ,"\n");
define("TAG_REGEX"      ,"#".TAG_OPEN."[\\s]*([^".TAG_CLOSE."]*)[\\s]*".TAG_CLOSE."([\\D\\S]*?)".TAG_OPEN.TAG_END."\\1".TAG_CLOSE."#");

//////////////////////////////////////////////////////////////////////////////
/// Eine XML-Datei in ein assoziatives Array parsen
/// Dabei werden rekursiv alle Einträge per RegEx durchgeparst und das
/// Ergebnis als assoziatives Array zurückgegeben
function xmltoarray($xml)
    {
    $result=false;
    $xmlresult=array();

    $xml=trim($xml);

    //Tags und Text zerlegen (mit Lazy Modifier, um gleichnamige Tags zu erkennen)
    if (preg_match_all(TAG_REGEX,$xml,$xmlresult,PREG_PATTERN_ORDER)>0)
       {
       //Die Treffer holen
       reset($xmlresult);
       $tags  =next($xmlresult);
       $values=next($xmlresult);

       //Alle Tags durchgehen
       foreach ($tags as $key => $tag)
           {
           //Dazugehörige Daten holen
           $data=$values[$key];

            //Tags sind immer in Kleinschrift
           $tag=strtolower($tag);

           //Haben die Daten noch Tags ?
           if (strpos($data,TAG_OPEN)!==FALSE)
               {
               //Dann rekursiv auflösen
               $data=xmltoarray($data);
               }

           //Abspeichern wenn alles OK war
           if ($data!==FALSE)
               {
               $result[$tag]=$data;
               }
           }
       }
    return($result);
    }

//////////////////////////////////////////////////////////////////////////////
//Aus einem beliebig verschachtelten Array eine XML-Darstellung machen
//(Hauptfunktion)
//format = TRUE formatiert die Rückgabe automatisch
function arraytoxml($array,$format=TRUE)
    {
    $result=_array2xml($array,0,$format);
    return($result);
    }

//Aus einem beliebig verschachtelten Array eine XML-Darstellung machen
//(Unterfunktion)
function _array2xml($array,$level,$format)
    {
    $space ="";
    $result="";
    $nl    ="";

    //Automatisches Einrücken
    if ($format)
        {
        for ($fill=0; $fill < $level; $fill++)
            {
            $space.="   ";
            }
        $nl=TAG_NEXTLINE;
        }

    //Jeden Eintrag durchgehen
    if (is_array($array))
    {
    foreach ($array as $index => $value)
        {
        //Tags sind immer Kleinschrift
        $index=strtolower($index);

        //Recursiv durchgehen
        if (is_array($value))
            {
            $result.=$space.TAG_OPEN.$index.TAG_CLOSE.$nl;
            $result.=_array2xml($value,$level+1,$format);
            $result.=$space.TAG_OPEN.TAG_END.$index.TAG_CLOSE.$nl;
            }
        else
            {
            $result.=$space.TAG_OPEN.$index.TAG_CLOSE.$value.TAG_OPEN.TAG_END.$index.TAG_CLOSE.$nl;
            }
        }
    }
    return($result);
    }
</script>