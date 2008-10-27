<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
//////////////////////////////////////////////////////////////////////////////
///
/// Quick and Dirty XML-Funktionen
/// Damit lassen sich schnell Arrays in XML-Dateien wandeln und zur�ck.
/// Einschr�nkungen :
/// - Keine Properties e.g. <tag abc="property">text</tag> wird zu <tag>text</tag>
/// - Mehrere identische Tags auf der gleichen Ebene k�nnen prinzipbedingt
///   nicht in ein assoziatives Array geparst werden.
/// - Es wird keine Fehlerpr�fung durchgef�hrt
//////////////////////////////////////////////////////////////////////////////
define("TAG_OPEN"       ,"<");
define("TAG_CLOSE"      ,">");
define("TAG_END"        ,"/");
define("TAG_NEXTLINE"   ,"\n");
define("TAG_REGEX"      ,"#".TAG_OPEN."[\\s]*([^".TAG_CLOSE."]*)[\\s]*".TAG_CLOSE."([\\D\\S]*?)".TAG_OPEN.TAG_END."\\1".TAG_CLOSE."#");

//////////////////////////////////////////////////////////////////////////////
/// Eine XML-Datei in ein assoziatives Array parsen
/// Dabei werden rekursiv alle Eintr�ge per RegEx durchgeparst und das
/// Ergebnis als assoziatives Array zur�ckgegeben
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
           //Dazugeh�rige Daten holen
           $data=$values[$key];

            //Tags sind immer in Kleinschrift
           $tag=strtolower($tag);

           //Haben die Daten noch Tags ?
           if (strpos($data,TAG_OPEN)!==FALSE)
               {
               //Dann rekursiv aufl�sen
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
function arraytoxml($array,$spacer="")
    {
    $result="";
    if (is_array($array)==TRUE)
        {
        foreach ($array as $tag => $val)
            {
            $result.=$spacer.TAG_OPEN.$tag.TAG_CLOSE;
            if ( is_array($val) == TRUE )
              {
              $result.=TAG_NEXTLINE.arraytoxml($val,$spacer."  ");
              $result.=$spacer.TAG_OPEN.TAG_END.trim($tag).TAG_CLOSE.TAG_NEXTLINE;
              }
            else
              {
              $result.=trim($val);
              $result.=TAG_OPEN.TAG_END.trim($tag).TAG_CLOSE.TAG_NEXTLINE;
              }
            }
        }
    return($result);
    }
</script>