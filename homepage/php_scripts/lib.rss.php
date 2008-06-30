<script language="php">
//////////////////////////////////////////////////////////////////////////////
///
/// RSS-Helper Bibliothek
///
//////////////////////////////////////////////////////////////////////////////
///
///
/// Beispiel
///
/// print_r(rsstoarray("http://www.abandonia.com/rss.php"));
///
//////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("LIB_RSS","lib_rss");
define ("LIB_RSS_VERSION","0.02");
if (isset($debug)) $debug->add(LIB_RSS,"version ".LIB_RSS_VERSION);

//Die XML Klasse nachziehen
require_once("class.easyxml.php");

//Alle Items eines RSS-Feed in ein Array wandeln
function rsstoarray($url)
    {
    $result=array();
    //URL-Lesen
    $xml=new xml();
    if ($xml->read($url))
        {
        //Nur die Items holen
        $index=1;
        $item=$xml->findfirst("item");
        while ($item)
            {
            //Das ganze Item in ein Array parsen
            foreach ($item->child as $entry)
                {
                $result["item".$index][$entry->name]=$entry->value;
                }
            $index++;
            $item=$xml->findnext($item);
            }
        }
    return($result);
    }
</script>