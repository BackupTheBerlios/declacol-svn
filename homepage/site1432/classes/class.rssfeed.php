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
/// Simples Feedinterface
///
////////////////////////////////////////////////////////////////////////////////////////////////////

/*
Aufbau eines Feeds nach Version 2.0
<?xml version="1.0" encoding="utf-8"?>
<rss version="2.0">
  <channel>
    <title>Titel des Feeds</title>
    <link>URL der Webpräsenz</link>
    <description>Kurze Beschreibung des Feeds</description>
    <language>Sprache des Feeds (z. B. "de-de")</language>
    <copyright>Autor des Feeds</copyright>
    <pubDate>Erstellungsdatum("Tue, 8 Jul 2008 2:43:19")</pubDate>
    <image>
      <url>URL einer einzubindenden Grafik</url>
      <title>Bildtitel</title>
      <link>URL, mit der das Bild verknüpft ist</link>
    </image>
    <item>
      <title>Titel des Eintrags</title>
      <description>Kurze Zusammenfassung des Eintrags</description>
      <link>Link zum vollständigen Eintrag</link>
      <author>Autor des Artikels, E-Mail-Adresse</author>
      <guid>Eindeutige Identifikation des Eintrages</guid>
      <pubDate>Datum des Items</pubDate>
    </item>
 
    <item>
      ...
    </item>
  </channel>
</rss>
*/
require_once("conf.classes.php");


//Eigentliche Klasse
class rssfeed
    {
    var $maxcount  = 5;
    //Private
    var $_registry = FALSE;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function rssfeed(&$registry)
        {
        //Unsere Registrieung intern ablegen
        $this->_registry=$registry;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        if ( $this->_registry !== FALSE )
            {
            $this->_registry->flush();
            $this->_registry->destroy();
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "rssfeed";  //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "rssfeed";  //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";      //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = TRUE;       //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;      //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = TRUE;       //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;       //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= TRUE;       //Soll die Datenbank komprimiert werden (gz)

        $result[CLASS_INDEX_RUNLEVEL]  = 5;          //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }


    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die XML-Datei schreiben
    function write($rssfile)
      {
      $rss="<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
      $rss.="<rss version=\"2.0\">\n";

      //Alle Kanäle lesen
      $channels=$this->_registry->enum("");

      foreach ($channels as $channelid => $channel)
        {
        //Kopf des Kanals
        $channel=unserialize($this->_registry->read($channelid,"data",""));

        $rss.=" <channel>\n";
        $rss.=$this->arraytoxml($channel,"  ");

        //Alle Items
        $items=$this->_registry->enum($channelid."/items/");
        foreach($items as $itemid => $item)
            {
            $item=unserialize($this->_registry->read($channelid."/items/",$itemid,""));
            
            $rss.="  <item>\n";
            $rss.=$this->arraytoxml($item,"   ");
            $rss.="  </item>\n";
            }

        $rss.=" </channel>\n";
        }
      $rss.="</rss>";
      file_put_contents($rssfile,$rss);
      unset($rss);
      return(file_exists($rssfile));
      }
       
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Simple ArrayToXML-Konvertierung
    function arraytoxml($array,$spacer="")
      {
      $result="";
      if (is_array($array)==TRUE)
        {
        foreach ($array as $tag => $val)
            {
            $result.=$spacer."<".$tag.">";
            if ( is_array($val) == TRUE )
              {
              $result.=$this->arraytoxml($val,$spacer." ");
              }
            else
              {
              $result.=$val;
              }
            $result.="</".$tag.">\n";
            }
        }
      return($result);
      }
        
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen neuen Kanal initialisieren
    function addchannel($channel,$title,$link,$description)
        {
        $channelid=md5($channel);
        if ($this->_registry->exists("",$channelid)==FALSE)
            {
            $this->_registry->write($channelid,"data",serialize(array("title"=>$title,
                                                                      "link"=>$link,
                                                                      "description"=>$description,
                                                                      "pubDate"=>$this->_feeddate(CURRENT_TIME)
                                                                      )));
            }
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen RSS-Eintrag machen
    function additem($channel,$title,$link,$author,$description)
      {
      //Simple ID bilden
      $channelid=$this->createchannelid($channel);
      
      //Gibt es diesen Kanal überhaupt ?
      if ( $this->_registry->exists("",$channelid) == TRUE )
        {
        //Und die Datenbank shrinken
        $this->shrinkdb($channelid);

        //Paket bauen
        $item=$this->_feeditem();
        $item["title"]      = $title;
        $item["link"]       = $link;
        $item["description"]= $description;
        $item["author"]     = $author;
        $item["pubDate"]    = $this->_feeddate(CURRENT_TIME);

        //Die ID ist ein Timestamp mit Microsekunden
        list($usec, $sec) = explode(' ',microtime());
        $item["guid"]       = $sec.$usec;

        //Abspeichern
        $this->_registry->write($channelid."/items/",$item["guid"],serialize($item) );
        $result=TRUE;
        }
      else
        {
        $result=FALSE;
        }
      return($result);
      }
      
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Den FeedKopf erzeugen
    function _feedhead()
      {
      $result=array();
      $result["title"]      = "Titel";
      $result["link"]       = "LinkToFeedHomePage";
      $result["description"]= "Description";
//      $result["language"]   = "language";
//      $result["copyright"]  = "copyright";
      $result["pubDate"]    = "creationdate";
//      $result["image"]      = array("url"=>"imageurl",
//                                   "title"=>"imagetitle",
//                                   "link"=>"linkfromimage");
      return($result);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein FeedItem erzeugen    
    function _feeditem()
      {
      $result=array();
      $result["title"]      ="itemtitle";
      $result["link"]       ="LinkToFeedArticle";
      $result["description"]="Description";
      $result["author"]     ="author";
      $result["guid"]       ="uniqueid";
      $result["pubDate"]    ="creationdate";
      return($result);
      }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Feeddatum erzeugen
    function _feeddate($time)
      {
      //Thu, 21 Feb 2008 08:47:25 +0100
      return(date("D, d M Y H:i:s +0100",$time));
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //ID für einen Kanal bilden
    function createchannelid($channel)
        {
        return(md5($channel));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Datenbank eines Kanals reduzieren
    function shrinkdb($channelid)
      {
      $path=$channelid."/items/";
      
      //Einträge sortiert holen
      $items=$this->_registry->enum($path);
      ksort($items);

      //Und den Kanal schrumpfen
      while (count($items) > $this->maxcount)
        {
        //ID rausholen
        $item = unserialize(array_shift($items));
        $item = $item["guid"];

        //Eintrag aus der Registry nehmen
        $this->_registry->del($path,$item);
        }
      }
    }
</script>