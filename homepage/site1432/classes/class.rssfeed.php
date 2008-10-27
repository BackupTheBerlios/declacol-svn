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
/// Newsletterklasse,
/// die Neuigkeiten als EMail an registrierte Benutzer verschickt und
/// parallel dazu einen RSS-Feed anbietet
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
require_once(PATH_LIBS."lib.xml.php");

//Eigentliche Klasse
class rssfeed
    {
    var $maxcount  = 200;
    //Private
    var $_registry = FALSE;
    var $_basedir  = "feed/";

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
    //Die XML-Datei schreiben
    function write($rssfile)
      {
      $rss="<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
      $rss.="<rss version=\"2.0\">\n";

      //Alle Kanäle lesen
      $channels=$this->_registry->enum($this->_basedir);

      echo "<pre>";

      foreach ($channels as $channelid => $channel)
        {
        //Kopf des Kanals
        $channelpath=$this->_basedir.$channelid;
        $channel=unserialize($this->_registry->read($channelpath,"data",""));

        $rss.=" <channel>\n";
        $rss.=arraytoxml($channel,"  ");

        //Alle Items
        $itempath=$channelpath."/items/";
        $items=$this->_registry->enum($itempath);
        krsort($items);
        foreach($items as $itemid => $item)
            {
            $item=unserialize($this->_registry->read($itempath,$itemid,""));
            
            $rss.="  <item>\n";
            $rss.=arraytoxml($item,"   ");
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
    //Einen neuen Kanal initialisieren
    function addchannel($channelid,$title,$link,$description)
        {
        $channelid=$this->createchannelid($channelid);
        if ($this->_registry->exists("",$channelid)==FALSE)
            {
            $this->_registry->write($this->_basedir.$channelid,
                                    "data",
                                    serialize(array("title"=>$title,
                                                    "link"=>$link,
                                                    "description"=>$description,
                                                    "pubDate"=>$this->_feeddate(CURRENT_TIME)
                                                    )));
            }
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Kanal löschen
    function delchannel($channelid)
        {
        $channelid=$this->createchannelid($channelid);
        return ( $this->_registry->del($this->_basepath, $channelid) );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen RSS-Eintrag machen
    function additem($channelid,$title,$link,$author,$description)
      {
      //Simple ID bilden
      $channelid=$this->createchannelid($channelid);
      
      //Gibt es diesen Kanal überhaupt ?
      if ( $this->_registry->exists($this->_basedir,$channelid) == TRUE )
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
        $this->_registry->write($this->_basedir.$channelid."/items/",$item["guid"],serialize($item) );
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
      $path=$this->_basedir.$channelid."/items/";
      
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