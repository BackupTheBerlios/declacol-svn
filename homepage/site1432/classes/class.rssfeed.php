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
/// Beispielklasse,
/// An diese Vorlage sollten sich alle Klassen halten. Insbesonder, wenn diese per automatische
/// Initialisierung durch die Install-Methode verfügbar gemacht werden sollen.
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

//require_once("conf.classes.php");

$rss=new rssfeed("rss.db");
$rss->add("Mein erster Feed","http://sven-of-nine.de","Sven of Nine","Mein erster Feed in der langen Version");
$rss->write("feed.rss");
echo file("feed.rss");
$rss->destroy();

//Eigentliche Klasse
class rssfeed
    {
    var $channel  = array();
    var $items    = array();
    
    //Anzahl der maximal gepufferten Einträge
    var $maxcount = 10;
    
    //Name zur Datenbankdatei
    var $dbfile   = FALSE;
      
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function rssfeed($dbfile)
        {
        $this->dbfile=$dbfile;
        $this->loaddb();
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->savedb();
        unset($this);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die XML-Datei schreiben
    function write($rssfile)
      {
      //Altes entfernen
      $this->shrinkdb($this->maxcount);

      //Und einfach als String bauen
      $rss ="<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
      $rss.="<rss version=\"2.0\">\n";
      $rss.="<channel>\n\n";
      $rss.=$this->arraytoxml($this->_feedhead());

      $item=end($this->items);
      while ( $item != FALSE )
        {
        $rss.="<item>\n";
        $rss.=$this->arraytoxml($item);
        $rss.="</item>\n\n";
        $item=prev($this->items);
        }
      
      $rss.="</channel>\n";
      $rss.="</rss>\n";
      
      echo $rss;
      file_put_contents($rssfile,$rss);
      }
       
    //Simple ArrayToXML-Konvertierung
    function arraytoxml($array)
      {
      $result="";
      
      foreach ($array as $tag => $val)
        {
        $result.="<".$tag.">";
        if ( is_array($val) == TRUE )
          {
          $result.=$this->arraytoxml($val);
          }
        else
          {
          $result.=$val;
          }
        $result.="</".$tag.">\n";
        }
      return($result);
      }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen RSS-Eintrag machen
    function add($title,$link,$author,$description)
      {
      $item=$this->_feeditem();
      $item["title"]      = $title;
      $item["link"]       = $link;
      $item["description"]= $description;
      $item["author"]     = $author;
      $item["pubDate"]    = $this->_feeddate(time());
      $item["guid"]       = md5($title.$author.$item["pubDate"].$description.rand(0,65535));
      $this->items[]=$item;
      }
      
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Den FeedKopf erzeugen
    function _feedhead()
      {
      $result=array();
      $result["title"]      = "Titel";
      $result["link"]       = "LinkToFeedHomePage";
      $result["description"]= "Description";
      $result["language"]   = "language";
      $result["copyright"]  = "copyright";
      $result["pubDate"]    = "creationdate";
      $result["image"]      = array("url"=>"imageurl",
                                   "title"=>"imagetitle",
                                   "link"=>"linkfromimage");
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
      return(date("d.m.Y H:i:s",$time));
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Datenbank reduzieren
    function shrinkdb($count)
      {
      while (count ($this->items) > $count )
        {
        array_shift($this->items);
        }
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Datenbank schreiben
    function savedb()
      {
      $fp=fopen($this->dbfile,"w+");
      if ($fp !== FALSE)
        {
        $this->shrinkdb($this->maxcount);

        //Und den Rest wegschreiben
        foreach ($this->items as $item)
          {
          if (is_array($item)==TRUE)
            {
            fputs($fp,$this->_encode($item)."\n");
            }
          }
        fclose($fp);
        }
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Datenbank lesen
    function loaddb()
      {
      if (file_exists($this->dbfile)==TRUE)
        {
        $fp=fopen($this->dbfile,"r");
        
        if ($fp !== FALSE)
          {
          $this->items=array();
            
          while ( feof($fp) != TRUE )
            {
            //Jede Zeile ist ein Datensatz
            $item=$this->_decode(fgets($fp));
            if (is_array($item)==TRUE)
              {
              $this->items[]=$item;
              }
            }
          fclose($fp);
          }
        }
      }
      
    function _encode($input)
      {
      $input=serialize($input);
      $input=gzcompress($input,5);
      $input=base64_encode($input);
      return($input);
      }

    function _decode($input)
      {
      if ($input != "")
        {
        $input=base64_decode($input);
        $input=gzuncompress($input);
        $input=unserialize($input);
        }
      return($input);
      }

    }
</script>