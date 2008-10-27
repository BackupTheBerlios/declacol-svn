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
require_once("conf.classes.php");
require_once(PATH_LIBS."lib.xml.php");


class newsletter
    {
    var $_registry = FALSE;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function newsletter(&$registry)
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
        $result[CLASS_INDEX_ID]        = "newsletter";  //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "newsletter";  //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";         //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = TRUE;          //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;         //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = TRUE;          //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;          //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= TRUE;          //Soll die Datenbank komprimiert werden (gz)

        $result[CLASS_INDEX_RUNLEVEL]  = 10;             //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Ausgabedatei für einen RSS-Feed setzen
    function setrssfile($filename)
        {
        $this->_registry->write("","feedfile",$filenam);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Wieviele Items werden pro Kanal maximal vorgehalten ?
    function setrssmax($value)
        {
        $this->_registry->write("","rsscount",intval($value));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Neuigkeit zufügen
    function add($modul,$title,$link,$message,$author)
        {
        $this->addfeed($modul,$title,$link,$message,$author);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Newsletter-Email schicken
    function addmail($modul,$title,$link,$message,$author)
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Newsfeed eintragen
    function addfeed($modul,$title,$link,$message,$author)
        {
        //Den RSS-Feed aktualisieren
        require_once(PATH_CLASSES."class.rssfeed.php");

        //Zur Sicherheit den Kanal miteinrichten
        $channel=strtolower($modul);
        $rss=new rssfeed($this->_registry);
        $rss->maxcount=$this->_registry->read("","rsscount",200);
        $rss->addchannel($channel,$modul,$link,$modul);
        $rss->additem   ($channel,$title,$link,$author,$message);
        $rss->write( $this->_registry->read("","feedfile","feed.rss") );
        }
    }
</script>