<script language="php">
//////////////////////////////////////////////////////////////////////////////
///
/// allgemeine XML-Klasse
///
/// Parsed eine XML-Datei in einen Node-Baum (siehe type.basics.php)
/// -KEINE Fehlerprüfung (werden ignoriert)
/// -KEINE Attribute  (werden ignoriert)
///
///Beispiel (Aus Datei)
///
///$xml=new xml();
/////Datei einlesen
///$xml->open("meinexml.xml");
/////Ausgeben
///print_r($xml->write());
///$xml->destroy();
///
///oder (Aus String)
///
///$xml=new xml();
/////Datei einlesen
///$xml->import("<xml><item>Item1</item></xml>");
/////Ausgeben
///print_r($xml->write());
///$xml->destroy();
///
//////////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("CLASS_EASYXML","class_easyxml");
define ("CLASS_EASYXML_VERSION","0.10");
if (isset($debug)) $debug->add(CLASS_EASYXML,"version ".CLASS_EASYXML_VERSION);

//////////////////////////////////////////////////////////////////////////////
/// Zur fexiblen Handhabung die Tag-Limiter per Const definieren
define("EXML_TAG_OPEN"       ,"<");
define("EXML_TAG_CLOSE"      ,">");
define("EXML_TAG_END"        ,"/");
define("EXML_TAG_NEXTLINE"   ,"\n");
define("EXML_TAG_REGEX"      ,"#<[\\W]*([^ ]+)[^>]*>([\\W\\w]+?)</[\\W]*\\1[\\W]*>#");

//Basistypen laden (Hier werden die Nodes definiert)
require_once("type.basics.php");

//////////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
class xml
    {
    var $tree         = array();
    var $internal_key = 0;

    //////////////////////////////////////////////////////////////////////////////
    /// Konstruktor
    function xml()
        {
        //Immer einen Basisknoten setzen
        $this->clear();
        }
        
    //////////////////////////////////////////////////////////////////////////////
    /// Destruktor
    function destroy()
        {
        //Den Baum freigeben
        $this->clear();
        
        //Und den Basisknoten auch löschen
        $this->tree->destroy();
        }
        
    //////////////////////////////////////////////////////////////////////////////
    /// Einen übergebenen String parsen
    function import($string)
        {
        //Alles initialisieren
        $this->clear();
        //Eingabestring aufräumen
        $this->_trim($string);
        //Und los
        $this->_parsexml($string,$this->tree);

        //Wenn wir nur einen FirstChild haben,
        //setzen wir diesen als basis
        if ($this->tree->childcount==1)
            {
            $this->tree=reset($this->tree->child);
            }

        return($this->tree->childcount>0);
        }
        
    //////////////////////////////////////////////////////////////////////////////
    /// Eine Datei parsen
    function read($filename)
        {
        return( $this->import ( @file_get_contents($filename) ) );
        }
        
        
    //////////////////////////////////////////////////////////////////////////////
    /// Den Baum als String zurückgeben
    function write()
        {
        $result="";
        
        //Mal wieder alles rekursiv machen
        $this->_write($this->tree,$result);
        
        return($result);
        }

        
    //////////////////////////////////////////////////////////////////////////////
    /// Den XML-Baum löschen und einen neuen Basisknoten anlegen
    function clear()
        {
        if ($this->tree!=FALSE)
            {
            $this->tree->destroy();
            }
        $this->tree = new xmlnode("base");
        $this->internal_key=0;
        }
        
    //////////////////////////////////////////////////////////////////////////////
    /// Node mit dem Name $name unterhalb des übergebenen Knotens finden
    function searchbyname($name,$startnode=FALSE)
        {
        //Wird kein Startnode übergeben, durchsuchen wir den gesamten Baum
        if ($startnode===FALSE)
            {
            $startnode=$this->tree;
            }
        //Ab die Post
        $node=$this->_searchname($name,$startnode);

        //Wenn es ein Ergebnis gibt, dann den neuen Knoten
        //in eine XML-Klasse packen
        if ($node!=FALSE)
            {
            //Neue XML-Klasse erzeugen
            $result=new xml();
            $result->tree=$node;
            }
        else
            {
            $result=FALSE;
            }

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////////
    /// Node mit dem Value $value unterhalb des übergebenen Knotens finden
    function searchbyvalue($value,$startnode=FALSE)
        {
        //Wird kein Startnode übergeben, durchsuchen wir den gesamten Baum
        if ($startnode===FALSE)
            {
            $startnode=$this->tree;
            }
        //Ab die Post
        $node=$this->_searchvalue($value,$startnode);

        //Wenn es ein Ergebnis gibt, dann den neuen Knoten
        //in eine XML-Klasse packen
        if ($node!=FALSE)
            {
            //Neue XML-Klasse erzeugen
            $result=new xml();
            $result->tree=$node;
            }
        else
            {
            $result=FALSE;
            }

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////////
    /// Node mit dem Value $value unterhalb des übergebenen Knotens finden
    function searchbykey($key,$startnode=FALSE)
        {
        //Wird kein Startnode übergeben, durchsuchen wir den gesamten Baum
        if ($startnode===FALSE)
            {
            $startnode=$this->tree;
            }
        //Ab die Post
        $node=$this->_searchkey($key,$startnode);

        //Wenn es ein Ergebnis gibt, dann den neuen Knoten
        //in eine XML-Klasse packen
        if ($node!=FALSE)
            {
            //Neue XML-Klasse erzeugen
            $result=new xml();
            $result->tree=$node;
            }
        else
            {
            $result=FALSE;
            }

        return($result);
        }


    //////////////////////////////////////////////////////////////////////////////
    //Den ersten und den nächsten Eintrag eines Suchlaufes holen holen
    function findfirst($name)
        {
        return($this->_searchname($name,$this->tree));
        }

    function findnext($lastnode)
        {
        $result=FALSE;
        if ($lastnode!=FALSE)
            {
            //Den Vater holen
            $parent=$this->searchbykey($lastnode->parent);
            
            //Und den nächsten Eintrag greifen
            if ($parent!=FALSE)
                {
                //Nun einfach das Childarray durchgehen
                for ($index = $lastnode->id; $index < $parent->tree->childcount; $index++)
                    {
                    //Node existiert ?
                    if (isset($parent->tree->child[$index]))
                        {
                        //Name OK ?
                        if ($parent->tree->child[$index]->name==$lastnode->name)
                            {
                            //Gefunden
                            $result=$parent->tree->child[$index];
                            $index=$parent->tree->childcount;
                            }
                        }
                    }
                }
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////////
    /// Den Vater des Nodes liefern
    function getparent($node)
        {
        return($this->searchbykey($node->parent,$this->tree));
        }

    //////////////////////////////////////////////////////////////////////////////
    /// Ab hier nur noch interne Funktionen
    //////////////////////////////////////////////////////////////////////////////

    //Den baum in einen String umsetzen
    function _write($startnode,&$result,$level=0)
        {
        $space=str_pad("",$level << 3," ");
        
        //Starttag
        $result.=$space;
        $result.=EXML_TAG_OPEN.$startnode->name.EXML_TAG_CLOSE.EXML_TAG_NEXTLINE;

        //Wenn es einen Wert gibt, diesen aufzeichnen
        if ($startnode->value!="")
            {
            $result.=$space;
            $result.=$startnode->value.EXML_TAG_NEXTLINE;
            }

        //Wenn der aktuelle Knoten kein Kinder hat,
        //dann einfach nur seinen Tag setzen
        //ansonsten rekursiv aufrufen
        if ($startnode->has_child)
            {

            foreach ($startnode->child as $child)
                {
                $this->_write($child,$result,$level+1);
                }
            }

        //Endtag
        $result.=$space;
        $result.=EXML_TAG_OPEN.EXML_TAG_END.$startnode->name.EXML_TAG_CLOSE.EXML_TAG_NEXTLINE;
        }


    //Den nächsten gültigen Key zurückgeben
    function _getnextkey()
        {
        $this->internal_key++;
        return($this->internal_key);
        }

    //////////////////////////////////////////////////////////////////////////////
    //Einen Baum nach dem Node mit dem Namen $name durchsuchen (rekursiv)
    function _searchname($name,$startnode)
        {
        $result=FALSE;
        
        //Sind wir selbst der Treffer ?
        if ($startnode->name==$name)
            {
            $result=$startnode;
            }
        else
            {
            //Nein, dann alle Kinder durchsuchen
            $child=reset($startnode->child);
            while ( ($child) && (!$result) )
                {
                $result=$this->_searchname($name,$child);
            
                //Nächstes Kind greifen
                $child=next($startnode->child);
                }
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////////
    //Einen Baum nach dem Node mit dem Value $value durchsuchen (rekursiv)
    function _searchvalue($value,$startnode)
        {
        $result=FALSE;

        //Sind wir selbst der Treffer ?
        if ($startnode->name==$value)
            {
            $result=$startnode;
            }
        else
            {
            //Nein, dann alle Kinder durchsuchen
            $child=reset($startnode->child);
            while ( ($child) && (!$result) )
                {
                $result=$this->_searchvalue($value,$child);

                //Nächstes Kind greifen
                $child=next($startnode->child);
                }
            }
        return($result);
        }

    //////////////////////////////////////////////////////////////////////////////
    //Einen Baum nach dem Node mit dem Value $value durchsuchen (rekursiv)
    function _searchkey($key,$startnode)
        {
        $result=FALSE;

        //Sind wir selbst der Treffer ?
        if ($startnode->key==$key)
            {
            $result=$startnode;
            }
        else
            {
            //Nein, dann alle Kinder durchsuchen
            $child=reset($startnode->child);
            while ( ($child) && (!$result) )
                {
                $result=$this->_searchkey($key,$child);

                //Nächstes Kind greifen
                $child=next($startnode->child);
                }
            }
        return($result);
        }
        
    //////////////////////////////////////////////////////////////////////////////
    //Ungebetene Einträge ausfiltern
    function _trim($input)
        {
        //SingleTags rausnehmen, da diese noch nicht geparst werden
        $input=preg_replace("#<[^>]*/>#","",$input);
        //Leerzeilen brauchen wir nicht
        $input=preg_replace("#\\s\\r|\\n#","",$input);
        return($input);
        }


    //////////////////////////////////////////////////////////////////////////////
    //Die Kernfunktion, um XML in Nodes zu parsen
    function _parsexml($xml,&$base)
        {
        @set_time_limit(10);
        
        //Ergebnisarray initialisieren
        $xmlresult=array();

        //Cleanen
        $xml=trim($xml);

        //Tags und Text zerlegen (mit Lazy Modifier, um gleichnamige Tags zu erkennen)
        if (preg_match_all(EXML_TAG_REGEX,$xml,$xmlresult,PREG_PATTERN_ORDER)>0)
            {
            //Die Treffer holen
            reset($xmlresult);
            $tags  =next($xmlresult);
            $values=next($xmlresult);

            //Alle Tags durchgehen
            foreach ($tags as $key => $tag)
                {
                //Tags sind immer in Kleinschrift
                $tag=strtolower($tag);

                //Dazugehörige Daten holen
                $data=trim($values[$key]);

                //Haben die Daten noch Tags ?
                if (strpos($data,EXML_TAG_OPEN)!==FALSE)
                    {
                    //Wenn es vor dem Tag noch eine Bezeichnung gibt, nehmen wir diese raus
                    $pos=strpos($data,EXML_TAG_OPEN);
                    if ($pos!==0)
                        {
                        $value=trim(substr($data,0,$pos-1));
                        $data =substr($data,$pos,strlen($data));
                        }
                    else
                        {
                        $value="";
                        }
                    //Neuen Node erzeugen
                    $node=new xmlnode($tag,$value);
                    $node->key=$this->_getnextkey();
                    $node->parent=$base->key;

                    //Dann rekursiv auflösen
                    $this->_parsexml($data,$node);

                    //Und dem Baum zufügen
                    $base->addchild($node);
                    $node->destroy();
                    }
                else
                    {
                    //Ansonsten den Eintrag anhängen
                    $node=new xmlnode ( $tag , trim($data) );
                    $node->key=$this->_getnextkey();
                    $node->parent=$base->key;

                    $base->addchild($node);
                    }
                }
            }
        }
    }
</script>