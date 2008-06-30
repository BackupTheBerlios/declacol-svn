<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Treeview erlaubt das Einfache erstellen und einbinden von
/// Baumstrukturen in die eigene HTML-Seite
///
/// Einfach mit Add(ID_NONE,Entry,CallBackFunktion,Daten) einen Knoten
/// einf�gen oder mit Add(IDEinesKnotens,Entry,CallBackFunktion)
/// unter einen bestehenden Knoten einf�gen
///
/// Show() Stellt das ganze dann dar und verwaltet die Verlinkung
/// Die Struktur TreeView->Activenode enth�lt dann die bei Add �bergebenen
/// Daten oder False, fals kein Knoten gew�hlt wurde
///
//////////////////////////////////////////////////////////////////////////
require_once(DIR_LIB."type.basics.php");

//Versioninfo speichern
define ("CLASS_TREEVIEW","class_treeview");
define ("CLASS_TREEVIEW_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_TREEVIEW,"version ".CLASS_TREEVIEW_VERSION);

//////////////////////////////////////////////////////////////////////////
//Trennzeich im Steuerlink
define ("TREE_SLICE"  ,"x");

//Verlinkungsmodus
define ("TREE_LINK_CMD"              ,"cmd");
define ("TREE_LINK_CMDID"            ,"cmdid");
define ("TREE_LINK_SUBCMD"           ,"subcmd");
define ("TREE_LINK_SUBCMDID"         ,"subcmdid");


//Erweitrung, um den Status leichter behandlen zu k�nnen
class treenode extends simplenode
    {
    var $haschild = FALSE;
    var $isopen   = FALSE;
    }

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class treeview
    {
    //Aktueller Auffaltstatus des Baumes
    var $status        = array();
    //Daten des ausgew�hlten Knoten oder FALSE
    var $activenode    = FALSE;
    var $activeid      = ID_NONE;
    //Einr�ckbreite
    var $span          = 2;

    //Um das ganze zu beschleunigen
    //legen wir nach jeder Suche einen CacheEintrag an
    var $icache=array();
    
    var $icon_closed   ="+";
    var $icon_open     ="o";
    var $icon_node     ="-";

    //Auf welcher Ebene wird die Verlinkung gemacht
    var $linkmode      = TREE_LINK_CMD;

    //Private Eigenschaften
    var $internal_tree = array();
    var $internal_id   = ID_NONE;
    

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function treeview($name="root")
        {
        //Alle Werte auf Default setzen
        $this->setdefault($name);
        }

    //////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        unset($this->internal_tree);
        }

    //////////////////////////////////////////////////////////////////////////
    //Verbindungsaufbau
    function open()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    //Verbindungsabbau
    function close()
        {
        }
    
    //////////////////////////////////////////////////////////////////////////
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault($name)
        {
        $this->internal_tree = new treenode();
        $this->internal_tree->data["name"]    =$name;
        $this->internal_tree->data["callback"]=FALSE;
        $this->internal_tree->data["data"]    =FALSE;
        $this->internal_id   = ID_NONE;
        $this->status        = array(ID_NONE);
        $this->cache         = array();
        $this->activenode    = FALSE;
        }
    
    //////////////////////////////////////////////////////////////////////////
    //Datenbank und alles andere erzeugen
    //wird hier nicht benutzt
    function install()
        {
        return(TRUE);
        }
    
    //////////////////////////////////////////////////////////////////////////
    //Datenbank und alles andere zerst�ren
    //wird hier nicht benutzt
    function uninstall()
        {
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Einen Eintrag zuf�gen und die ID des Nodes zur�ckgeben
    //Dazu gehen wir den Datenbaum rekursiv durch und
    //h�ngen an der entsprechenden Stelle ein
    //Data kann mit beliebigen Daten beladen werden
    //Open gibt an, ob dieser Punkt ge�ffnet sein soll
    //Selected gibt an, ob ein Node ausgew�hlt sein soll
    function add($id,$name,$callback,$data)
        {
        global $html;
        
        //Name und Referenz der Callbackfunktione in einem neuen Knoten ablegen
        $node=new treenode();
        $node->id              = ID_NONE;
        $node->data["name"]    = $name;
        $node->data["callback"]= $callback;
        $node->data["data"]    = $data;

        //Den gew�nschten Vaterknoten suchen
        if ( $this->findnode($id) !== FALSE)
            {
            //Dann Node in die Referenz einh�ngen
            $node->id=$this->_getnextid();
            $this->cache[$id]->child[$node->id]=$node;
            $this->cache[$id]->haschild=TRUE;
            
            //Und den Knoten selbst cachen
            $this->cache[$node->id]=&$this->cache[$id]->child[$node->id];
            }
        return($node->id);
        }
        
    //////////////////////////////////////////////////////////////////////////////////////
    //Einen Knoten anhand seiner ID finden
    //Das Ergebis ist entweder die ID oder FALSE
    //Der gefundene Knoten wird als Referenz im Cache abgelegt
    function findnode(&$id)
        {
        $result=FALSE;
        //Treffer gecached ?
        if ( isset($this->cache[$id]) )
            {
            $result=$id;
            }
        else
            {
            //Dann eben die harte Tour
            if ( $this->_findnode($this->internal_tree,$id) != FALSE )
                {
                $result=$id;
                }
            }
        return($result);
        }
        
    //Die Subfunktion sucht nach einem Knoten und legt eine Referenz unter seiner ID
    //Im Cachearray ab. Leider geht es nicht mit Referenzen auf den Knoten als R�ckgabe
    //wert., da PHP4 damit massive Probleme hat
    function _findnode(&$startnode,$id)
        {
        $result=FALSE;
        
        //Sind wir selbst der Treffer ?
        if ($startnode->id==$id)
            {
            //Den Fund cachen, um sp�tere Suchen zu beschleunigen
            $this->cache[$startnode->id]=&$startnode;
            $result=TRUE;
            }
        else
            {
            foreach ($startnode->child as $index=>$child)
                {
                //Rekursion machen wir indirekt, da PHP4 kein Foreach mit Referenzen hat
                $result=$this->_findnode($startnode->child[$index],$id);
                if ( $result != FALSE )
                    {
                    break;
                    }
                }
            }
        return($result);
        }


    //////////////////////////////////////////////////////////////////////////////////////
    //Den Status f�r jeden Knoten ermitteln und updaten
    //sollte unbedingt vor einem Show aufgerufen werden
    function rescan(&$startnode)
        {
        //�ffnungsstatus
        $startnode->isopen=in_array($startnode->id,$this->status);

        //Recursiv weiter scannen wenn der Zweig offen ist, ansonsten
        //k�nnen wir uns ein Update sparen
        if ($startnode->isopen)
            {
            foreach ($startnode->child as $index=>$child)
                {
                $this->rescan($startnode->child[$index]);
                }
            }
        }

    //////////////////////////////////////////////////////////////////////////////////////
    //Alles anzeigen
    function show()
        {
        global $html;
        global $vars;

        //Der Status enth�lt als Reihe die IDs der ge�ffneten Zweige
        //Um einfacher darauf zugreifen zu k�nnen, explodieren wir alles in das
        //Statusarray
        switch ($this->linkmode)
            {
            case (TREE_LINK_CMD)     :   $input=$vars->cmd;  break;
            case (TREE_LINK_CMDID)   :   $input=$vars->cmdid;  break;
            case (TREE_LINK_SUBCMD)  :   $input=$vars->subcmd;  break;
            case (TREE_LINK_SUBCMDID):   $input=$vars->subcmdid;  break;
            }

        if (strpos($input,TREE_SLICE)!==FALSE)
            {
            $this->status=explode(TREE_SLICE,$input);
            
            //Die letzte aktive ID speichern
            $lastid=array_pop($this->status);
            //Die neue gew�nschte ID speichern
            $cmdid =array_pop($this->status);

            //Entweder die neue Auswahl oder den letzten Status einschalten
            $cmdid=($cmdid=="a"?$lastid:$cmdid);

            if ( $this->findnode($cmdid) !== FALSE)
                {
                //Daten des Knotens speichern
                $this->activenode=$this->cache[$cmdid]->data;
                $this->activeid  =$cmdid;
                $this->docallback($cmdid);
                }
            }

        //Knotenzustand aktualisieren
        $this->rescan($this->internal_tree);

        //Und anzeigen
        $text=$this->_show($this->internal_tree,0);
        
        //Fertig
        $html->text_parsed($text);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Die Callbackfunktion aufrufen
    function docallback($id)
        {
        //Ist die Funktion auch definiert ?
        $function=$this->cache[$id]->data["callback"];
        if ( function_exists( $function ) )
            {
            //Funktion aufrufen und die Daten des Knoten mitgeben
            call_user_func($function,$this->activenode);
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Unterfunktion von show, um den Aufruf f�r den Benutzer so einfach wie m�glich zu halten
    //Der Link enth�lt eine Liste alle offenene Indizes. Der Letzte Eintrag in der Liste ist
    //die ID der aufzurufenden CallBackFunktion
    function _show($startnode,$spaces)
        {
        global $session;

        //Zur Vereinfachung den Namen rausziehen
        $id=$startnode->id;
        
        //Basisknoten mit anzeigen ?
        $name=( $this->activeid!=$id?$startnode->data["name"]:"[BLACK]>".$startnode->data["name"]."[/BLACK]");

        //Leveleinr�ckung
        $text=str_repeat("&ensp;",$spaces);

        //Eine Abzweigung
        if ($startnode->haschild==TRUE)
            {
            //Je nach Status das Zeichen wechseln
            $text.="[!LINK=".$this->_statuslink($id,FALSE)."]";
            $text.=($startnode->isopen!=FALSE?$this->icon_open:$this->icon_closed);
            $text.="[/LINK] ";
            }
        else
            {
            $text.=$this->icon_node;
            }

        //CallBackLink drunter
        $text.="[!LINK=".$this->_statuslink(FALSE,$id)."]".$name."[/LINK][BR]";

        //Die Kinder ganz unten dranh�ngen
        if ($startnode->isopen)
            {
            //Und die Kinder scannen
            foreach ($startnode->child as $child)
                {
                $text.=$this->_show($child,$spaces + $this->span);
                }
            }
            
        //Nun haben wir alles als BBCode zusammengesetzt und lassen es
        //parsen
        return($text);
        }

    //////////////////////////////////////////////////////////////////////////
    //Die n�chste ID liefern
    function _getnextid()
        {
        $this->internal_id++;
        return($this->internal_id);
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Das Statusarray in einen Link wandeln und die �bergeben ID
    //toggeln
    function _statuslink($switchid,$commandid=FALSE)
        {
        global $session;
        
        //Nur mit einer lokalen Kopie arbeiten
        $local=$this->status;
        
        //Ein Switch gew�nscht ?
        if ($switchid !== FALSE)
            {
            $index=array_search($switchid,$local);
            if ($index!==FALSE)
                {
                unset($local[$index]);
                }
            else
                {
                $local[]=$switchid;
                }
            }
            
        $result=implode(TREE_SLICE,$local);

        //Ein Kommando gew�nscht ?
        $result.=TREE_SLICE.($commandid===FALSE?"a":$commandid);

        //ID des aktiven Knotens anh�ngen
        $result.=TREE_SLICE.$this->activeid;

        //Und den Link bauen
        switch ($this->linkmode)
            {
            case (TREE_LINK_CMD)    : $result=$session->createlink("",$result,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE); break;
            case (TREE_LINK_CMDID)  : $result=$session->createlink("","",$result,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE); break;
            case (TREE_LINK_SUBCMD) : $result=$session->createlink("","","",$result,FALSE,FALSE,FALSE,FALSE,FALSE); break;
            case (TREE_LINK_CMDID)  : $result=$session->createlink("","","","",$result,FALSE,FALSE,FALSE,FALSE); break;
            }
            
        return($result);
        }
    }

</script>