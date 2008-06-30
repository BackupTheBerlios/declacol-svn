<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Dropdownmenu-Klasse
///
//////////////////////////////////////////////////////////////////////////
///Mittels Javascript wird eine Dropdownmenu simuliert.
///
///Das untere Beispiel zeigt die Verwendung. Die Methode Show blendet
///das Menü ein. Die Methode Process ruft eine evtl. registrierte Callback-Funktion auf,
///wenn der entsprechende Menüeintrag gewählt wurde.
///Die Klasse arbeitet NICHT asynchron. D.h. bei einem Klick auf einen Menüpunkt wird
///die kpl Sseite neu geladen und es muß das kpl. Menukonstrukt identisch zum vorherigen
///Aufbau initialisiert werden bevor ->process aufgerufe wird.
///
//////////////////////////////////////////////////////////////////////////
///Eigenschaften
///table_width="x%" Breite der Menutabelle in Prozent
///
///Methoden
///add_menu("Name"):$id
///Fügt einen Menutitel ein und gibt die ID zurück. Diese wird benötigt, um
///dem Title Items zuzufügen
///
///add_menu_entry($menuid,$text,$callbackfunktion,$data)
///Fügt dem Menu mit der ID $menuid einen neuen Eintrag zu. Im Eintrag
///wird der Text $text dargestellt. Er kann die Standard HTMLCodes benutzen ([LINK= etc.)
///$callbackfunktion wird aufgrufen, wenn ein Link angeklickt und die Mtehode Process aufgerufen
///wird. Der Callback-Funktion wird als Parameter $data übergeben
///
///show()
///bindet den HTML-Code des Menüs ein
///
///process()
///verarbeitet die Parameter aus dem Seitenaufruf und springt ggf. die passende
///Callbackfunktion an.
//////////////////////////////////////////////////////////////////////////
///Beispiel
///$menu=new dropdownmenu();
///
///$id=$menu->add_menu("File");
///$menu->add_menu_entry($id ,"[IMG=".URL_ICONS."icon_import.png] Open" ,"callback","open");
///$menu->add_menu_entry($id ,"[IMG=".URL_ICONS."icon_export.png] Save" ,"callback","save");
///$menu->add_menu_entry($id ,"[IMG=".URL_ICONS."icon_close.png] Close","callback","close");
///$menu->add_menu_entry($id ,"_________");
///$menu->add_menu_entry($id ,"[IMG=".URL_ICONS."icon_cancel.png] Exit" ,"callback","exit");
///
///$id=$menu->add_menu("Help");
///$menu->add_menu_entry($id,"Help");
///$menu->add_menu_entry($id ,"_________");
///$menu->add_menu_entry($id,"About");
///
///$menu->show();
///
///$menu->process();
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_DROPDOWNMENU","class_dropdownmenu");
define ("CLASS_DROPDOWNMENU_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_DROPDOWNMENU,"version ".CLASS_DROPDOWNMENU_VERSION);



//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class dropdownmenu
    {
    var $table_width       = "10%";
    
    
    //Private Eigenschaften
    var $internal_title    =array();
    var $internal_items    =array();
    var $internal_allitems =array();
    var $internal_callback =array();
    var $internal_cbdata   =array();

    //Interne ID Counter
    var $menuid            =ID_NONE;
    var $itemid            =ID_NONE;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function dropdownmenu()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        //Der Welt schönster Destruktor
        unset($this);
        }

    //////////////////////////////////////////////////////////////////////////
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->internal_title    = array();
        $this->internal_items    = array();
        $this->internal_allitems = array();
        $this->internal_callback = array();

        $this->menuid=ID_NONE;
        $this->itemid=ID_NONE;
        }

    //////////////////////////////////////////////////////////////////////////
    //Das Menu ausgeben
    function show()
        {
        global $html;
        global $session;
        
        $html->table_open($this->table_width,"module");
        $html->row_open("nohover");
        foreach ($this->internal_title as $mid => $menu)
            {
            $scriptover="showstuff('".$mid."')";
            $scriptout ="hidestuff('".$mid."')";

            //Der Titel
            $html->cell_open("");
            $html->link_over($scriptover,$scriptout,$menu);
            $html->nextline();

            //Das versteckte DIV mit den Menüeinträgen hinterher
            $html->div_open_id("hiddenoverlay",$mid,$scriptover,$scriptout);
            foreach ($this->internal_items[$mid] as $iid => $item)
                {
                $html->link_open($session->createlink("",$iid,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE));
                $html->text_parsed($item);
                $html->link_close();
                $html->nextline();
                }
            $html->div_close();
            $html->cell_close();
            }
        $html->row_close();
        $html->table_close();
        }


    //////////////////////////////////////////////////////////////////////////
    //evtl. Datenübergabe aus der letzten Seite (Linkklick) verarbeiten
    //Ist eine Callbackfunktion zu einem Menüpunkt registriert, wird diese
    //ausgeführt
    function process()
        {
        global $vars;

        $iid=string_filter($vars->cmd,FILTER_ALPHANUM);
        if (isset($this->internal_callback[$iid])!=FALSE)
            {
            if (is_callable($this->internal_callback[$iid])==TRUE)
                {
                call_user_func($this->internal_callback[$iid],$this->internal_cbdata[$iid]);
                }
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Eine Neue Sektion einfügen und die ID zurückgeben
    function add_menu($title)
        {
        $id="m".(++$this->menuid);

        $this->internal_title[$id]=$title;
        $this->internal_items[$id]=array();

        return($id);
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Einen Untermenüpunkt in der Sektion ID einfügen
    //und die ID des Menüpunktes zurückliefern
    function add_menu_entry($mid,$item,$callback="",$callbackdata=FALSE)
        {
        $iid="i".(++$this->itemid);

        //Einmal unter dem Menü speichern
        $this->internal_items[$mid][$iid]=$item;
        
        //Und einmal der leichteren Adressierung wegen kpl.
        $this->internal_allitems[$iid]=$item;
        
        if ($callback!="")
            {
            $this->internal_callback[$iid]=$callback;
            $this->internal_cbdata  [$iid]=$callbackdata;
            }
        return($iid);
        }
    }
</script>