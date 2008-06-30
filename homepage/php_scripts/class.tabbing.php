<script language="php">
///////////////////////////////////////////////////////////////////////////
///
/// Hilfsklasse für Tabbed Seiten
///
/// Diese Klasse erleichtert den Umgang mit Tabbed-Seiten, indem die
/// Verwaltung (nahezu) kpl. übernommen wird.
///
///
///////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_TABBING","class_tabbing");
define ("CLASS_TABBING_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_TABBING,"version ".CLASS_TABBING_VERSION);


//Einige Definitionen zur Linkverarbeitung
define ("TAB_LINK_CMD"              ,"cmd");
define ("TAB_LINK_CMDID"            ,"cmdid");
define ("TAB_LINK_SUBCMD"           ,"subcmd");
define ("TAB_LINK_SUBCMDID"         ,"subcmdid");

//Die Klasse zur Darstellung und Verarbeitung der Tabs
//Diese Klasse wiecht
//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class tabbing
        {
        //Öffentliche Eigenschaftem
        var $linkmode      = TAB_LINK_CMD;
        
        var $width         = 80;

        //Arrays mit Tabs
        var $tabs          =array();
        //Array mit Funktionsnamen, die aufgerufen werden, wenn ein Tab angezeigt wird.
        //Der Index der Funktionen muß mit dem Index des Tabnamens identisch sein.
        //z.B. $tabs     =array("main"=>"Main"    ,"admin"=>"Admin");
        //     $functions=array("main"=>"showmain","admin"=>"ShowAdminPage");
        var $functions     =array();

        //Array mit Daten, die dem Tab zugeordnet werden können
        var $data          =array();

        //Flag für automatischen Funktionsaufruf
        var $autocall      =TRUE;

        //Function für den Aufruf vor dem eigentlichen Inhalt
        var $preshow       =FALSE;
        //Function für den Aufruf nach dem eigentlichen Inhalt
        var $postshow      =FALSE;

        var $activetab=FALSE;
        
        var $activetabname="";

        //////////////////////////////////////////////////////////////////////////
        //Konstruktor
        function tabbing()
                {
                //Alle Werte auf Default setzen
                $this->setdefault();
                }

        //Destruktor
        function destroy()
            {
            }

        //Verbindungsaufbau
        function open()
                {
                return(TRUE);
                }

        //Verbindungsabbau
        function close()
                {
                return(TRUE);
                }

        //Defaultwerte der internen und externen Variablen setzen
        function setdefault()
                {
                $this->tabbing_level=TAB_LINK_CMD;
                $this->tabs      =array();
                $this->functions =array();
                $this->data      =array();
                $this->autocall  =TRUE;

                //Die Standardfunktionen einklinken
                $this->preshow =FALSE;
                $this->postshow=FALSE;

                //Der active Tab
                $this->activetab=FALSE;
                $this->activetabname="";
                }

        //Datenbank und alles andere erzeugen
        //Wird hier nicht benötigt
        function install()
                {
                return(TRUE);
                }

        //Datenbank und alles andere zerstören
        //Wird hier nicht benötigt
        function uninstall()
                {
                return(TRUE);
                }

        //////////////////////////////////////////////////////////////////////////
        ///Ab hier die eigentlichen Funktionen
        //////////////////////////////////////////////////////////////////////////
    //Einen neuen Tab zufügen
    function add($name,$callback=FALSE,$data=FALSE)
        {
        $this->tabs[]     =$name;
        $this->functions[]=$callback;
        $this->data[]     =$data;
        }

    //Alle Tabs löschen
    function clear()
        {
        $this->tabs     =array();
        $this->functions=array();
        $this->data     =array();
        }

    //Die Tabverwaltung ausführen
    function show()
        {
        //Die Tabs einblenden
        $this->showtabs();

        $this->openmain();

        //Und die Inhaltsseite hinterher
        $this->callactivetab($this->getactivetabid());

        $this->closemain();
        }

        //Die Tabs anzeigen
    function showtabs ()
        {
        global $html;
        global $session;

        //Den aktiven Tab holen
        $this->activetab=$this->getactivetabid();

        //Und den Namen merken
        $this->activetabname=$this->gettabname($this->activetab);

        //Die Tabdarstellung einblenden
        $html->div_open("tabhead");
        foreach ($this->tabs as $id => $tab)
            {
            //Der aktive Tab ?
            if ($id==$this->activetab)
                {
                $style="tabhead_entry_active";
                }
            else
                {
                $style="tabhead_entry";
                }

            switch ($this->linkmode)
                {
                //Default ist Link als "cmd"
                case    (TAB_LINK_CMD)      : $html->link_short($session->createlink("",$id,FALSE,FALSE,FALSE,"","",FALSE,FALSE),$tab,"",$style); break;
                case    (TAB_LINK_CMDID)    : $html->link_short($session->createlink("","",$id,FALSE,FALSE,"","",FALSE,FALSE),$tab,"",$style); break;
                case    (TAB_LINK_SUBCMD)   : $html->link_short($session->createlink("","","",$id,FALSE,"","",FALSE,FALSE),$tab,"",$style); break;
                case    (TAB_LINK_SUBCMDID) : $html->link_short($session->createlink("","","","",$id,"","",FALSE,FALSE),$tab,"",$style); break;
                }
            }
        $html->div_close();
        }

    //Index des aktuellen Tabs zurückgeben
    function getactivetabid()
        {
        global $vars;

        //Ist der Tab schon gesetzt worden ?
        if ($this->activetab===FALSE)
            {
            switch ($this->linkmode)
                {
                case (TAB_LINK_CMD)       : $this->activetab=intval($vars->cmd);            break;
                case (TAB_LINK_CMDID)     : $this->activetab=intval($vars->cmdid);          break;
                case (TAB_LINK_SUBCMD)    : $this->activetab=intval($vars->subcmd);         break;
                case (TAB_LINK_SUBCMDID)  : $this->activetab=intval($vars->subcmdid);       break;
                default                   : $this->activetab=FALSE;
                }
            //Nochmal kurz filtern
            if ($this->activetab!==FALSE) $this->activetab=string_filter($this->activetab,FILTER_ALPHANUM);
            }
        //Fertig
        return($this->activetab);
        }

    //Aus einer ID den Namen holen
    function gettabname($id)
        {
        $result=FALSE;
        if (isset($this->tabs[$id]))
            {
            $result=$this->tabs[$id];
            }
        return($result);
        }

    //Aus einer ID die Function holen
    function gettabfunction($id)
        {
        $result=FALSE;
        if (isset($this->functions[$id]))
            {
            $result=$this->functions[$id];
            }
        return($result);
        }

    //Aus einer ID die Daten holen
    function gettabdata($id)
        {
        $result=FALSE;
        if (isset($this->data[$id]))
            {
            $result=$this->data[$id];
            }
        return($result);
        }


    //Die zu einem Tab zugehörige Funktion starten
    function callactivetab($id)
        {
        global $html;
        $result=FALSE;
        //Soll ein automatischer Aufruf erfolgen ?
        if ($this->autocall)
            {
            //Gibt es den Funktionseintrag ?
            if (isset($this->functions[$id]))
                {
                $function=$this->functions[$id];
                //Ist die Funktion auch definiert ?
                if (function_exists($function))
                    {
                    //Darstellung des entprechenden Tabs aufrufen
                    //Mit Verweis auf das Tabbingobject
                    call_user_func($function,$this);
                    $result=TRUE;
                    }
                }
            }
        return($result);
        }

    //Tags vor dem Hauptteil
        function openmain()
           {
           global $html;
       $html->div_open("module");
       $html->div_open("tabdata");
           }

    //Tags nach dem Hauptteil
        function closemain()
           {
           global $html;
       $html->div_close();
       $html->div_close();
           }
        }

</script>