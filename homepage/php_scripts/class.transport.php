<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Variablen Klasse
///
/// Dient zur Übertragung von Post und Get-Variablen
///
/// folgende Variablen werden immer belegt
/// GET
/// $session        die aktuelle SessionID
/// $page           die aktuelle Seite
/// $cmd            ein per GET angeforderter Befehl
/// $cmdid          ID zum Befehl
/// $subcmd            Unterkommando
/// $subcmdid       ID zum Unterkommando
///
/// $admin          AdminBefehl
/// $adminid        ID zum Adminbefehl
///
/// POST
/// $username       Ein Benutzername
/// $password       Das Kennwort
///
/// $data:array()    Beliebige Daten werden einfach als name="datax" value="meinedaten" übergeben.
///                 X gibt dabei den numerischen Index an. Die Konstante 
///                 TRANSPORT_MAX_DATA gibt an, wieviele dateneinträge maximal
///                 übergeben werden können (default=20)
/// $selected       Auswahl (muss Base64-Kodiert übergeben werden) Die dekodierung
///                 erfolgt automatisch in der Klasse Transport.
///                 ImagePicker übergibt den Pfad zu gewählten Grafik per 
///                 selected.
///
/// FLAGS
/// $logouttry      =isset(POST["logout"])
///
/// $logintry       =isset(POST["username"]) && isset(POST["password"])
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_TRANSPORT","class_transport");
define ("CLASS_TRANSPORT_VERSION","0.1");
if (isset($debug)) $debug->add(CLASS_TRANSPORT,"version ".CLASS_TRANSPORT_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("TRANSPORT_MAX_DATA",20);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class vars
    {
    //Öffentliche Eigenschaftem
    //Diese Daten gibts nur per GET
    var $ip         ="0.0.0.0";
    var $session    ="";
    var $page       ="";
    var $cmd        ="";
    var $cmdid      ="";
    var $subcmd     ="";
    var $subcmdid   ="";
    var $sort       ="";
    var $sortorder  ="";
    var $notsortorder="";

    var $captcha    =FALSE;

    var $admin      ="";
    var $adminid    ="";

    //Festgelegte Variable für alle Auswahlklassen
    //z.B Imagepicker
    var $selected   ="";

    //Diese nur per Post
    var $search     ="";

    var $data       =array();

    var $username   ="";
    var $password   ="";
    
    var $upload=FALSE;
    var $upfile="";

    //Status

    //Sind Username und Kennwort gesetzt ?
    var $logintry =FALSE;
    var $logouttry=FALSE;

    //Private Eigenschaften
    var $capid = "sadfkjgha478g";
    var $capval= FALSE;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function vars()
        {
        //Die bescheuerten Magic-Quotes ausschalten
        set_magic_quotes_runtime(0);
        
        //Alle Werte auf Default setzen
        $this->setdefault();

        //Und direkt öffnen
        $this->open();
        }

    //Destruktor
    function destroy()
        {
        $this->close();
        }

    //Alle Variablen parsen
    function open()
        {
        //Debug-Objekt holen
        global $debug;

        //Ist ein Upload initiiert ?
        if (isset($_POST["phpupload"]))
            {
            $this->upload=FALSE;
            //Datei geladen ?
            if (is_uploaded_file($_FILES["upload"]["tmp_name"]))
                {
                @set_time_limit(10 * 60);
                //Datei in den Tempordner kopieren
                $newfile=DIR_TEMP.crypt_create_unique_id();
                if (move_uploaded_file($_FILES["upload"]["tmp_name"],$newfile))
                    {
                    $this->upfile=$_FILES["upload"]["name"];
                    //Neuen Namen merken
                    $this->upload=$newfile;
                    }
                //Und die Tempdatei löschen
                @unlink($_FILES["upload"]["tmp_name"]);
                }
            }

        //Hier parsen wir einfach alle Posts und Gets in die vorgegebene Form
        if (isset($_GET["session"]))    $this->session  =string_left(string_filter($_GET["session"]     ,FILTER_ALPHANUM),64);
        if (isset($_GET["page"]))       $this->page     =string_left(string_filter($_GET["page"]        ,FILTER_ALPHANUM),64);
        if (isset($_GET["cmd"]))        $this->cmd      =string_left(string_filter($_GET["cmd"]         ,FILTER_URL),256);
        if (isset($_GET["cmdid"]))      $this->cmdid    =string_left(string_filter($_GET["cmdid"]       ,FILTER_URL),256);
        if (isset($_GET["subcmd"]))     $this->subcmd   =string_left(string_filter($_GET["subcmd"]      ,FILTER_URL),256);
        if (isset($_GET["subcmdid"]))   $this->subcmdid =string_left(string_filter($_GET["subcmdid"]    ,FILTER_URL),256);
        if (isset($_GET["admin"]))      $this->admin    =string_left(string_filter($_GET["admin"]       ,FILTER_URL),256);
        if (isset($_GET["adminid"]))    $this->adminid  =string_left(string_filter($_GET["adminid"]     ,FILTER_URL),256);
        if (isset($_GET["sort"]))       $this->sort     =string_left(string_filter($_GET["sort"]        ,FILTER_ALPHANUM),32);
        if (isset($_GET["sortorder"]))  $this->sortorder=$_GET["sortorder"]==SORT_DOWN?SORT_DOWN:SORT_UP;

        //Prüfen, ob die Captcha-Eingabe OK war
        if (isset($_POST["capid"]))      $this->capid    =string_left(string_filter($_POST["capid"]       ,FILTER_ALPHANUM),64);
        if (isset($_POST["capval"]))     $this->capval   =string_left(string_filter($_POST["capval"]      ,FILTER_ALPHANUM),16);
        $this->captcha = $this->capid === crypt_create_hash($this->capval);

        //Und die inverse Richtung mitanlegen
        $this->notsortorder=$this->sortorder==SORT_UP?SORT_DOWN:SORT_UP;

        //Select-Flags (Werden immer Base64-Decode verschickt)
        if (isset($_POST["selected"]))$this->selected    =string_filter(base64_decode(string_left($_POST["selected"],512)),FILTER_URL);

        //Ein Logout angefordert ?
        if (isset($_POST["logout"]))   $this->logouttry=TRUE;

        //Evtl. Logindaten
        //Brauchen wir nicht filtern, da die Sicherheitschecks in der UserAdminKlasse durchgeführt werden
        if (isset($_POST["username"])) $this->username        =string_left($_POST["username"],32);
        if (isset($_POST["password"])) $this->password        =string_left($_POST["password"],32);

        //Wenn beides gesetzt ist, wird ein Login versucht
        $this->logintry=($this->username!="") && ($this->password!="");

        //Nun die Dataposts (Aber nur maximal TRANSPORT_MAX_DATA)
        for ($index = 0; $index < TRANSPORT_MAX_DATA; $index++)
            {
            if (isset($_POST["data".$index]))
                {
                //Filterung am laschesten, um die Daten SQL-Sicher zu machen,
                //Muß zusätzlich mit string_replace_entities gefiltert werden
                $this->data[$index]=$_POST["data".$index];
                }
            else
                {
                $this->data[$index]=FALSE;
                }
            }

        //Suchbegriff
        if (isset($_POST["search"]))
            {
            $this->search=string_filter($_POST["search"],FILTER_SECURE);
            }
        else
            {
            $this->search=FALSE;
            }


        //Unsere IP-Adresse setzen
        if (isset($_SERVER["REMOTE_ADDR"]))
            {
            $this->ip=$_SERVER["REMOTE_ADDR"];
            }

        //Und die Debuginformationen speichern
        //Username und Kennwort
        if (DEBUG)
            {
            $debug->add(CLASS_TRANSPORT,"username :".$this->username);
            $debug->add(CLASS_TRANSPORT,"password :".$this->password);
            $debug->add(CLASS_TRANSPORT,"logintry :".string_bool2str($this->logintry));

            //Alle GETS
            $debug->add(CLASS_TRANSPORT,"session  :".$this->session);
            $debug->add(CLASS_TRANSPORT,"page     :".$this->page);
            $debug->add(CLASS_TRANSPORT,"cmd      :".$this->cmd);
            $debug->add(CLASS_TRANSPORT,"cmdid    :".$this->cmdid);
            $debug->add(CLASS_TRANSPORT,"subcmd   :".$this->subcmd);
            $debug->add(CLASS_TRANSPORT,"subcmdid :".$this->subcmdid);
            $debug->add(CLASS_TRANSPORT,"sort     :".$this->sort);
            $debug->add(CLASS_TRANSPORT,"sortorder:".$this->sortorder);

            //Selected
            $debug->add(CLASS_TRANSPORT,"selected :".$this->selected);

            //Upload
            $debug->add(CLASS_TRANSPORT,"upload   :".$this->upload);

            //Search
            $debug->add(CLASS_TRANSPORT,"search   :".$this->search);

            //Und alle Posts
            for ($index = 0; $index < TRANSPORT_MAX_DATA; $index++)
                {
                $debug->add(CLASS_TRANSPORT,"data".$index." :".$this->data[$index]);
                }
            }
            
        //Ich bin schon groß und kann mit Escapes umgehen
        if (get_magic_quotes_gpc()==1)
            {
            $this->_stripit();
            }
        }
        
    //Alles schließen
    //Hat hier keine Funktion
    function close()
        {
        }

    //Defaults setzen
    function setdefault()
        {
        $this->ip         ="0.0.0.0";
        $this->session    =FALSE;
        $this->page       =FALSE;
        $this->cmd        =FALSE;
        $this->cmdid      =FALSE;
        $this->subcmd     =FALSE;
        $this->subcmdid   =FALSE;
        $this->admin      =FALSE;
        $this->admincmd   =FALSE;
        $this->sort       =FALSE;
        $this->sortorder  =FALSE;
        $this->notsortorder  =FALSE;
        $this->data       =array();
        $this->username   =FALSE;
        $this->password   =FALSE;
        $this->logintry   =FALSE;
        $this->logouttry  =FALSE;
        $this->selected   =FALSE;
        $this->search     =FALSE;
        $this->upload     =FALSE;
        }
    
    //Datenbank erzeugen
    //Hat hier keine Funktion
    function install()
        {
        }
    
    //Datenbank zerstören
    //Hat hier keine Funktion
    function uninstall()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Einen "link" mit allen GET-Parametern holen
    function getparam()
        {
        $result="";
        if ($this->page    !="") $result.="&amp;page="        .$this->page;
        if ($this->cmd     !="") $result.="&amp;cmd="        .$this->cmd;
        if ($this->cmdid   !="") $result.="&amp;cmdid="        .$this->cmdid;
        if ($this->subcmd  !="") $result.="&amp;subcmd="    .$this->subcmd;
        if ($this->subcmdid!="") $result.="&amp;subcmdid="    .$this->subcmdid;
        if ($this->admin   !="") $result.="&amp;admin="        .$this->admin;
        if ($this->adminid!="") $result.="&amp;adminid="    .$this->adminid;
        return(str_replace(" ","%20",$result));
        }
        
        
    //////////////////////////////////////////////////////////////////////////
    //Wenn auf dem Server Magic_Quotes an sind, wird hier gestrippt
    //////////////////////////////////////////////////////////////////////////
    function _stripit()
        {
        //Das Dataarray
        for ($index=0; $index < TRANSPORT_MAX_DATA; $index++)
            {
            $this->data[$index]= $this->data[$index]!==FALSE?stripslashes($this->data[$index]):FALSE;
            }
            
        //Alle anderen gehen wir so durch
        $this->cmd      =$this->cmd!==FALSE?stripslashes($this->cmd):FALSE;
        $this->cmdid    =$this->cmdid!==FALSE?stripslashes($this->cmdid):FALSE;
        $this->subcmd   =$this->subcmd!==FALSE?stripslashes($this->subcmd):FALSE;
        $this->subcmdid =$this->subcmdid!==FALSE?stripslashes($this->subcmdid):FALSE;
        $this->admin    =$this->admin!==FALSE?stripslashes($this->admin):FALSE;
        $this->adminid  =$this->adminid!==FALSE?stripslashes($this->adminid):FALSE;

        $this->search   =$this->search!==FALSE?stripslashes($this->search):FALSE;

        $this->username =$this->username!==FALSE?stripslashes($this->username):FALSE;
        $this->password =$this->password!==FALSE?stripslashes($this->password):FALSE;
        $this->captcha  =$this->captcha!==FALSE?stripslashes($this->captcha):FALSE;
        }
    }

</script>