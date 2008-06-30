<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Cron-Klasse
///
/// Kapselt ein CRON-Ereignis
///
/// Das funktioniert allerdings nur, wenn das Script cron.php im Root-
/// verzeichnis in regelmäßigen Abständen (z.B. 1/2hour) aufgerufen wird.
/// Es müssen dabei keine Parameter übergeben werden.
///
/// Ist keine CRON-Funktion verfügbar, so kann das PlugIn CRON-Helper
/// installiert werden, das bei jedem Seitenaufruf prüft, ob der CRON-
/// Dienst gestartet werden muß und dies ggf. durchführt,
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_CRON","class_cron");
define ("CLASS_CRON_VERSION","0.02");
if (isset($debug)) $debug->add(CLASS_CRON,"version ".CLASS_CRON_VERSION);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class cron
    {
    //Öffentliche Eigenschaftem
    var $id                    =FALSE;
    var $enabled            =FALSE;
    var $filename            =FALSE;

    //Datum, wann der Task zuletzt ausgeführt wurde
    var $executed            =FALSE;

    //Wenn der Wert auf TRUE steht, wird der Task in jeweils diesem Interval ausgeführt
    //Mehrfach setzen ist möglich.
    var $year                =FALSE;
    var $month                =FALSE;
    var $week                =FALSE;
    var $day                =FALSE;
    var $hour                =FALSE;

    //Unser internes Log
    var    $log                =array();
    

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function cron($id=UNDEFINED)
        {
        //Alle Werte auf Default setzen
        $this->setdefault();

        //Und den entsprechenden Cronjob öffnen
        if ($id!=UNDEFINED)
            {
            $this->open($id);
            }
        }

    //Destruktor
    function destroy()
        {
        }

    //Verbindungsaufbau
    function open($id)
        {
        global $mysql;

        $result=FALSE;

        $this->log[]="open cronjob ".$id;

        //Daten holen
        $data=$mysql->query("SELECT * FROM ".DB_CRON." WHERE id='".$id."'");

        //Wenn wir was gefunden haben, dann uns selbst zuordnen
        if (is_array($data))
            {
            $this->id=$id;
            $this->name    =$data[0]["name"];
            $this->filename=$data[0]["filename"];

            $this->year    =(boolean) $data[0]["every_year"];
            $this->month   =(boolean) $data[0]["every_month"];
            $this->week    =(boolean) $data[0]["every_week"];
            $this->day     =(boolean) $data[0]["every_day"];
            $this->hour    =(boolean) $data[0]["every_hour"];

            $this->executed=$data[0]["date_executed"];
            
            $this->enabled =(boolean) $data[0]["enabled"];

            $this->log[]="done";

            $result=TRUE;
            }
        else
            {

            $this->log[]="failed";
            }
        return($result);
        }

    //Verbindungsabbau
    //Wird hier nicht benötigt
    function close()
        {
        return(TRUE);
        }
    
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->id                =FALSE;
        $this->enabled            =FALSE;
        $this->filename            =FALSE;

        $this->executed            =FALSE;

        $this->year                =FALSE;
        $this->month            =FALSE;
        $this->week                =FALSE;
        $this->day                =FALSE;
        $this->hour                =FALSE;

        $this->log                =array();
        }
    
    //Diesen Cronjob installieren
    function install()
        {
        global $mysql;

        $this->log[]="installing cronjob";

        //Filename flatten
        $this->filename=string_extractfilename($this->filename);

        //Name filtern
        $this->name=string_filter($this->name,FILTER_EMAIL);

        //Unsere ID bestimmen
        $this->id=crypt_create_hash($this->filename);

        //Fehler abfangen
        if (!file_exists(DIR_CRON.$this->filename))
            {
            $this->log[]=$this->filename." not found";

            return(FALSE);
            }
    

        //Und einfach einen Eintrag in die Datenbank machen
        $query ="INSERT INTO ".DB_CRON;
        $query.="(id,name,filename,every_hour,every_day,every_week,every_month,every_year,date_executed,enabled) ";
        $query.=sprintf("VALUES('%s','%s','%s',%u,%u,%u,%u,%u,%u,%u)",$this->id,$this->name,$this->filename,$this->hour,$this->day,$this->week,$this->month,$this->year,0,(int)$this->enabled);

        $this->log[]="done";

        return($mysql->query($query));
        }
    
    //Diesen Cronjob aus der Datenbank entfernen
    function uninstall()
        {
        global $mysql;


        $this->log[]="uninstalling";

        if ($mysql->query("DELETE FROM ".DB_CRON." WHERE id='".$this->id."'"))
            {
            $this->log[]="done";
            return(TRUE);
            }
        else
            {
            $this->log[]="failed";
            return(FALSE);
            }
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Unsere Daten abspeichern
    //Damit kann ein Cronjob leicht bearbeitet werden.
    function save ()
        {
        global $mysql;

        $this->log[]="saving crondata";

        $query ="UPDATE ".DB_CRON;
        $query.=sprintf(" SET name='%s',every_hour=%u,every_day=%u,every_week=%u,every_month=%u,every_year=%u,enabled=%u",$this->name,$this->hour,$this->day,$this->week,$this->month,$this->year,(int)$this->enabled);
        $query.=" WHERE id='".$this->id."'";

        return($mysql->query($query));
        }

    //Den Job ausführen (Interne Funktion)
    //Hier wird nicht auf das Datum geprüft
    function _execute()
        {
        //Alles was ein Cron-Job darf
        global $mysql;
        global $mailer;
        global $debug;
        global $user;

        //Einfach die Datei includen
        if (file_exists(DIR_CRON.$this->filename))
            {
            $this->log[]="including ".$this->filename;

            include (DIR_CRON.$this->filename);
        
            //Das Ausführungdatum merken
            $mysql->query("UPDATE ".DB_CRON." SET date_executed=".time()." WHERE id='".$this->id."'");

            $this->log[]="done";
            return(TRUE);
            }
        else
            {
            $this->log[]="failed";
            return(FALSE);
            }
        }

    //Einen Cronjob ausführen. Bevor er ausgeführt wird,
    //prüft das Skript, ob er überhaupt dran ist.
    function execute()
        {
        $this->log[]="[execution]";

        //Sind wir überhaupt eingeschaltet ?
        if (!$this->enabled)
            {
            $this->log[]="execution failed (job disabled)";

            //Abbruch
            return(FALSE);
            }

        //Fehler annehmen
        $exec=FALSE;

        //Aktuelles Datum holen und in ein Array parsen
        $mydate  =explode(" ",date("h d W m Y",time()));
        //Executed Datum holen und in ein Array parsen
        $donedate=explode(" ",date("h d W m Y",$this->executed));

        //Nun vergleichen wir die beiden Arrays und prüfen,
        //Ob sie unterschiedlich sind.
        $exec|=( ($this->hour)  && ($mydate[0]!=$donedate[0]) );
        $exec|=( ($this->day )  && ($mydate[1]!=$donedate[1]) );
        $exec|=( ($this->week)  && ($mydate[2]!=$donedate[2]) );
        $exec|=( ($this->month) && ($mydate[3]!=$donedate[3]) );
        $exec|=( ($this->year)  && ($mydate[4]!=$donedate[4]) );

        $exec=TRUE;

        //Was zu erledigen ?
        if ($exec != FALSE)
            {
            //Dann über die interne Funktion aufrufen
            //Setzen der Exec-Zeit passiert dort
            $exec=$this->_execute();
            }
        else
            {
            $this->log[]="execution failed (nothing todo)";
            }

        //Und das Ergebnis zurückliefern
        return($exec);
        }

    //Erzwingt die Ausführung des Jobs.
    //Eine Datumsprüfung findet nicht statt
    function force()
        {
        $this->log[]="[forced execution]";

        //Nur die interne Funktion aufrufen
        return($this->_execute());
        }

    }

</script>