<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// CRON-Admin-Klasse
///
/// verwaltet die CronJobs und Datenbankzugriffe
/// Die eigentlicher Verwaltung der Cronjobs erfolgt in der CRON-Klasse
/// selbst
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_CRONADMIN","class_cronadmin");
define ("CLASS_CRONADMIN_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_CRONADMIN,"version ".CLASS_CRONADMIN_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class cronadmin
	{
	//Öffentliche Eigenschaftem
	//Array mit allen CRONJobs als Objekten
	var $jobs = array();

	//////////////////////////////////////////////////////////////////////////
	//Konstruktor
	function cronadmin()
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
		}

	//Verbindungsabbau
	function close()
		{
		}

	//Defaultwerte der internen und externen Variablen setzen
	function setdefault()
		{
		//Alle offenen Jobs entladen
		foreach ($this->jobs as $onecron)
			{
			$onecron->destroy();
			}

		//Und das Array resetten
		$this->jobs = array();
		}

	//Datenbank und alles andere erzeugen
	function install()
		{
		//Datenbankzugriff holen
		global $mysql;

		//Abfrage aufbauen
		$query="CREATE TABLE IF NOT EXISTS ".DB_CRON." (id char (64), name char(64),filename char(64),every_hour int(32),every_day int(32), every_week int(32), every_month int(32),every_year int(32),date_executed int(32), enabled int(32), PRIMARY KEY (id))";

		//Und eine neue Datenbank anlegen
		$result=$mysql->query($query);

		return($result);
		}

	//Datenbank und alles andere zerstören
	function uninstall()
		{
		//Datenbankzugriff holen
		global $mysql;

		//Abfrage aufbauen
		$query="DROP TABLE IF EXISTS ".DB_CRON;

		//Und eine neue Datenbank anlegen
		$result=$mysql->query($query);

		return($result);
		}

	//////////////////////////////////////////////////////////////////////////
	///Ab hier die eigentlichen Funktionen
	//////////////////////////////////////////////////////////////////////////

	//Einen Cronjob zufügen
	function add ($name,$filename,$hour,$day,$week,$month,$year,$enabled)
		{
		//Einen Neuen Job machen
		$newcron=new cron(UNDEFINED);

		//Die Daten übergeben
		$newcron->name=$name;
		$newcron->filename=$filename;
		$newcron->hour=$hour;
		$newcron->day=$day;
		$newcron->week=$week;
		$newcron->month=$month;
		$newcron->year=$year;
		$newcron->enabled=$enabled;

		//Und installieren
		$newcron->install();
		$newcron->destroy();

		//Nun enumerieren wir neu durch, um die IDs richtig zu setzen
		//IDs werden nähmlich von der Datenbank per AUTOINC vergeben
		$this->enumerate();

		}

	//Alle verfügbaren Cronjobs zufügen
	function add_all()
		{
		//Alle Files suchen
		$crons=file_scan(DIR_CRON,array("php"),FALSE);

		//Was gefunden ?
		if (is_array($crons))
			{
			foreach ($crons as $cron)
				{
				//Alle Jobs deaktiviert eintragen
				$this->add($cron,$cron,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE);
				}
			}
		}

	//Alle installierten Cronjobs ausgeben
	function enumerate()
		{
		global $mysql;

		//Alle IDs holen
		$ids=$mysql->query("SELECT id,filename FROM ".DB_CRON." ORDER BY filename");

		//Wenn wir was gefunden haben, dann für jede ID ein Objekt erzeugen
		//(wenn es noch nicht existiert)
		if (is_array($ids))
			{
			foreach ($ids as $id)
				{
				if (!isset($this->jobs[$id["id"]]))
					{
					$this->jobs[$id["id"]]=new cron($id["id"]);
					}
				}
			}

		//Und die Liste zurückgeben
		return($this->jobs);
		}

	//Alle nicht mehr verfügbaren Cron-Skripte aus der Datenbank werfen
	function clean()
		{
		//Alle Jobs holen
		$crons=$this->enumerate();

		//Und durchgehen
		if (is_array($crons))
			{
			foreach ($crons as $onecron)
				{
				if (!file_exists(DIR_CRON.$onecron->filename))
					{
					$onecron->uninstall();
					}
				//Speicher freigeben
				$onecron->destroy();
				}
			}
		}
	}
</script>