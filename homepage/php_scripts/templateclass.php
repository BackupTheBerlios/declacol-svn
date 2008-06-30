<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Template einer Klasse
///
///
///Prototypen
///
///constructor			//Initialisiert alle Daten mit Defaults
///destructor           //Entlädt alle wichtigen Speicherbereiche etc.
///open                 //Lädt notwendiges oder baut Verbindungen auf
///install()			//Erzeugt eine Datenbank (wenn für Klasse notwendig)
///uninstall()			//Verwirft die Datenbank der Klasse
///setdefault()			//Setzt alle internen Werte auf default
///close()				//Schließt alle Verbindungen
///
///Nameskonventionen
///
///add(mixed)			//Fügt Daten zu
///remove(mixed)		//Entfernt Daten
///clean(mixed)			//Räumt auf
///flush(mixed)			//Buffer ausgeben
///write(mixed)			//Daten ausgeben
///save(mixed)			//Daten in eine Datei schreiben
///load(mixed)			//Daten aus einer Datei lesen
///enumerate(mixed)		//Objekte als array ausgeben (z.B. alle User)
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_MYCLASS","class_myclass");
define ("CLASS_MYCLASS_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_MYCLASS,"version ".CLASS_MYCLASS_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("MYCLASS_DEF1" ,1);
define ("MYCLASS_DEF2" ,2);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class class
	{
	//Öffentliche Eigenschaftem
	var $active				=FALSE;

	//Private Eigenschaften
	var $internal_index 	=0;


	//////////////////////////////////////////////////////////////////////////
	//Konstruktor
	function class()
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
		$this->active=FALSE;
		$this->internal_index=0;
		}
	
	//Datenbank und alles andere erzeugen
	function install()
		{
		//Datenbankzugriff holen
		global $mysql;
		
		//Abfrage aufbauen
		$query="CREATE TABLE IF NOT EXISTS mytable (id char(64), name char(64), PRIMARY KEY (id))";

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
		$query="DROP TABLE IF EXISTS mytable";

		//Und eine neue Datenbank anlegen
		$result=$mysql->query($query);

		return($result);
		}

	//////////////////////////////////////////////////////////////////////////
	///Ab hier die eigentlichen Funktionen
	//////////////////////////////////////////////////////////////////////////

	}

</script>