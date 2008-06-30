<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Einfache Klasse, um die Dokumentation von anderen Klassen zu
/// erleichtern
///
/// im Moment werden einfach die Quelltexte geöffnet und alle Funktionen
/// und Konstanten extrahiert.
///
/// Beginnt eine Funktion mit _, so wird angenommen sie sei private
/// ansonsten werden Funktionen als Public deklariert.
/// Kommentare vor dem Funktionsbeginn werden bis zur nächsten leere Zeile
/// als Kommentar in die Dokumentation eingefügt.
///
/// Existiert ein Kommentarkopf am Beginn des Quelltextes, so wird er der
/// Doku vorangestellt.
///
/// Beispiel
///
/// //Mein Quelltextbeispielr
///
/// //Dies ist meine Funktion
/// //auch diese Zeile ist ein Kommentar
/// function _meineprivatefunktion()
///  {
///  }
///
/// //Meine Publicfunktion
/// function meinepublicfunktion()
///  {
///  }
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_DOCUMENTER","class_documenter");
define ("CLASS_DECUMENTER_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_DOCUMENTER,"version ".CLASS_DOCUMENTER_VERSION);


//Bei direktem aufruf ein paar Bibliotheken nachladen
if (defined("LIB_FILES")===FALSE)
	{
	require_once("lib.files.php");
	}
if (defined("LIB_STRINGS")===FALSE)
	{
	require_once("lib.strings.php");
	}

//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling

define ("DOX_INTERNAL_METHODS_PREFIX" 	,"_");
define ("DOX_EXTERNAL_METHODS_PREFIX" 	,"");
define ("DOX_INTERNAL_VARS_PREFIX" 		,"internal_");
define ("DOX_EXTERNAL_VARS_PREFIX" 		,"");

define ("DOX_MAGIC"						,"42");
define ("DOX_EXTENSION"					,".txt");

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class documenter
	{
	///Globale Variablen
	var $doku = array();

	var $internal_buffer=array();


	//////////////////////////////////////////////////////////////////////////
	//Konstruktor
	function documenter()
		{
		//Alle Werte auf Default setzen
		$this->setdefault();
		}

	//Destruktor
	function destroy()
		{
		$this->close();
		}

	//Eine Datei dokumentieren
	function open($filename)
		{
		$this->setdefault();
		if (file_exists($filename))
			{
			//Datei lesen
			$this->internal_buffer=file($filename);

			//Alles trimmen
			$index=0;
			while ($index < count($this->internal_buffer))
				{
				$this->internal_buffer[$index] = trim($this->internal_buffer[$index]);
				$index++;
				}
			$this->doku[]="################################################################";
			$this->doku[]="#";
			$this->doku[]="# File";
			$this->doku[]="# ".$filename;
			$this->doku[]="#";
			$this->doku[]="################################################################";

			//Und die Doku erzeugen
			$this->_create_head_dox();
			$this->_create_constants_dox();
			$this->doku[]="";
			$this->doku[]="";
			$this->_create_function_dox();
			}
		}

	//Einen kpl. Verzeichniszweig durchgehen und dokumentieren
	//Für jeden gefundene Datei wird in output-dir eine DokuDatei angelegt
	//extarray ist ein Array mit allen erlaubten extensionen (ohne punkt)
	function scan($inputdir,$outputdir,$extarray)
		{
		//Fehler abfangen
		if (!is_dir($inputdir))
			{
			return(FALSE);
			}
		if (!is_dir($outputdir))
			{
			return(FALSE);
			}
		if (!is_array($extarray))
			{
			return(FALSE);
			}

		//Pfadnamen normieren
		$inputdir =string_add($inputdir ,"/");
		$outputdir=string_add($outputdir,"/");

		//Alle Dateien holen
		$files = file_scan($inputdir,$extarray,TRUE,TRUE,FALSE);

		//Alles resetten
		$this->setdefault();

		//Und einzeln verarbeiten
		foreach ($files as $file)
			{
			//Datei laden
			$this->open($file);

			//Ausgabedateiname ist Ausgabeverzeichnis + Dateiname + .txt
			$newfile=$outputdir . string_extractfilename($file) . DOX_EXTENSION;


			//Doku speichern
			savearray($newfile,$this->doku);

			//Und alles wieder schließen
			$this->close();
			}
        return(TRUE);
		}

	//Speicher wieder freigeben
	function close()
		{
		$this->doku=array();
		}

	//Defaults
	function setdefault()
		{
		$this->doku=array();
		}

	//Datenbank erzeugen
	//Wird hier nicht benötigt
	function install()
		{
		}

	//Datenbank zerstören
	//Wird hier nicht benötigt
	function uninstall()
		{
		}

	//////////////////////////////////////////////////////////////////////////
	///Ab hier die eigentlichen Funktionen
	//////////////////////////////////////////////////////////////////////////

	//Den Kommentarkopf holen und oben an die Dokumentation setzen
	function _create_head_dox()
		{
		//Da wir nur den Kommentarkopf holen, suchen wir bis zum Beginn des ersten
		//Kommentares

		$index=0;
		while (
				($index < count ($this->internal_buffer)) &&
				(!$this->_is_comment($this->internal_buffer[$index],FALSE))
			  )
			{

			$index++;
			}

		//So, den Anfang haben wir
		//Nun nehmen wir alle Kommentare, bis ni mehr kommt
		while (
				($index < count ($this->internal_buffer)) &&
				($this->_is_comment($this->internal_buffer[$index],FALSE))
			  )
			{
			//Kommentarzeichen rausnehmen
			$line=$this->internal_buffer[$index];
			$this->doku[]=str_replace("/","",$line);
			$index++;
			}
		}

	//Alle Konstante holen und in die Doku kleben
	function _create_constants_dox()
		{
		$this->doku[]="################################################################";
		$this->doku[]="# Constantslist";
		$this->doku[]="################################################################";
		$this->doku[]="";

		//Array mit den Positionen der Funktionsdefinitionen
		$constants=array();

		//Alles durchgehen
		//Und die Funktionen raussuchen
		foreach ($this->internal_buffer as $index => $line)
			{
			//Konstanten suchen
			if (($constant=$this->_is_define($line)))
				{
				//Funktionsnamen und Position merken
				$constants[]=$constant;
				}
			}

		//Sortieren
		natsort($constants);

		//Und anfügen
		$this->doku=array_merge($this->doku,$constants);
		}

	//Alle Funktionen und deren Kommentare holen und in die Doku kleben
	function _create_function_dox()
		{

		$this->doku[]="################################################################";
		$this->doku[]="# Functionlist";
		$this->doku[]="################################################################";
		$this->doku[]="";

		//Array mit den Positionen der Funktionsdefinitionen
		$functions=array();

		//Alles durchgehen
		//Und die Funktionen raussuchen
		foreach ($this->internal_buffer as $index => $line)
			{
			//Funktionen suchen
			if (($functionname=$this->_is_function($line)))
				{
				//Funktionsnamen und Position merken
				$functions[$functionname]=$index;
				}
			}

//		ksort($functions);

		//Nun da wir die Functionen haben, ziehen wir die Kommentare davor raus
		foreach ($functions as $name => $index)
			{
			//Den Index lokal holen
			$idx=$index;

			//Array löschen
			$comments=array();


			//Von der Stelle der Funktion rückwärtslaufen,
			//bis kein Kommentar oder eine Leerzeile mehr kommt
			$idx--;
			$line=$this->internal_buffer[$idx];
			while( ($comment=$this->_is_comment($line,TRUE)))
				{
				//Abspeichern
				if ($comment!==TRUE)
					{
					$comments[]=$comment;
					}

				//Nächste Zeile holen
				$idx--;
				$line=$this->internal_buffer[$idx];
				}
			//Wenn wir durch sind,
			//drehen wir noch schnell das Kommetnararray um
			$comments=array_reverse($comments);

			//Intern oder Extern ?
			$prefix="(public)  ";
			if (strpos($name,DOX_INTERNAL_METHODS_PREFIX)===0)
				{
				$prefix="(private) ";
				}

			//Funktionseintrag kpl. nehmen
			$this->doku[]=sprintf("[%s] %s %s",$index,$prefix,$this->internal_buffer[$index]);
			$this->doku[]="----------------------------------------------------------------";
			//Und die Dokumentation hintendran
			//Alle Dokueintrage inklusive des Prototype
			foreach ($comments as $comment)
				{
				$this->doku[]=$comment;
				}
			$this->doku[]="";
			$this->doku[]="";
			$this->doku[]="";
			}
		}

	//////////////////////////////////////////////////////////////////////////
	/// Hilfsfunktionen
	//////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////
	//Prüfen, ob die Zeile ein Kommentar ist (Also // mit Text) oder $strict=FALSE nur //
	function _is_comment($line,$strict)
		{
		//Entweder auf Kommentarzeichen und Text achten
		if ($strict)
			{
			//Im Strict-Mode die Leerzeilen ignorieren
			if ($this->_is_empty($line))
				{
				return(TRUE);
				}

            $result=FALSE;
			if (preg_match("/^(\/\/*)(.*)/",$line,$result)==1)
				{
				//Und den Kommentartext zurückgeben
				return($result[2]);
				}
			}
		else
			{
			//Oder nur auf Kommentarzeichen
            $result=FALSE;
			if (preg_match("/^(\/\/*)/",$line,$result)==1)
				{
				//Und den Kommentartext zurückgeben
				return($line);
				}
			}

		return(FALSE);
		}

	//Prüfen, ob die Zeile leer ist
	function _is_empty($line)
		{
		//Und leere Zeilen nehmen wir auch gleich raus
		if ($line=="")
			{
			return(TRUE);
			}
        return(FALSE);
		}

	//////////////////////////////////////////////////////////////////////////
	//Prüfen, ob die Zeile eine Section einleitet
	function _is_function($line)
		{
		//Einfach mit einem Regulären Ausdruck checken
        $result=FALSE;
		if (preg_match("/^function[ \t]*([a-zA-Z_]*[0-9]*)[ \t]*\(/",$line,$result)==1)
			{
			//Und den Namen extrahieren
			return($result[1]);
			}

		return(FALSE);
		}

	//////////////////////////////////////////////////////////////////////////
	//Prüfen, ob die Zeile ein Define enthält
	function _is_define($line)
		{
		//Einfach mit einem Regulären Ausdruck checken
        $result=FALSE;
		if (preg_match("/^define[ \t]*\([ \t]*\"([a-zA-Z0-9_]*)\"[ \t]*,[ \t]*(.*)[ \t]*\)/",$line,$result)==1)
			{
			//Und den Namen extrahieren
			return($result[1]." = ".$result[2]);
			}

		return(FALSE);
		}

	}


//Für die eigenen Dateien bei Aufruf die Dox erzeugen
$doku=new documenter();

$doku->scan("../_scripts/","../dox/classes/",array("php"));
$doku->scan("../_constants/","../dox/constants/",array("php"));
$doku->scan("../_templates/plugins/","../dox/plugins/",array("php"));
$doku->scan("../_templates/modules/","../dox/modules/",array("php"));

$doku->destroy();

</script>