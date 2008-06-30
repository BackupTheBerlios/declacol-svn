<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// FTP-Klasse
///
/// Kapselt den FTP-Zugriff
///
/// Diese Klasse befindet sich zur Zeit noch im Beta-Stadium und sollte mit
/// Vorsicht benutzt werden.
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_FTP","class_ftp");
define ("CLASS_FTP_VERSION","0.1");
if (isset($debug)) $debug->add(CLASS_FTP,"version ".CLASS_FTP_VERSION);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class ftp
	{
	//Öffentliche Eigenschaftem
	var	$log			 = array();
	var	$active			 = FALSE;

	//Private Eigenschaften
	var $internal_handle = FALSE;
	var $internal_server = "simtel.com";
	var	$internal_port   = 21;
	var $internal_passive= FALSE;
	var	$current_dir	 = "";
	var $internal_regex  = "";
	var $internal_systype= "unknown";

	//Die verschiedenen Filter für Unterschiedliche Systeme
	//UNIX
	var $unix_regex  ="([-dl])([rwxst-]{9}).* ([0-9]*) ([a-zA-Z]+[0-9: ]*[0-9]{2}:?[0-9]{2}) (.+)";
	//NOVELL
	var $novell_regex="([-dl]) \[([RF-]{8})\] [a-zA-Z0-9_.]* [ ]* ([0-9]*) ([a-zA-Z]+[0-9: ]*[0-9]{2}:?[0-9]{2}) (.+)";


	//////////////////////////////////////////////////////////////////////////
	///Konstruktor
	function ftp ($server,$port=21,$passivemode=FALSE)
		{
		//Hier werden einfach nur die Nutzerdaten übernommen
		$this->internal_server =$server;
		$this->internal_port   =$port;
		$this->internal_passive=$passivemode;

		$this->setdefault();
		}

	///Destruktor
	function destroy()
	    {
	    }

	///Verbindungsaufbau zum FTP-Server
	function open($user="anonymous",$pass="abc@def.gh")
		{
		//Log
		$this->addlog("connecting to ".$this->internal_server.":".$this->internal_port);

		//Verbindung aufbauen
		$result=ftp_connect($this->internal_server,$this->internal_port);

		if ($result!==FALSE)
			{
			//Handle übernehmen
			$this->internal_handle=$result;

			//Logeintrag
			$this->addlog("connected to ".$this->internal_server);

			//Verbunden sind wir, nun melden wir uns an
			$result=ftp_login($this->internal_handle,$user,$pass);

			//Erfolgreich eingeloggt ?
			if ($result!==FALSE)
				{
				$this->addlog("logged in");

				//Den vorgegebenen Passivmodus setzen
				$this->setpassive($this->internal_passive);

				//Servertyp holen
				$this->internal_systype=ftp_systype($this->internal_handle);

				//Und den passenden Filter setzen
				$this->setdirfilter($this->internal_systype);

				//Status anpassen
				$this->active=TRUE;
				}
			else
				{
				$this->addlog("unable to log in");
				$this->close();
				//Status anpassen
				$this->active=FALSE;
				}
			}
		else
			{
			$this->addlog("can't connect to ".$this->internal_server);
			//Status anpassen
			$this->active=FALSE;
			}
		return($result);
		}

	///Verbindung schließen
	function close()
		{
		$result=FALSE;
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			//Dann schließen
			ftp_quit($this->internal_handle);
			$this->addlog("connection closed");
			$result=TRUE;
			}
		//Handle verwerfen
		$this->internal_handle=FALSE;

		//Status anpassen
		$this->online=FALSE;

		//Fertig
		return($result);
		}

	///Defaults setzen
	function setdefault()
		{
		//Verzeichnis auf Hautpzweig setzen
		$this->current_dir="/";
		//Servertyp resetten
		$this->internal_systype= "unknown";

		//Log löschen
		$this->log=array();
		$this->addlog("ftp class created");
		}

	///Datenbank erzeugen
	///Wird hier nicht benutzt
	function install()
		{
		}

	///Datenbank zerstören
	///Wird hier nicht benutzt
	function uninstall()
		{
		}

	//////////////////////////////////////////////////////////////////////////
	///Ab hier die eigentlichen Funktionen
	//////////////////////////////////////////////////////////////////////////

	///Den Directoryfilter zwangsweise setzen
	///Wird normalerweise automatisch gesetzt
	function setdirfilter($systype)
		{
		//Und den Filter entsprechend des Serverstyps setzen
		switch ($systype)
			{
			case ("NETWARE") : $this->internal_regex=$this->novell_regex;	$this->addlog("dirfilter = NETWARE"); break;
			case ("UNIX")    : $this->internal_regex=$this->unix_regex;		$this->addlog("dirfilter = UNIX");	  break;
			case ("WINNT")   : $this->internal_regex=$this->unix_regex;		$this->addlog("dirfilter = WINNT");	  break;
			default 		 : $this->internal_regex=$this->unix_regex;		$this->addlog("dirfilter = DEFAULT"); break;
			}
		}

	///Passivemodus umschalten [TRUE/FALSE]
	function setpassive($passivemode)
		{
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			//Passivmodus setzen
			if (ftp_pasv($this->internal_handle,$passivemode))
				{
				$this->addlog("passivemode ".($this->internal_passive?"on":"off"));
				}
			else
				{
				$this->addlog("set passivemode failed");
				}

			}
		//Wenn kein Handle verfügbar ist, zumindest den Passivmodus speichern
		$this->internal_passive=$passivemode;
		}

	///Log mitführen
	function addlog($text)
		{
		$this->log[]=date("H:i:s d.m.y",time())."  ".$text;
		}

	///Die Verzeichnisstruktur lesen
	function getdir()
		{
		$result=FALSE;
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			//Verzeichnis holen
			//Einmal RAW
			$result=ftp_rawlist($this->internal_handle,$this->current_dir);

			//Hat funktioniert ?
			if ($result!=FALSE)
				{
				$this->addlog("get listing ".$this->current_dir);

				//So nun spalten wir den ganzen Spaß auf
				$out=array();
				$regs=array();
				foreach ($result as $index => $line)
					{
					//Alles zerlegen
					ereg($this->internal_regex, $line, $regs);

					//Und die Dateitypen zuordnen
					//Wenn die Auflösung funktioniert hat
					if (count($regs)==6) switch ($regs[1])
						{
						case ("d") :	$out["dirs"][] =$regs[5];	break;
						case ("l") :	$out["links"][]=$regs[5];	break;
						default	   :	$out["files"][]=$regs[5];	break;
						}
					}

				//Dann als ersten Array Eintrag den kpl. Pfad reinschreiben
				$out["path"]=$this->current_dir;

				//Als Ergebnis fertigstellen
				$result=$out;
				//Speicher freigeben
				unset($out);
				}
			else
				{
				$this->addlog("get listing ".$this->current_dir." failed");
				}
			}
		return($result);
		}

	///Ein Verzeichnis direkt wechseln
	function dirset($newdir)
		{
		$result=FALSE;
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			//Driekt wechseln
			$result=ftp_chdir($this->internal_handle,$newdir);

			//Bei Erfolg neues Verzeichnis merken
			if ($result!==FALSE)
				{
				$this->current_dir=$newdir;
				$this->addlog("changed dir to ".$this->current_dir);
				}
			else
				{
				$this->addlog("unable to change to ".$newdir);
				}

			}
		return($result);
		}

	///Ein Verzeichnis nach unten wechseln (Eine kpl. Pfadangabe ist nicht nötig)
	///es reicht einfach den namen zu übergeben.
	function dirdown($newdir)
		{
		$result=FALSE;
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			//Neues Verzeichnis zusammenbauen
			$newdir=$this->current_dir."/".$newdir;

			$result=ftp_chdir($this->internal_handle,$newdir);

			//Bei Erfolg neues Verzeichnis merken
			if ($result!==FALSE)
				{
				$this->current_dir=$newdir;
				$this->addlog("changed dir to ".$this->current_dir);
				}
			else
				{
				$this->addlog("unable to change to ".$newdir);
				}
			}
		return($result);
		}

	///Ein Verzeichnis nach oben wechseln
	///Kein Pfad notwendig (logisch)
	function dirup()
		{
		$result=FALSE;
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			//Neues Verzeichnis zusammenbauen
			$newdir=substr($this->current_dir,0,strrpos($this->current_dir,"/"));

			$result=ftp_chdir($this->internal_handle,"..");

			//Bei Erfolg neues Verzeichnis merken
			if ($result!==FALSE)
				{
				$this->current_dir=$newdir;
				$this->addlog("changed dir to ".$this->current_dir);
				}
			else
				{
				$this->addlog("unable to change to ".$newdir);
				}
			}
		return($result);
		}

	///Eine Datei runterladen
 	///Bei Textfiles Binary=FALSE ansonsten immer TRUE
	function fileget($serverfile,$localfile,$binary=FALSE)
		{
		$result=FALSE;
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			$result=ftp_get($this->internal_handle,$localfile,$serverfile,($binary?FTP_BINARY:FTP_ASCII));

			//Erfolg ?
			if ($result!==FALSE)
				{
				$this->addlog($serverfile." saved to ".$localfile);
				}
			else
				{
				$this->addlog("unable to download ".$serverfile);
				}
			}
		return($result);
		}

	///Eine Datei hochladen
 	///Bei Textfiles Binary=FALSE ansonsten immer TRUE
	function fileput($serverfile,$localfile,$binary=FALSE)
		{
		$result=FALSE;
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			$result=ftp_put($this->internal_handle,$serverfile,$localfile,($binary?FTP_BINARY:FTP_ASCII));

			//Erfolg ?
			if ($result!==FALSE)
				{
				$this->addlog($localfile." uploaded to ".$serverfile);
				}
			else
				{
				$this->addlog("unable to upload ".$serverfile);
				}
			}
		return($result);
		}

	///Ein kpl. Verzeichnis laden
	///Noch in Arbeit
	function mirror($serverdir,$localdir,$recursive)
		{
		$result=FALSE;
		//Echter Handle verfügbar ?
		if ($this->internal_handle!==FALSE)
			{
			//Das lokale Verzeichnis erstellen, wenn es noch nicht existiert
			if (!is_dir($localdir))
				{
				if (mkdir($localdir))
					{
					$this->addlog("create local directory ".$localdir);
					}
				else
					{
					//Das lokale Verzeichnis läst sich nicht erstellen,
					//dann müssen wir hier abbrechen
					$this->addlog("unable to create local directory ".$localdir);
					return($result);
					}
				}
			}
		return($result);
		}
	}

</script>