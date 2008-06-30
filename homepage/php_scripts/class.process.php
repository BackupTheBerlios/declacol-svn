<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Klasse zur Verwaltung externer Prozesse
///
///
///
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_PROCESS","class_process");
define ("CLASS_PROCESS_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_PROCESS,"version ".CLASS_PROCESS_VERSION);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class process
	{
	//Öffentliche Eigenschaftem
	var $active				=FALSE;
	var $eof				=TRUE;			//Ende des Ausgabepuffers erreicht
	var $size				=0;				//Größe des Puffers
	var $ignore_empty		=FALSE;			//Leerzeilen ignorieren ?

	var $internal_buffer	=array();		//Puffer für die Ausgaben
	var $internal_index		=0;				//Zeiger auf aktuelle Zeile im Puffer
	var $internal_handle	=FALSE;			//Handle auf den Process

	//////////////////////////////////////////////////////////////////////////
	//Konstruktor
	function process()
		{
		//Alle Werte auf Default setzen
		$this->setdefault();
		}

	//Destruktor
	function destroy()
	    {
		//Sicherheitshalber den Prozess schließen
		$this->close();
	    }

	//Process ausführen
	function execute($processname,$params="")
		{
		//Wenn noch ein Handle offen ist, diesen erst schließen
		if ($this->internal_handle!==FALSE)
			{
			$this->close();
			}

		//Wenn die Parameter ein Array sind, dieses auflösen
		$parameter=" ";
		if ($params!="")
			{
			//Alle Einträge in das Array als Parameter werten
			if (is_array($params))
				{
				foreach ($params as $param)
					{
					$parameter.="\x22".escapeshellcmd($param)."\x22 ";
					}
				}
			else
				{
				$parameter.="\x22".escapeshellcmd($params)."\x22 ";
				}
			}

		//Anwendung ausführen
		$this->internal_handle=popen($processname.$parameter,"r");

		//Ergebnis melden
		$this->active=$this->internal_handle!=FALSE;

		//Wenn alles Funktioniert hat, die Variablen setzen
		if ($this->active)
			{
			//Puffer füllen
			$this->_fill_buffer();
			$this->eof=FALSE;
			$result=TRUE;
			}
		else
            {
            $result=TRUE;
            }
		return($result);
		}

	//Verbindungsabbau
	function close()
		{
		if ($this->internal_handle!=FALSE)
			{
			pclose($this->internal_handle);
			}

		//Alle Flags wieder killen
		$this->setdefault();

		return(TRUE);
		}

	//Defaultwerte der internen und externen Variablen setzen
	function setdefault()
		{
		$this->active			=FALSE;
		$this->internal_handle	=FALSE;
		$this->internal_buffer	=array();
		$this->internal_index	=0;
		$this->size				=0;
		$this->eof				=TRUE;
		}

	//Datenbank und alles andere erzeugen
	//Wird hier nicht benutzt
	function install()
		{
		return(TRUE);
		}

	//Datenbank und alles andere zerstören
	//Wird hier nicht benutzt
	function uninstall()
		{
		return(TRUE);
		}

	//////////////////////////////////////////////////////////////////////////
	///Ab hier die eigentlichen Funktionen
	//////////////////////////////////////////////////////////////////////////

	//Den Ausgabepuffer füllen
	function _fill_buffer()
		{
		if ($this->active)
			{
			while (!feof($this->internal_handle))
				{
				//Zeile lesen
				$input=trim(fgets($this->internal_handle));

				//Leerzeile zugelassen ?
				if ($input!="")
					{
					$this->internal_buffer[]=$input;
					}
				else
					{
					if (!$this->ignore_empty)
						{
						$this->internal_buffer[]=$input;
						}
					}


				}
			}
		//Füllstand des Puffers speichern
		$this->size=count($this->internal_buffer);
		//Zeiger resetten
		$this->internal_index=0;
		}

	//Eine Zeile des Ausgabepuffers lesen
	function getfirst()
		{
		//Zeiger zurücksetzen
		$this->internal_index=0;

		//EOF-Flag setzen
		$this->eof=$this->internal_index >= $this->size;

		//Und Eintrag lesen
		return($this->getnext());
		}

	//Eine Zeile des Ausgabepuffers lesen
	function getnext()
		{
		//Wenn wir noch nicht am Ende sind dann den Eintrag holen
		if (!$this->eof)
			{
			$result=$this->internal_buffer[$this->internal_index];
			}
		else
			{
			$result=FALSE;
			}

		//Auf nächsten Wert zielen
		$this->internal_index++;

		//EOF-Flag setzen
		$this->eof=$this->internal_index >= $this->size;

		//Fertig
		return($result);
		}
	}

</script>