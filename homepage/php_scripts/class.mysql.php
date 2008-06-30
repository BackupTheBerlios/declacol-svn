<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// MySQL-Klasse
///
/// Wrapper für MySQL-Zugriff
///
/// Bietet nur einige wenige Methoden an, die das Leben mit DBs leichter machen.
///
/// Um zu prüfen, ob eine Abfrage erfolgreich war, sollte man folgenden
/// Code benutzen
///
/// $result=$mysql->query("SELECT FROM BLA");
/// if (is_array($result))
///    {
///    //Daten gefunden
///    }
/// else if ($result)
///    {
///    //Abfrage OK aber keine Daten gefunden
///    }
/// else
///    {
///    //Abfrage fehlgeschlagen
///    }
///
//////////////////////////////////////////////////////////////////////////
///History :
///
///0.3 Neu Funktion doquery, die Parameter einparst
///    Dabei werden die Fragezeichen in ihrer Reihenfolge durch die Array-
///    Einträge ersetzt
///    Bsp. $mysql->doquery("SELECT FROM ? WHERE id=?",array("mydb",10));
///
///0.2 Funktion "flatten" eingefügt
///
///
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_MYSQL","class_mysql");
define ("CLASS_MYSQL_VERSION","0.4");
if (isset($debug)) $debug->add(CLASS_MYSQL,"version ".CLASS_MYSQL_VERSION);


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("MYSQL_ERROR_NOHANDLE","no handle given. try connecting to db");

//Befehl um den Dienst zu starten und zu stoppen
//Funktioniert natürlich nur, wenn der Server Localhost ist
//Win32
define ("MYSQL_DAEMON_STOP","net stop mysql");
define ("MYSQL_DAEMON_START","net start mysql");
//Unixoid
//define ("MYSQL_DAEMON_STOP","/etc/init.d/mysql stop");
//define ("MYSQL_DAEMON_START","/etc/init.d/mysql start");


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class mysql
    {
    //Öffentliche Eigenschaftem
    var $error="";
    var $active=FALSE;
    //Private Eigenschaften
    //Zugriffsdaten
    //später nach Konstants umziehen
    var $internal_dbname=FALSE;
    var $internal_dbhost=FALSE;
    var $internal_handle=FALSE;

    //Liste mit allen Tabellen in der Datenbank
    var $internal_tables=array();
    var $internal_tables_burned=TRUE;


    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function mysql($dbhost,$dbname)
        {
        //Namen für später merken
        $this->internal_dbname=$dbname;
        $this->internal_dbhost=$dbhost;

        $this->setdefault();
        }

    //Destruktor
    //Eine bestehende Verbindung wird mitgeschlossen
    function destroy()
        {
        //Haben wir noch Kontakt ?
        if ($this->internal_handle!=FALSE)
            {
            //Dann schließen
            $this->close();
            }
        }

    //Verbindungsaufbau zur Datenbank
    function Open($user,$pass)
        {
        global $debug;

        //Verbindung aufbauen (Immer eine neue)
        $this->internal_handle=@mysql_connect($this->internal_dbhost,$user,$pass,TRUE);

        //Verbindung OK ?
        if ($this->internal_handle!=FALSE)
            {
            //Datenbank auswählen
            if (!mysql_select_db($this->internal_dbname,$this->internal_handle))
                {
                //Fehler merken
                $this->error=mysql_error();

                //Debuginfo speichern
                $debug->add(CLASS_MYSQL,"unable to connect".$this->internal_dbhost);

                //Datenbank nicht gefunden ?
                //Dann Verbindung kappen
                mysql_close($this->internal_handle);
                $this->internal_handle=FALSE;
                $this->active=FALSE;
                }
            else
                {
                //Fehler merken
                $this->error=mysql_error();

                //Und active-flag setzen
                $this->active=TRUE;
                }
            }
        else
            {
            $this->error="unable to connect to server";
            }


        //Refresh der Tabellenliste beim nächsten Mal erzwingen
        $this->internal_tables_burned=TRUE;

        //Fertig
        return($this->internal_handle != FALSE);
        }

    //Die Verbindung zur Datenbank schließen
    function close()
        {
        global $debug;

        //Fehler abfangen
        if ($this->internal_handle==FALSE)
            {
            $this->error=MYSQL_ERROR_NOHANDLE;

            //Debuginfo speichern
            $debug->add(CLASS_MYSQL,"close without connection ");

            return(FALSE);
            }

        //Alles schließen
        mysql_close($this->internal_handle);

        //Und Flags anpassen
        $this->internal_handle=FALSE;
        $this->active=FALSE;

        return (TRUE);
        }

    //Defaults
    function setdefault()
        {
        $this->error="";
        $this->active=FALSE;
        }

    //Datenbank erzeugen
    //wird hier nicht benutzt
    function install()
        {
        }

    //Datenbank zerstören
    //wird hier nicht benutzt
    function uninstall()
        {
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    ////////////////////////////////////////////////////////////////////////////

    //Eine Abfrage machen
    //Als Antwort entweder :
    // Array, wenn Daten gefunden wurden
    // TRUE wenn die Aktion erfolgreich war, aber keine Daten geliefert wurden
    // FALSE wenn ein Fehler aufgetreten ist
    function Query($query)
        {
        //Debugobjekt holen
        global $debug;

        //Fehler abfangen
        if ($this->internal_handle==FALSE)
            {
            //Fehler merken
            $this->error=MYSQL_ERROR_NOHANDLE;

            //Debuginfo speichern
            $debug->add(CLASS_MYSQL,"query without connection ".$query);

            return(FALSE);
            }

        //Zeitlimit erhöhen
        @set_time_limit(30);

        //Und abfrage machen
        $result=mysql_query($query,$this->internal_handle);

        switch ($result)
            {
            //Nix machen bei Fehler
            case FALSE :     break;
            //Ergebnisse auswerten
            default       :     if ($result!==TRUE)
                                {
                                if (mysql_num_rows($result)>0)
                                    {
                                    //Alles auslesen
                                    $temp_result=array();
                                    while($row=mysql_fetch_assoc($result))
                                        {
                                        $temp_result[]=$row;
                                        }

                                    //Dann zurückgeben
                                    $result=$temp_result;
                                    //Speicher freigeben
                                    unset($temp_result);
                                    }
                                else
                                    {
                                    //Ansonsten Erfolg melden
                                    $result=TRUE;
                                    }
                                //Tabellenliste beim nächsten Mal erzeugen
                                $this->internal_tables_burned=TRUE;
                                }
                            else
                                {
                                $result=TRUE;
                                }
                            break;

            }
        //Fehler merken
        $this->error=mysql_error();

        //Debuginfo merken
        $debug->add(CLASS_MYSQL,"query ".$query." [".$this->error."]");

        //Fertig
        return($result);
        }


    ////////////////////////////////////////////////////////////////////////////
    //Eine Abfrage etwas einfacher bauen
    //Jedes Fragezeichen im String wird durch ein Arrayvalue ersetzt
    //Strings werden automatisch in Anführungsstriche gesetzt
    function doquery($query,$values=array())
        {
        //Immer als Array angehen
        if (!is_array($values))
            {
            $values=array($values);
            }

        //Jeden Eintrag durchgehen
        foreach ($values as $replace)
            {
            //Strings automatisch in Anführungsstriche
            if (gettype($replace)=="string")
                {
                //Sonderzeichen Escapen
                $replace=str_replace("?" ,"$%3F%$",$replace);
                $replace=str_replace("'" ,"\'"   ,$replace);

                $replace="'".$replace."'";
                
                }

            //Abfrage ersetzen
            $query=preg_replace("/\\?/",$replace,$query,1);
            }
            
        //Fragezeichen zurückkonvertieren
        $query=str_replace("$%3F%$","?",$query);

        //Und die Abfrage durchführen
        return($this->query($query));
        }

    ////////////////////////////////////////////////////////////////////////////
    //Prüfen, ob eine Tabelle existiert
    function TableExists($tablename)
        {
        //Fehler abfangen
        if ($this->internal_handle==FALSE)
            {
            $this->error=MYSQL_ERROR_NOHANDLE;
            return(FALSE);
            }

        //Refresh der Tabellenliste gefordert ?
        if ($this->internal_tables_burned===TRUE)
            {
            //Dann los
            $result=mysql_list_tables($this->internal_dbname,$this->internal_handle);
            }

        //Abfrage OK?
        if ($result!=FALSE)
            {
            //Alte Liste löschen
            $this->internal_tables=array();

            //Erste Tabelle holen
            $table=mysql_fetch_assoc($result);
            //Den Key holen
            $key=key($table);

            //Neue holen
            do
                {
                $this->internal_tables[]=$table[$key];
                }
            while ($table=mysql_fetch_assoc($result));

            //Und als not burned setzen
            $this->internal_tables_burned=FALSE;
            }

        //Und Ergebnis bestimmen
        return(array_search($tablename,$this->internal_tables)!==FALSE);
        }
        
    ////////////////////////////////////////////////////////////////////////////
    //Den Index-Key einer Tabelle holen
    function index($dbname)
        {
        $result=FALSE;
        $dbresult=$this->query("SHOW INDEX FROM ".$dbname);
        if (is_array($dbresult))
            {
            $result=$dbresult[0]["Column_name"];
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////
    //Alle Spalten einer Tabelle holen
    function columns($dbname)
        {
        $result=FALSE;
        $dbresult=$this->query("SHOW COLUMNS FROM ".$dbname);
        if (is_array($dbresult))
            {
            $result=$this->flatten($dbresult,"Field");
            }
        return($result);
        }


    ////////////////////////////////////////////////////////////////////////////
    //Zählt die Einträge einer Datenbank
    function count($dbname)
        {
        $result=FALSE;
        $dbresult=$this->query("SELECT COUNT(*) AS anzahl FROM ".$dbname);
        if (is_array($dbresult))
            {
            $result=$dbresult[0]["anzahl"];
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////
    //Ein "eindimensionales" Array von seiner Nummerierung befreien
    function flatten($data,$index)
        {
        if (!is_array($data))
            {
            return(FALSE);
            }

        //Einfach alle Einträge durchgehen und den Index rausnehmen
        $result=array();
        foreach ($data as $entry)
            {
            $result[]=$entry[$index];
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////
    //Versuchen den SQL-Server zu starten
    function start()
        {
        exec (MYSQL_DAEMON_START);
        }

    ////////////////////////////////////////////////////////////////////////////
    //Versuchen den SQL-Server zu stoppen
    function stop()
        {
        exec (MYSQL_DAEMON_STOP);
        }


    ////////////////////////////////////////////////////////////////////////////
    //Eine kpl. Tablle in eine Datei schreiben
    function dumpdb($file,$db)
        {
        $result=FALSE;

        $fp=fopen($file,"a+");
        if ($fp!=FALSE)
            {
            //Kurzen Kopf schreiben
            fwrite($fp,"##################################################################\r\n");
            fwrite($fp,"# MySQL-Dump\r\n");
            fwrite($fp,"# MySQLClass :".CLASS_MYSQL_VERSION."\r\n");
            fwrite($fp,"##################################################################\r\n");

            //Datenbank Definition
            $out=$this->_createdefinition($db);
            if ($out!="")
                {
                fwrite($fp,$out);
                }

            //Und die Daten hinterher
            $offset=0;
            $size  =50;
            
            //Um Speicher zu sparen ziehen wir das Backup immer Blockweise
            
            do
                {
                $dbresult=$this->query("SELECT * FROM ".$db." LIMIT ".$offset.",".$size);
                if (is_array($dbresult))
                    {
                    //Jede Zeile mit einem Insert bauen.
                    //Ist zwar etwas aufwendiger, erlaubt aber das einfache rauspicken
                    //einzelner Zeilen
                    foreach ($dbresult as $line)
                        {
                        //Alle Spaltennamen
                        $cols=implode(",",array_keys($line));
                        //Alle Werte
                        $vals=implode("','",$line);
                        $out="INSERT INTO ".$db." (".$cols.") VALUES('".$vals."');\r\n";
                        fwrite($fp,$out);
                        }
                    $result=TRUE;
                    }
                else
                    {
                    //Kein Zugriff auf die Datenbank
                    //Oder das Ende erreicht
                    }

                $offset+=$size;
                }
            while (is_array($dbresult));
            fclose($fp);
            }
        else
            {
            //Kein Zugriff auf die Datei
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////
    //Die Definitoin einer Tabelle zusammenbauen
    function _createdefinition($db)
        {
        global $mysql;

        $result="";

        //Spalten Informationen abfragen
        $dbresult=$mysql->query("SHOW COLUMNS FROM ".$db);
        if (is_array($dbresult))
            {
            $result="DROP TABLE IF EXISTS `".$db."`;\r\n";
            $result.="CREATE TABLE `".$db."` (";
            $primary="";
            foreach ($dbresult as $entry)
                {
                $entry=array_change_key_case($entry,CASE_LOWER);
                
                $result.="`".$entry["field"]."` ";
                $result.=$entry["type"]." ";

                //Und die Flags verarbeiten
                if ($entry["null"]=="NO")
                    {
                    $result.=" NOT NULL";
                    }

                if ( ($entry["default"]!==NULL) &&  ($entry["default"]!="") )
                   {
                   $result.=" DEFAULT '".$entry["default"]."'";
                   }

                if ($entry["key"]=="PRI")
                    {
                    $primary="PRIMARY KEY (`".$entry["field"]."`)";
                    }

                $result.=" ,";
                }
            //Den Key anhängen, ansonsten das letzte Komma entfernen
            if ($primary!="")
                {
                $result.=$primary;
                }
            else
                {
                $result=substr($result,0,strlen($result)-1);
                }
            $result.=");\r\n";
            }

        return($result);
        }

    }
</script>