<script language="PHP">
///////////////////////////////////////////////////////////////////////////////////////
///
/// Ein paar kleine Hilfsfunktionen um einzelen Modulen das Logging zu erleichtern
///
///////////////////////////////////////////////////////////////////////////////////////
if (!defined("DIR_LOG")) define ("DIR_LOG","/");


log_add("c:/temp/dogosch.log","testeintrag","kjgs454");

///////////////////////////////////////////////////////////////////////////////////////
//Einfach einen Text mit Uhrzeit und allem anderen loggen
function log_add($filename,$text,$id=FALSE)
    {
    global $session;
    $result=FALSE;
    
    $out=date("H:i:s d.n.Y",time())."|";

    //Evtl. verfügbare Sessiondaten einbinden
    if (isset($session)!=FALSE)
        {
        $out.=$session->id."|";
        $out.=$session->user->name."|";
        }

    if ($id!=FALSE)
        {
        $out.=$id."|";
        }

    //Verhindern das Logdateien in ein anderes Verzeichnis als
    //das gewünschte wandern
    $filename=DIR_LOG.basename($filename);
    

    //Einfach an die Datei anhängen
    //Kein Locking etc.
///#so9 Todo Locking
    if (is_writeable($filename))
        {
        $fp=fopen($filename."a");
        if ($fp!=FALSE)
            {
            fwrite($fp,$out."\r\n");
            fclose($fp);
            }
        }
    //Fertig
    return($result);
    }
    
function log_read($filename)
    {
    $result=array();
    
    
    $filename=DIR_LOG.basename($filename);

///#so9 Todo Locking
    if (is_readable($filename))
        {
        $fp=fopen($filename."r");
        if ($fp!=FALSE)
            {
            while (feof($fp)==FALSE)
                {
                $result[]=fgetcsv($fp,512,"|");
                }
            fclose($fp);
            }
        }
    return($result);
    }
    

///////////////////////////////////////////////////////////////////////////////////////
//Eine Datei löschen
function log_clear($filename)
    {
    if (is_writeable($filename)==TRUE)
        {
        $fp=fopen($filename,"w");
        fclose($fp);
        }
    }

</script>