<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Bibliothek für Dateifunktionen
///
/// Sammlung einiger (sehr) praktischer Datei und Verzeichnisfunktionen
///
///
//////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("LIB_FILES","lib_files");
define ("LIB_FILES_VERSION","0.011");
if (isset($debug)) $debug->add(LIB_FILES,"version ".LIB_FILES_VERSION);

///Die Dateien eines Verzeichnisses als Array ausgeben
    function file_scan($basepath,$ext_array=array(),$fullpath=TRUE,$recursive=FALSE,$hidden=FALSE,$maxlevel=65535)
        {
        //Fehler abfangen
        if (!is_dir($basepath)) return(FALSE);
        $files=array();
        $dir = dir($basepath);
        while (false !== ($entry = $dir->read()))
            {
            //Filter gesetzt ?
            if ($hidden!=FALSE)
                {
                //Hidden ?
                if (strpos($entry,$hidden)!==FALSE)
                    {
                    //Eintrag löschen
                    $entry=FALSE;
                    }
                }

            //Datei gefunden ?
            if ($entry!==FALSE)
                {
                if (!is_dir($basepath.$entry))
                    {
                    //Extension holen
                    $pathinfo=pathinfo($entry);
                    @$extension=$pathinfo["extension"];

                    //Und checken
                    if (in_array($extension,$ext_array)!=FALSE)
                        {
                        if ($fullpath!=FALSE)
                            {
                            $files[]=$basepath.$entry;
                            }
                        else
                            {
                            $files[]=$entry;
                            }
                        }
                    }
                //Es ist ein Verzeichnis, dann recursiv weiter
                else
                    {
                    //Recursiv suchen ?
                    if (($recursive) and ($entry[0]!=".") and ($maxlevel > 0))
                        {
                        $files=array_merge($files,file_scan($basepath.$entry."/",$ext_array,$fullpath,$recursive,$hidden,($maxlevel-1)));
                        }
                    }
                }
            }
        $dir->close();
        sort($files);
        return($files);
        }

///Die Verzeichnisses eines Verzeichnisses als Array ausgeben
    function dir_scan($basepath,$fullpath=TRUE,$recursive=FALSE,$hidden=FALSE,$maxlevel=65535)
        {
        //Fehler abfangen
        if (!is_dir($basepath)) return(FALSE);
        $dirs=array();
        $dir = dir($basepath);
        while (false !== ($entry = $dir->read()))
            {
            //Filter gesetzt ?
            if ($hidden!=FALSE)
                {
                //Hidden ?
                if (strpos($entry,$hidden)!==FALSE)
                    {
                    //Eintrag löschen
                    $entry=FALSE;
                    }
                }

            if ($entry!==FALSE)
                {
                //Verzeichnis ?
                if (is_dir($basepath.$entry))
                    {
                    if ($entry[0]!=".")
                        {
                        if ($fullpath)
                            {
                            $dirs[]=$basepath.$entry;
                            }
                        else
                            {
                            $dirs[]=$entry;
                            }
                        }
                    }
                else
                    {
                    //Recursiv suchen ?
                    if (($recursive) and ($entry[0]!=".") and ($maxlevel > 0))
                        {
                        $dirs=array_merge($dirs,dir_scan($basepath.$entry."/",$fullpath,$recursive,($maxlevel - 1)));
                        }
                    }
                }
            }
        $dir->close();
        if (is_array($dirs)) sort($dirs);
        return($dirs);
        }

/// Prüfen, ob ein Verzeichnis leer ist
    function IsDirEmpty($path)
        {
        if (! is_dir($path) ) return(TRUE);

        $found=0;

        $dir=dir($path);

        //Dateien im Verzeichnis lesen
        while ( (FALSE !== ($entry = $dir->read())) & ($found==0) )
            {
            if ($entry[0]!='.') $found++;
            }
        $dir->Close();
        if ($found==0) return(TRUE); else return (FALSE);
        }

/// Ein Verzeichnis rekursiv löschen
    function DelTree($path)
        {
        if (! is_dir($path) ) return(FALSE);

        $dir=dir($path);
        //Dateien im Verzeichnis lesen
        while (FALSE !== ($entry = $dir->read()))
            {
            if ($entry[0]!='.')
                {
                if (is_dir($path."/".$entry))
                    {
                    //Rekursion
                    DelTree($path."/".$entry);
                    }
                else
                    {
                    //Eine Datei, dann löschen
                    unlink($path."/".$entry);
                    }
                }
            }
        $dir->close();
        rmdir($path);
        return(TRUE);
        }

/// Alle leeren Verzeichnisse rekursiv löschen
    function DelEmptyTree($path)
        {
        if (! is_dir($path) ) return(FALSE);

        $dir=dir($path);
        //Dateien im Verzeichnis lesen
        while (FALSE !== ($entry = $dir->read()))
            {
            if ($entry[0]!='.')
                {
                if (is_dir($path."/".$entry))
                    {
                    //Rekursion
                    DelEmptyTree($path."/".$entry);
                    }
                }
            }
        $dir->close();
        //Wenn das Verzeichnis leer ist, dann löschen
        if (IsDirEmpty($path)) rmdir($path);
        return(TRUE);
        }
        
//Verzeichnisse rekurisv erstellen
    function CreateTree($path)
        {
        $result=FALSE;
        
        $split=explode("/",$path);
        $base="";
        
        foreach ($split as $subdir)
            {
            $base.=$subdir."/";
            if (file_exists($base)==FALSE)
                {
                mkdir($base);
                }
            }
        
        return (is_dir($path));
        }
        

/// Die neueste Datei in einem Verzeichnis holen
    function GetNewestFile($path)
        {
        if (! is_dir($path) ) return(FALSE);

        $time=0;
        $file=FALSE;
        $dir=dir($path);
        //Dateien im Verzeichnis lesen
        while (FALSE !== ($entry = $dir->read()))
            {
            if ($entry[0]!='.')
                {
                if (is_file($path."/".$entry))
                    {
                    $temp=filemtime($path."/".$entry);
                    //Datei neuer ?
                    if ($temp > $time)
                        {
                        //Dann merken
                        $file=$path."/".$entry;
                        $time=$temp;
                        }
                    }
                }
            }
        $dir->close();
        return($file);
        }

/// Alle Dateien löschen, die seit $timeout Sekunden nicht mehr geändert wurden
/// Alle Verzeichnisse löschen, die leer sind
    function DelOldFiles($path,$timeout)
        {
        if (! is_dir($path) ) return(FALSE);

        $filter =time()-$timeout;
        $dir    =dir($path);

        //Dateien im Verzeichnis lesen
        while (FALSE !== ($entry = $dir->read()))
            {
            if ($entry[0]!='.')
                {
                if (is_dir($path."/".$entry))
                    {
                    DelOldFiles($path."/".$entry,$timeout);
                    }
                else
                    {
                    if (filemtime($path."/".$entry) < $filter)
                        {
                        unlink ($path."/".$entry);
                        }
                    }
                }
            }
        $dir->close();
        return(TRUE);
        }

    //Ein Array abspeichern
    //Wobei das Array beliebig viele Unterarrays enthalten darf
    function savearray($path,$array)
        {
        //Fehler abfangen
        if (!is_array($array))
            {
            return(FALSE);
            }

        //Und nun einfach alles abspeichern
        $fp=fopen($path,"w+");
        if ($fp!=FALSE)
            {
            //An den Anfang des Array springen
            reset($array);
            //Und über eine Unterfunktion alles rekursiv speichern
            _savearray($fp,$array);
            }
        else
            {
            return (FALSE);
            }

        return(TRUE);
        }

    //Hilfsfunktion für SaveArray
    function _savearray($filepointer,&$array)
        {
        foreach ($array as $data)
            {
            //Sind wir ein Array, dann rufen wir und selbst auf
            if (is_array($data))
                {
                _savearray($filepointer,$data);
                }
            else
                {
                //Ansonsten den Wert schreiben
                fwrite($filepointer,$data."\r\n");
                }
            }
        }
</script>