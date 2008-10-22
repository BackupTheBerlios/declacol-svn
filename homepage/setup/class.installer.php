<script language="PHP">
///////////////////////////////////////////////////////////////////////////////////////////////////
//Ein einfaches Script, um einen Installer unter PHP zu realisieren.
//Alle Dateien in einem vorgegebenen Verzeichnis werden zusammengepackt
//und mit Ihren Rechten gespeichert.
//Beim entpacken wird die Verzeichnisstruktur und die Rechtevergabe
//wiederhergestellt
///////////////////////////////////////////////////////////////////////////////////////////////////

//Typen der Archivpakete
define ("TYPE_FILE"         ,1);    //Eintrag ist eine Datei
define ("TYPE_DIRECTORY"    ,2);    //Eintrag ist ein Verzeichnis
define ("TYPE_COMMAND"      ,3);    //Eintrag ist ein PHP-Befehl

//Die IDs in den Datenarrays
define ("ID_BASE"           ,"a");
define ("ID_TYPE"           ,"b");
define ("ID_SIZE"           ,"c");
define ("ID_HEAD"           ,"d");
define ("ID_PATH"           ,"e");
define ("ID_DATA"           ,"f");
define ("ID_NAME"           ,"g");
define ("ID_TIME"           ,"h");
define ("ID_CRC"            ,"i");
define ("ID_PERM"           ,"j");

//Fehlercodes
define ("ERROR_NONE"            ,0);
define ("ERROR_NO_SOURCE"       ,100);
define ("ERROR_NO_TARGET"       ,101);
define ("ERROR_INVALID_ARCHIVE" ,102);
define ("ERROR_CRC_ERROR"       ,103);
define ("ERROR_WRITE_ERROR"     ,104);
define ("ERROR_READ_ERROR"      ,105);


//Diverse andere Konstanten
define ("PACK_LEVEL"        ,9);
define ("EOL"               ,chr(0x0a)); //EOL-Kennung im Archiv quasi beliebig
define ("LINE"              ,"-------------------------------------------------------------------------------------------------");
define ("FULL_PERM"         ,511);  //Volle Schreibrechte (=0777)
define ("FILE_HEADER"       ,"PharFromHome 0.1");
define ("MAX_TIME"          ,30);


class installer
    {
    var $overwrite  = FALSE;
    var $update     = TRUE;
    var $packlevel  = 9;        //Kompressionslevel 0=Niedrig - 9=Hoch
    var $eol        = "\n";     //EOL-Kennung
    var $testrun    = FALSE;
    
    //Welche Verzeichnisse werden ignoriert
    var $ignore     = array(".","..",".svn",".csv");

    var $lasterror  = ERROR_NONE;
    
    var $globalsize = 0;
    
    var $filecount  = 0;
    var $dircount   = 0;
    var $errorcount = 0;
    var $skipcount  = 0;
    var $updatecount= 0;
    var $logout     = FALSE;    //Logs gleichzeitig ¸ber ECHO ausgeben ?

    var $log        = array();

    function archiver()
        {
        }
        
    function destroy()
        {
        unset($this);
        }
        
    //Errorflag setzen und eine Logmeldung absetzen
    function seterror($errorid,$text="")
        {
        $this->lasterror=$errorid;
        
        switch ($errorid)
            {
            case (ERROR_NONE)            : $this->addlog("no error ".$text);                     break;
            case (ERROR_NO_SOURCE)       : $this->addlog("no source file/dir : '".$text."'");    break;
            case (ERROR_NO_TARGET)       : $this->addlog("no target file/dir : '".$text."'");    break;
            case (ERROR_INVALID_ARCHIVE) : $this->addlog("invalid archive '".$text."'");         break;
            case (ERROR_CRC)             : $this->addlog("checksum error in '".$text."'");       break;
            case (ERROR_WRITE)           : $this->addlog("unable to write to '".$text."'");      break;
            case (ERROR_READ)            : $this->addlog("unable to read '".$text."'");          break;
            default : break;
            }
        }
        
    //Logeintrag zuf¸gen
    function addlog($text)
        {
        $text=time().":".trim($text);
        $this->log[]=$text;
        
        if ($this->logout==TRUE)
            {
            echo $text."\n";
            }
        }

    //Log lˆschen
    function clearlog()
        {
        $this->log=array();
        }
    

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Das Archive testen
    function test($archive)
        {
        $this->testrun=TRUE;
        $result=$this->unpack($archive,"./");
        $this->testrun=FALSE;
        return($result);
        }


    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Das Archive entpacken
    function unpack($archive,$targetpath,$extractfile="")
        {
        $this->lasterror=ERROR_NONE;
        $this->dircount   = 0;
        $this->filecount  = 0;
        $this->errorcount = 0;
        $this->skipcount  = 0;
        $this->updatecount= 0;

        //Verzeichnis OK ?
        if (is_dir($targetpath)==FALSE)
            {
            $this->seterror(ERROR_NO_TARGET,$targetpath);
            return(FALSE);
            }

        //Pfad normieren
        $target=$this->normalizepath($targetpath);
        
        $this->addlog("target : ".$target);
        $this->addlog(LINE);

        //Fehler abfangen
        if ($this->testrun==FALSE)
            {
            //Wenn wir nicht im Testmodus sind erzwingen wir den Zielpfad
            $this->outputdirectory($target,FULL_PERM,time());

            if ( (is_dir($target)==FALSE) || (is_writable($target)==FALSE) )
                {
                $this->seterror(ERROR_WRITE.$targetpath);
                return (FALSE);
                }
            }

        //Datei OK ?
        $handle=fopen($archive,"rb");
        if ($handle!=FALSE)
            {
            $char=TRUE;

            //Header lesen und pr¸fen
            $header=fgets($handle);
            if (trim($header)!=trim(FILE_HEADER))
                {
                //Alles abbrechen
                fclose($handle);
                $this->seterror(ERROR_INVALID_ARCHIVE,$archive);
                return(FALSE);
                }

            //Alle Zeilen lesen
            while ($char!==FALSE)
                {
                @set_time_limit(MAX_TIME);
                $buffer="";

                $char=fgetc($handle);
                //Eine Zeile lesen
                while ( ($char!=EOL) && ($char!==FALSE) )
                    {
                    $buffer.=$char;
                    $char=fgetc($handle);
                    }
                    
                //Analysieren
                $buffer=unserialize ($buffer);

                //Ist OK ?
                if (is_array($buffer))
                    {
                    //Melden
                    $this->addlog($buffer[ID_PATH].$buffer[ID_HEAD][ID_NAME]." [".decoct($buffer[ID_HEAD][ID_PERM])."] ".date("H:i:s d.m.Y",$buffer[ID_HEAD][ID_TIME]));

                    //CRC OK ?
                    if (md5($buffer[ID_DATA])==$buffer[ID_CRC])
                        {
                        //Im Testrun nur Status melden
                        if ($this->testrun==TRUE)
                            {
                            $this->addlog("ok");
                            }
                        else
                            {
                            //Ansonsten richtig verarbeiten
                            $filename=$target.$buffer[ID_PATH].$buffer[ID_HEAD][ID_NAME];
                            switch ($buffer[ID_TYPE])
                                {
                                case (TYPE_FILE)        :   if ( ($extractfile=="") || (basename($filename)==$extractfile) )
                                                                {
                                                                //Einzeldateien in das Basisverzeichnis entpacken
                                                                $filename=(basename($filename)==$extractfile?$target.basename($filename):$filename);
                                                                
                                                                //Enpacken
                                                                $this->outputfile( $filename,
                                                                                   gzuncompress(base64_decode($buffer[ID_DATA])),
                                                                                   $buffer[ID_HEAD][ID_PERM],
                                                                                   $buffer[ID_HEAD][ID_TIME]
                                                                                 );
                                                                $this->filecount+=1;
                                                                }
                                                            break;
                                //Vezeichnisse nur erzeugen, wenn wir nicht nach einer Datei suchen
                                case (TYPE_DIRECTORY)   :   if ($extractfile=="")
                                                                {
                                                                $this->addlog(LINE);
                                                                $this->outputdirectory( $filename,
                                                                                        $buffer[ID_HEAD][ID_PERM],
                                                                                        $buffer[ID_HEAD][ID_TIME]
                                                                                      );
                                                                //Datei mitz‰hlen
                                                                $this->dircount+=1;
                                                                }
                                                            break;
                                case (TYPE_CODE)        :   break;
                                default                 :   die("unknown data type"); break;
                                }
                            }
                        }
                    else
                        {
                        $this->errorcount++;
                        $this->lasterror=ERROR_CRC;
                        $this->addlog("checksum failure");
                        }
                    }
                }
            fclose($handle);

            $this->addlog(LINE);
            $this->addlog("files in archive : ".$this->filecount);
            $this->addlog("dirs created     : ".$this->dircount);
            $this->addlog("files unpacked   : ".$this->updatecount);
            $this->addlog("files skipped    : ".$this->skipcount);
            $this->addlog("errors           : ".$this->errorcount);
            }
        else
            {
            $this->seterror(ERROR_READ,$archive);
            }
        $this->addlog(LINE);

        return(TRUE);
        }


    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein kpl. Archiv erzeugen
    function create($basepath,$output)
        {
        $this->lasterror=ERROR_NONE;
        $result=FALSE;
        
        //Verzeichnis OK ?
        if (is_dir($basepath)==FALSE)
            {
            $this->seterror(ERROR_NO_SOURCE,$basepath);
            }
        else
            {
            //Basispfad in ein einheitliches Format bringen
            $base=$this->normalizepath($basepath);

            //Alle Daten ziehen
            $result=array();
            $this->readall($base,$filedata);

            //Statusfelder rausnehmen
            //Wird im Moment noch nicht benˆtigt, aber wer weiﬂ
            $status=$filedata["?status?"];
            unset($filedata["?status?"]);

            //Alles komprimieren
            if ($this->createfile($output)==TRUE)
                {
                $result=TRUE;
                //Alle Dateien komprimieren und in eine Datei schreiben
                foreach ($filedata as $path=>$directory)
                    {
                    //Absoluten Pfad zur Datei machen
                    $mybase=$this->removeslash($base).$path;

                    foreach ($directory as $file)
                        {
                        @set_time_limit(MAX_TIME);

                        $pack=array();
                        $this->addlog("compressing : ".$mybase.$file[ID_NAME]);

                        //Datei komprimieren
                        $pack[ID_TYPE]=(is_file($mybase.$file[ID_NAME])==TRUE?TYPE_FILE:TYPE_DIRECTORY);
                        $pack[ID_PATH]=$path;
                        $pack[ID_HEAD]=$file;
                        $pack[ID_DATA]=base64_encode(gzcompress(@file_get_contents($mybase.$file[ID_NAME]),PACK_LEVEL));
                        $pack[ID_CRC] =md5($pack[ID_DATA]);

                        //Schˆn in String konvertieren
                        $pack=serialize($pack);

                        //Anf¸gen
                        $this->writeline($output,$pack);
                        }
                    $this->addlog(LINE);
                    }
                }
            else
                {
                $this->seterror(ERROR_WRITE,$output);
                $result=FALSE;
                }
            }
        return($result);
        }


    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Alle Daten durchscannen und das Ergebnis in Result liefern
    function readall($startpath,&$result)
        {
        $start=$this->normalizepath($startpath);

        if (is_dir($start)==FALSE)
            {
            $this->seterror(ERROR_NO_SOURCE,$start);
            return(FALSE);
            }

        //Ausgabe als Array erzwingen
        if (is_array($result)==FALSE)
            {
            $result=array();
            }

        //Alle Verzeichnisse holen
        $this->globalsize = 0;
        $data=array();
        if ($this->readdirs($start,$data)!=FALSE)
            {
            ksort($data);

            //Statusdaten retten
            $result["?status?"][ID_SIZE] =$this->globalsize;
            $result["?status?"][ID_BASE] =$start;

            //Alle Pfade auf den Basispfad k¸rzen
            reset($data);
            $base=key($data);
            $len=strlen($base);
            foreach ($data as $path=>$entry)
                {
                $key="/".substr($path,$len,8192);
                $result[$key]=$entry;
                }
            return(TRUE);
            }
        return(FALSE);
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Recursiv alle Verzeichnisse lesen und in ein Array speichern
    function readdirs($startpath,&$data)
        {
        $result=FALSE;
        $mysize=0;
        $start=$this->normalizepath($startpath);

        $handle=opendir($start);
        if ($handle!=FALSE)
            {
            $result=TRUE;

            $entry=readdir($handle);
            while ($entry!=FALSE)
                {
                @set_time_limit(MAX_TIME);
                
                //Nur Zugelassene Dateien scannnen
                if (in_array($entry,$this->ignore)==FALSE)
                    {
                    //Rekursiv in Unterverzeichnisse springen
                    if (is_dir($start.$entry)==TRUE)
                        {
                        $subscan=array();
                        if ($this->readdirs($start.$entry,$subscan)!=FALSE)
                            {
                            $data=array_merge($data,$subscan);
                            }
                        //Speicher freigeben
                        unset($subscan);
                        }
                    else
                        {
                        //Dateien ablegen
                        $data[$start][$entry][ID_NAME]=$entry;
                        $data[$start][$entry][ID_TIME]=filemtime($start.$entry);
                        $data[$start][$entry][ID_SIZE]=filesize ($start.$entry);
                        //Dateirechte (dezimal speichern)
                        $data[$start][$entry][ID_PERM]=(fileperms($start.$entry) & FULL_PERM);

                        //Verzeichnisgrˆﬂe mitrechnen
                        $mysize=$data[$start][$entry][ID_SIZE];
                        }
                    }
                //N‰chsten Eintrag aus dem Verzeichnis holen
                $entry=readdir($handle);
                }
            //Daten des Verzeichnisses speichern
            //Dadurch werden auch leere Verzeichnisse automatisch erstellt
            $data[$start]["?self?"][ID_NAME]="";
            $data[$start]["?self?"][ID_TIME]=filemtime($start);
            $data[$start]["?self?"][ID_SIZE]=$mysize;
            //Dateirechte (dezimal speichern)
            $data[$start]["?self?"][ID_PERM]=(fileperms($start) & FULL_PERM);

            //Fertig
            closedir($handle);
            }
        //Daten ¸berarbeiten
        ksort($data[$start]);
        
        //Gesamtgrˆﬂe anpassen
        $this->globalsize+=$mysize;

        return($result);
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Verzeichnispfad normieren
    function normalizepath($name)
        {
        $name=realpath($name);
        $name=$this->convertslashes($name);
        $name=$this->appendslash($name);

        return($name);
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Slash an den Verzeichnisnamen anh‰ngen
    function appendslash($name)
        {
        if ($name[strlen($name)-1]!="/")
            {
            $name.="/";
            }
        return($name);
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Endslash entfernen
    function removeslash($name)
        {
        $len=strlen($name);
        if ($name[$len-1]=="/")
            {
            $name=substr($name,0,$len-1);
            }
        return($name);
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Slashes im Pfad konvertieren
    function convertslashes($name)
        {
        return(str_replace("\\","/",$name));
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Dateifunktionen (Zum erzeugen des Archives)
    function createfile($filename)
        {
        $handle=fopen($filename,"wb+");
        if ($handle!=FALSE)
            {
            fwrite($handle,FILE_HEADER."\n");
            fclose($handle);
            }
        return(is_file($filename));
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Dateifunktionen (Zum erzeugen des Archives)
    //Immer neu ˆffnen ist zwar etwas aufw‰ndig, aber daf¸r sicher im
    //abspeichern
    function writeline($filename,$data)
        {
        $result=FALSE;

        $handle=fopen($filename,"a");
        if ($handle!=FALSE)
            {
            fwrite($handle,$data.EOL);
            fclose($handle);
            $result=TRUE;
            }
        return($result);
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Dateifunktionen (Zum entpacken)
    //Sicher alle Zwischenverzeichnisse erzwingen
    //Und Modus und Zeit setzen
    //Dateizeit wird im Moment noch nicht unterst¸tzt
    function outputdirectory($path,$mode,$filemtime)
        {
        $subs=explode("/",$path);
        $base="";
        foreach ($subs as $sub)
            {
            $base.=$sub."/";
            if (is_dir($base)==FALSE)
                {
                mkdir($path);
                }
            }

        //Modus setzen
        chmod($path,FULL_PERM);
        chmod($path,$mode);

        //Status ausgeben
        $result=is_dir($path);
        if ($result==FALSE)
            {
            $this->Addlog("unable to create ".$path);
            }
        else
            {
            $this->dircount++;
            $this->addlog("dir  ".decoct($mode)." : ".$path." OK");
            }
        return($result);
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Datei schreiben und den Modus und die Zeit setzen
    //Dateizeit wird im Moment noch nicht unterst¸tzt
    function outputfile($path,$data,$mode,$filemtime)
        {
        //Datei existiert schon ?
        //Und es soll nicht ¸berschrieben werden ?
        $skip=FALSE;
        if ( (is_file($path)==TRUE) && ($this->overwrite==FALSE) )
            {
            //Soll aber ein Update zugelassen sein ?
            if ($this->update==TRUE)
                {
                //Dateien identisch ?
                if ($this->identicaloutput($data,$filemtime,$path)==TRUE)
                    {
                    $skip=TRUE;
                    }
                else
                    {
                    //Schreibrechte erzwingen
                    chmod($path,FULL_PERM);
                    }
                }
            else
                {
                $skip=TRUE;
                }
            }

        //Datei ¸berspringen
        if ($skip==TRUE)
            {
            $this->skipcount++;
            $this->addlog("skipped");
            return(TRUE);
            }

        //Datei einfach hart schreiben
        $handle=fopen($path,"wb");
        if ($handle!=FALSE)
            {
            fwrite($handle,$data);
            fclose($handle);
            chmod($path,$mode);
            }

        //Status ausgeben
        $result=is_file($path);
        if ($result==FALSE)
            {
            $this->errorcount++;
            $this->addlog("<b>unable to create");
            }
        else
            {
            $this->updatecount++;
            $this->addlog("OK");
            }
        return($result);
        }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    //Zwei Dateien vergleichen
    function identicaloutput($data,$filemtime,$targetfile)
        {
        //Wenn die Dateien schon unterschiedlich groﬂ sind,
        //brauchen wir keinen MD5-Check
        if (filesize($targetfile)!=strlen($data))
            {
            $result=TRUE;
            }
        else
            {
            $result=md5($data)==md5_file($targetfile);
            }
        return($result);
        }
    }
</script>