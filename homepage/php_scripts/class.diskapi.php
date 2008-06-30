<script language="PHP">
///////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Klasse mit allen Dateizugriffsfunktionen
///
///
///
///
///////////////////////////////////////////////////////////////////////////////////////////////////
require_once("lib.strings.php");


class filepermission
    {
    var $read        = FALSE;
    var $write       = FALSE;
    var $execute     = FALSE;
    }

///////////////////////////////////////////////////////////////////////////////////////////////////
//Zugriff auf Dateien und Ihre Rechte
class fileapi
    {
    var $path       = "";
    var $filename   = "";
    var $ctime      = 0;
    var $atime      = 0;
    var $size       = 0;

    //String mit den Zugriffrechten
    var $permstr    = "";
    var $permoct    = 0000;

    //Zugriffsrechte
    var $owner      = FALSE;
    var $group      = FALSE;
    var $all        = FALSE;

    var $internal_ok= FALSE;

    //Konstruktor
    function fileapi($path,$filename)
        {
        $this->path=$path;
        $this->filename=$filename;

        $this->internal_ok=file_exists($this->path.$this->filename);

        $this->refresh();
        }

    //Destruktor
    function destroy()
        {
        }

    //Informationen neu einlesen
    function refresh()
        {
        if ($this->internal_ok)
            {
            $this->_getpermission();
            $this->_getstatus();
            return(TRUE);
            }
        else
            {
            return(FALSE);
            }
        }


    //Inhalt der Datei lesen
    function read()
        {
        if ($this->internal_ok)
            {
            return(file_get_contents($this->path.$this->filename));
            }
        else
            {
            return(FALSE);
            }
        }

    //Daten in Datei schreiben
    function write($data)
        {
        if ($this->internal_ok)
            {
            //OK
            $fp=fopen($this->path.$this->filename,"wb+");
            if ($fp!=FALSE)
                {
                fputs($fp,$data);
                fclose($fp);
                }
            }
        }

    //Daten in Datei schreiben
    function create()
        {
        $fp=fopen($this->path.$this->filename,"wb+");
        if ($fp!=FALSE)
            {
            fputs($fp,"");
            fclose($fp);
            }

        //Und refreshen
        $this->internal_ok=file_exists($this->path.$this->filename);
        $this->refresh();
        }


    //Zeiten holen
    function _getstatus()
        {
        $this->atime=fileatime($this->path.$this->filename);
        $this->ctime=filectime($this->path.$this->filename);
        $this->size =filesize ($this->path.$this->filename);
        }

    //Zugriffsrechte für eine Datei holen
    function _getpermission()
        {
        //Rechte holen
        $filename=$this->path.$this->filename;
        $this->permoct=string_right(strval(decoct(fileperms($filename))),3);

        //Zerlegen
        $this->owner=$this->_decode_perm($this->permoct,"owner");
        $this->group=$this->_decode_perm($this->permoct,"group");
        $this->all  =$this->_decode_perm($this->permoct,"all");

        //String setzen
        $this->permstr=$this->_permstostr($this->permoct);
        }

    //Die Zugriffsrechte Lesbar machen
    function _permstostr($perms)
        {
        //               0        1        2        3        4        5        6        7
        $masks = array( '[---]', '[--x]', '[-w-]', '[-wx]', '[r--]', '[r-x]', '[rw-]', '[rwx]' );
        $perm  = array( "/0/","/1/","/2/","/3/","/4/","/5/","/6/","/7/");
        return(preg_replace($perm,$masks,$perms));
        }

    //Zugriff auf die einzelnen Stellen der Rechte bieten
    function _decode_perm($octal,$index)
        {
        $result=new filepermission();

        //Octal => String => Array
        $perm=$this->_permstostr($octal);
        $perm=explode("[",$perm);

        switch ($index)
            {
            case ("owner")  : $data=$perm[1]; break;
            case ("group")  : $data=$perm[2]; break;
            default         : $data=$perm[3]; break;
            }

        //Zuordnen
        $result->read =strpos($data,"r")==0;
        $result->write=strpos($data,"w")==1;
        $result->exec =strpos($data,"x")==2;

        return($result);
        }

    //Zugriffsrechte setzen
    function setpermission($o_read,$o_write,$o_exec,$g_read,$g_write,$g_exec,$a_read,$a_write,$a_exec)
        {
        if ($this->internal_ok)
            {
            $perm=0000;
            if ($o_read) $perm+=400;
            if ($o_write)$perm+=200;
            if ($o_exec) $perm+=100;

            if ($g_read) $perm+=40;
            if ($g_write)$perm+=20;
            if ($g_exec) $perm+=10;

            if ($a_read) $perm+=4;
            if ($a_write)$perm+=2;
            if ($a_exec) $perm+=1;

            return(chmod($this->path."/".$this->filename,octdec($perm)));
            }
        else
            {
            return(FALSE);
            }
        }
    }

///////////////////////////////////////////////////////////////////////////////////////////////////
//Zugriff auf Verzeichnisse und Ihre Rechte
class driveapi
    {
    var $path       = "";
    var $base64path = "";
    var $basepath   = "";
    var $relpath    = "";

    var $dirs       = array();
    var $files      = array();
    
    //Nur Verzeichnisse unter diesem erlaubt

    var $diskfree   = 0;
    var $disksize   = 0;

    //Konstruktor
    function driveapi($workpath,$basepath="")
        {
        //Bißchen filtern
        $basepath=string_filter($basepath,FILTER_URL);
        $workpath=string_filter($workpath,FILTER_URL);
        
        //Basispfad setzen
        $basepath=string_add(realpath($basepath),"/");
        $basepath=string_replace($basepath,"\\","/");
        $this->basepath=$basepath;

        if (!is_dir($workpath))
            {
            $this->path=$basepath;
            }
        else
            {
            $this->path=$workpath;
            }

        //Immer mit einem Slash enden
        $this->path=string_add(realpath($this->path),"/");
        $this->path=string_replace($this->path,"\\","/");

        //Immer auf den Basispfad beschränken
        if (strpos($this->path,$this->basepath)!==0)
            {
            $this->path=$this->basepath;
            }

        //Die Pfadvarianten bestimmen
        $this->base64path=base64_encode($this->path);
        $this->relpath=substr($this->path,strlen($this->basepath),8192);
        
        //Inhalt einlesen
        $this->_scan();
        }

    //Destruktor
    function destroy()
        {
        }

    //Alle Daten neu einlesen
    function refresh()
        {
        $this->_scan();
        }

    //eine Datei einlesen
    function read($filename)
        {
        //Gibt es diesen Dateinamen überhaupt im aktuellen Verzeichnis ?
        $result=FALSE;
        if ($this->isvalidfile($filename))
            {
            //OK
            $myfile=new fileapi($this->path,$filename);
            $result=$myfile->read();
            $myfile->destroy();
            }
        return($result);
        }

    //eine Datei erzeugen
    function create($filename,$data)
        {
        $filename=string_filter($filename,FILTER_URL);
        
        $myfile=new fileapi($this->path,$filename);
        $result=$myfile->create();
        $result=$myfile->write($data);
        $myfile->destroy();
        }

    //eine Datei abspeichern
    function write($filename,$data)
        {
        //Gibt es diesen Dateinamen überhaupt im aktuellen Verzeichnis ?
        if ($this->isvalidfile($filename))
            {
            $myfile=new fileapi($this->path,$filename);
            $result=$myfile->write($data);
            $myfile->destroy();
            }
        else
            {
            return(FALSE);
            }
        }

    //Datei umbenennen
    function rename($oldfile,$newfile)
        {
        if ($this->isvalidfile($oldfile))
            {
            rename($this->path.$oldfile,$this->path.$newfile);
            }
        }

    //Datei löschen
    function remove($filename)
        {
        if ($this->isvalidfile($filename))
            {
            unlink($this->path.$filename);
            }
        }

    //Ein Verzeichnis erstellen
    function adddir($dirname)
        {
        if (!$this->isvaliddir($dirname))
            {
            //Keine Hidden erzeugen
            if (substr($dirname,0,1)!=".")
                {
                mkdir($this->path.$dirname);
                }
            }
        }
        
    //Ein Verzeichnis löschen (MUß leer sein)
    function deldir($dirname)
        {
        if ($this->isvaliddir($dirname))
            {
            //Keine Hidden erzeugen
            if (substr($dirname,0,1)!=".")
                {
                rmdir($this->path.$dirname);
                }
            }
        }

    //Directory enumerator
    function _scan()
        {
        $this->dirs=array();
        $this->files=array();

        if (is_dir($this->path))
            {
            $mydir=dir($this->path);

            while (($entry = $mydir->read())!==FALSE)
                {
                if (is_dir($this->path.$entry))
                    {
                    //Index ist der Name und der Wert die Größe
                    $this->dirs[$entry]=0;
                    }
                else
                    {
                    $this->files[$entry]=new fileapi($this->path,$entry);
                    }
                }
            ksort($this->files);
            ksort($this->dirs);

            $mydir->close();
            }

        //Und noch ein paar Statusinformationen lesen
        @$this->diskfree=disk_free_space($this->path);
        @$this->disksize=disk_total_space($this->path);
        }

    //Filenamecheck
    function isvalidfile($filename)
        {
        //Gibt es diesen Dateinamen überhaupt im aktuellen Verzeichnis ?
        return(isset($this->files[$filename]));
        }
        
    //Verzeichnischeck
    function isvaliddir($dirname)
        {
        //Gibt es dieses Verzeichnis überhaupt im aktuellen Verzeichnis ?
        return(isset($this->dirs[$dirname]));
        }
    }
</script>