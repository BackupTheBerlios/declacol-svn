<script language="PHP">
if (defined("IN_FRAME")!=TRUE) die("Error");
if (LOGIN==FALSE) die("Error");
////////////////////////////////////////////////////////////////////////////////
/// Erlaubt das einfache Updaten der Oberfläche mit einem Phararchive
////////////////////////////////////////////////////////////////////////////////
require_once("scripts/class.installer.php");

define ("UPDATEPACK"   ,TEMPBASE."updatepack.phar");
define ("VERSIONFILE"  ,"constants.php");

echo "Current Version : ".VERSION;

//Ist eine Installation angefragt ?
$install=getparam("install","no",FALSE);

//Installer initialisieren
$installer=new installer();
$installer->overwrite=TRUE;

//alte Updatepakete löschen
if ($install=="clean")
    {
    cleartemp();
    }


//alte Updatepakete löschen
if ( (file_exists(UPDATEPACK)==TRUE) &&
     ($install=="no"))
    {
    @unlink(UPDATEPACK);
    }
    
//Updatepack installieren ?
if ( (file_exists(UPDATEPACK)==TRUE) &&
     ($install=="yes"))
    {
    doinstall($installer,UPDATEPACK);
    }
    
//Updatepack erstellen ?
if ($install=="package")
    {
    echo "<hr/>";
    //Tempverzeichnis leeren
    cleartemp();

    //Und Paket erzeugen
    $archive="easygoing_v".VERSION."-".date("d.m.y",time()).".phar";

    //Packen
    echo "creating package";
    $installer->create("./","./temp/".$archive);
    
    //Testen
    echo "<br/>checking package";
    $installer->test("./temp/".$archive);
    if ($installer->lasterror==ERROR_NONE)
        {
        echo "<br/>download <a href=\"./temp/".$archive."\">".$archive."</a>";
        }
    else
        {
        cleartemp();
        echo "<br/>failed";
        }
    echo "<br/><br/><a href=\"".CURRENT_LINK."&amp;install=clean\">cleanup</a>";
    }
else
    {
    echo "<br/><a href=\"".CURRENT_LINK."&amp;install=package\">create package of current version</a>";
    }


if (isset($_FILES["updatefile"])==TRUE)
    {
    $updatefile=$_FILES["updatefile"]["tmp_name"];
    if (is_uploaded_file($updatefile))
        {
        echo "<hr/>";
        //Den Upload ins Tempverzeichnis schieben
        move_uploaded_file($updatefile,UPDATEPACK);
        
        //Versionsdatei entpacken
        $installer->unpack(UPDATEPACK,TEMPBASE,VERSIONFILE);

        if ($installer->lasterror==ERROR_NONE)
            {
            //Version auslesen
            $version=extractversion(TEMPBASE.VERSIONFILE);
            echo "archive version :".$version."<br/>\n";
            echo "archive ok<br/>\n";

            echo "install ? ";
            echo "<a href=\"".CURRENT_LINK."&amp;install=yes\">yes</a> / ";
            echo "<a href=\"".CURRENT_LINK."&amp;install=no\">no</a>";
            
            //Versionsdatei brauchen wir nicht mehr
            @unlink(TEMPBASE.VERSIONFILE);
            }
        else
            {
            echo "invalid archive<br/>\n";
            @unlink(UPDATEPACK);
            print_r($installer->log);
            }
        }
    }
$installer->destroy();


//Die Versionsdefinition aus den Konstanten ziehen
function extractversion($file)
    {
    $result="?";

    $data=file($file);
    $pregresult=array();
    foreach ($data as $line)
        {
        if (preg_match("#\"VERSION\"[\s]*,[\s]*\"[0-9\.]+\"[\s]*#",$line,$pregresult)>0)
            {
            //Einzelteile holen
            $pregresult=explode(",",reset($pregresult));
            $pregresult=end($pregresult);
            $result=str_replace("\"","",$pregresult);
            }
        }
    return($result);
    }

//Das Archiv installieren
function doinstall($engine,$archive)
    {
    echo "<hr/>installing ".$archive."<hr>";
    
    //Archiv zur Sichreheit nochmal testen
    $engine->test($archive);
    if ($engine->lasterror==ERROR_NONE)
        {
        //Und ins lokale Verzeichnis updaten
        $engine->logout   = TRUE;
        $engine->overwrite= TRUE;
        
        //REM Konfiguration und Defaultseite retten retten
        copy(CONFIGFILE ,CONFIGFILE.".old");
        copy(DEFAULTPAGE,DEFAULTPAGE.".old");

        echo "<a href=\"".CURRENT_LINK."\">restart engine</a>";
        echo "<pre>";
        $engine->unpack($archive,"./");
        echo "</pre>";
        echo "<a href=\"".CURRENT_LINK."\">restart engine</a><br/><hr><br/>";

        //Und wieder einspielen
        copy(CONFIGFILE .".old",CONFIGFILE);
        copy(DEFAULTPAGE.".old",DEFAULTPAGE);

        if (file_exists(CONFIGFILE)==TRUE)
            {
            @unlink(CONFIGFILE .".old");
            @unlink(DEFAULTPAGE.".old");
            }
        }
    else
        {
        echo "update aborted ! Archive corrupt<br/>\n";
        }
    }



</script>
<hr>
<form action="<script language="PHP">echo CURRENT_LINK</script>" method="post" enctype="multipart/form-data" name="upload">
select update archive<br/>
<input type="file" size="60" name="updatefile"><br/>
<input type="hidden" name="fileupload" value="1"/>
<button name="upload" type="submit" value="upload" >upload</button>
</form>