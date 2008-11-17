<script language="php">
define("SETUP",TRUE);
//Nur diese Basisconfigurationen ziehen,
//der Rest liegt in den Reg-Files
require_once("./config/config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classinst.php");
require_once(PATH_CLASSES."class.classload.php");
require_once(PATH_CLASSES."class.configurator.php");

//Alle Klassen initialisieren
$inst=new classinst(PATH_REGISTRY);
$inst->registerall(PATH_CLASSES);
$inst->destroy();

//Und direkt laden
$loader=new classload(PATH_REGISTRY);
$loader->load();

//Setupschritt holen
$step = callmethod("request","getrequest","page",1,FILTER_NUMBER);;




//Die Installationsschritte auswählen
switch ($step)
    {
    case 2  : getmaildata($step);   break;
    case 3  : getrootdata($step);   break;
    case 4  : transferfiles($step); break;
    case 5  : done();               break;
    default : welcome(1);           break;
    }

//Einfach die Templateengine nutzen
callmethod("unimatrix","assign","step"    ,intval($step));
callmethod("unimatrix","assign","nextstep",intval($step+1));
echo callmethod("unimatrix","render","egal","setup_main.txt");

$loader->destroy();

////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Ab hier nur noch Funktionen
////////////////////////////////////////////////////////////////////////////////////////////////////////
//Wilkommen
function welcome($step)
    {
    //Evtl. Schreibrechte anfordern
    $access=is_writable("setup.php") && is_writable("./config/local.config.php");
    callmethod("unimatrix","assign","access"  ,$access);
    callmethod("unimatrix","assign","noaccess",!$access);
    callmethod("unimatrix","assign","setup" ,!is_writable("setup.php"));
    callmethod("unimatrix","assign","config",!is_writable("./config/local.config.php"));

    //Pluginseite einhängen
    callmethod("unimatrix","assign","pagefile","setup_welcome.txt");
    }
    
////////////////////////////////////////////////////////////////////////////////////////////////////////
function done()
    {
    //Pluginseite einhängen
    callmethod("unimatrix","assign","pagefile","setup_done.txt");
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////
//Aller Mailerdaten abfragen
function getmaildata($step)
    {
    //Pluginseite einhängen
    callmethod("unimatrix","assign","pagefile","setup_mailer.txt");
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////
//Den Root-User anlegen
function getrootdata($step)
    {
    //Pluginseite einhängen
    callmethod("unimatrix","assign","pagefile","setup_rootuser.txt");
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////
//Die angepasste Installation in das Zielverzeichnis übertragen
function transferfiles($step)
    {
    //Pluginseite einhängen
    callmethod("unimatrix","assign","pagefile","setup_transfer.txt");
    }

</script>