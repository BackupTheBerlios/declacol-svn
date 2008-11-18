<script language="php">
define("SETUP",TRUE);
//Nur diese Basisconfigurationen ziehen,
//der Rest liegt in den Reg-Files
require_once("./setup/class.unimatrix.php");
require_once("./setup/class.configurator.php");

$render=new unimatrix();
$render->basepath="./setup/templates/";

if (checkdata()!=TRUE)
    {
    welcome();
    }
else
    {
    transferfiles();
    }


$render->destroy();

////////////////////////////////////////////////////////////////////////////////////////////////////////
/// Ab hier nur noch Funktionen
////////////////////////////////////////////////////////////////////////////////////////////////////////
//Wilkommen
function welcome()
    {
    global $render;
    
    //Evtl. Schreibrechte anfordern
    $access=is_writable("setup.php") && is_writable("./config/local.config.php");
    $render->assign("access"  ,$access);
    $render->assign("noaccess",!$access);

    //Dateirechte
    $render->assign("setup" ,!is_writable("setup.php"));
    $render->assign("config",!is_writable("./config/local.config.php"));

    //Ausgabe
    $render->assign("pagefile","setup_welcome.txt");
    echo $render->render("egal","setup_main.txt");
    }
    
////////////////////////////////////////////////////////////////////////////////////////////////////////
//Die angepasste Installation in das Zielverzeichnis übertragen
function transferfiles()
    {
    global $render;

    //Ausgabe
    $render->assign("pagefile","setup_working.txt");
    echo $render->render("egal","setup_main.txt");
    }
    
////////////////////////////////////////////////////////////////////////////////////////////////////////
//Fertig
function done()
    {
    global $render;

    //Ausgabe
    $render->assign("pagefile","setup_working.txt");
    echo $render->render("egal","setup_main.txt");
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////
function checkdata()
    {
    $result=FALSE;
    if (isset($_POST["data0"])==TRUE)
        {
        if ( $_POST["data0"]!="" )
            {
            $result=TRUE;
            }
        }
    return($result);
    }
</script>