<script language="php">
define("SETUP",TRUE);
//Nur diese Basisconfigurationen ziehen,
//der Rest liegt in den Reg-Files
require_once("./site/classes/class.unimatrix.php");
require_once("./site/classes/class.configurator.php");

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

    //Schon eingegebene Daten merken
    $render->assign("data0",getdata("data0",""));
    $render->assign("data1",getdata("data1",""));
    $render->assign("data2",getdata("data2",""));
    $render->assign("data3",getdata("data3",""));
    $render->assign("data4",getdata("data4",""));
    $render->assign("data5",getdata("data5",""));
    $render->assign("data6",getdata("data6",""));
    $render->assign("data7",getdata("data7",""));

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
    $render->assign("access"  ,TRUE);
    $render->assign("noaccess",FALSE);

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
    
    $usemail    =getdata("data0","off")=="off";
    $mailserver =getdata("data1","");
    $mailuser   =getdata("data2","");
    $mailpass   =getdata("data3","");
    $mailfrom   =getdata("data4","");
    
    $rootuser   =getdata("data5","");
    $rootpass   =getdata("data6","");
    $rootmail   =getdata("data7","");

    //Mail Eintrag OK
    $result =(!$usemail || (($mailserver!="") && ($mailuser!="") && ($mailpass!="")  && ($mailfrom!="")))&&
             (($rootuser!="") && ($rootpass!="") && ($rootmail!=""));
             
    return($result);
    }
    
////////////////////////////////////////////////////////////////////////////////////////////////////////
//Daten lesen
function getdata($name,$default)
    {
    if (isset($_POST[$name])==TRUE)
        {
        $result=$_POST[$name];
        }
    else
        {
        $result=$default;
        }

    return($result);
    }
</script>