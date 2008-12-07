<script language="php">
/*
 _|    _|            _|                              _|                _|
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|

(c) 2008 Borg@sven-of-nine.de
*/
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Hauptseite
////////////////////////////////////////////////////////////////////////////////////////////////////
if (filesize("./config/local.config.php") > 0)
  {
  die ("software already configured! empty local.config to restart setup");
  }

//Los gehts
define ("SETUP",TRUE);
require_once("./site/classes/class.unimatrix.php");
require_once("./site/classes/class.configurator.php");


$render=new unimatrix();
$render->basepath="./setup/templates/";

//Evtl. Schreibrechte anfordern
$access=is_writable("setup.php") && is_writable("./config/local.config.php");
$render->assign("access"  ,$access);
$render->assign("noaccess",!$access);

//Dateirechte
$render->assign("setup" ,!is_writable("setup.php"));
$render->assign("config",!is_writable("./config/local.config.php"));


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

    createconfig();
    registerclasses();
    createuser();
    stripcode();

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
    if (isset($_REQUEST[$name])==TRUE)
        {
        $result=$_REQUEST[$name];
        }
    else
        {
        $result=$default;
        }

    return($result);
    }
////////////////////////////////////////////////////////////////////////////////////////////////////////
//Salz erzeugen
function getsalt()
  {
  $input =rand(9999,9999999);

  //Und nun bilden wir daraus einen simplen String
  $result="";
  while ($input > 0)
    {
    //Nur Buchstaben aus dem ASCII-Satz nehmen
    $result.=chr( ($input % 26) + 97);
    $input=$input >> 2;
    }
  return($result);
  }

////////////////////////////////////////////////////////////////////////////////////////////////////////
//Die Konfigurationsdatei erzeugen
function createconfig()
  {
  //Die Config-Daten holen
  $usemail    =getdata("data0","off")=="off";
  $mailserver =getdata("data1","");
  $mailuser   =getdata("data2","");
  $mailpass   =getdata("data3","");
  $mailfrom   =getdata("data4","");

  //Die Config-Datei schreiben
  $config=new configurator();
  $config->open("./config/local.config.php");
  $config->add("SALT" ,getsalt(),"global crypt salt");
  $config->add("SALT1",getsalt(),"global crypt salt");
  $config->add("SALT2",getsalt(),"global crypt salt");
  $config->add("TIMEZONE","Europe/Berlin","timezone for datefunctions");
  $config->add("EMAIL_MODE","smtp","");
  $config->add("EMAIL_SMTP",$mailserver,"smtp server");
  $config->add("EMAIL_POP3",$mailserver,"pop3 server");
  $config->add("EMAIL_USER",$mailuser,"login for smtp-server");
  $config->add("EMAIL_PASS",$mailpass,"login for smtp-server");
  $config->add("EMAIL_FROM",$mailfrom,"email sender");
  $config->add("EMAIL_AUTH",($mailpass!=""),"login to mailserver");
  $config->add("EMAIL_SUPPORT",$usemail,"use mail");
  $config->add("SETUP_DONE",TRUE,"setup successfully completed");

  $config->flush();
  $config->close();
  $config->destroy();
  clearstatcache();
  }

function registerclasses()
  {
  //Config und notwendige Klassen ziehen
  require_once("./config/config.php");
  require_once(PATH_CLASSES."class.registry.php");
  require_once(PATH_CLASSES."class.classinst.php");

  //Alle Klassen initialisieren
  $inst=new classinst(PATH_REGISTRY);
  $inst->registerall(PATH_CLASSES);
  $inst->destroy();
  }

function createuser()
  {
  //Config und notwendige Klassen ziehen
  require_once("./config/config.php");
  require_once(PATH_CLASSES."class.registry.php");
  require_once(PATH_CLASSES."class.classload.php");

  $rootuser   =getdata("data5","");
  $rootpass   =getdata("data6","");
  $rootmail   =getdata("data7","");

  $loader=new classload(PATH_REGISTRY);
  $loader->load();

  callmethod("user","add","Administrator",$rootmail,$rootuser,$rootpass);

  $loader->destroy();
  }

function stripcode()
  {
  $stripcode  =getdata("data8","off")!="off";

  if ($stripcode==TRUE)
    {
    shrinkfiles("./site/");
    }
  }
////////////////////////////////////////////////////////////////////////////////////////////////////
//Alle PHP-Dateien strippen
function shrinkfiles($source)
    {
    $result=array();

    $dirs=scandir($source);

    foreach ($dirs as $entry)
        {
        //Traverse und Subversion ignorieren
        if ( ($entry != ".") && ($entry!="..") && ($entry!=".svn") )
            {
            if ( is_dir($source.$entry)==TRUE )
                {
                shrinkfiles($source.$entry."/");
                }
            else
                {
                if (substr($entry,-4,4)==".php")
                   {
//                   echo "stripping :".$source.$entry."<br>\n";
                   $filedata=file_get_contents($source.$entry);
                   file_put_contents($source.$entry,php_strip_whitespace($filedata));
                   }
                }
            }
        }
    }
</script>