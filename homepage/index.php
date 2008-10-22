<script language="php">
//define ("DEBUG",TRUE);

//Ab hier gehts los
require_once("config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classload.php");

//Der Classloader öffnet automatisch alle notwendigen Klassen und initialisiert sie
$loader=new classload(PATH_REGISTRY);
$loader->load();

//echo "<pre>";

//Templateengine einhängen
$CLASSES["unimatrix"]->cacheengine=$CLASSES["cache"];
$CLASSES["unimatrix"]->assign("title","Startseite");
$CLASSES["unimatrix"]->assign("sitename","Guru-Meditation");


$CLASSES["unimatrix"]->assign("version","1.0");
$CLASSES["unimatrix"]->assign("user","Sven Lorenz");
$CLASSES["unimatrix"]->assign("login",TRUE);

$CLASSES["unimatrix"]->assign("menu",array( "http://www.google.de"=>"google",
                                            "http://www.sven-of-nine.de"=>"Sven Of Nine"
                                          ));

$CLASSES["unimatrix"]->render("main.txt");


//print_r($CLASSES["unimatrix"]);

//Alle Klassen entladen und Inhalte flushen
$loader->destroy();


</script>