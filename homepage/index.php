<script language="php">
echo "<pre>";

//define ("DEBUG",TRUE);

//Ab hier gehts los
require_once("config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classload.php");

//Der Classloader öffnet automatisch alle notwendigen Klassen und initialisiert sie
$loader=new classload(PATH_REGISTRY);
$loader->load();



//Templateengine einhängen
setproperty("unimatrix","cacheengine" ,$CLASSES["cache"]);
setproperty("unimatrix","cachetimeout",10);
//Seite nur erzeugen, wenn sie nicht gepuffert ist
if (classcall("unimatrix","iscached","main.txt") == FALSE)
    {
    classcall("unimatrix","assign","title","Startseite");
    classcall("unimatrix","assign","sitename","Guru-Meditation");
    classcall("unimatrix","assign","version","1.0");
    classcall("unimatrix","assign","user","Sven Lorenz");
    classcall("unimatrix","assign","login",TRUE);
    classcall("unimatrix","assign","menu",array( "http://www.google.de"=>"google",
                                                "http://www.sven-of-nine.de"=>"Sven Of Nine"
                                          ));
    }
//Here we go
//classcall("unimatrix","render","main.txt");
print_r($CLASSES["request"]);

echo classcall("request","getrequest","admin",FALSE,FILTER_ALPHA);

//Alle Klassen entladen und Inhalte flushen
$loader->destroy();


</script>