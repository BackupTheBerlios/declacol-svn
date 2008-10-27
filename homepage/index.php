<script language="php">
//Ab hier gehts los
require_once("config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classload.php");

//Der Classloader öffnet automatisch alle notwendigen Klassen und initialisiert sie
$loader=new classload(PATH_REGISTRY);
$loader->load();

//Templateengine einhängen
$pagefile =strtolower(classcall("request","getrequest","page","news",FILTER_ALPHANUM));
$template ="main.txt";
$id=$template.$pagefile;

setproperty("unimatrix","cacheengine" ,$CLASSES["cache"]);
setproperty("unimatrix","cachetimeout",300);
//Seite nur erzeugen, wenn sie nicht gepuffert ist
if (classcall("unimatrix","iscached",$id) == FALSE)
    {
    classcall("unimatrix","assign","pagetitle",$pagefile);
    classcall("unimatrix","assign","sitename","Guru-Meditation");
    classcall("unimatrix","assign","pagefile",$pagefile.".txt");
    classcall("unimatrix","assign","version","1.0");
    classcall("unimatrix","assign","user","Sven Lorenz");
    classcall("unimatrix","assign","login",TRUE);
    classcall("unimatrix","assign","menu",array("?page=news"       => "NEWS",
                                                "?page=impressum"  => "IMPRESSUM",
                                                "?page=links"      => "LINKS",
                                                "?page=faq"        => "FAQ",
                                          ));
    }

//Here we go
//echo classcall("unimatrix","render",$id,$template);

$CLASSES["rssfeed"]->addchannel("hauptprojekt","Hauptprojekt","http://bopfen.com","Alles was im Hauptprojekt liegt");
$CLASSES["rssfeed"]->addchannel("verwaltung",  "Projektverwaltung","http://bopfen.com","Alles was im Hauptprojekt liegt");


$CLASSES["rssfeed"]->additem("hauptprojekt","mytitle","http://www.google.de","Ich","Description");
$CLASSES["rssfeed"]->additem("hauptprojekt","mytitle","http://www.google.de","Ich","Description");
$CLASSES["rssfeed"]->additem("hauptprojekt","mytitle","http://www.google.de","Ich","Description");
$CLASSES["rssfeed"]->additem("hauptprojekt","mytitle","http://www.google.de","Ich","Description");
$CLASSES["rssfeed"]->additem("verwaltung","mytitle","http://www.google.de","Ich","Description");

//print_r($CLASSES["rssfeed"]);
$CLASSES["rssfeed"]->write("./dogosch.rss");


//Alle Klassen entladen und Inhalte flushen
$loader->destroy();
</script>