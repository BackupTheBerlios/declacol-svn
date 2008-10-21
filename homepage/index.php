<pre>
<script language="php">
require_once("config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classload.php");

//Der Classloader öffnet automatisch alle notwendigen Klassen und initialisiert sie
$loader=new classload(PATH_REGISTRY);
$loader->load();

for ($index = 0; $index < 100; $index++)
    {
    //$CLASSES["user"]->add("Sven Lorenz","borg@Sven-of-Nine.de","svenofnine".$index.time(),"hybris10");
    }
    
$CLASSES["user"]->add("Sven Lorenz","borg@Sven-of-Nine.de","svenofnine".time(),"hybris10");


//Damit sind alle Klassen im globalen Array $CLASSES verfügbar
print_r($CLASSES["user"]);



//Alle Klassen entladen und Inhalte flushen
$loader->destroy();
</script>