<pre>
<script language="php">
//Nur diese Basisconfigurationen ziehen,
//der Rest liegt in den Reg-Files
require_once("config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classinst.php");
require_once(PATH_CLASSES."class.classload.php");

//Alle Klassen initialisieren
$inst=new classinst(PATH_REGISTRY);
$inst->registerall(PATH_CLASSES);


$inst->destroy();

$loader=new classload(PATH_REGISTRY);
$loader->load();

print_r($CLASSES);
$loader->destroy();
</script>