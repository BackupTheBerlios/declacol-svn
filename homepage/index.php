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
//Dieser Kopf muß immer so sein
require_once("./config/config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classload.php");
$classloader=new classload(PATH_REGISTRY);
$classloader->load();
if (DEBUG) callmethod("debug","addlog","page","call dispatcher");
////////////////////////////////////////////////////////////////////////////////////////////////////
//Nun einfach die Funktion auswählen
$action = callmethod("request","getrequest","action",FALSE,FILTER_ALPHANUM);

switch ($action)
    {
    case ("file") : include(PATH_BASE."push.php");  break;
    case ("rpc")  : include(PATH_BASE."rpc.php");   break;
    case ("cron") : include(PATH_BASE."cron.php");  break;
    case ("srv")  : include(PATH_BASE."srv.php");   break;

    default       : include(PATH_BASE."index.php"); break;
    }

//Alle Klassen entladen und Inhalte flushen
$classloader->destroy();
</script>