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
require_once("config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classload.php");
$classloader=new classload(PATH_REGISTRY);
$classloader->load();
////////////////////////////////////////////////////////////////////////////////////////////////////

//Nun einfach die Funktion auswählen
if ( classcall("request","getrequest","rpc",FALSE,FILTER_SECURE) != FALSE)
    {
    //Ein Remotecall ?
    include(PATH_BASE."rpc.php");
    }
else
    {
    //Ein Download ?
    if ( classcall("request","getrequest","file",FALSE,FILTER_SECURE) != FALSE)
        {
        include(PATH_BASE."push.php");
        }
    else
        {
        //Alles andere
        include(PATH_BASE."index.php");
        }
        

    }
//Alle Klassen entladen und Inhalte flushen
$classloader->destroy();
</script>