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
///
/// Liefert Dateien au Anfrage aus
///
////////////////////////////////////////////////////////////////////////////////////////////////////
//Ab hier gehts los
require_once("config.php");
require_once(PATH_CLASSES."class.registry.php");
require_once(PATH_CLASSES."class.classload.php");
$loader=new classload(PATH_REGISTRY);
$loader->load();

//Datei OK holen
$pushfile=classcall("request","getrequest","file",FALSE,FILTER_URL);

if ($pushfile != FALSE)
  {
  require_once(PATH_LIBS."lib.web.php");
  
  web_push_file($pushfile);
  }

echo $pushfile;

$loader->destroy();
</script>