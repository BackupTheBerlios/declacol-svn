<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
define ("DEBUGMODE" , FALSE);

//Konstanten lesen
require_once("constants.php");

//Lokale Konfiguration lesen
require_once("local.config.php");

if (defined("SETUP_DONE")==FALSE)
  {
  die("run <a href=\"setup.php\">setup</a> to use this software");
  }

//Alle Pfade
@define ("PATH_BASE"     ,str_replace("\\","/",realpath("./"))."/site/");
@define ("PATH_CLASSES"  ,PATH_BASE."classes/");
@define ("PATH_LIBS"     ,PATH_BASE."libs/");
@define ("PATH_REGISTRY" ,PATH_BASE."registry/");

@define ("PATH_DATA"     ,PATH_BASE."files/");
@define ("PATH_IMAGES"   ,PATH_DATA."images/");
@define ("PATH_FILES"    ,PATH_DATA."files/");
@define ("PATH_TEMPLATES",PATH_DATA."templates/");
@define ("PATH_TEMP"     ,PATH_DATA."temp/");
@define ("PATH_CACHE"    ,PATH_DATA."cache/");
@define ("PATH_EXTERN"   ,PATH_DATA."extern/");

//Allgemeine Definitionen
@define ("CURRENT_TIME"  ,time());
@define ("DEFAULTTIMEOUT",3600);
@define ("SESSION_TIMEOUT" ,60 * 15);
@define ("CACHE_TIMEOUT"   ,60 * 5);

//Timezone setzen, um CORE-Errors zu vermeiden
date_default_timezone_set(TIMEZONE);

define("DEBUG",DEBUGMODE);
</script>