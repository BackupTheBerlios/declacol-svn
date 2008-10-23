<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
//Alle Pfade
define ("PATH_SALT"     ,"1432"); //Pfad Obfuscator wird einmal bei der Installation gesetzt
define ("PATH_BASE"     ,str_replace("\\","/",realpath("./"))."site".PATH_SALT."/");
define ("PATH_CLASSES"  ,PATH_BASE."classes/");
define ("PATH_LIBS"     ,PATH_BASE."libs/");
define ("PATH_REGISTRY" ,PATH_BASE."registry/");
define ("PATH_EXTERN"   ,PATH_BASE."extern/");

define ("PATH_DATA"     ,PATH_BASE."files/");
define ("PATH_IMAGES"   ,PATH_DATA."images/");
define ("PATH_FILES"    ,PATH_DATA."files/");
define ("PATH_TEMPLATES",PATH_DATA."templates/");
define ("PATH_TEMP"     ,PATH_DATA."temp/");
define ("PATH_CACHE"    ,PATH_DATA."cache/");

//Alles mit EMail
define ("EMAIL_SMTP"    ,"mail.test.com");
define ("EMAIL_POP3"    ,"mail.test.com");
define ("EMAIL_USER"    ,"user");
define ("EMAIL_PASS"    ,"pass");
define ("EMAIL_FROM"    ,"check@test.com");
define ("EMAIL_AUTH"    ,TRUE);
define ("EMAIL_SUPPORT" ,TRUE);
define ("EMAIL_MODE"    ,"smtp");

//Allgemeine Definitionen
define ("CURRENT_TIME"  ,time());
define ("ID_NONE"       ,0);
define ("SALT"          ,"876");

if (is_dir(PATH_BASE) != TRUE)
  {
  die("unable to access basepath check your config");
  }

</script>