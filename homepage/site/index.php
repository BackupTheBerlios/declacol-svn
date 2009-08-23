<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
if (DEBUG) callmethod("debug","addlog","page","call index");
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Hauptseite
////////////////////////////////////////////////////////////////////////////////////////////////////
//Session starten
callmethod("session","start",callmethod("request","getcookie","session",ID_NONE));

//"Wir-Sind-Da-Cookies" setzen
callmethod("request","setcookie","answer" ,EVERYTHING);
callmethod("request","setcookie","session",getproperty("session","id",ID_NONE));

//ID aus allen Requestparametern ($_POST und $_GET) erzeugen um eine CacheID zu bekommen
//damit können auch Suchabfragen gecached werden. 
$id=getproperty("session","user",0)->id.callmethod("request","getid");

//Angefragte Seite ziehen
$pagefile =strtolower(callmethod("request","getrequest","page","news",FILTER_ALPHANUM));
$template ="main.txt";
       
//Die Cacheengine einhängen
setproperty("unimatrix","cacheengine" ,callmethod("cache","getthis"));
setproperty("unimatrix","cachetimeout",CACHE_TIMEOUT);

//Seite nur erzeugen, wenn sie nicht gepuffert ist
if (callmethod("unimatrix","iscached",$id) == FALSE)
    {
    callmethod("unimatrix","assign","pagetitle",getproperty("session","id",ID_NONE));
    callmethod("unimatrix","assign","sitename","Guru-Meditation");
    callmethod("unimatrix","assign","pagefile",$pagefile.".txt");
    callmethod("unimatrix","assign","pagelink",callmethod("request","getlink"));
    callmethod("unimatrix","assign","version","1.0");
    
    //Alle Session- und Uservariablen
    callmethod("unimatrix","assign","sessionid",getproperty("session","id",ID_NONE));
    callmethod("unimatrix","assign","user_name"     ,getproperty("session","user",0)->realname);
    callmethod("unimatrix","assign","user_email"    ,getproperty("session","user",0)->email);
    callmethod("unimatrix","assign","user_active"   ,getproperty("session","user",0)->active);
    callmethod("unimatrix","assign","user_login"    ,getproperty("session","user",0)->login);
    }

//Here we go
echo callmethod("unimatrix","render",$id,$template);
</script>