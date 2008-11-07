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
//"Wir-Sind-Da-Cookie" setzen
callmethod("request","setcookie","answer",EVERYTHING);

//ID aus allen Requestparametern ($_POST und $_GET) erzeugen
$id=callmethod("request","getid");

//Angefragte Seite ziehen
$pagefile =strtolower(callmethod("request","getrequest","page","news",FILTER_ALPHANUM));
$template ="main.txt";

//Die Cacheengine einhängen
setproperty("unimatrix","cacheengine" ,callmethod("cache","getthis"));
setproperty("unimatrix","cachetimeout",300);

//Seite nur erzeugen, wenn sie nicht gepuffert ist
if (callmethod("unimatrix","iscached",$id) == FALSE)
    {
    callmethod("unimatrix","assign","pagetitle",$pagefile);
    callmethod("unimatrix","assign","sitename","Guru-Meditation");
    callmethod("unimatrix","assign","pagefile",$pagefile.".txt");
    callmethod("unimatrix","assign","pagelink",callmethod("request","getlink"));
    callmethod("unimatrix","assign","version","1.0");
    callmethod("unimatrix","assign","user","Sven Lorenz");
    callmethod("unimatrix","assign","login",FALSE);
    }
//Here we go
echo callmethod("unimatrix","render",$id,$template);
callmethod("cache","clear");
</script>