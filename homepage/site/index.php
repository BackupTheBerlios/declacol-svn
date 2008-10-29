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
//Wir sind da cookie setzen
callmethod("request","setcookie","answer","42",0,"/");

//Templateengine einh�ngen
$id=callmethod("request","getid");
$pagefile =strtolower(callmethod("request","getrequest","page","news",FILTER_ALPHANUM));
$template ="main.txt";

//Die Cacheengine einh�ngen
setproperty("unimatrix","cacheengine" ,$CLASSES["cache"]);
setproperty("unimatrix","cachetimeout",300);

//Seite nur erzeugen, wenn sie nicht gepuffert ist
if (callmethod("unimatrix","iscached",$id) == FALSE)
    {
    callmethod("unimatrix","assign","pagetitle",$pagefile);
    callmethod("unimatrix","assign","sitename","Guru-Meditation");
    callmethod("unimatrix","assign","pagefile",$pagefile.".txt");
    callmethod("unimatrix","assign","version","1.0");
    callmethod("unimatrix","assign","user","Sven Lorenz");
    callmethod("unimatrix","assign","login",FALSE);
    }
    
//Here we go
echo callmethod("unimatrix","render",$id,$template);
callmethod("cache","clear");
</script>