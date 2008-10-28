<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
if (DEBUG) callmethod("debug","addlog","index","call");
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Hauptseite
////////////////////////////////////////////////////////////////////////////////////////////////////
//Wir sind da cookie setzen
callmethod("request","setcookie","answer","42",0,"/");

//Templateengine einhängen
$pagefile =strtolower(callmethod("request","getrequest","page","news",FILTER_ALPHANUM));
$template ="main.txt";
$id=$template.$pagefile;

//Die Cacheengine einhängen
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
    callmethod("unimatrix","assign","login",TRUE);
    callmethod("unimatrix","assign","menu",array("?page=news"       => "NEWS",
                                                "?page=impressum"  => "IMPRESSUM",
                                                "?page=links"      => "LINKS",
                                                "?page=faq"        => "FAQ",
                                          ));
    }
    
//Here we go
echo callmethod("unimatrix","render",$id,$template);
</script>