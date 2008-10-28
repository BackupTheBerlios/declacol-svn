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

//Templateengine einhängen
$pagefile =strtolower(classcall("request","getrequest","page","news",FILTER_ALPHANUM));
$template ="main.txt";
$id=$template.$pagefile;

setproperty("unimatrix","cacheengine" ,$CLASSES["cache"]);
setproperty("unimatrix","cachetimeout",300);
//Seite nur erzeugen, wenn sie nicht gepuffert ist
if (classcall("unimatrix","iscached",$id) == FALSE)
    {
    classcall("unimatrix","assign","pagetitle",$pagefile);
    classcall("unimatrix","assign","sitename","Guru-Meditation");
    classcall("unimatrix","assign","pagefile",$pagefile.".txt");
    classcall("unimatrix","assign","version","1.0");
    classcall("unimatrix","assign","user","Sven Lorenz");
    classcall("unimatrix","assign","login",TRUE);
    classcall("unimatrix","assign","menu",array("?page=news"       => "NEWS",
                                                "?page=impressum"  => "IMPRESSUM",
                                                "?page=links"      => "LINKS",
                                                "?page=faq"        => "FAQ",
                                          ));
    }
    
//Here we go
echo classcall("unimatrix","render",$id,$template);
</script>