<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
if (DEBUG) callmethod("debug","addlog","page","call push");
////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Liefert Dateien auf Anfrage aus
///
////////////////////////////////////////////////////////////////////////////////////////////////////
//Dateiname holen
require_once(PATH_LIBS."lib.mime.php");

//User angemeldet ?
$answer   = callmethod("request","getcookie","answer","16");

//Datei holen (Pfade sind im Pushmodus nicht zugelassen)
$filename = basename( callmethod("request","getrequest","data",FALSE,FILTER_URL));
$pushfile = PATH_FILES.$filename;
if (DEBUG) callmethod("debug","addlog","push","request ".$pushfile);

//Originaldatei nicht gefunden oder nicht angemeldet ?
if ( ( file_exists($pushfile) == FALSE) || ($answer != EVERYTHING) )
    {
    //Dann nur externe Dateien zulassen
    $pushfile=PATH_EXTERN.$filename;
    if (DEBUG) callmethod("debug","addlog","push","redirect to EXTERN");
    }

//Datei OK?
if (DEBUG) callmethod("debug","addlog","push","request ".$pushfile);
if (file_exists($pushfile)==TRUE)
    {

    //Um auch Downloads mit dem Pushscript zu ermöglichen,
    //trenne wir in push und write auf
    if ( strpos(mime_get_type($pushfile),"application/")!==FALSE )
        {
        //Streamtypes bieten wir zum download an
        web_push_file(basename($pushfile));
        }
    else
        {
        //Andere schreiben wir in den Buffer
        echo file_get_contents($pushfile);
        }
    }
else
    {
    header("HTTP/1.0 404 Not Found");
    echo "<h1>404</h1><hr>";
    echo "<h2>file not found [".$filename."]</h2>";
    }
</script>