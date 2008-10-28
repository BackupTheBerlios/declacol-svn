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
/// Liefert Dateien auf Anfrage aus
///
////////////////////////////////////////////////////////////////////////////////////////////////////
//Dateiname holen
require_once(PATH_LIBS."lib.mime.php");

$pushfile=classcall("request","getrequest","file",FALSE,FILTER_URL);

//Angemeldet ?
if (1 != 1)
    {
    $pushfile=PATH_FILES.basename($pushfile);
    }
else
    {
    $pushfile=PATH_EXTERN.basename($pushfile);
    }

//Datei OK?
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
    echo "file not found";
    }
</script>