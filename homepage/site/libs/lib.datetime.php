<script language="php">
/*
 _|    _|            _|                              _|                _|
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|

(c) 2008 Borg@sven-of-nine.de
*/
///////////////////////////////////////////////////////////////////////////
///
/// Funktionsbibliothek für Datums und Zeitfunktionen
///
///////////////////////////////////////////////////////////////////////////
//Ein paar schöne defines
define ("SECS_PER_MINUTE"      ,60);
define ("SECS_PER_HOUR"        ,3600);
define ("SECS_PER_DAY"         ,86400);
define ("DAYS_PER_WEEK"        ,7);
define ("SECS_PER_WEEK"        ,604800);
define ("SECS_PER_MONTH"       ,2592000);
define ("SECS_PER_YEAR"        ,31104000);

define ("CURRENT_DAY"          ,intval(date("d",time())));
define ("CURRENT_WEEK"         ,intval(date("W",time())));
define ("CURRENT_MONTH"        ,intval(date("m",time())));
define ("CURRENT_YEAR"         ,intval(date("Y",time())));
define ("CURRENT_DATE"         ,intval(time()));
define ("NOW"                  ,CURRENT_DATE));

//Aus einem Timestamp die KW machen
function timetokw($timestamp)
    {
    //Der Monat
    return(date("W",$timestamp));
    }

//Aus einer KW und dem Jahr einen Timestamp machen
function kwtotime($kw,$jahr)
    {
    //Jahr anpassen
    if (intval($jahr) < 50)  $jahr+=2000;

    //Timestamp der Woche bestimmen
    $result =$kw * DAYS_PER_WEEK * SECS_PER_DAY;
    $result+= mktime(0,0,0,1,1,$jahr);

    //Und auf den Anfang der Woche runtertrimmen
    $result=$result - date("w",$result) * SECS_PER_DAY;

    return($result);
    }

//Timestamp mit Microsekunden erzeugen
function usecs()
    {
    //Thx to Marius
    list($usec, $sec) = explode(' ',microtime());
    return ((float)$usec + (float)$sec);
    }

//Gehört zwar nicht ganz hier rein, aber was solls
function extend_runtime($secs)
    {
    if (!SAFE_MODE)
        {
        set_time_limit($secs);
        }
    }
    
</script>