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

////////////////////////////////////////////////////////////////////////////////////////////////////
//"Wir-Sind-Da-Cookies" setzen
callmethod("request","setcookie","answer" ,EVERYTHING);
callmethod("request","setcookie","session",getproperty("session","id",ID_NONE));

////////////////////////////////////////////////////////////////////////////////////////////////////
//Wenn der User etwas eingegeben hat daten an alle schicken
$newline=callmethod("request","getrequest","data","",FILTER_NONE,TRUE);
if ($newline !="")
  {
  callmethod("message","broadcast","chatline",getproperty("session","user",ID_NONE)->username." says ".$newline);
  }

////////////////////////////////////////////////////////////////////////////////////////////////////
//Jeden neuen Text holen und in den Sessiondaten speichern
//Die Nachrichten sind automatisch nach ihrem timestamp sortiert
$text=callmethod("session","load","chattext",array());
do
  {
  $newline=callmethod("message","receive","chatline",TRUE);
  if ($newline !== FALSE)
    {
    $text[]=$newline;
    }
  }
while ($newline !== FALSE);

////////////////////////////////////////////////////////////////////////////////////////////////////
//Textarray begrenzen und abspeichern
while (count ($text) > 30)
  {
  array_shift($text);
  }
callmethod("session","save","chattext",$text);

////////////////////////////////////////////////////////////////////////////////////////////////////
//Den Text einfach ausgeben
foreach ($text as $line)
  {
  echo $line."<br/>";
  }

</script>
<hr/>
<form action="/" method="post">
  <p>Say : <input name="data" type="text" size="80" maxlength="80"></p>
</form>
