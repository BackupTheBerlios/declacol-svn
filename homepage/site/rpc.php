<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
if (DEBUG) callmethod("debug","addlog","page","rpccall rpc");
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Interface f�r RPC-Calls
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once(PATH_CLASSES."class.rpc.php");

//Daten von der Requestklasse holen
$xml=callmethod("request","getrequest","data","",FILTER_SECURE);

//Verarbeiten
$rpc = new rpc();
if ($rpc->process($xml)!=TRUE)
  {
  //Fehler dumpen
  echo "<pre>";
  print_r(xmltoarray($rpc->result));  
  echo "</pre>";
  }
else
  { 
  //Ausgeben
  echo $rpc->result;
  }

//Alle Klassen entladen und Inhalte flushen
$rpc->destroy();
</script>