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
/// Interface für RPC-Calls
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once(PATH_CLASSES."class.rpc.php");

//Daten von der Requestklasse holen
$xml=classcall("request","getrequest","rpc","",FILTER_SECURE);

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