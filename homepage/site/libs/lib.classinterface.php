<script language="PHP">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2009 Borg@sven-of-nine.de
*/
//////////////////////////////////////////////////////////////////////////
///
/// Erlaubt den Zugriff auf durch ClassLoad geladene Methoden
/// und Eigenschaften
///
//////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////
//Prüfen, ob eine Klasse existiert
function classexists($classname)
    {
    global $CLASSES;

    return( isset($CLASSES[$classname]) );
    }

//////////////////////////////////////////////////////////////////////////
//Prüfen, ob eine Methode existiert
function rpcmethodexists($classname,$method)
    {
    global $CLASSES;
   
    if ( methodexists($classname,$method)==TRUE)
        {
        //Ist die Funktion überhaupt zulässig?
        $result=isset($CLASSES[$classname]->export[$method]);
        }
    else
        {
        $result=FALSE;
        }

    return($result);
    }

//////////////////////////////////////////////////////////////////////////
//Prüfen, ob eine Methode existiert
function methodexists($classname,$method)
    {
    global $CLASSES;

    if (isset($CLASSES[$classname])==TRUE)
      {
      $result=method_exists($CLASSES[$classname],$method);
      }
    else
      {
      $result=FALSE;
      }
    return($result);
    }

//////////////////////////////////////////////////////////////////////////
//Prüfen, ob eine Eigenschaft existiert
function propertyexists($classname,$property)
    {
    global $CLASSES;

    if (isset($CLASSES[$classname])==TRUE)
      {
      $result=property_exists($CLASSES[$classname],$property);
      }
    else
      {
      $result=FALSE;
      }
    return($result);
    }


//////////////////////////////////////////////////////////////////////////
//Eine Methode aufrufen, die per RPC angefordert wurde. Dazu wird überprüft, ob die angefragte Klasse
//überhaupt die methode exportiert
function callrpcmethod($classname,$method)
    {
    global $CLASSES;

    //Ist die Funktion überhaupt zulässig?
    if (isset($CLASSES[$classname]->export[$method])==TRUE)
        {
        $args=func_get_args();
        $result=callmethod($args);
        }
    else
        {
        //Bei Debug lassen wir Fehler weiterlaufen
        if (!DEBUG) trigger_error("method not exported".$classname.":".$method);
        $result=FALSE;
        }
    }

//////////////////////////////////////////////////////////////////////////
//Hier die Interfacefunktion, um auf die geladenen Klassen zugreifen zu können
function callmethod($classname,$method)
    {
    global $CLASSES;
    
    //Existiert die Methode
    if (method_exists($classname,$method)==TRUE)
       {
       //Alle Argumente holen
       $args=func_get_args();

       //Klasse und Methode rausnehmen
       array_shift($args);
       array_shift($args);

       //Und auf die Methode zugreifen
       $result=call_user_func_array(array($CLASSES[$classname],$method),$args);
       }
    else
        {
        //Bei Debug lassen wir Fehler weiterlaufen
        if (!DEBUG) trigger_error("invalid call ".$classname.":".$method);
        $result=FALSE;
        }
        
    return($result);
    }

//////////////////////////////////////////////////////////////////////////
//Eigenschaft setzen
function setproperty($classname,$property,$value)
    {
    global $CLASSES;

    if (property_exists($classname,$property)==TRUE)
       {
       $CLASSES[$classname]->$property=$value;
       $result=TRUE;
       }
    else
       {
       //Bei Debug lassen wir Fehler weiterlaufen
       if (!DEBUG) trigger_error("invalid property ".$classname.":".$property);
       $result=FALSE;
       }
       
    return($result);
    }

//////////////////////////////////////////////////////////////////////////
//Eigenschaft lesen und im Fehlerfall den Defaultwert liefern
function getproperty($classname,$property,$default)
    {
    global $CLASSES;

    if (property_exists($classname,$property)==TRUE)
       {
       $result=$CLASSES[$classname]->$property;
        }
    else
       {
       //Bei Debug lassen wir Fehler weiterlaufen
       if (!DEBUG) trigger_error("invalid object ".$classname.":".$property);
       $result=$default;
       }
       
    return($result);
    }

</script>