<script language="PHP">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2008 Borg@sven-of-nine.de
*/
//////////////////////////////////////////////////////////////////////////
///
/// Erlaubt den Zugriff auf durch ClassLoad geladene Methoden
/// und Eigenschaften
///
//////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////
//Pr�fen, ob eine Klasse existiert
function classexists($classname)
    {
    global $CLASSES;

    return(isset($CLASSES[$classname]));
    }


//////////////////////////////////////////////////////////////////////////
//Pr�fen, ob eine Methode existiert
function methodexists($classname,$method)
    {
    global $CLASSES;

    return( method_exists($CLASSES[$classname],$method) );
    }

//////////////////////////////////////////////////////////////////////////
//Pr�fen, ob eine Eigenschaft existiert
function propertyexists($classname,$method)
    {
    global $CLASSES;

    return( property_exists($CLASSES[$classname],$method) );
    }


//////////////////////////////////////////////////////////////////////////
//Hier die Interfacefunktion, um auf die geladenen Klassen zugreifen zu k�nnen
//Der Aufruf ist zwar ziemlich schr�g, ober eine bessere Methode habe ich noch nicht gefunden
function callmethod($classname,$method,$p1=FALSE,$p2=FALSE,$p3=FALSE,$p4=FALSE,$p5=FALSE,$p6=FALSE,$p7=FALSE,$p8=FALSE,$p9=FALSE,$p10=FALSE)
    {
    global $CLASSES;
    $result=FALSE;
    
    if (isset($CLASSES[$classname])==TRUE)
        {
        if (method_exists($CLASSES[$classname],$method)==TRUE)
            {
            $result=$CLASSES[$classname]->$method($p1,$p2,$p3,$p4,$p5,$p6,$p7,$p8,$p9,$p10);
            }
        else
            {
            //Bei Debug lassen wir fehler weiterlaufen
            if (!DEBUG) trigger_error("invalid method ".$classname.":".$method);
            }
        }
    else
        {
        if (!DEBUG) trigger_error("invalid object ".$classname.":".$method);
        }
    return($result);
    }

//////////////////////////////////////////////////////////////////////////
//Eigenschaft setzen
function setproperty($classname,$property,$value)
    {
    global $CLASSES;
    $result=FALSE;
    if (isset($CLASSES[$classname])==TRUE)
        {
        if (property_exists($CLASSES[$classname],$property)==TRUE)
            {
            $CLASSES[$classname]->$property=$value;
            $result=TRUE;
            }
        else
            {
            if (!DEBUG) trigger_error("invalid property ".$classname.":".$property);
            }
        }
    else
        {
        if (!DEBUG) trigger_error("invalid object ".$classname.":".$property);
        }
    return($result);
    }

//////////////////////////////////////////////////////////////////////////
//Eigenschaft lesen und im Fehlerfall den Defaultwert liefern
function getproperty($classname,$property,$default)
    {
    global $CLASSES;
    $result=$default;
    if (isset($CLASSES[$classname])==TRUE)
        {
        if (property_exists($CLASSES[$classname],$property)==TRUE)
            {
            $result=$CLASSES[$classname]->$property;
            }
        else
            {
            if (!DEBUG) trigger_error("invalid property ".$classname.":".$property);
            }
        }
    else
        {
        if (!DEBUG) trigger_error("invalid object ".$classname.":".$property);
        }
    return($result);
    }
</script>