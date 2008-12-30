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
/// RPC-Klasse
/// Verwaltet alle externen Datenzugriffe über RPC
////////////////////////////////////////////////////////////////////////////////////////////////////
///Ein RPC-Paket übergibt eine UserID,
///den gewünschten Funktionsaufruf und die dazugehörigen Parameter
///Grundsätzlich lassen sich alle Funktionen der automatisch geladenen Klassen aufrufen
///
///<rpc>
/// <accessid>ID</accessid>
/// <connectionid>SelfmadeID</connectionid>
///  <call>
///   <class>classname</class>
///   <function>functionname</function>
///   <parameter>
///     <paramname>%value%</paramname>
///     <paramname>%value%</paramname>
///     <paramname>%value%</paramname>
///     <paramname>%value%</paramname>
///   <parameter>
///   <result>
///    %value%
///   </result>
///  </call>
///</rpc>
///
///Value spaltet sich auf in
///<type> : [boolean|string|array|integer]
///bool   : true|false
///string : base64(string)
///array  : <keyname>Value</keyname>
///int    : int
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///
///Beispiel
///Anfrage
///<rpc>
/// <accessid>ab253ba3a5bf</accessid>
/// <connectionid>MyID01</connectionid>
///  <call>
///   <class>registry</class>
///   <function>read</function>
///   <parameter>
/// Die Reihenfolge der Parameter muß der Reihenfolge im Funktionsaufruf gleichen
/// Braucht kein Parameter übergeben werden, muß ein Parameter mit dem namen "void" übergeben werden
///     <path><string>base64_encode("user/")</string></path>
///     <key><string>base64_encode("email")</sring></key>
///     <default><string>base64_encode("unknown")</string></default>
///   <parameter>
///  </call>
///</rpc>
///
///Antwort
///<rpc>
/// <accessid>ab253ba3a5bf</accessid>
/// <connectionid>MyID01</connectionid>
///  <call>
///   <class>registry</class>
///   <function>read</function>
///   <parameter>
///     <path><string>base64_encode("user/")</string></path>
///     <key><string>base64_encode("email")</sring></key>
///     <default><string>base64_encode("unknown")</string></default>
///   <parameter>
///   <result>
///   <string>base64_encode("myuser@mydomain.void")</string>
///   </result>
///  </call>
///</rpc>
///
////////////////////////////////////////////////////////////////////////////////////////////////////
///Beispiel für den Aufbau eins Remotecalls
///require_once(PATH_CLASSES."class.rpc.php");
///$rpc=new rpc();
///$rpc->userstatus=ID_NONE;
///$paramarray=array("modul"  =>"Hauptprojekt",
///                  "title"  =>"Mein Neuer Eintrag",
///                  "link"   =>"http://www.google.de",
///                  "message"=>"Nur eine kleine Nachricht",
///                  "author" =>"Sven Lorenz");
///$temp = $rpc->create ("vfbtygvsljcandhb",rand(0,65535),"newsletter","add",$paramarray);
///$rpc->process($temp);
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");
require_once(PATH_LIBS."lib.xml.php");

//Ein paar Fehlrekonstanten
define ("RPC_ERROR_NONE"              , 0);
define ("RPC_ERROR_INVALID_CALL"      , 2);
define ("RPC_ERROR_INVALID_ID"        , 4);
define ("RPC_ERROR_INVALID_CLASS"     , 8);
define ("RPC_ERROR_INVALID_FUNCTION"  ,16);


//Eigentliche Klasse
class rpc
    {
    var $userstatus = USER_GUEST;   // Ab welchem Benutzerstatus ist RPC zugelassen
    
    var $xmlhead  = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";

    var $errorstr = array ( RPC_ERROR_INVALID_CALL     => "rpc-packet not valid",
                            RPC_ERROR_INVALID_ID       => "access denied",
                            RPC_ERROR_INVALID_CLASS    => "access to unknown class or object",
                            RPC_ERROR_INVALID_FUNCTION => "access to unknown function or method",
                            RPC_ERROR_NONE             => "none",
                           );
    var $result   = "";

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function rpc()
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Zeiger auf This liefern
    function getthis()
      {
      $self=&$this;
      return($self);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Ein Paket bauen
    function create ($accessid,$connectionid,$class,$function,$paramarray)
        {
        $result=array();
        $result["rpc"]["accessid"]=$accessid;
        $result["rpc"]["connectionid"]=$connectionid;
        $result["rpc"]["call"]["class"]=$class;
        $result["rpc"]["call"]["function"]=$function;
        
        foreach ($paramarray as $name => $value)
            {
            $result["rpc"]["call"]["parameter"][$name]=$this->encodevalue(gettype($value),$value);
            }

        return ($this->xmlhead.arraytoxml($result));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eingabe
    function process($xmldata)
        {
        $time=time();

        //Einmal in beider Richtungen parsen,
        //um das Paket sauber zu kriegen
        $data=xmltoarray($xmldata);
        $data=arraytoxml($data);
        $data=xmltoarray($data);

        if ($this->rpcvalid($data)==TRUE)
            {
            if ($this->rpcaccess($data["rpc"]["accessid"]) >= $this->userstatus)
                {
                //Da der Klassenaufruf ein wenig ungelenkt ist,
                //sieht auch der Aufbau der Parameter doof aus
                $class=$data["rpc"]["call"]["class"];
                $func =$data["rpc"]["call"]["function"];
                $p1  = $this->decodevalue(reset( $data["rpc"]["call"]["parameter"] ) );
                $p2  = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );
                $p3  = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );
                $p4  = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );
                $p5  = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );
                $p6  = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );
                $p7  = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );
                $p8  = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );
                $p9  = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );
                $p10 = $this->decodevalue(next ( $data["rpc"]["call"]["parameter"] ) );

                //Alle möglichen Probleme abfangen
                if ( rpcmethodexists($class,$func) == TRUE )
                    {
                    $temp = callrpcmethod($class,$func,$p1,$p2,$p3,$p4,$p5,$p6,$p7,$p8,$p9,$p10);

                    $data["rpc"]["result"]=$this->encodevalue(gettype($temp),$temp);
                    $this->rpcerror($data,RPC_ERROR_NONE);
                    }
                else
                    {
                    $this->rpcerror($data,RPC_ERROR_INVALID_FUNCTION);
                    }
                }
            else
                {
                $this->rpcerror($data,RPC_ERROR_INVALID_ID);
                }
            }
        else
            {
            $this->rpcerror($data,RPC_ERROR_INVALID_CALL);
            }

        //Und als XML-Paket ablegen
        $data["rpc"]["runtime"]=(time() - $time);
        $this->result=$this->xmlhead.arraytoxml($data);
        
        return ( $data["rpc"]["errorno"] == RPC_ERROR_NONE );
        }
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Prüfen, ob der Benutzer Zugriff hat
    function rpcaccess($id)
        {
        return (callmethod("user","exists",USER_PREFIX.$id));
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Fehler dem Paket zufügen
    function rpcerror(&$rpcarray,$errno)
        {
        $rpcarray["rpc"]["errorno"] = $errno;
        $rpcarray["rpc"]["errorstr"]= $this->errorstr[$errno];
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Das RPC-Array auf Validität prüfen
    function rpcvalid($array)
        {
        $result =TRUE;
        $result&=isset($array["rpc"]["accessid"]);
        $result&=isset($array["rpc"]["call"]);
        $result&=isset($array["rpc"]["call"]["class"]);
        $result&=isset($array["rpc"]["call"]["function"]);
        $result&=isset($array["rpc"]["call"]["parameter"]);
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Variablen dekodieren
    function decodevalue($inputarray)
        {
        if (is_array($inputarray)==TRUE)
            {
            $val=reset($inputarray);
            $key=key($inputarray);
            switch ($key)
                {
                case ("array")   : $result = $val;
                                   settype($result,$key);
                                   break;
                case ("boolean") : $result = trim($val)=="true";
                                   settype($result,$key);
                                   break;
                case ("string")  : $result = base64_decode(trim($val));
                                   settype($result,$key);
                                   break;
                case ("void")    : $result = "void";
                                   break;
                //Als Standard das vollkommen ungefährliche INT nehmen
                default          : $result = intval(trim($val));
                                   settype($result,"integer");
                                   break;
                }
            }
        else
            {
            $result=FALSE;
            }
        return($result);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Variablen enkodieren
    function encodevalue($type,$value)
        {
        switch ($type)
            {
            case ("array")   : break;
            case ("boolean") : $result["bool"]    = $value==TRUE?"true":"false";    break;
            case ("string")  : $result["string"]  = base64_encode($value);          break;
            case ("void")    : $result["void"]    = "void";                         break;
            default          : $result["integer"] = intval($value);                 break;
            }
        return($result);
        }
    }
</script>