<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2009 Borg@sven-of-nine.de
*/
///////////////////////////////////////////////////////////////////////////
///
/// Eine Tokenizer-Klasse, die mit regulären Ausdrücken gefüttert
/// einen beliebigen String in Tokens wandeln kann
///
/// Standardmäßig sind diese token geladen :
///  TOKEN_STRING = gibt alle Alpha-Zeichenfolgen aus
///  TOKEN_NUMBER = gibt alle Number-Zeichenfolgen aus
///
/// Funktion
/// $tokenizer= new tokenizer()                     //Klasse instanzieren
/// $tokenizer->input="dies ist nur ein t3xt";      //Eingabe laden
/// $tokenizer->reset                               //Sicherheitshalber Position resetten
/// $mytoken=new token;                             //Ergebnistoken erzeugen
/// while ($tokenizer->findnext($mytoken))          //Alle durchgehen
///     {
///     switch ($mytoken->id)
///         {
///         case (TOKEN_STRING)  : echo "\nein  text     :".$mytoken->result;  break;
///         case (TOKEN_NUMBER)  : echo "\neine zahl     :".$mytoken->result;  break;
///         }
///     }
/// $tokenizer->destroy();
///
/// Ein eigenes Token fügt man einfach mit $tokenizer->add(TOKEN_ID,"regulärerausdruck") zu;
/// Bei der Zerlegung wir immer das am weitesten vorne gefundene Token benutzt.
/// Die Reihenfolge des Zufügens ist vollkommen unwichtig.
///
///////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////
//Ein paar Definitionen zum leichteren Handling
define ("TOKEN_NONE"    ,0);
define ("TOKEN_STRING"  ,65535);
define ("TOKEN_NUMBER"  ,65534);
define ("TOKEN_EMPTY"   ,"");

//////////////////////////////////////////////////////////////////////////
/// Record eines Tokens
//////////////////////////////////////////////////////////////////////////
class token
    {
    var $id       = TOKEN_NONE;
    var $token    = TOKEN_EMPTY;
    var $result   = TOKEN_EMPTY;
    var $length   = 0;
    var $position = 0;
    }

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class tokenizer
    {
    //Public
    var $input = "";

    //Private
    var $_registry = FALSE;

    //für rpc exportierte funktionen
    var $export      = array();

    //Hilfsvariablen    
    var $internal_tokens  =array();   //Array mit allen Token
    var $internal_position=0;         //Aktuelle Position im Buffer
    var $internal_expression="";
    
    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function tokenizer()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        unset($this);
        }
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]        = "tokenizer";      //ID unserer Klasse, nur alphanumerisch (mit diesem Namen wird das Objekt instanziert)
        $result[CLASS_INDEX_NAME]      = "tokenizer";      //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.1";            //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = FALSE;            //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;            //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]   = FALSE;            //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;             //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= FALSE;            //Soll die Datenbank komprimiert werden?
        $result[CLASS_INDEX_ENCRYPTED] = FALSE;            //Soll die Datenbank verschlüsselt werden?

        $result[CLASS_INDEX_RUNLEVEL]  = 1;                //In welchen Runlevel soll die Klasse geladen werden

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Hier können bei der Installation Daten in die Registry geschrieben werden
    function preset(&$registry)
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Zeiger auf This liefern
    function getthis()
        {
        $self=&$this;
        return($self);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Datei zerlegen
    function open($filename)
        {
        $this->input=file_get_contents($filename);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Verbindungsabbau
    function close()
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->cleartoken();
        $this->reset();
        
        //Und ein paar Standardtoken laden
        $this->addtoken(TOKEN_STRING,"/[a-zA-Z]+/");
        $this->addtoken(TOKEN_NUMBER,"/[0-9]+/");
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Ein Token zufügen
    function addtoken($id,$token)
        {
        //Einfach dem Tokenarray zufügen
        if ($token!="")
            {
            //Neues Token erzeugen
            $mytoken=new token;
            $mytoken->id=$id;
            $mytoken->position=0;
            $mytoken->token=$token;

            //Und zufügen
            $this->internal_tokens[]=$mytoken;
            }
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Die Tokenliste löschen
    function cleartoken()
        {
        $this->internal_tokens=array();
        $this->internal_position=0;
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Die Tokensuche vorn beginnen
    function reset()
        {
        $this->internal_position=0;
        }

    //////////////////////////////////////////////////////////////////////////
    //Alle Token (unique) in den Daten suchen
    //Rückgabe ist ein Array mit allen benutzen Token
    function findall()
        {
        $result=array();
        $mytoken=new token();
        
        $this->reset();
        while ( $this->findnext($mytoken)!=FALSE )
            {
            $result[$mytoken->id]=$mytoken;
            }
        return($result);
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Das erste Token holen
    function findfirst(&$resulttoken)
        {
        //Zeiger an den Anfang setzen
        $this->internal_position=0;

        //Und den nächsten holen
        return($this->findnext($resulttoken));
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Das nächste Token suchen
    function findnext(&$resulttoken)
        {
        $result=FALSE;

        //Token mit Standardwerten füllen
        $resulttoken->id=TOKEN_NONE;
        $resulttoken->position=strlen($this->input);
        
        //Alle Token durchgehen
        $mytoken=reset($this->internal_tokens);
        while ($mytoken != FALSE)
            {
            //Alle Token nacheinander checken
            if (preg_match($mytoken->token,$this->input,$regresult,PREG_OFFSET_CAPTURE,$this->internal_position)==1)
                {
                //Array auf das erste Ergebnis flatten
                $regresult=reset($regresult);
                
                //Wenn die Position des aktuellen Tokens kleiner ist als die des Vorherigen,
                //dann merken wir uns das neue
                if (end($regresult) < $resulttoken->position)
                    {
                    //Ergebnisse speichern
                    $resulttoken->id      =$mytoken->id;
                    $resulttoken->token   =$mytoken->token;
                    $resulttoken->result  =reset($regresult);
                    $resulttoken->position=end($regresult);
                    }
                }
            $mytoken=next($this->internal_tokens);
            }
            
        //Die neue Position merken, wenn wir was gefunden haben
        if ($resulttoken->id!=TOKEN_NONE)
            {
            $resulttoken->length=strlen($resulttoken->result);
            $this->internal_position=($resulttoken->position + $resulttoken->length);
            $result=TRUE;
            }

        //Fertig
        return($result);
        }
    }
</script>