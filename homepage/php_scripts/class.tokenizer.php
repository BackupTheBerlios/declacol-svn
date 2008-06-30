<script language="php">
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

//Versioninfo speichern
define ("CLASS_TOKENIZER","class_tokenizer");
define ("CLASS_TOKENIZER_VERSION","0.02");
if (isset($debug)) $debug->add(CLASS_TOKENIZER,"version ".CLASS_TOKENIZER_VERSION);


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
    var $input = "";
    
    //Private Eigenschaften
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

    //Destruktor
    function destroy()
        {
        }

    //Verbindungsaufbau
    function open($filename)
        {
        $this->input=file_get_contents($filename);
        }

    //Verbindungsabbau
    function close()
        {
        }

    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->clear_token();
        $this->reset();
        
        //Und ein paar Standardtoken laden
        $this->add(TOKEN_STRING,"/[a-zA-Z]+/");
        $this->add(TOKEN_NUMBER,"/[0-9]+/");
        }

    //Datenbank und alles andere erzeugen
    //Wird hier nicht benutzt
    function install()
        {
        return(TRUE);
        }

    //Datenbank und alles andere zerstören
    //Wird hier nicht benutzt
    function uninstall()
        {
        return(FALSE);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Ein Token zufügen
    function add($id,$token)
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
    function clear_token()
        {
        $this->internal_tokens=array();
        $this->internal_position=0;
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Die Token suche vorn beginnen
    function reset()
        {
        $this->internal_position=0;
        }

    //////////////////////////////////////////////////////////////////////////
    //Das erste Token holen
    function findfirst(&$resulttoken)
        {
        //Zeiger an den Anfang setzen
        $this->reset();

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

    //////////////////////////////////////////////////////////////////////////
    // Interne Funktionen
    //////////////////////////////////////////////////////////////////////////
    }
</script>