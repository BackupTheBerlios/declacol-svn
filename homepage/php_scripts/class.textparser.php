<script language="PHP">
///////////////////////////////////////////////////////////////////////////////////////
///
/// Textparser zur Wandlung deutscher Eingaben in SQL-Statements
///
///
///
///////////////////////////////////////////////////////////////////////////////////////


//Der Sprachvorrat

//Alle Grundverben werden als Konstanten definiert

///////////////////////////////////////////////////////////////////////////////////////
//Verben
///////////////////////////////////////////////////////////////////////////////////////
//Bewegungen
$count=1000;
define ("PARSER_VERB"           , $count++);
define ("PARSER_VERB_GO"        , $count++);
define ("PARSER_VERB_STAY"      , $count++);
define ("PARSER_VERB_LEFT"      , $count++);
define ("PARSER_VERB_RIGHT"     , $count++);
define ("PARSER_VERB_BACK"      , $count++);
define ("PARSER_VERB_FORTH"     , $count++);

//Vergleiche
define ("PARSER_VERB_COMPARE"   , $count++);

//Modifikatoren
define ("PARSER_VERB_INCREASE"  , $count++);
define ("PARSER_VERB_DECREASE"  , $count++);
define ("PARSER_VERB_REMOVE"    , $count++);
define ("PARSER_VERB_MINIMIZE"  , $count++);
define ("PARSER_VERB_MAXIMIZE"  , $count++);
define ("PARSER_VERB_ZERO"      , $count++);
define ("PARSER_VERB_INVERT"    , $count++);
define ("PARSER_VERB_ABSOLUTE"  , $count++);

//Berechnungen
define ("PARSER_VERB_AVERAGE"   , $count++);
define ("PARSER_VERB_MULT"      , $count++);
define ("PARSER_VERB_DIV"       , $count++);
define ("PARSER_VERB_ADD"       , $count++);
define ("PARSER_VERB_SUB"       , $count++);
define ("PARSER_VERB_DIFF"      , $count++);

//Tätigkeiten
define ("PARSER_VERB_GET"       , $count++);
define ("PARSER_VERB_PUT"       , $count++);

define ("PARSER_VERB_BE"        , $count++);
define ("PARSER_VERB_CALLED"    , $count++);

define ("PARSER_VERB_HAS"       , $count++);

///////////////////////////////////////////////////////////////////////////////////////
//Substantive
///////////////////////////////////////////////////////////////////////////////////////
//Namen
$count=2000;
define ("PARSER_NOUN"           , $count++);

///////////////////////////////////////////////////////////////////////////////////////
//Eigenschaften
///////////////////////////////////////////////////////////////////////////////////////
$count=3000;
define ("PARSER_PROP"           , $count++);

///////////////////////////////////////////////////////////////////////////////////////
//Adjektive
///////////////////////////////////////////////////////////////////////////////////////
$count=4000;
define ("PARSER_ADJ"            , $count++);
define ("PARSER_ADJ_BIG"        , $count++);
define ("PARSER_ADJ_SMALL"      , $count++);

///////////////////////////////////////////////////////////////////////////////////////
//Fragen
///////////////////////////////////////////////////////////////////////////////////////
$count=5000;
define ("PARSER_QUEST"          , $count++);
define ("PARSER_QUEST_WHERE"    , $count++);
define ("PARSER_QUEST_WHO"      , $count++);
define ("PARSER_QUEST_WHAT"     , $count++);
define ("PARSER_QUEST_HOWMUCH"  , $count++);
define ("PARSER_QUEST_HOW"      , $count++);

///////////////////////////////////////////////////////////////////////////////////////
//Orte
///////////////////////////////////////////////////////////////////////////////////////
$count=5000;
define ("PARSER_PLACE"          , $count++);
define ("PARSER_PLACE_PLANET"   , $count++);
define ("PARSER_PLACE_COUNTRY"  , $count++);
define ("PARSER_PLACE_TOWN"     , $count++);
define ("PARSER_PLACE_BUILDING" , $count++);
define ("PARSER_PLACE_ROOM"     , $count++);

///////////////////////////////////////////////////////////////////////////////////////
//Personen
///////////////////////////////////////////////////////////////////////////////////////
$count=6000;
define ("PARSER_PERSON"         , $count++);
define ("PARSER_PERSON_I"       , $count++);
define ("PARSER_PERSON_NAME"    , $count++);

///////////////////////////////////////////////////////////////////////////////////////
//Die Wortbibliothek
///////////////////////////////////////////////////////////////////////////////////////
class words
    {
    var $count=0;
    var $words=array();
    
    //Konstruktor
    function words()
        {
        $this->count=0;
        
        $this->init();
        }

    function init()
        {
        }

    //Ein neues Wort zufügen
    function add($word,$id)
        {
        //Umlaute ersetzen
        $word=strtolower($word);
//        $word=preg_replace(array("/ä/","/ö/","/ü/","/ß/"),array("ae","oe","ue","ss"),$word);

        //Alles nichtalphanumerische kicken wir
        $word=preg_replace("/[^a-zA-Z0-9äÄöÖüÜß]/","",$word);

        if (!isset($this->words[$word]))
            {
            $this->count++;
            $this->words[$word]=$id;
            }
        }
    }


class nouns extends words
    {
    function init()
        {
        }
    }
    
class persons extends words
    {
    function init()
        {
        $this->add("ich",PARSER_PERSON_I);
        }
    }


class verbs extends words
    {
    function init()
        {
        $this->add("geh"           ,  PARSER_VERB_GO);
        $this->add("renn"          ,  PARSER_VERB_GO);
        $this->add("lauf"          ,  PARSER_VERB_GO);

        $this->add("steh"          ,  PARSER_VERB_STAY);
        $this->add("bleib"         ,  PARSER_VERB_STAY);

        $this->add("links"         ,  PARSER_VERB_LEFT);
        $this->add("rechts"        ,  PARSER_VERB_RIGHT);
        $this->add("zurück"        ,  PARSER_VERB_BACK);
        $this->add("vor"           ,  PARSER_VERB_FORTH);

        $this->add("vergleich"     ,  PARSER_VERB_COMPARE);
        $this->add("vergleiche"    ,  PARSER_VERB_COMPARE);
        $this->add("erhöhe"        ,  PARSER_VERB_INCREASE);
        $this->add("vermindere"    ,  PARSER_VERB_DECREASE);
        $this->add("erniedrige"    ,  PARSER_VERB_DECREASE);
        $this->add("entferne"      ,  PARSER_VERB_REMOVE);
        $this->add("lösche"        ,  PARSER_VERB_REMOVE);

        $this->add("minimiere"     ,  PARSER_VERB_MINIMIZE);
        $this->add("maximiere"     ,  PARSER_VERB_MAXIMIZE);
        $this->add("resette"       ,  PARSER_VERB_ZERO);
        $this->add("invertiere"    ,  PARSER_VERB_INVERT);
        $this->add("negiere"       ,  PARSER_VERB_INVERT);
        $this->add("absolut"       ,  PARSER_VERB_ABSOLUTE);

        $this->add("mittle"        ,  PARSER_VERB_AVERAGE);
        $this->add("multipliziere" ,  PARSER_VERB_MULT);
        $this->add("dividiere"     ,  PARSER_VERB_DIV);
        $this->add("teile"         ,  PARSER_VERB_DIV);
        $this->add("addiere"       ,  PARSER_VERB_ADD);
        $this->add("subtrahiere"   ,  PARSER_VERB_SUB);
        
        $this->add("hole"          ,  PARSER_VERB_GET);
        $this->add("zeige"         ,  PARSER_VERB_GET);
        $this->add("nehme"         ,  PARSER_VERB_GET);

        $this->add("lege"          ,  PARSER_VERB_PUT);
        
        $this->add("heißt"         ,  PARSER_VERB_CALLED);
        $this->add("ist"           ,  PARSER_VERB_BE);

        $this->add("hat"           ,  PARSER_VERB_HAS);
        $this->add("habe"          ,  PARSER_VERB_HAS);
        $this->add("haben"         ,  PARSER_VERB_HAS);
        $this->add("gibt"          ,  PARSER_VERB_HAS);
        }
    }

class properties extends words
    {
    function init()
        {
        }
    }

class questions extends words
    {
    function init()
        {
        $this->add("wo"            ,  PARSER_QUEST_WHERE);
        $this->add("wer"           ,  PARSER_QUEST_WHO);
        $this->add("wem"           ,  PARSER_QUEST_WHO);
        $this->add("was"           ,  PARSER_QUEST_WHAT);
        $this->add("wieviel"       ,  PARSER_QUEST_HOWMUCH);
        $this->add("wieviele"      ,  PARSER_QUEST_HOWMUCH);
        $this->add("wie"           ,  PARSER_QUEST_HOW);
        }
    }

class adjectives extends words
    {
    function init()
        {
        $this->add("gross"         ,  PARSER_ADJ_BIG);
        $this->add("klein"         ,  PARSER_ADJ_SMALL);
        }
    }

class places extends words
    {
    function init()
        {
        }
    }


////////////////////////////////////////////////////////////////////////////////////////////////////////
//Das Satzobjekt. Zerlegt bei seiner Erzeugung automatisch den Text in sein Bestandteile (Keine Analyse)
class sentence
    {
    var $input     = FALSE;
    var $parts     = array();
    var $question  = FALSE;
    var $nouns     = array();
    var $verbs     = array();
    var $adjectives= array();
    var $properties= array();
    var $places    = array();
    var $persons   = array();
    var $ending    = FALSE;
    
    function sentence($in)
        {
        $in=trim($in);
        
        //Das Letzte Zeichen ist das Satzzeichen
        $this->ending=substr($in,strlen($in)-1,1);

        $this->question=$this->ending=="?";

        //Umlaute ersetzen
//        $in=preg_replace(array("/ä/","/ö/","/ü/","/ß/"),array("ae","oe","ue","ss"),$in);

        //Alles nichtalphanumerische kicken wir
        $in=preg_replace("/[^a-zA-Z0-9äÄöÖüÜ\\s]/","",$in);
        
        //Keine doppelten Spaces
        $in=preg_replace("/[\\s]+/"," ",trim($in));

        //In ein Array zerlegen
        $this->parts=explode(" ",$in);

        //Ablegen
        $this->input=$in;
        }
    }


////////////////////////////////////////////////////////////////////////////////////////////////////////
class textparser
    {
    var $nouns      = FALSE;
    var $verbs      = FALSE;
    var $adjectives = FALSE;
    var $questiosn  = FALSE;
    var $properties = FALSE;
    var $places     = FALSE;
    var $persons    = FALSE;

    var $sentence   = FALSE;

    function textparser()
        {
        //Alle Wortkonstrukte erzeugen
        $this->nouns      = new nouns();
        $this->verbs      = new verbs();
        $this->adjectives = new adjectives();
        $this->questions  = new questions();
        $this->properties = new properties();
        $this->places     = new places();
        $this->persons    = new persons();
        
        $this->init();
        }
        
    function init()
        {
        }
        
    function parse($text)
        {
        //Den Satz in seine Teile zerlegen
        $this->sentence=$this->_split($text);
        }
        
    //Den Satz zerlegen
    function _split($input)
        {
        $result=new sentence($input);
        
        //Nun gehen wir alle Satzteile duchr und verteilen sie auf die passenden Arrays
        foreach ($result->parts as $word)
            {
            if (isset($this->nouns->     words[$word])) $result->nouns[$word]     =$this->nouns->words[$word];
            if (isset($this->verbs->     words[$word])) $result->verbs[$word]     =$this->verbs->words[$word];
            if (isset($this->adjectives->words[$word])) $result->adjectives[$word]=$this->adjectives->words[$word];
            if (isset($this->properties->words[$word])) $result->properties[$word]=$this->properties->words[$word];
            if (isset($this->questions-> words[$word])) $result->question         =$this->questions->words[$word];
            if (isset($this->places->    words[$word])) $result->places[$word]    =$this->places->words[$word];
            if (isset($this->persons->   words[$word])) $result->persons[$word]   =$this->persons->words[$word];
            }

        return($result);
        }
    }
    
    
    
class machinery extends textparser
    {
    var $db = "";
    
    function init()
        {
        //Unsere Eigenschaften Laden
        $count=PARSER_PROP+1;
        define ("PARSER_PROP_GA"      ,$count++);
        define ("PARSER_PROP_SALDO1"  ,$count++);
        define ("PARSER_PROP_SALDO2"  ,$count++);
        define ("PARSER_PROP_LOAD"    ,$count++);
        define ("PARSER_PROP_DAYS"    ,$count++);


        $this->properties->add("geldspieler"   ,  PARSER_PROP_GA);
        $this->properties->add("geräte"        ,  PARSER_PROP_GA);
        $this->properties->add("saldo1"        ,  PARSER_PROP_SALDO1);
        $this->properties->add("umsatz"        ,  PARSER_PROP_SALDO1);
        $this->properties->add("saldo2"        ,  PARSER_PROP_SALDO2);
        $this->properties->add("gewinn"        ,  PARSER_PROP_SALDO2);
        $this->properties->add("auslastung"    ,  PARSER_PROP_LOAD);
        $this->properties->add("tage"          ,  PARSER_PROP_DAYS);
        }
    
    
    function parse($text)
        {
        //Den Satz in seine Teile zerlegen
        $this->sentence=$this->_split($text);
        return($this->_questiontosql($this->sentence));
        }



    function _questiontosql($sentence)
        {
        //Für jede Eigenschaft müssen wir leider eine eigene Abfrage bauen
        $data  =array();
        foreach ($sentence->properties as $prop)
            {
            switch ($prop)
                {
                case (PARSER_PROP_GA)    :   $data[]="count(id) as anzahl";       break;
                case (PARSER_PROP_SALDO1):   $data[]="sum(saldo1_sum) as saldo1";     break;
                case (PARSER_PROP_SALDO2):   $data[]="sum(saldo2_sum) as saldo2";     break;
                case (PARSER_PROP_LOAD)  :   $data[]="AVG(load_day)as sum_workload";   break;
                case (PARSER_PROP_DAYS)  :   $days[]="journaldays";            break;
                }
            }
        //Standard ist Anzahl
        if (count($data)<1) $data[]="count(id) as anzahl";

            
        //Besitzer
        $filter  =array();
        //Besitzer
        foreach ($sentence->persons as $name=>$person)
            {
            $filter[]="(aufsteller ='".$name."')";
            }
        //Plätze
        foreach ($sentence->places as $name => $place)
            {
            switch ($place)
                {
                case (PARSER_PLACE_COUNTRY)        : $filter[]="(ort        =    '".$name."')";   break;
                case (PARSER_PLACE_TOWN)           : $filter[]="(halle      =    '".$name."')";   break;
                case (PARSER_PLACE_BUILDING)       : $filter[]="(name       like '".$name."%')";   break;
                case (PARSER_PLACE_ROOM)           : $filter[]="(zulassung  like '".$name."%')";   break;
                }
            }
        //Und alles zusammenbauen
        $result="SELECT ".implode(",",$data)." FROM ".$this->db." WHERE ".implode(" AND ",$filter);
        
        return($result);
        }
    }
</script>