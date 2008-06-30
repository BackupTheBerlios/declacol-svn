<script language="PHP">
///////////////////////////////////////////////////////////////////////////////////////
/// Kleine Programmierübung um mit Turing-Maschinen Erfahrung zu sammeln
///
/// Umsetzung eines Brainfuck interpreters in PHP
///
/// Siehe http://de.wikipedia.org/wiki/Brainfuck
///       http://home.arcor.de/partusch/html_de/bfd.html
///
///////////////////////////////////////////////////////////////////////////////////////
/// Befehle
///Zeichen  C-Äquivalent      Semantik
/// >       ++ptr;            inkrementiert den Zeiger
/// <       --ptr;            dekrementiert den Zeiger
/// +       ++*ptr;           inkrementiert den aktuellen Zellenwert
/// -       --*ptr;           dekrementiert den aktuellen Zellenwert
/// .       putchar(*ptr);    Gibt den aktuellen Zellenwert als ASCII-Zeichen auf der Standardausgabe aus
/// ,       *ptr = getchar(); Liest ein Zeichen von der Standardeingabe und speichert dessen ASCII-Wert in der aktuellen Zelle
/// [       while (*ptr) {    Springt nach vorne, hinter den passenden ]-Befehl, wenn der aktuelle Zellenwert null ist
/// ]       }                 Springt zurück, hinter den passenden [-Befehl, wenn der aktuelle Zellenwert verschieden von null ist
///////////////////////////////////////////////////////////////////////////////////////
/// Beispiel
/// Zählt von 0 bis 9 und gibt die ASCII-Zeichen dazu aus
/// ++++++++++++++++++++++++++++++++++++++++++++++++>++[->+++++[-<<.+>>]<]
/// Erläutertung :
/// Speicherzelle Null auf Ord(0) bringen  ++++++++++++++++++++++++++++++++++++++++++++++++
/// Speicherzelle Eins addressieren        >
/// Speicherzelle Eins um zwei erhöhen     ++
/// Schleifenbeginn                        [
/// Speicherzelle Eins um eins erniedrigen  -
/// Speicherzelle Zwei addressieren         >
/// Speicherzelle Zwei um fünf erhöhen      +++++
/// Schleifenbeginn                         [
/// Speicherzelle Zwei um eins erniedrigen   -
/// Speicherzelle Null addressieren          <<
/// Speicherzelle Null als ASCII ausgeben    .
/// Speicherzelle Eins um eins erhöhen       ++
/// Speicherzelle Zwei adressieren           >>
/// Schleifenende                           ]
/// Speicherzelle Eins addressieren         <
/// Schleifenende                          ]
/// Die äussere Schleife wird also zweimal durchlaufen und die innere fünfmal.
/// Dabei wird beginnend von Ord('0') jeweils die Speicherstelle null ausgegeben
/// und danach um eins erhöht. Dadurch wird 0123456789 ausgegeben
///////////////////////////////////////////////////////////////////////////////////////

//Die Sprachkonstrukte
define ("BRAIN_INC" ,"+");
define ("BRAIN_DEC" ,"-");
define ("BRAIN_LEFT","<");
define ("BRAIN_RIGHT",">");
define ("BRAIN_LOOP","[");
define ("BRAIN_POOL","]");
define ("BRAIN_WRITE",".");
define ("BRAIN_READ",",");


/// Beispielaufruf
///
/*

$bf=new brainfuck();
$bf->timelimit=10;
$bf->loadinstructions("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.");
$bf->run();

echo "<pre>";
echo "runtime :".$bf->runtime."\n";
echo(implode("",$bf->output));
if (count($bf->log)>0) print_r($bf->log);
echo "</pre>";
$bf->destroy();

*/
///
///////////////////////////////////////////////////////////////////////////////////////



class brainfuck
    {
    //Maximale Laufzeit
    var $timelimit=30;
    
    //Gelaufene Zeit
    var $runtime=0;

    //Der Speicher
    var $memory=array();

    //Programmpuffer
    var $instructions =array();
    
    //Eingaben (falls das Programm etwas anfordert)
    var $input =array();
    
    //Ausgaben (Falls das Programm etwas ausgibt)
    var $output=array();

    //Instructionpointer
    var $ip    =0;

    //Speicherpointer
    var $mp    =0;

    //Debugarray
    var $log   = array();
    //Soll debugged werden ?
    var $debug = FALSE;
    //Interner Puffer
    var $dbg   = "";

    //EOF-Flag (Wird intern verwendet)
    var $eof   =TRUE;
    
    ///////////////////////////////////////////////////////////////////////////////////////
    //Ein Paar Testprogramme
    ///////////////////////////////////////////////////////////////////////////////////////
    //Testprogramm mit der Ausgabe von "Hello World!"
    var $helloworld="++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.";
    //Testprogramm mit der Ausgabe "brainfuck"
    var $brainfuck =">++++[>++++++<-]>-[[<+++++>>+<-]>-]<<[<]>>>>--.<<<-.>>>-.<.<.>---.<<+++.>>>++.<<---.[>]<<.";
    //Testprogramm mit der Ausgabe "0123456789"
    var $counter   ="++++++++++++++++++++++++++++++++++++++++++++++++>++[->+++++[-<<.+>>]<]";
    //Testprogramm mit einer langen Schleife
    var $looper    ="-[->-[->-[->-[->-[-]]]]<<<<]";
    //Zeichnet ein Sierpinski-Dreieck (c) by N Y Y R I K K I
    var $triangle  =">++++[<++++++++>-]>++++++++[>++++<-]>>++>>>+>>>+<<<<<<<<<<[-[->+<]>[-<+>>>.<<]>>>[[->++++++++[>++++<-]>.<<[->+<]+>[->++++++++++<<+>]>.[-]>]]+<<<[-[->+<]+>[-<+>>>-[->+<]++>[-<->]<<<]<<<<]++++++++++.+++.[-]<]+++++";
    //Berechnet PI (c) Felix Nawothnig (felix.nawothnig@t-online.de)
    //Die + am Anfang bestimmt die Zahl der Stellen (5*+ = 5 Stellen)
    var $pi        ="> +++++ [<+>>>>>>>>++++++++++<<<<<<<-]>+++++[<+++++++++>-]+>>>>>>+[<<+++[>>[-<]<[>]<-]>>[>+>]<[<]>]>[[->>>>+<<<<]>>>+++>-]<[<<<<]<<<<<<<<+[->>>>>>>>>>>>[<+[->>>>+<<<<]>>>>>]<<<<[>>>>>[<<<<+>>>>-]<<<<<-[<<++++++++++>>-]>>>[<<[<+<<+>>>-]<[>+<-]<++<<+>>>>>>-]<<[-]<<-<[->>+<-[>>>]>[[<+>-]>+>>]<<<<<]>[-]>+<<<-[>>+<<-]<]<<<<+>>>>>>>>[-]>[<<<+>>>-]<<++++++++++<[->>+<-[>>>]>[[<+>-]>+>>]<<<<<]>[-]>+>[<<+<+>>>-]<<<<+<+>>[-[-[-[-[-[-[-[-[-<->[-<+<->>]]]]]]]]]]<[+++++[<<<++++++++<++++++++>>>>-]<<<<+<->>>>[>+<<<+++++++++<->>>-]<<<<<[>>+<<-]+<[->-<]>[>>.<<<<[+.[-]]>>-]>[>>.<<-]>[-]>[-]>>>[>>[<<<<<<<<+>>>>>>>>-]<<-]]>>[-]<<<[-]<<<<<<<<]++++++++++.";    ///////////////////////////////////////////////////////////////////////////////////////
    //

    //Lookups für Schleifen
    var $internal_loop=array();
    var $internal_pool=array();

    ///////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function brainfuck()
        {
        $this->reset();
        }
        
    ///////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->reset();
        }
        
    ///////////////////////////////////////////////////////////////////////////////////////
    //Alles initialisieren
    function reset()
        {
        //Speicher löschen
        $this->memory=array_fill(0,30000,0);
        
        //Programm löschen
        $this->inst  =array();

        //Puffer löschen
        $this->output=array();
        $this->input =array();

        //Instruction-Pointer resetten
        $this->ip=0;
        
        //Speicher Pointer
        $this->mp=0;
        
        //Hilfsarrays für Schleifen
        $this->internal_loop=array();
        $this->internal_pool=array();

        //Debugausgaben
        $this->log  = array();
        $this->debug= FALSE;

        //Ende des Programmes
        $this->eof=TRUE;
        }
        
    ///////////////////////////////////////////////////////////////////////////////////////
    //Daten laden
    function loadinput($input)
        {
        //String in ein Array zur einfacheren Addressierung
        $this->input=preg_split("//",$input,-1,PREG_SPLIT_NO_EMPTY);
        }
        
    ///////////////////////////////////////////////////////////////////////////////////////
    //Ook in Brain und umgekehrt

    function ook2brain($input)
        {
        //Nur Ook-Syntax zulassen
        $input=strtolower(preg_replace("/([^Ook!\\.\\?])/"," ",$input));
        $input=preg_replace("/[\s]+ /"," ",$input);

        //In ein Array
        $input=preg_split("/ /",$input);

        //Und los
        $result="";
        $one=reset($input);
        $two=next ($input);
        while ($one!==FALSE)
            {
            $result.=$this->_translate($one.$two);

            $one=next($input);
            $two=next($input);
            }
        return ($result);
        }

    function brain2ook($input)
        {
        //Nur Brainfuck-Syntax zulassen
        $input=strtolower(preg_replace("/([^+-<>,\\[\\]\\.]+)/","",$input));

        //In ein Array
        $input=preg_split("//",$input);

        //Und los
        $result="";
        foreach ($input as $code)
            {
            $result.=$this->_translate($code)."  ";
            }


        return (trim($result));
        }


    function _translate($input)
        {
        $result=FALSE;
        //Ook ?
        if (strpos($input,"ook")!==FALSE)
            {
            switch (strtolower(substr($input,0,8)))
                {
                case ("ook.ook.")   :   $result=BRAIN_INC;       break;
                case ("ook!ook!")   :   $result=BRAIN_DEC;       break;
                case ("ook.ook?")   :   $result=BRAIN_RIGHT;     break;
                case ("ook?ook.")   :   $result=BRAIN_LEFT;      break;
                case ("ook!ook?")   :   $result=BRAIN_LOOP;      break;
                case ("ook?ook!")   :   $result=BRAIN_POOL;      break;
                case ("ook!ook.")   :   $result=BRAIN_WRITE;     break;
                case ("ook.ook!")   :   $result=BRAIN_READ;      break;
                }
            
            }
        else
            {
            //Brainfuck
            switch (substr($input,0,1))
                {
                case (BRAIN_INC)     :   $result="Ook. Ook.";    break;
                case (BRAIN_DEC)     :   $result="Ook! Ook!";    break;
                case (BRAIN_RIGHT)   :   $result="Ook. Ook?";    break;
                case (BRAIN_LEFT)    :   $result="Ook? Ook.";    break;
                case (BRAIN_LOOP)    :   $result="Ook! Ook?";    break;
                case (BRAIN_POOL)    :   $result="Ook? Ook!";    break;
                case (BRAIN_WRITE)   :   $result="Ook! Ook.";    break;
                case (BRAIN_READ)    :   $result="Ook. Ook!";    break;
                }
            }
        return($result);
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Programm laden
    function loadinstructions($input)
        {
        //Um es etwas schneller zu machen, filtern wir alle Non-Brainfucks aus
        //Damit braucht es der Tokenizer nicht tun
        $input=preg_replace("/([^+-<>,\\[\\]\\.]+)/","",$input);

        //String in ein Array zur einfacheren Addressierung
        //Leerzeichen anfügen, um die Sprungvorhersage zu vereinfachen
        $this->instructions=preg_split("//",$input." ",-1,PREG_SPLIT_NO_EMPTY);

        //Lookuptables erzeugen
        $this->_lookup();

        //EOF-Flag anpassen
        $this->eof=!(boolean) count($this->instructions);
        
        //IP-Pointer resetten
        $this->ip=0;
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Sprungtabellen erzeugen, um die Ausführung zu beschleunigen
    //Hierzu werden Arrays mit Indizees zu den einzelnen zusammengehörenden
    //Schleifenbegrenzern erzeugt
    // loop[IP des Schleifenendes]  =IP des Schleifenbeginns
    // pool[IP des Schleifenbeginns]=IP des Schleifenendes
    function _lookup()
        {
        //Vorne anfangen
        $this->ip=0;

        while ( ($inst=$this->_next())!==FALSE)
            {
            //Schleife geht auf
            if ($inst=="[")
                {
                //Anfang merken (In Wirklichkeit eins hinter dem Beginn, da der IP
                //durch _next schon inkrementiert wird
                $start=$this->ip;

                //Passendes Ende suchen
                $level=0;
                do
                    {
                    if ($inst==BRAIN_LOOP) $level++;
                    if ($inst==BRAIN_POOL) $level--;
                    }
                while ( ( ($inst=$this->_next())!==FALSE) && ($level!=0) );

                //Einen zurück, da wir Fußgesteuert einen zu weit laufen
                $inst=$this->_previous();

                //Sprünge merken
                $this->internal_loop[$this->ip]=$start;
                $this->internal_pool[$start]=$this->ip;

                //Und wieder an den Anfang gehen
                $this->ip=$start;
                }
            }
        }



    ///////////////////////////////////////////////////////////////////////////////////////
    //Das ganze Programm ausführen
    function run()
        {
        //Zeitlimit großzügig bemessen
        @set_time_limit($this->timelimit * 2);
        $time=time()+$this->timelimit;
        $ti  =time();

        //Debugs löschen
        $this->dbg="";
        $this->log=array();
        
        do
            {
            //Debug ?
            if ($this->debug)
                {
                $this->dbg.="  IP=".$this->ip." Mem : ".$this->mp."=".$this->memory[$this->mp]."   ";
                $this->dbg.="<br/>\n";
                $this->log[]=$this->dbg;
                $this->dbg="";
                }
            }
        while ( ($this->step()) && ($time > time()) );
        
        //Zeiten übernehmen
        if ($time <= time())
            {
            $this->log[]="########### runtime exceeded ############";
            }
        $this->runtime=time()-$ti;
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Einen Schritt ausführen
    function step()
        {
        //Instruktion holen
        $inst=$this->_next();

        //Was gefunden ?
        if ($inst!==FALSE)
            {
            if ($this->debug) $this->dbg="Inst : ".$inst;
            
            //Befehle verarbeiten
            switch ($inst)
                {
                case (BRAIN_INC)   :  $this->_increase();     break;
                case (BRAIN_DEC)   :  $this->_decrease();     break;
                case (BRAIN_LEFT)  :  $this->_back();         break;
                case (BRAIN_RIGHT) :  $this->_forward();      break;
                case (BRAIN_READ)  :  $this->_read();         break;
                case (BRAIN_WRITE) :  $this->_write();        break;
                case (BRAIN_LOOP)  :  $this->_loop();         break;
                case (BRAIN_POOL)  :  $this->_pool();         break;
                }
            }
        else
            {
            $this->eof=TRUE;
            }
        return(!$this->eof);
        }
        

    ///////////////////////////////////////////////////////////////////////////////////////
    //Ab hier nur noch Instruktionen
    //Speicherstelle erhöhen
    function _increase()
        {
        $this->memory[$this->mp]++;

        //Da PHP leider nicht typisiert ist, müssen wir die Begrenzung selbst machen
        if ($this->memory[$this->mp]>255) $this->memory[$this->mp]=0;
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Speicherstelle erniedrigen
    function _decrease()
        {
        $this->memory[$this->mp]--;
        
        //Da PHP leider nicht typisiert ist, müssen wir die Begrenzung selbst machen
        if ($this->memory[$this->mp]<0) $this->memory[$this->mp]=255;
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Zeiger nach vorn
    function _forward()
        {
        $this->mp++;
        if ($this->mp >= count($this->memory))
            {
            $this->mp=0;
            }
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Zeiger nach hinten
    function _back()
        {
        $this->mp--;
        if ($this->mp < 0)
            {
            $this->mp=count($this->memory)-1;
            }
        }
        
    ///////////////////////////////////////////////////////////////////////////////////////
    //Nächstes Zeichen lesen
    function _read()
        {
        $this->memory[$this->mp]=ord(array_shift($this->input));
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Zeichen ausgeben
    function _write()
        {
        array_push($this->output,chr($this->memory[$this->mp]));
        }
        

    ///////////////////////////////////////////////////////////////////////////////////////
    //Schleife öffnen
    function _loop()
        {
        //Ist die Abbruchbedingung erfüllt ?
        if ($this->memory[$this->mp]==0)
            {
            //Aus den Lookups den neuen InstructionPointer setzen
            $this->ip=$this->internal_pool[$this->ip];
            }
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Schleife schließen
    function _pool()
        {
        //Ist die Abbruchbedingung erfüllt ?
        if ($this->memory[$this->mp]!=0)
            {
            //Aus den Lookups den neuen InstructionPointer setzen
            $this->ip=$this->internal_loop[$this->ip];
            }
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Nächste Instruktion holen
    function _next()
        {
        if (isset($this->instructions[$this->ip]))
            {
            $result=$this->instructions[$this->ip];
            $this->ip++;
            }
        else
            {
            $result=FALSE;
            }
        return($result);
        }

    ///////////////////////////////////////////////////////////////////////////////////////
    //Vorherige Instruktion holen
    function _previous()
        {
        $this->ip--;
        if (isset($this->instructions[$this->ip]))
            {
            $result=$this->instructions[$this->ip];
            }
        else
            {
            $result=FALSE;
            }
        return($result);
        }
    }
</script>