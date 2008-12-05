<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Der Profiler misst die Laufzeit von Funktionen und erlaubt so
/// eine Analyse des Optimierungsbedarfs
///
/// Am Beginn einer Funktion einfach
/// $profid=$GLOBALS["profiler"]->start("name");
/// Am Ende der Funktion
/// $GLOBALS["profiler"]->stop($profid);
///
/// Am Ende des Testlaufes einfach die Analyse starten
/// $GLOBALS["profiler"]->analyze() oder für eine einzelne Funktion
/// $GLOBALS["profiler"]->analyze("name");
/// $GLOBALS["profiler"]->analyze(array("name1","name2")
//////////////////////////////////////////////////////////////////////////
//Direkt beim Laden instanzieren
//Quasi als Singleton, was unter PHP4 nur extrem unelegant zu handhaben ist
if (!isset($GLOBALS["profiler"])) $GLOBALS["profiler"]=new profiler;

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
//Da der Profiler einigermaßen schnell sein sollte, um Testläufe nicht
//unnötig lang zu machen, verzichten wir hier kpl. auf eine aufwendige
//Klassenstruktur sondern speicher die Daten platt in Arrays ab
class profiler
    {
    //Öffentliche Eigenschaften
    var $performance=array();   //Ergebnisarray nach dem Analyselauf
    var $file    =FALSE;        //file=Dateiname speichert die Analyse ab
    var $active  =TRUE;         //Flag, ob mitgeschnitten werden soll
    var $output  =FALSE;        //Sollen Daten per Log oder Datei ausgegeben werden

    //Private Eigenschaftem
    var $perftime=array();      //Array mit den Laufzeiten
    var $perfname=array();      //Array mit den Modulnamen und ihnen zugeordneten Laufzeiten

    //Indexzähler für Calls
    var $perfcount  =0;         //Indexzähler

    //Linebreak
    var $linebreak="\r\n";      //Art der benutzten Zeilenumbrüche

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function profiler()
        {
        //Alle Werte auf Default setzen
        $this->reset();
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->reset();
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->perfcount=0;
        $this->performance=array();
        $this->perftime =array();
        $this->perfname =array();
        return(TRUE);
        }
    
    //////////////////////////////////////////////////////////////////////////
    //Den Profiler resetten
    function reset()
        {
        //Die Ausgabedatei löschen
        if (is_writable($this->file)) unlink($this->file);

        //Defaults herstellen
        $this->setdefault();
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    //Den Profiler initialisieren
    function init()
        {
        $this->reset();
        return(TRUE);
        }

    //////////////////////////////////////////////////////////////////////////
    //Den Counter starten
    function start($modulename)
        {
        //Nur mitlesen, wenn es gewünscht ist
        if (!$this->active) return(FALSE);

        //Den Index der Messung holen
        $index=$this->perfcount++;
        
        //Und den Timestamp in einem Array abspeichern
        $this->perftime[$index]=$this->usecs();

        //Den Modulnamen abspeichern
        $this->perfname[$modulename][]=$index;

        //Id zurückliefern
        return($index);
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Den Counter beenden
    //Die Analyse erfolgt erst durch Aufruf des Benutzers
    function stop ($index)
        {
        //Nur mitlesen, wenn es gewünscht ist
        if (!$this->active) return(FALSE);

        //Den Lauf beenden
        $this->perftime[$index]=$this->usecs() - $this->perftime[$index];
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Der Analyselauf
    function analyze($modulename="")
        {
        //Ein Modul angefragt ?
        if ($modulename!="")
            {
            //Mehrere Module angefragt ?
            if (is_array($modulename))
                {
                $modules=$modulename;
                }
            else
                {
                $modules=array($modulename);
                }
            }
        else
            {
            $modules=array_keys($this->perfname);
            }

        //Nach Namen sortieren
        asort($modules);

        //Jedes Gerät analysieren
        $this->out(sprintf("%-32s  %-10s %-10s %-10s %-10s %-10s","modulename","calls","sum","min","max","mid"));
        foreach($modules as $name)
            {
            $this->_analyze($name);
            }
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////
    //Die Laufzeiten eines Modules lesen und Werte bestimmen
    function _analyze($module)
        {
        //Modul auch existent ?
        if (isset($this->perfname[$module]))
            {
            //Anzahl der Läufe
            $count=count($this->perfname[$module]);
            
            $max=0;
            $min=0xfffffff;
            $sum=0;
            //Zeiten holen
            foreach ($this->perfname[$module] as $index)
                {
                //Gesamlaufzeit
                $sum+=$this->perftime[$index];
                
                //Minimum / Maximum
                if ($this->perftime[$index] < $min) $min=$this->perftime[$index];
                if ($this->perftime[$index] > $max) $max=$this->perftime[$index];
                }

            //mittlere Laufzeit
            $mid=$sum / $count;

            //Die Daten intern auch speichern
            $this->performance[$module]["count"]=$count;
            $this->performance[$module]["sum"]  =$sum;
            $this->performance[$module]["min"]  =$min;
            $this->performance[$module]["max"]  =$max;
            $this->performance[$module]["mid"]  =$mid;

            //Und alles schön ausgeben
            $this->out(sprintf("%-32s  %-10d %-10f %-10f %-10f %-10f",$module,$count,$sum,$min,$max,$mid));
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Einen Textausgeben
    function out($text)
        {
        if ($this->output)
            {
            if ($this->file!=FALSE)
                {
                $fp=fopen($this->file,"a+");
                if ($fp!=FALSE)
                    {
                    fwrite($fp,$text.$this->linebreak);
                    fclose($fp);
                    }
                }
            else
                {
                echo $text.$this->linebreak;
                }
            }
        }

    //Timestamp mit Microsekunden erzeugen
    function usecs()
        {
        //Thx to Marius
        list($usec, $sec) = explode(' ',microtime());
        return ((float)$usec + (float)$sec);
        }
    }

</script>