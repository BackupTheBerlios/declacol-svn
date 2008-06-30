<script language="PHP">
/////////////////////////////////////////////////////////////////////////////////
///
/// Klasse um flexibel eine Kalenderausgabe zu machen
///
///
///
///
/////////////////////////////////////////////////////////////////////////////////
class calender
    {
    var $data =array(array());  //Array für die Daten
    var $names=array();         //Array für die Monatsnamen
    
    var $mode ="year";         //Was und wie soll es dargestellt werden

    var $minlines = 12;         //Minimale Höhe einer Zelle

    var $curlines = 0;          //Interner Zähler
    
    function calender()
        {
        $this->setdefault();
        }

    function destroy()
        {
        $this->setdefault();
        unset($this);
        }

    function setdefault()
        {
        //Das Datenarray mit den Monaten füllen
        unset($this->data);
        unset($this->names);
        for ($month=1; $month < 13;$month++)
            {
            //Datenarray initialisieren
            $this->data[$month]=array();
            //Monatsnamen vorgeben
            $this->names[$month]=date("F",mktime(0,0,0,$month,1,2000));
            }
        $this->minlines=12;
        }
        
        
    function add($month,$data)
        {
        if (isset($this->data[$month]))
            {
            //Und ablegen
            $this->data[$month][]=$data;
            }
        }
        
    function show()
        {
        global $html;

        $data=reset($this->data);
        $html->table_open("","module");
        $html->row_open("");
        $m=0;
        while ($data!==FALSE)
            {
            //Nach dem halben Jahr umbrechen
            if ($m==6)
                {
                $html->row_close();
                $html->row_open();
                }

            //Eine Zelle
            $html->cell_open("calender");
            $html->bold($this->names[key($this->data)]);
            $html->line();
            
            $this->curlines=0;
            $this->_output($data);

            //Auf ein Mindestmaß an Zeilen auffüllen
            $lines=$this->minlines - $this->curlines;
            if ($lines>0) $html->nextline($lines);

            $html->cell_close();

            //Nächster Monat
            $data=next($this->data);
            $m++;
            }
        $html->row_close();
        $html->table_close();
        }
        
    //
        
    //Ein Array oder Datum recursiv ausgeben
    function _output($array,$key=FALSE)
        {
        global $html;
        if (is_array($array))
            {
            array_walk($array,array(&$this,"_output"));
            }
        else
            {
            $html->text_parsed($array);
            $html->nextline();
            $this->curlines++;
            }
        }
    }
</script>