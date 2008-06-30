<script language="php">
//////////////////////////////////////////////////////////////////////////
// Extrem vereinfachte Stackklasse
//
// Da PHP schwach typisiert ist, kann man alles (sogar Klassen)
// in den Stack schieben, ohne mit Zeigern etc. hantieren zu müssen
//
// Allerdings verlieren Klassen nach einem Load ihren Methoden
//
//////////////////////////////////////////////////////////////////////////
class stack
    {
    var $internal_stack = FALSE;
    
    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function stack()
        {
        $this->clear();
        //Der Welt schönster Destruktor
        unset($this);
        }

    //////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        unset ($this->internal_stack);
        }

    //////////////////////////////////////////////////////////////////////////
    //Einen Wert auf den Stack legen
    function push($data)
        {
        array_push($this->internal_stack,$data);
        }

    //////////////////////////////////////////////////////////////////////////
    //Einen Wert vom Stack holen
    function pop()
        {
        return(array_pop($this->internal_stack));
        }

    //////////////////////////////////////////////////////////////////////////
    //Stack löschen
    function clear()
        {
        unset ($this->internal_stack);
        $this->internal_stack=array();
        }
        
    //////////////////////////////////////////////////////////////////////////
    //Stack speichern
    //ACHTUNG : Klassen verlieren ihre Methoden
    function save($filename)
        {
        $fp=fopen($filename,"w+");
        if ($fp!=FALSE)
            {
            $data=reset($this->internal_stack);
            while ($data !== FALSE)
                {
                //In einen String wandeln
                fputs($fp,serialize($data)."\n");
                $data=next($this->internal_stack);
                }
            fclose($fp);
            }
        }

    //////////////////////////////////////////////////////////////////////////
    //Stack laden
    //ACHTUNG : Klassen verlieren ihre Methoden
    function load($filename)
        {
        //Und alles einlesen
        $fp=fopen($filename,"r");
        if ($fp!=FALSE)
            {
            //Stack löschen
            $this->clear();

            //Daten einlesen
            while (!feof($fp))
                {
                $this->internal_stack[]=unserialize(trim(fgets($fp)));
                }
            fclose($fp);
            }
        }
    }
</script>