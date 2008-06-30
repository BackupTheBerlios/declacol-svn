<script language="php">
////////////////////////////////////////////////////////////////////////////////
/// Ein paar schöene Farbfunktionen
///
/// Sammlung einiger Farbverwaltungsfunktionen.
/// z.B einen Farbverlauf bestimmen etc.
///
///
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////
//Klasse für RGB-Farben
class rgb
    {
    var $r = 0;
    var $g = 0;
    var $b = 0;

    //Konstruktor
    function rgb($r=0,$g=0,$b=0)
        {
        $this->setrgb($r,$g,$b);
        }

    //Den HTML-Code holen
    function html()
        {
        return(sprintf("#%02x%02x%02x",$this->r,$this->g,$this->b));
        }
        
    //Den Grauwert bestimmen
    function gray()
        {
        return(round($this->r * 0.299 + $this->g * 0.587 + $this->b * 0.114));
        }
        
    //RGB direkt setzen
    function setrgb($r,$g,$b)
        {
        $this->_limit($r,$g,$b);
        }
        
    //Differenz addieren
    function addlight($diff)
        {
        $this->_limit($this->r + $diff,$this->g + $diff,$this->b + $diff);
        }

    function lighted($diff)
        {
        //Und sebst kopieren
        $result=$this;
        
        //Licht drauf
        $result->_limit($this->r + $diff,$this->g + $diff,$this->b + $diff);
        
        //Fertig
        return($result);
        }

    //Farbraum begrenzen
    function _limit($r,$g,$b)
        {
        //Farbraum begrenzen
        $r=abs($r);
        $g=abs($g);
        $b=abs($b);

        if ($r > 255) $r=255;
        if ($g > 255) $g=255;
        if ($b > 255) $b=255;

        $this->r=round($r,0);
        $this->g=round($g,0);
        $this->b=round($b,0);
        }
    }

////////////////////////////////////////////////////////////////////////////////
//Spaltet einen HTML-Farbwert in seine Kanäle auf
function colorexplode($input)
    {
    //Schwarz vorgeben
    $result=array(0,0,0);

    //Fabrstring zerlegen
    $split=array();
    if (preg_match("/#([a-zA-Z0-9]{2})([a-zA-Z0-9]{2})([a-zA-Z0-9]{2})/",$input,$split)==1)
        {
        //Und die Komponenten zuordnen
        $result[0]=hexdec($split[1]);
        $result[1]=hexdec($split[2]);
        $result[2]=hexdec($split[3]);
        }

    return($result);
    }

////////////////////////////////////////////////////////////////////////////////
//Setzt aus RGB einen HTML-Wert zusammen
function colorimplode($input)
    {
    if (count($input)==3)
        {
        return(sprintf("#%02x%02x%02x",$input[0],$input[1],$input[2]));
        }
    else
        {
        return(FALSE);
        }
    }

////////////////////////////////////////////////////////////////////////////////
//Bestimmt zwischen zwei Farben die Different
function colordiff($color1,$color2)
    {
    $result=array(0,0,0);

    $result[0]=$color2[0]-$color1[0];
    $result[1]=$color2[1]-$color1[1];
    $result[2]=$color2[2]-$color1[2];

    return($result);
    }

//Den Grauwert einer Farbe bestimmen
//TODO
function colorgray($color)
    {
    //Erstmal mit dem Mittelwert arbeiten
//    return(array_sum($color) / count($array));
    return($color);
    }


//Faded entsprechend der Position zwischenm Min und Max die Farbwert zwischen MinCol und MaxCol
function colorgradient($min,$max,$pos,$mincol,$maxcol)
    {
    //Die Farben auflösen
    $col_min=colorexplode($mincol);
    $col_max=colorexplode($maxcol);
    $col_pos=array(0,0,0);

    //Farbdifferenzen bestimmen
    $col_diff=colordiff($col_min,$col_max);


    //Auf 0 normieren
    $max-=$min;
    $pos-=$min;
    $min=0;

    //Abstand in Prozent bestimmen
    $position=$pos / $max;

    //Die Endfarbe bestimmen
    $col_pos[0]=$col_min[0] +  round($col_diff[0] * $position);
    $col_pos[1]=$col_min[1] +  round($col_diff[1] * $position);
    $col_pos[2]=$col_min[2] +  round($col_diff[2] * $position);

    //Auf 255 normieren
    if ($col_pos[0]>255) $col_pos[0]=255;
    if ($col_pos[1]>255) $col_pos[1]=255;
    if ($col_pos[2]>255) $col_pos[2]=255;

    if ($col_pos[0]<1) $col_pos[0]=0;
    if ($col_pos[1]<1) $col_pos[1]=0;
    if ($col_pos[2]<1) $col_pos[2]=0;

    return(colorimplode($col_pos));
    }
</script>