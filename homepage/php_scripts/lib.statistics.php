<script language="PHP">
////////////////////////////////////////////////////////////////////////////////
///
/// Kleine Bibliothek mit Statistischen Grundfunktionen
///
////////////////////////////////////////////////////////////////////////////////

$x=array();
$y=array();

for ($index=0; $index < 20;$index++)
    {
    $x[]=$index;
    $y[]=2 * $index + rand(0,1);
    }

$m=0;
$b=0;

regression($x,$y,$m,$b);


echo $m;
echo "<br>\n";
echo $b;
echo "<br>\n";







//Bestimmt die Regressionsgerade
//Übergeben werden die Datenreihen x und y,
//Zurückgeliefert werden dir Steigung m und dier Nullpunktverschiebung b
function regression($xarray,$yarray,&$m,&$b)
    {
    //Fehler abfangen
    if ( (!is_array($xarray)) ||
         (!is_array($yarray)) )
        {
        return(FALSE);
        }

    if ( (count($xarray)==0) ||
         (count($yarray)==0) )
        {
        return(FALSE);
        }

    //Mittelwerte bestimmen
    $xmid=array_sum($xarray) / count($xarray);
    $ymid=array_sum($yarray) / count($yarray);
    
    //Und nun die Partialsummen bestimmen
    $zaehler=0;
    $nenner =0;
    $xysum=0;
    $count=0;

    $x=reset($xarray);
    $y=reset($yarray);
    while ( ($x!==FALSE) && ($y!==FALSE) )
        {
        //Partialsummen
        $xdiff=$x - $xmid;
        $ydiff=$y - $ymid;
        
        //Zaehler und Nenner bestimmen
        $zaehler+=$xdiff * $ydiff;
        $nenner +=$xdiff * $xdiff;

        //Nächste Werte ziehen
        $x=next($xarray);
        $y=next($yarray);
        $count++;
        }

    //Endberechnung
    $m=$zaehler / $nenner;
    $b=$ymid - ($m * $xmid);
    
    return(TRUE);
    }

/// Winkeldifferenzen zweier Geraden bestimmen
/// m = Steigung / b = Nullpunkverschiebung
function linediff($m1,$b1,$m2,$b2)
    {




    }



</script>