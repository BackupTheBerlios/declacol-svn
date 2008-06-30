<script language="PHP">
////////////////////////////////////////////////////////////////////////////////
///
/// Alle Funktionen die mathematischer Natur sind
///
/// Hautpsächlich umformatierungen
///
///
////////////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("LIB_MATH","lib_math");
define ("LIB_MATH_VERSION","0.01");
if (isset($debug)) $debug->add(LIB_MATH,"version ".LIB_MATH_VERSION);

//Ein paar Standarddefinitionen
define ("MATH_PI"       ,3.141592);
define ("MATH_2PI"      ,6.283184);
define ("MATH_SQR_PI"   ,9.869600);
define ("MATH_SQRT_PI"  ,1.772454);
define ("MATH_SQRT_2PI" ,2.506628);

//Auf die nächste Dekade auf oder abrunden
function decade($input,$up)
    {
    if ($up===TRUE)
        {
        $result=ceil($input/10)*10;
        return($result+10);
        }
    else
        {
        $result=floor($input/10)*10;
        return($result-10);
        }
    }

//Das Nummernformat aller Zahlen in einem Aray tauschen
function math_format($data)
    {
    if (!is_array($data))
        {
        return(FALSE);
        }
    //Inhalte
    foreach ($data as $outerindex => $row)
        {
        foreach ($row as $innerindex => $cell)
            {    
            $data[$outerindex][$innerindex]=number_format($cell,2,",","");
            }
        }
    return($data);
    }
    
//Normalverteilung bestimmen
function gauss($x,$middle=0,$max=1)
    {
    $result=1 / MATH_SQRT_2PI * exp(-pow($x,2) / 2);
    
    //auf den Mittelpunkt setzen
    $result += $middle;
    
    //Maximum anpassen
    if ($result!=0)
        {
        $result *= ($max / 0.4);
        }
    return($result);
    }
</script>