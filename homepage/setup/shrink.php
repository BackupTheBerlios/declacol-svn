<pre>
<script language="php">
/*
 _|    _|            _|                              _|                _|
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|

(c) 2008 Borg@sven-of-nine.de
*/
////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Kopiert alle Dateien aus dem Setup-Ordner in einen Zielordner und stripped dabei die php-seiten
///
////////////////////////////////////////////////////////////////////////////////////////////////////
$sourcepath="./site/";
$targetpath="./target/";

$dirs=getdirs($sourcepath);
$dirs[]=$sourcepath;

//Nach Länge sortieren, um die Verzeichnisse oben zu haben
usort($dirs,"lencmp");

foreach ($dirs as $source)
    {
    $target=str_replace($sourcepath,$targetpath,$source);
    
    if (substr($source,-1,1)=="/")
        {
        echo "mkdir ".$source;
        if (is_dir($target)==FALSE) mkdir($target);
        }
    else
        {
        if (substr($source,-4,4)==".php")
            {
            echo "shrink ".$source;
            file_put_contents($target,php_strip_whitespace($source));
            }
        else
            {
            echo "copy ".$source;
            if (file_exists($target)) unlink($target);
            copy($source,$target);
            }
        }

    echo "\n";
    }

function getdirs($source)
    {
    $result=array();

    $dirs=scandir($source);
    
    foreach ($dirs as $entry)
        {
        //Traverse und Subversion ignorieren
        if ( ($entry != ".") && ($entry!="..") && ($entry!=".svn") )
            {
            if ( is_dir($source.$entry)==TRUE )
                {
                $result[]=str_replace("\\","/",$source.$entry."/");
                $result=array_merge($result,getdirs($source.$entry."/"));
                }
            else
                {
                $result[]=str_replace("\\","/",$source.$entry);
                }
            }
        }
    return( array_unique($result) );
    }


function lencmp($a,$b)
    {
    $lena=strlen($a);
    $lenb=strlen($b);
    if ($lena==$lenb)
        {
        return(0);
        }
    return ( ($lena < $lenb?-1:1 ));
    }

</script>