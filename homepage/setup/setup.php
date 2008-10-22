<pre>
<script language="PHP">
require_once("class.installer.php");
$basepath="./";

echo "EasyGoing Installer\n<hr/>";

//Alle verfügbaren Pakete auflisten

if (isset($_GET["install"])==TRUE)
    {
    $package=basename(base64_decode($_GET["install"]));
    if (is_readable($basepath.$package))
        {
        $installer=new installer();
        echo "testing package ";
        $installer->test($basepath.$package);
        if ($installer->lasterror==ERROR_NONE)
            {
            echo " ok\n";
            echo "installing ";
            $installer->unpack($basepath.$package,$basepath);
            if ($installer->lasterror==ERROR_NONE)
                {
                echo " ok\n";
                echo "don't forget to remove setup.php, class.installer.php and the phar-packages!";
                echo "<a href=\"./\">done</a><br/>";
                }
            else
                {
                echo " failed";
                }
            }
        else
            {
            echo " failed";
            }
        }
    }
else
    {
    echo "available packages :\n";
    $packages=getpackages($basepath);
    foreach ($packages as $package)
        {
        echo "<a href=\"?install=".base64_encode($package)."\">".$package."</a><br/>";
        }
    echo "<hr>";
    }

function getpackages($path)
    {
    $result=array();
    
    $handle=opendir($path);
    if ($handle!=FALSE)
        {
        $entry=readdir($handle);
        while ($entry!=FALSE)
            {
            if (strpos($entry,".phar")!==FALSE)
                {
                $result[]=$entry;
                }
            $entry=readdir($handle);
            }

        
        closedir($handle);
        }
    return($result);
    }
</script>
</pre>