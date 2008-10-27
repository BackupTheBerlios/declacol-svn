<?php

/**
 * Text_Figlet example script.
 *
 * Renders "Hello, world!" using slant.flf font
 *
 * @package Text_Figlet
 */

include_once ("./site1432/classes/class.figlet.php");

$figlet = new Text_Figlet();
echo "<pre>";
$out="";
$fonts=scandir("./site1432/files/figlets/");
foreach ($fonts as $font)
  {
  if (strpos($font,".flf")!==FALSE)
    {
    $retcode  = $figlet->LoadFont("./site1432/files/figlets/".$font);
    if ($retcode === TRUE)
      {
      $out.="<b>".$font."</b>\n";  
      $out.= $figlet->LineEcho("Guru-Meditation");
      $out.="\n===================================================================================================";
      $out.="===================================================================================================\n";
      }
    else
      {
      echo 'Error: ', $retcode['message'];
      }
    }
  }
file_put_contents("fonts.txt",$out);
echo $out;
?>
