<script language="php">
////////////////////////////////////////////////////////////////////////////////
/// Ein paar schöene Grafikfunktionen
///
///
////////////////////////////////////////////////////////////////////////////////

#TODO SO9 Funktion für Wasserzeichen einfügen
#TODO SO9 Funktion für Sigatur einfügen

////////////////////////////////////////////////////////////////////////////////
//Die Weite eine Grafik in Pixeln. Aber nicht mehr als Maximum
function image_width($filename,$maximum=0)
    {
    $result=FALSE;

    $size=getimagesize($filename);

    if ($size!=FALSE)
        {
        $result=reset($size);
        }

    //Weite evtl. auf Maximum beziehen
    if ( ($maximum < $result) && ($result!=FALSE) )
        {
        $result=$maximum;
        }

    return($result);
    }


////////////////////////////////////////////////////////////////////////////////
//Die Höhe eine Grafik in Pixeln. Aber nicht mehr als Maximum
function image_height($filename,$maximum=0)
    {
    $result=FALSE;

    $size=getimagesize($filename);

    if ($size!=FALSE)
        {
        $result=reset($size);
        $result=next ($size);
        }

    //Weite evtl. auf Maximum beziehen
    if ( ($maximum < $result) && ($result!=FALSE) )
        {
        $result=$maximum;
        }

    return($result);
    }

////////////////////////////////////////////////////////////////////////////////
//Die Breite einer Grafik bei gegebener Höhe bestimmen
function image_calc_width($filename,$height)
    {
    $result=FALSE;

    //Weite holen
    $size=getimagesize($filename);

    if ($size!=FALSE)
        {
        $oldwidth =reset($size);
        $oldheight=next ($size);

        //Ratio einbeziehen
        $result=round(($oldwidth / $oldheight) * $height,0);
        }
    return($result);
    }

////////////////////////////////////////////////////////////////////////////////
//Die Höhe einer Grafik bei gegebener Weite bestimmen
function image_calc_height($filename,$width)
    {
    $result=FALSE;

    //Weite holen
    $size=getimagesize($filename);
    if ($size!=FALSE)
        {
        $oldwidth =reset($size);
        $oldheight=next($size);

        //Ratio einbeziehen
        $result=round(($oldheight / $oldwidth) * $width,0);
        }
    return($result);
    }

////////////////////////////////////////////////////////////////////////////////
//Seitenverhältnis bestimmen
function image_calc_ratio($filename)
    {
    $result=FALSE;

    //Weite holen
    $size=getimagesize($filename);
    if ($size!=FALSE)
        {
        $oldwidth =reset($size);
        $oldheight=next($size);

        //Ratio einbeziehen
        $result=$oldwidth / $oldheight;
        }
    return($result);
    }

////////////////////////////////////////////////////////////////////////////////
//Thumbnail einer Grafik erzeugen
function image_thumb($filename,$thumbfilename,$width,$height)
        {
        $result=FALSE;
        //Die Ausmaße holen
        $ratio=image_calc_ratio($filename);

        //Hochkant ?
        if ($ratio>1)
            {
            $height=round($width / $ratio,0);
            }
        else
            {
            $width =round($height * $ratio,0);
            }
            
        $image_old=FALSE;
        switch (strtolower(string_extractfileext($filename)))
            {
            case ("png")   :  $type="png";  $image_old=imagecreatefrompng ($filename); break;
            case ("jpg")   :  $type="jpg";  $image_old=imagecreatefromjpeg($filename); break;
            case ("jpeg")  :  $type="jpeg"; $image_old=imagecreatefromjpeg($filename); break;
            default         :  $type="png"; break;
            }


        if ($image_old!=FALSE)
            {
            $image_new=imagecreatetruecolor($width,$height);
            //Resampling
            if (imagecopyresampled($image_new,$image_old,0,0,0,0,$width,$height,imagesx($image_old),imagesy($image_old)))
                {
                //Thumb abspeichern
                @unlink ($thumbfilename);
                switch ($type)
                    {
                    case ("png")   :  imagepng  ($image_new,$thumbfilename); break;
                    case ("jpg")   :  imagejpeg ($image_new,$thumbfilename); break;
                    case ("jpeg")  :  imagejpeg ($image_new,$thumbfilename); break;
                    }
                imagedestroy($image_old);
                imagedestroy($image_new);

                $result=TRUE;
                }
            }
        return($result);
        }


</script>