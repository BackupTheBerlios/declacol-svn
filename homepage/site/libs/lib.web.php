<script language="PHP">
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
/// Diverse Webfunktionen
/// die Das Leben leichter machen
///
//////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////
//Schickt eine Postanfrage an eine Internetseite und gibt die Antwort als String zurück
//Values muß ein Array mit werten nach dem Muster name=>datum sein
function web_post($url,$values,$cutheader=TRUE,$timeout=20)
    {
    if (!is_array($values))
        {
        return(FALSE);
        }

    $result=FALSE;
    $errno =0;
    $errstr="";

    //Daten aufbereiten
    //HTTP raus
    $url = preg_replace("@^http://@i", "", $url);
    //Nur den Host greifen
    $host = substr($url, 0, strpos($url, "/"));
    //Der komplette Pfad
    $uri = strstr($url, "/");

    //Alle Werte hintereinanderkleben
    $postdata = "";

    foreach ($values as $name => $data)
        {
        //Werte durch & trennen
        if ($postdata!="")
            {
            $postdata.= "&";
            }
        //Und zusammensetzen
        $postdata.= $name."=".$data;
        }

     //Jetzt haben wir alles zusammengesetzt, nun bauen wir die Abfrage
     $contentlength = strlen($postdata);
     $header =  "POST ".$uri." HTTP/1.1\r\n".
                "Host: ".$host."\r\n".
                "User-Agent: Unknown\r\n".
                "Content-Type: application/x-www-form-urlencoded\r\n".
                "Content-Length: ".$contentlength."\r\n\r\n".
                $postdata."\r\n";

      //Da wir direkt zugreifen müssen, funktionieren solche sachen wir Readfile oder so
      //nicht. Wir brauchen also einen Socketzugriff
      $socket = fsockopen($host, 80, $errno, $errstr);

      //Verbindung ?
      if ($socket)
        {
        //Ja, dann abschicken
        fputs($socket, $header);

        $result="";

        //Und auf die Antwort warten
        //Aber nicht länger als X Sekunden
        @set_time_limit($timeout * 2);
        $runtime=time();
        while ( (!feof($socket)) && ( time()-$runtime < $timeout ) )
            {
            $result.= fgets($socket, 1024);
            }

        //Soll der Header automatisch entfernt werden ?
        if ($cutheader)
            {
            $result=substr($result,strpos($result,"\r\n\r\n"),strlen($result));
            }

        //Verbindung schließen
        fclose($socket);
        }
    //Fertig
    return($result);
    }


////////////////////////////////////////////////////////////////////////////////////////////////////
//Schickt eine Postanfrage an eine Internetseite und gibt die Antwort als String zurück
//Values muß ein Array mit werten nach dem Muster name=>datum sein
function web_xmlrpc($url,$xml,$cutheader=TRUE,$timeout=20)
    {
    $result=FALSE;
    $errno =0;
    $errstr="";

    //Daten aufbereiten
    //HTTP raus
    $url = preg_replace("@^http://@i", "", $url);
    
    //Nur den Host greifen
    $host= preg_replace("@[^a-z0-9.]*[?/]+[\w\W]*@","",$url);
    
    //Der komplette Pfad
    $uri = strstr($url, "/");

     //Jetzt haben wir alles zusammengesetzt, nun bauen wir die Abfrage
     $contentlength = strlen($xml);
     $header =  "POST ".$uri." HTTP/1.0\n".
                "Host: ".$host."\n".
                "User-Agent: Unknown\n".
                "Content-Type: text/xml\n".
                "Content-Length: ".$contentlength."\n\n".
                $xml."\n";

      //Da wir direkt zugreifen müssen, funktionieren solche sachen wir Readfile oder so
      //nicht. Wir brauchen also einen Socketzugriff
      $socket = fsockopen($host, 80, $errno, $errstr);

      //Verbindung ?
      if ($socket)
        {
        //Ja, dann abschicken
        fputs($socket, $header);

        $result="";

        //Und auf die Antwort warten
        //Aber nicht länger als X Sekunden
        @set_time_limit($timeout * 2);
        $runtime=time();
        while ( (!feof($socket)) && ( time()-$runtime < $timeout ) )
            {
            $result.= fgets($socket, 1024);
            }

        //Soll der Header automatisch entfernt werden ?
        if ($cutheader)
            {
            $result=substr($result,strpos($result,"\r\n\r\n"),strlen($result));
            }

        //Verbindung schließen
        fclose($socket);
        }
    //Fertig
    return($result);
    }

////////////////////////////////////////////////////////////////////////////////////////////////////
//Schickt eine GET Anfrag an eine Internetseite und gibt die Antwort als STring zurück
//Values muß ein Array mit werten nach dem Muster name=>datum sein
function web_get($url,$values)
    {
    if (!is_array($values))
        {
        return(FALSE);
        }

    $result=FALSE;

    //Alle Werte hintereinanderkleben
    $getdata = "";

    foreach ($values as $name => $data)
        {
        //Werte durch & trennen
        if ($getdata!="")
            {
            $getdata.= "&";
            }
        //Und zusammensetzen
        $getdata.= $name."=".urlencode($data);
        }

    //Daten einfach an die URL ankleben
    $url=$url."?".$getdata;

    //Und per File abschicken
    $result=file_get_contents($url);

    return($result);
    }
    
////////////////////////////////////////////////////////////////////////////////////////////////////
//Eine Datei pushen
//Funktioniert nur, wenn vorher keine Daten geschicket wurden
function web_push_file($filepath,$name=FALSE,$maxspeed=50)
    {
    $result=FALSE;

    //Wenn kein Name übergeben wurde, den Dateinamen nehmen
    if ($name==FALSE) $name=basename($filepath);
    $name=str_replace(" ","_",$name);

    //Prüfen, ob es ihn gibt
    if (file_exists($filepath))
        {
        require_once(PATH_LIBS."lib.mime.php");

        //Unseren Header schicken
        header("Content-Description: File Transfer",TRUE);
        header("Content-Type: ".mime_get_type($name),TRUE);
        header("Content-Length: ".filesize($filepath),TRUE);
        header("Content-Transfer-Encoding: binary",TRUE);
        header("Content-Disposition: attachment; filename=".$name,TRUE);

        //Speed gewählt ?
        if ($maxspeed<1)
            {
            $size=1024 * 1024;
            }
        else
            {
            $size=$maxspeed * 1024;
            }

        //Und die Datei gleich hinterher
        $fp=fopen($filepath,"rb");
        if ($fp!=FALSE)
            {
            while (!feof($fp))
                {
                @set_time_limit(30);
                if ($maxspeed>0)
                    {
                    sleep(1);
                    }
                $out = fread($fp, $size);
                print ($out);
                flush();
                }
            $result=TRUE;
            }
        }
    return($result);
    }


////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen String pushen
//Funktioniert nur, wenn vorher keine Daten geschicket wurden
function web_push_string($string,$name=FALSE)
    {
    //Wenn kein Name übergebn wurde, einen Defaultnamen nehmen
    if ($name==FALSE) $name="data.bin";

    //Unseren Header schicken
    header("Content-Description: File Transfer",TRUE);
    header("Content-Type: application/octet-stream",TRUE);
    header("Content-Length: ".strlen($string),TRUE);
    header("Content-Transfer-Encoding: binary",TRUE);
    header("Content-Disposition: attachment; filename=".$name,TRUE);

    //Und die Daten gleich hinterher
    print ($string);
    }

////////////////////////////////////////////////////////////////////////////////////////////////////
//Auf eine neue Seite umleiten
//Funktioniert nur, wenn vorher keine Daten geschicket wurden
function web_redirect($newurl)
    {
    header("Location: ".$newurl);
    }

</script>