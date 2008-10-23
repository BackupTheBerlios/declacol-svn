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
///
///
///
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("LIB_WEB","lib_web");
define ("LIB_WEB_VERSION","0.03");
if (isset($debug)) $debug->add(LIB_WEB,"version ".LIB_WEB_VERSION);

////////////////////////////////////////////////////////////////////////////////////////////////////
//Einige Libs erzwingen
require_once("lib.crypt.php");


////////////////////////////////////////////////////////////////////////////////////////////////////
//Prüfen, ob ein Useragent ein Bot ist
function web_isbot($agent="")
    {
    $bot_array      =array("baidu","nhnnoon","mjbotv","twiceler","msnbotmedia","seekbot","jeevesteoma","msnbot","slurp","jeevestemoa","gulper","googlebot","linkwalker","validator","webaltbot","wget");
    if ($agent=="")
        {
        @$agent=$_SERVER["HTTP_USER_AGENT"];
        }

    $agent=strtolower(string_filter($agent,FILTER_ALPHA));
    return((BOOL)count(array_intersect(explode(" ",$agent),$bot_array)));
    }

//////////////////////////////////////////////////////////////////////////////////
/// Den Useragenten extrahieren
function web_get_useragent()
    {
    //Alle Agenten
    $agent_array  =array("mjbotv","mozilla","opera","firefox","safari","msie","gecko","epiphany","galeon","konquerer","netpositive");
    //Alle Bots
    $bot_array    =array("baidu","nhnnoon","twiceler","msnbotmedia","seekbot","jeevesteoma","msnbot","slurp","jeevestemoa","gulper","googlebot","linkwalker","validator","webaltbot","wget");
    //Alle Systeme
    $system_array =array("windows","macintosh","linux","redhat","bsd","zeta","beos","macpowerpc");

    //Agenten holen
    if (isset($_SERVER["HTTP_USER_AGENT"]))
        {
        $agent=$_SERVER["HTTP_USER_AGENT"];
        }
    else
        {
        $agent="unknown";
        }

    //Den Agenten cleanen
    $agentalpha=strtolower(string_filter($agent,FILTER_ALPHA));

    //In seine Teile zerlegen
    $agentalpha=explode(" ",$agentalpha);

    //Und durchprüfen
    $browser=implode(" ",array_unique(array_intersect($agentalpha,$agent_array)));
    $bot    =implode(" ",array_unique(array_intersect($agentalpha,$bot_array)));
    $system =implode(" ",array_unique(array_intersect($agentalpha,$system_array)));

    //Wenn ein Teil nicht gesetzt ist, dann den ganzen Agenten anzeigen
    if ( ($system=="") || ($browser=="") )
        {
        //Evtl. ein bot ?
        if ($bot!="")
            {
            $result="[BOT] ".$bot;
            }
        else
            {
            $result=implode(" ",$agentalpha);
            }
        }
    else
        {
        $result=$browser." ".$system;
        }
    return($result);
    }



////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen Port checken
function web_checkport($url,$port)
    {
    //Ein evtl. Vorhandenes Protokoll rausnehmen
    $url=preg_replace(array("#(.*://)#","#(.*:\\\\)#"),"",$url);

    $ip=gethostbyname($url);
    //Einmal HTTP
    $error=FALSE;
    $errorstring="";
    @$socket=fsockopen($ip,$port,$error,$errorstring,1);

    //Und auswerten
    if ($socket===FALSE)
        {
        return(FALSE);
        }
    else
        {
        fclose($socket);
        return(TRUE);
        }
    }


////////////////////////////////////////////////////////////////////////////////////////////////////
//"Echtes" Ping durchführen. Die Antwort ist die Laufzeit in ms
function web_ping($ip)
    {
    global $debug;

    //Das hier ist ein ICMP-Packet, um ein Ping zu simulieren
    $icmp_paket  = "\x08\x00";    //Header = Echo Request
    $icmp_paket .= "\x19\x2f\x00\x00\x00\x00\x70\x69\x6e\x67"; //IP-Buffer und Prüfsumme

    //Immer Fehler annehmen
    $result=99999;

    //Ein ICMP-Socket aufmachen
    @$sock = socket_create(AF_INET, SOCK_RAW, 1);
       //Fehler abfangen
       if ($sock!==FALSE)
           {
        $debug->add("ping","socket open");
        //Optionen setzen
        //Timeout auf zwei Sekunden setzen (VPN hat manchmal hohe Latenzen)
           socket_set_option($sock,SOL_SOCKET,SO_RCVTIMEO,array("sec" => 2, "usec" => 0));

        //Kanal öffnen (Ohne Port)
        if (socket_connect($sock, $ip, null))
            {
            $debug->add("ping","socket connected");
            //Laufzeitmessung starten
            //####ACHTUNG microtime ist nicht auf jedem System verfügbar####
               $runtime=microtime(TRUE);

               //Paket abschicken
            if (socket_send($sock, $icmp_paket, strlen($icmp_paket),0)==strlen($icmp_paket))
                {
                $debug->add("ping","packet send");
                   //Und as Ergebnis lesen (Wenn es eines gibt)
                   //Der Klammeraffe unterdrückt evtl. Warnungen oder Fehler
                if(@socket_read($sock, 255))
                    {
                    $debug->add("ping","answer recieved");
                    //Messung beendet
                    $result=(microtime(TRUE)-$runtime);
                    }
                else
                    {
                    $debug->add("ping","received");
                    }
                }
            }

        //Socket schließen
        socket_close($sock);
        $debug->add("ping","socket closed");
           }
    $debug->add("ping","runtime ".$runtime);
    //Und Laufzeit zurückgeben
    return($result);
    }



////////////////////////////////////////////////////////////////////////////////////////////////////
//Die Remote-Adresse holen
function web_get_ip()
    {
    //Fehler abfangen
    if (isset($_SERVER["REMOTE_ADDR"]))
        {
        return($_SERVER["REMOTE_ADDR"]);
        }
    else
        {
        return("0.0.0.0");
        }
    }

////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen Text mit dem Google-Service übersetzen
//Input ist die gesuchte Phrase
//Languagepair gibt die Richtung der Übersetzung an
//Die Sprachkürzel sind
// de Deutsch
// en English
// fr Französisch
// it Italienisch
// pt Portugisisch
// ar Arabisch
// ja Japanisch
// ko Koreanisch
//
// Erst die Quellsprache dann die Zielsprache
// also von Deutsch nach Koreanisch
// de|ko

function web_translate($query,$languagepair)
    {
    //BasisURL
    $url="http://translate.google.com/translate_t?hl=en&ie=UTF8&Translate";

    $result=FALSE;

    //Leerzeichen ersetzen
    $q=str_replace(" ","%20",$query);

    //Zusammenbauen
    $file=$url."&langpair=".$languagepair."&text=".$q;

    //Google fragen
    $file=file_get_contents($file);

    //Ergebnis extrahieren
    $trans=FALSE;
    if (preg_match("#<textarea[^>]*>([a-zA-Z0-9,.\\?! ]*)</textarea[^>]*>#i",$file,$trans)==1)
        {
        //Was gefunden !"
        $result=TRUE;
        $query=trim($trans[1]);
        }

    //Und Ergebnis zurückgeben
    return($result);
    }


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
function web_push_file($filepath,$name=FALSE,$maxspeed=50,$encrypted=FALSE)
    {
    $result=FALSE;
    require_once(DIR_CLASS."lib.mime.php");

    //Wenn kein Name übergebn wurde, den Dateinamen nehmen
    if ($name==FALSE) $name=string_extractfilename($filepath);
    $name=str_replace(" ","_",$name);

    //Prüfen, ob es ihn gibt
    if (file_exists($filepath))
        {
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
                
                //Verschlüsselung beachten
                if ($encrypted)
                    {
                    $out=crypt_decode($out);
                    }
                
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