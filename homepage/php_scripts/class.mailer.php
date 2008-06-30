<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Mail-Support
///
/// Wrapper zur Verinfachung des class.smtp handlings
///
/// Es werden zwei Mail-Verfahren unterstützt.
/// send(..)  verschickt direkt eine Nachricht per EMail
/// add(..)   fügt eine Nachricht der Datenbank zu
///           diese wird dann per cron_automailer verschickt.
/// flush(..) kummuliert alle Nachrichten in der Datenbank, die an den gleichen
///           Empfänger gehen und verschickt diese als EINE EMail.
///           Über den CRON-Job Mailer kann dies automatisiert werden.
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("CLASS_MAILER","class_mailer");
define ("CLASS_MAILER_VERSION","0.02");
if (isset($debug)) $debug->add(CLASS_MAILER,"version ".CLASS_MAILER_VERSION);


//Timeout, nach welchem EMails verworfen werden
//(In Tagen)
define ("MAILER_TIMEOUT"    ,2);

//Unseren Datenbanknamen als Konstante definieren
define ("DB_MAILER",DB_PREFIX."mailer");

//Überschrift in der kummulierten Mail
//define ("LNG_SUMMAIL"     ,"Diese Mail enthaelt %count% Nachricht(en) fuer Sie");
define ("LNG_SUMMAIL"     ,"this email contains %count% messages");

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class mailer
    {
    //Öffentliche Eigenschaftem
    //Mehrere Mails an einen Empfänger zusammenfassen ?
    var $groupmails            =TRUE;

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function mailer()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        }

    //Verbindungsaufbau
    function open()
        {
        }

    //Verbindungsabbau
    function close()
        {
        }

    //Defaultwerte der internen und externen Variablen setzen
    function setdefault()
        {
        $this->groupmails=TRUE;
        }

    //Datenbank und alles andere erzeugen
    function install()
        {
        //Datenbankzugriff holen
        global $mysql;

        //Abfrage aufbauen
        $query="CREATE TABLE IF NOT EXISTS ".DB_MAILER." (id int(32) NOT NULL AUTO_INCREMENT, mail_to char(64),mail_from char(64), mail_subject char (128), mail_body longtext, time int (32), PRIMARY KEY (id));";

        //Und eine neue Datenbank anlegen
        $result=$mysql->query($query);

        return($result);
        }

    //Datenbank und alles andere zerstören
    function uninstall()
        {
        //Datenbankzugriff holen
        global $mysql;

        //Abfrage aufbauen
        $query="DROP TABLE IF EXISTS ".DB_MAILER;

        //Und eine neue Datenbank anlegen
        $result=$mysql->query($query);

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Eine Email über die PHP-Mailerklasse schicken
    function _phpmailer($to,$from,$subject,$body)
        {
        global $debug;

        $mail = new PHPMailer();

        //Serverdaten setzen
        switch (MAIL_MODE)
            {
            case ("mail")     : $mail->IsMail();            break;
            case ("smtp")     : $mail->IsSMTP();            break;
            case ("sendmail") : $mail->IsSendmail();        break;
            }
            
        $mail->Host = MAIL_SMTP;
        $mail->SMTPAuth = MAIL_AUTH;
        $mail->Username = MAIL_USER;
        $mail->Password = MAIL_PASS;

        //Adressdaten setzen
        $mail->From     = string_filter(MAIL_FROM,FILTER_EMAIL);
        $mail->FromName = $from;

        //Empfänger immer ein Array
        $receiver=array();
        if (!is_array($to))
          {
          $receiver[]=$to;
          }
        else
          {
          $receiver=$to;
          }

        //Alle Namen zufügen
        foreach ($receiver as $to)
          {
          $mail->AddAddress(string_filter($to,FILTER_EMAIL));
          }

        $mail->AddReplyTo(MAIL_FROM);

        //Body setzen
        $mail->WordWrap = 50;
        $mail->Subject = $subject;
        $mail->Body    = $body;
//        $mail->AltBody = $body;

        //Mail abschicken
        $result=$mail->Send();

        //Bei Fehler einen Debug-Eintrag machen
        if (!$result)
            {
            if ($debug)
                {
                $debug->add(CLASS_MAILER,$mail->ErrorInfo);
                }
            }

        return($result);
        }

    //Eine EMail direkt schicken
    function send ($to,$subject,$body,$from=PRODUCT_NAME)
        {
        global $debug;

        //Fehler abfangen
        if ( ($to=="")||
             ($subject=="") ||
             ($body=="")
            )
            {
            return(FALSE);
            }

        //Den Empfänger brauchen wir bei direktem Versand nicht zu filtern, da dies die
        //Sendefunktion übernimmt.
        //Entities zurückwandeln
        $to     =string_filter($to,FILTER_EMAIL);
        $subject=string_replace_html($subject);
        $body   =string_replace_html($body);

        //Um Zeilenumbrüche zu vereinheitlichen
        $body=str_replace("\r\n","\n",$body);
        $body=str_replace("\n","\r\n",$body);

        //Debugausgabe
        if (DEBUG)
            {
            $debug->add(CLASS_MAILER,array("receiver"=>$to,"subject"=>$subject,"body"=>$body));
            }

        //Je nach verfügbarkeit eine unterschiedliche Mail-Engine nehmen
        $result=$this->_phpmailer($to,$from,$subject,$body);


        //Und los
        return ($result);
        }

    //Eine EMail-Nachricht zufügen und in der Datenbank ablegen
    function add ($to,$subject,$body,$from=PRODUCT_NAME)
        {
        global $mysql;

        //Fehler annehmen
        $result=FALSE;

        //Eingangsdaten filtern
        //Hier auch die EMail-Adressen um SQL-Injection vorzubeugen
        $from    =string_filter($from    ,FILTER_EMAIL);
        $subject =string_filter($subject ,FILTER_SECURE);
        $body    =string_filter($body    ,FILTER_SECURE);

        //Und Subject und Body nochmals nachbehandeln
        $subject =string_replace_entities($subject);
        $body     =string_replace_entities($body);


        //Empänger muß immer ein Array sein
        if (!is_array($to))
          {
          $receiver[]=$to;
          }
        else
          {
          $receiver=$to;
          }

        //Alles erledigt, nun machen wir einen Eintrag in die Datenbank
        foreach ($receiver as $to)
            {
            //Auf EMail filtern, um keine bösen Überraschungen im MySQL zu erleben
            $to=string_filter($to      ,FILTER_EMAIL);
            //Und los
              if ( ($to!="") && ($subject!=""))
                   {
                //Query zusammenbauen
                 $query ="INSERT INTO ".DB_MAILER." ";
                 $query.="(mail_to,mail_from,mail_subject,mail_body,time)";
                 $query.=sprintf ("VALUES ('%s','%s','%s','%s',%u)",$to,$from,$subject,$body,time());

                 //Abschuß
                 $mysql->query($query);

                 $result=TRUE;
                 }
            }

        return($result);
        }

    //Verschickt alle EMails aus der Datenbank
    function flush()
        {
        //Sollen die Mails gruppirt verschickt werden ?
        if ($this->groupmails)
            {
            return($this->_groupedflush());
            }
        else
            {
            return($this->_singleflush());
            }
        }


    //Alle Emails in der Datenbank verschicken
    function _groupedflush()
        {
        global $mysql;

        //Timeouted EMails immer löschen
        $query="DELETE FROM ".DB_MAILER." WHERE time < ".(time()-MAILER_TIMEOUT*SECS_PER_DAY);
        $mysql->query($query);

        //Alle Empfänger lesen
        $query="SELECT mail_to,mail_from,mail_subject,mail_body FROM ".DB_MAILER." ORDER BY mail_subject,time,mail_to";
        $receiver=$mysql->query($query);

        //Wenn wir nix haben , brechen wir hier ab
        if (!is_array($receiver))
            {
            return;
            }

        //Alle Mails durchgehen
        //und in einem Array speichern
        $summail=array();
        foreach ($receiver as $mail)
            {
            //Hash von Ihalt und Subject erzeugen
            $hash=crypt_create_hash($mail["mail_body"].$mail["mail_subject"]);
            
            //Ablegen (Gleicher Hash ist gleicher Inhalt und gleiches Subject)
            $summail[$mail["mail_to"]][$mail["mail_from"]]["body"][$hash]      =$mail["mail_body"];
            $summail[$mail["mail_to"]][$mail["mail_from"]]["subject"][$hash]   =$mail["mail_subject"];
            }

        //Speicher freigeben
        unset($receiver);

        //Im Array liegen nun die Mails nach Empfänger sortiert.
        //Jetz gehen wir einfach das Mail-Array durch und löschen die Einträge raus, die erfolgreich
        //verschickt wurden
        foreach ($summail as $receiver => $mailpack)
            {
            //Und nun alle Mails nach Absender durchgehen
            foreach ($mailpack as $sender => $mail)
                {
                //Wenn wir nur eine Mail haben, dann diese einfach weiterschicken
                if (count($mail["body"])==1)
                    {
                    $subject=reset($mail["subject"]);
                    $body   =reset($mail["body"]);
                    }
                else
                    {
                    //Subject bauen
                    $subject=str_replace("%count%",count($mail["body"]),LNG_SUMMAIL);

                    //Body zusammensetzen
                    $count=1;
                    $body=$subject."\n";
                    foreach ($mail["body"] as $index => $text)
                       {

                       //Die einzelnen Bodies zusammenkitten
                       $body.=LNG_SEPARATOR.$count.LNG_SEPARATOR;
                       $body.="Betreff \x22".$mail["subject"][$index]."\x22\n\n";
                       $body.="Inhalt :\n\n".$text;
                       $count++;
                       }
                    }
                 //Mail abschicken
                 if ($this->send($receiver,$subject,$body,$sender))
                    {
                    //Erfolgreich verschickte aus der Datenbank löschen
                    $mysql->query("DELETE FROM ".DB_MAILER." WHERE mail_to='".$receiver."'");
                    }
               }
            }
        }
    //Alle Emails in der Datenbank verschicken
    function _singleflush()
        {
        //Not yet implemented
        return;
/*

        global $mysql;

        //Timeouted EMails immer löschen
        $query="DELETE FROM ".DB_MAILER." WHERE time < ".(time()-MAILER_TIMEOUT*SECS_PER_DAY);
        $mysql->query($query);

        //Alle Empfänger lesen
        $query="SELECT id,mail_to,mail_from,mail_subject,mail_body FROM ".DB_MAILER." ORDER BY mail_to";
        $receiver=$mysql->query($query);

        foreach ($receiver as $mail)
              {
               //Mail abschicken
               if ($this->send($to,$subject,$body,$sender))
                    {
                    //Erfolgreich verschickte aus der Datenbank löschen
                    $mysql->query("DELETE FROM ".DB_MAILER." WHERE mail_to='".$receiver."'");
                    }
              }
*/        }

    //Alle EMails aus der Warteschleife anzeigen
    function enumerate()
        {
        global $mysql;

        $query="SELECT * FROM ".DB_MAILER." ORDER BY mail_to";

        return($mysql->query($query));
        }
    }

</script>