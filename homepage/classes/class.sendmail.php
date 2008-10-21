<script language="php">
////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Mailwrapper
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");
require_once(PATH_EXTERN."class.phpmailer.php");

define ("SENDMAIL_INSTANT"  ,-1);

//Eigentliche Klasse
class sendmail
    {
    //Private
    var $_registry = FALSE;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function sendmail(&$registry)
        {
        //Unsere Registrieung intern ablegen
        $this->_registry=$registry;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        //Alle delayed Mails versenden
        $this->flush();

        //Speicher freigeben
        if ( $this->_registry !== FALSE )
            {
            $this->_registry->flush();
            $this->_registry->destroy();
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zurück
    function install()
        {
        $result[CLASS_INDEX_ID]       = "sendmail";      //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]     = "sendmail";      //Name der Klasse
        $result[CLASS_INDEX_VERSION]  = "0.1";           //Version der Klasse
        $result[CLASS_INDEX_REGISTRY] = TRUE;            //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE] = FALSE;           //Wird eine Datenbank benötigt

        $result[CLASS_INDEX_CLEANUP]  = TRUE;            //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD] = TRUE;           //Soll die Klasse beim Systemstart geladen werden ?

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Email senden. Mailto kann ein Array mit Empfängern sein
    function send($mailto,$mailfrom,$subject,$body,$delay=SENDMAIL_INSTANT)
        {
        if (is_array($mailto)==FALSE)
            {
            $mailto=array($mailto);
            }
        
        //Mail zusammenbauen
        $id=md5($subject.$body);
        $mail=serialize(array("sender"=>$mailfrom,"subject"=>$subject,"body"=>$body,"delay"=>(time() + $delay)));
        
        //Und ablegen
        foreach ($mailto as $address)
            {
            $this->_registry->write("mails/".$address,$id,$mail);
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Delayed EMails flushen
    function flush()
        {
        //Alle Empfänger durchgehen
        $addresses=$this->_registry->enum("mails/");
        foreach ($addresses as $address => $dummy)
            {
            //EMails durchgehen
            $mails=$this->_registry->enum("mails/".$address."/");
            foreach ($mails as $mail)
                {
                $mail=unserialize($mail);

                //Abschicken, wenn es soweit ist
                if ($mail["delay"] < time())
                    {
                    $this->_send($address,$mail["sender"],$mail["subject"],$mail["body"]);
                    $this->_registry->del("mails/",$address);
                    }
                }
            }
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine EMail verschicken
    function _send($mailto,$mailfrom,$subject,$body)
        {
        $this->_phpmailer(array($mailto),$mailfrom,$subject,$body);
        echo "sending ".$subject." to ".$mailto." from ".$mailfrom."\n";
        }


        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Email über die PHP-Mailerklasse schicken
    function _phpmailer($to,$from,$subject,$body)
        {
        global $debug;

        $mail = new PHPMailer();

        //Serverdaten setzen
        switch (EMAIL_MODE)
            {
            case ("mail")     : $mail->IsMail();            break;
            case ("smtp")     : $mail->IsSMTP();            break;
            case ("sendmail") : $mail->IsSendmail();        break;
            }

        $mail->Host     = EMAIL_SMTP;
        $mail->SMTPAuth = EMAIL_AUTH;
        $mail->Username = EMAIL_USER;
        $mail->Password = EMAIL_PASS;

        //Adressdaten setzen
        $mail->From     = EMAIL_FROM;
        $mail->FromName = $from;

        //Alle Namen zufügen
        foreach ($to as $receiver)
            {
            $mail->AddAddress($receiver);
            }

        $mail->AddReplyTo(EMAIL_FROM);

        //Body setzen
        $mail->WordWrap = 50;
        $mail->Subject = $subject;
        $mail->Body    = $body;

        //Mail abschicken
        $result=$mail->Send();
        
        return($result);
        }
    }
</script>