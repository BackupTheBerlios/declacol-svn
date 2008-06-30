<script language="PHP">
////////////////////////////////////////////////////////////////////////////////
///
/// Klasse für einfaches BBScript
///
///
///
/// [LINK=http://abc.de]MeinLink[/LINK]     Link mit Target Blank
/// [!LINK=http://abc.de]MeinLink[/LINK]     Link ohne Target
/// [IMG=http://abc.de/meinbild.png]        Bild einfügen
/// [LIMG=http://abc.de/meinbild.png]       Bild links einfügen
/// [RIMG=http://abc.de/meinbild.png]       Bild rechts einfügen
/// [B]Text[/B]                             Text fett
/// [L]Text[/L]                             Text groß
/// [S]Text[/S]                             Text klein
/// [P]Text[/P]                             Text vorformatiert
/// [F]Text[/F]                             Text im Rahmen
/// [HR]                                    Horizontale Linie
/// [BR]                                    Neue Zeile
/// [BLUE]Text[/BLUE]                       Text blau
/// [RED]Text[/RED]                         Text rot
/// [GREEN]Text[/GREEN]                     Text gruen
/// [BLACK]Text[/BLACK]                     Text schwarz
/// [WHITE]Text[/WHITE]                     Text weiß
/// [ON]                                    Anzahl der aktuellen Sessions
/// [USR]                                   Name des aktuellen Benutzers
/// [STATUS=ip:port]                        Zustand des gewünschten Ports (online/offline)
/// [RELOAD=x]                              Aktuelle Seite nach x Sekunden neu laden
/// [AGENT]                                 Zeigt den aktuellen Useragenten an
/// [TAB=Name]Inhalt[/TAB]                  Erzeigt einen Karteireiter
//////////////////////////////////////////////////////////////////////////

//Versioninfo speichern
define ("LIB_HTMLSCRIPT","lib_htmlscript");
define ("LIB_HTMLSCRIPT_VERSION","0.02");
if (isset($debug)) $debug->add(LIB_HTMLSCRIPT,"version ".LIB_HTMLSCRIPT_VERSION);

//Hauptaufruf
function HTMLScript_Parse($input)
    {
    $result=HTMLScript_ParseTAB  ($input);
    $result=HTMLScript_ParseLINK ($result);
    $result=HTMLScript_ParseFONT ($result);
    $result=HTMLScript_ParseAlign($result);
    $result=HTMLScript_ParseWEB  ($result);
    $result=HTMLScript_ParseJS   ($result);

    //Umrüche mitparsen
    $result=nl2br($result);

    return ($result);
    }

//Links konvertieren
function HTMLScript_ParseLINK($input)
    {
    //Perl-Kompatibel, da ereg keinen Lazy-Modifier kennt (zumindest habe ich ihn nicht gefunden)
    //Filter ist Link-Anfang und Link-Ende
    $filter=array( "/\[LINK=(.*?)\]/",
                   "/\[\!LINK=(.*?)\]/",
                   "/\[IMG=(.*?)\]/",
                   "/\[LIMG=(.*?)\]/",
                   "/\[RIMG=(.*?)\]/",
                   "/\[\/LINK\]/"
                   );
    //Und wird ersetzt, durch std. HTML-Tags
    $replace=array( '<a href="${1}" target="_BLANK">',
                    '<a href="${1}">',
                    '<img src="${1}" alt="image" class="include"/>',
                    '<img src="${1}" alt="image" class="include" style="float:left;" />',
                    '<img src="${1}" alt="image" class="include" style="float:right;" />',
                    '</a>');
    //Und zurückgeben
    return(preg_replace($filter,$replace,$input));
    }

//Javascript-Funktionen konvertieren
function HTMLScript_ParseJS($input)
    {
    //Perl-Kompatibel, da ereg keinen Lazy-Modifier kennt (zumindest habe ich ihn nicht gefunden)
    //Filter ist Link-Anfang und Link-Ende
    $filter=array( "/\[RELOAD=(.*?)\]/",
                   );
    //Und wird ersetzt, durch std. HTML-Tags
    $replace=array( '<script type="text/javascript" language="JavaScript">window.setTimeout(\'parent.location.href=parent.location.href\',${1}000);</script>');
    //Und zurückgeben
    return(preg_replace($filter,$replace,$input));
    }


//Webfunktionen konvertieren
function HTMLScript_ParseWEB($input)
    {
    //Checken, ob ein PORT Kommando vorkommt
    if (strpos($input,"[STATUS=")===FALSE)
        {
        return($input);
        }

    //Unser Filter
    $pattern='/\[STATUS=(.*?):([0-9]*)\]/';
    //Alle Einträge als Array rausholen
    $matches=FALSE;
    if (preg_match_all ($pattern,$input,$matches)!=0)
        {
        //Dann alle ersetzen
        foreach ($matches[0] as $key => $val)
            {
            //URL und Port bastelen
            if (web_checkport($matches[1][$key],$matches[2][$key]))
                {
                $input=str_replace($matches[0][$key],"online",$input);
                }
            else
                {
                $input=str_replace($matches[0][$key],"offline",$input);
                }
            }
        }
    return($input);
    }

//Fontsteuerung konvertieren
function HTMLScript_ParseFONT($input)
    {
    global $session;

    $filter=array(  '/\[B\]/','/\[\/B\]/',
                    '/\[I\]/','/\[\/I\]/',
                    '/\[L\]/','/\[\/L\]/',
                    '/\[S\]/','/\[\/S\]/',
                    '/\[P\]/','/\[\/P\]/',
                    '/\[F\]/','/\[\/F\]/',
                    '/\[BLUE\]/','/\[\/BLUE\]/',
                    '/\[RED\]/','/\[\/RED\]/',
                    '/\[GREEN\]/','/\[\/GREEN\]/',
                    '/\[BLACK\]/','/\[\/BLACK\]/',
                    '/\[WHITE\]/','/\[\/WHITE\]/',
                    '/\[HR\]/',
                    '/\[BR\]/',
                    '/\[ON\]/',
                    '/\[USR\]/',
                    '/\[AGENT\]/',
                    '/\[BLIND\]/'
                );
    $replace=array( "<font style=\x22font-weight : bold\x22>","</font>",
                    "<font style=\x22font-style  : italic\x22>","</font>",
                    "<font style=\x22font-size   : larger\x22>","</font>",
                    "<font style=\x22font-size   : smaller\x22>","</font>",
                    "<pre>","</pre>",
                    "<fieldset>","</fieldset>",
                    "<font style=\x22color  : blue\x22>","</font>",
                    "<font style=\x22color  : red\x22>","</font>",
                    "<font style=\x22color  : green\x22>","</font>",
                    "<font style=\x22color  : black\x22>","</font>",
                    "<font style=\x22color  : white\x22>","</font>",
                    "<hr/>",
                    "<br/>",
                    $session->count,
                    $session->user->name,
                    $session->user->agent,
                    TEXT_BLIND
                    );
    return(preg_replace($filter,$replace,$input));
    }

//Textsatz konvertieren
function HTMLScript_ParseAlign($input)
    {
    global $session;

    $filter=array(    '/\[AL]/','/\[\/AL\]/',
                    '/\[AR]/','/\[\/AR\]/',
                    '/\[AC]/','/\[\/AC\]/',
                    '/\[AJ]/','/\[\/AJ\]/'
                );
    $replace=array( "<div style=\x22text-align : left\x22>","</div>",
                    "<div style=\x22text-align : right\x22>","</div>",
                    "<div style=\x22text-align : center\x22>","</div>",
                    "<div style=\x22text-align : justify\x22>","</div>",
                    );
    return(preg_replace($filter,$replace,$input));
    }

//Karteireiter erzeugen
//Das ist nicht ganz so Trivial, wie es zunächste erscheint.
//Zur Steuerung werden JavaScripte benutzt
function HTMLScript_ParseTAB($input)
    {
    //Einigermaßen Eindeutige ID bilden, falls mehrere Tabs vorkommen
    $field=chr(rand(65,90)).rand(0,65535);

    //Alle Tabs extrahieren
    $splitted=array();
    if (preg_match_all('/\[TAB=(.*?)\]([\s\S]*?)\[\/TAB\]/',$input,$splitted)>0)
        {
        //Erster Eintrag kann verworfen werden, da er nur Suchmaske enthält
        reset($splitted);

        //Die Karteireiter einblenden
        $tabreplace ="<div class=\"module\" id=\"".$field."\">";
        $tabreplace.="<div class='tabhead'>";
        $heads=next($splitted);
        $index=0;
        $class="tabhead_entry_active";
        foreach ($heads as $head)
            {
            $tabreplace.="<a id=\"h".$field.$index."\" class=\"".$class."\" href=\"javascript:switchtab('".$field."','".$field.$index."','tabhead_entry','tabhead_entry_active');\">".$head."</a>";
            $class="tabhead_entry";
            $index++;
            }
        $tabreplace.="</div>";


        //Und untendrunter jeweils der Inhalt
        $tabs=next($splitted);
        $index=0;
        foreach ($tabs as $tab)
            {
            $tabreplace.="<div id=\"d".$field.$index."\" class=\"tabdata\" style=\"display:".($index==0?"block":"none")."\">";
            $tabreplace.=$tab;
            $tabreplace.="</div>";
            $index++;
            }
        $tabreplace.="</div>";

        //Nun müssen wir nur noch alles innerhalb der TabTags durch unsere neuen Daten ersetzen
        $input=preg_replace("/\[TAB[\s\S]*\[\/TAB\]/",$tabreplace,$input);
        }

    return($input);
    }
</script>