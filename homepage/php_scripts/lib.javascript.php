<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Alle Funktionen in Verbindung mit JavaScript
///
/// Die eigentlichen Scripte müssen in der Datei
/// _template/header.php geladen werden.
/// Die Funktionen in dieser Bibliothek bilden nur das Interface zu
/// Javascript
///
///
//////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("LIB_JAVASCRIPT","lib_javascript");
define ("LIB_JAVASCRIPT_VERSION","0.02");
$debug->add(LIB_JAVASCRIPT,"version ".LIB_JAVASCRIPT_VERSION);

    //Interface zur Javascriptfunktion für die Tooltips
    function ShowToolTip($text,$tiptext,$head=FALSE,$width=200)
        {
        global $html;

        if ($head!==FALSE)
            {
            $tiptext="<b>".$head."</b><hr/>".$tiptext;
            }
        //Hover-Tooltip einblenden
        $html->write("<div onMouseover=\x22ddrivetip('".$tiptext."','".$width."')\x22 onMouseout=\x22hideddrivetip()\x22>");
        $html->write($text);
        $html->write("</div>");
        }

    //Interface zur Javascriptfunktion für die Tooltips
    function ShowIconToolTip($icon,$text,$width=200)
        {
        global $html;

        //Hover-Tooltip einblenden
        $html->write("<img src=\x22".$icon."\x22 alt=\x22\x22 onMouseover=\x22ddrivetip('".$text."','".$width."')\x22 onMouseout=\x22hideddrivetip()\x22 style=\x22cursor : help; \x22 />\n");
        }
</script>