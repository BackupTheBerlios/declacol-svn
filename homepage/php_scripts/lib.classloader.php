<script language="php">
////////////////////////////////////////////////////////////////////////////////
/// Hier werden alle Bibliotheken nachgeladen, die wir brauchen
///
/// Eine der wichtigsten Scripte der Engine.
/// Hier werden alle wichtigen Klassen und Bibliotheken nachgeladen.
/// Der Classloader wird direkt aus zentralen Scripten aufgerufen
/// index.php
/// services.php
/// cron.php
///
////////////////////////////////////////////////////////////////////////////////

//Die Typenbibliothek
require_once(DIR_LIB."type.basics.php");

//Alle Bibliotheken (In genau dieser Reihenfolge!!!)
require_once(DIR_LIB."lib.strings.php");
require_once(DIR_LIB."lib.crypt.php");
require_once(DIR_LIB."lib.files.php");
require_once(DIR_LIB."lib.colors.php");
require_once(DIR_LIB."lib.images.php");
require_once(DIR_LIB."lib.javascript.php");
require_once(DIR_LIB."lib.htmlscript.php");
require_once(DIR_LIB."lib.datetime.php");
require_once(DIR_LIB."lib.dirtyxml.php");
require_once(DIR_LIB."lib.web.php");

//Die Sprachbibliothek nachladem
include_once(FILE_LANGUAGE);

//Die Admin-Konstanten nachladen
require_once(DIR_CONST."admincmd.php");

//Alle Klassen (In genau dieser Reihenfolge!!!)
require_once(DIR_CLASS."class.mysql.php");                //MySQL-Wrapper
require_once(DIR_CLASS."class.logging.php");            //Logging-Klasse
require_once(DIR_CLASS."class.transport.php");            //Parsing der POST und GETs

require_once(DIR_CLASS."class.group.php");                //Gruppe
require_once(DIR_CLASS."class.user.php");                //User
require_once(DIR_CLASS."class.session.php");            //Sessions

require_once(DIR_CLASS."class.imagepicker.php");        //Grafikanzeiger
require_once(DIR_CLASS."class.template.php");            //Template-Engine
require_once(DIR_CLASS."class.design.php");                //Skinning

require_once(DIR_CLASS."class.useradmin.php");            //Userverwaltung
require_once(DIR_CLASS."class.groupadmin.php");            //Gruppenverwaltung

require_once(DIR_CLASS."class.object.php");                //Object (Module und Plugins)
require_once(DIR_CLASS."class.objectadmin.php");        //Verwaltung für Objecte

require_once(DIR_CLASS."class.cron.php");                //CRON-Job Objekt
require_once(DIR_CLASS."class.cronadmin.php");            //Verwaltung für Cronjobs

require_once(DIR_CLASS."class.cookie.php");                //Das Cookiehandling

require_once(DIR_CLASS."class.newsletter.php");                //Newsletter
require_once(DIR_CLASS."class.comments.php");                //Globale Kommentarklasse

//Mailsupport angefragt ?
if (MAIL_SUPPORT)
    {
    //Dann Klasse laden und initialisieren
    require_once(DIR_CLASS."class.mailer.php");
    require_once(DIR_EXCLASS."phpmailer/class.phpmailer.php");

    $mailer = new mailer();
    }

//Ab hier können Optionale Klassen eingebunden werden
//Ein Viewer der verschiedene Dateitypen darstellt
require_once(DIR_CLASS."class.viewer.php");

//Die FreePDF-Engine
if (PDF_SUPPORT)
    {
    require_once(DIR_EXCLASS."fpdf/fpdf.php");

    //Erweiterungen geladen ?
    if (defined("PDF_SUPPORT_EXTENDED"))
        {
        //PDF-Support mit Erweiterungen
        $pdf = new PDF;

        $pdf->title="printed using ".PRODUCT_NAME." ".PRODUCT_VERSION;
        }
    else
        {
        //PDF-Support ohne Erweiterungen
        $pdf = new FPDF;
        }
    $pdf->SetAuthor(PRODUCT_NAME);
    $pdf->SetCreator(PRODUCT_NAME);
    }


</script>