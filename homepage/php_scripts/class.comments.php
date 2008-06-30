<script language="PHP">
/////////////////////////////////////////////////////////////////////////////////
///
/// Klasse zur Erzeugung und Verwaltung von Kommentaren
///
/// Dies wird zentral vom System gemacht, so daß Module sich nur wenig um
/// das Handling kümmern müssen.
///
///
///Beispiel
///
///function show_comments($subjectid)
///    {
///    global $comments;
///
///    $opinions=$comments->get($subjectid);
///    if ($opinions != FALSE)
///        {
///        foreach ($opinions as $opinion)
///            {
///            echo $opinion;
///            }
///        }
///    }
///
///function save_comment($subjectid,$text,$additonals)
///    {
///    global $comments;
///
///    $comments->add($subjectid,$text,$additionals);
///    }
///
///
/////////////////////////////////////////////////////////////////////////////////


class comments
    {
    var $dbname = FALSE;

    /////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function comments()
        {
        $this->setdefault();
        }

    /////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        $this->setdefault();
        unset($this);
        }

    /////////////////////////////////////////////////////////////////////////////////
    //Datenbank initialisieren
    function install()
        {
        global $mysql;
        
        //Comment Datenbank erzwingen
        $query="CREATE TABLE IF NOT EXISTS `".$this->dbname."` (
                `id` int(10) unsigned NOT NULL auto_increment,
                `module`  varchar(45) NOT NULL default '',
                `related` varchar(45) NOT NULL default '',
                `comment` longtext NOT NULL default '',
                `additional`   varchar(128) NOT NULL default '',
                `created` int(10) NOT NULL default '0',
                PRIMARY KEY  (`id`))";
        return($mysql->query($query));
        }

    /////////////////////////////////////////////////////////////////////////////////
    //Datenbank deinstallieren
    function uninstall()
        {
        global $mysql;
        
        $query="DROP TABLE IF NOT EXISTS ".$this->dbname;
        return($mysql->query($query));
        }

    /////////////////////////////////////////////////////////////////////////////////
    //Interne Defaults setzen
    function setdefault()
        {
        $this->dbname=DB_PREFIX."comments";
        }
        
    /////////////////////////////////////////////////////////////////////////////////
    //Eintrag zufügen
    function add($relatedid,$text,$additional)
        {
        global $vars;
        global $mysql;
        
        //Die Page-Variable enthält die ID des aktuell geladenen Moduls
        $module=$vars->page;
        
        //Text MySQL-tauglich machen
        $text      =string_replace_entities($text);
        $additional=string_replace_entities($additional);
        $relatedid =string_filter($relatedid,FILTER_ALPHANUM);
        $module    =string_filter($module   ,FILTER_ALPHANUM);

        //Rein damit
        $query="INSERT INTO ".$this->dbname." (module,related,comment,additional,created) VALUES(?,?,?,?,?)";
        $result=$mysql->doquery($query,array($module,$relatedid,$text,$additional,CURRENT_DATE));

        //Fertig
        return ($result);
        }
        
    /////////////////////////////////////////////////////////////////////////////////
    //Einträge zur ID id holen
    function get($relatedid)
        {
        global $vars;
        global $mysql;

        //Die Page-Variable enthält die ID des aktuell geladenen Moduls
        $module=$vars->page;

        //Text MySQL-tauglich machen
        $relatedid=string_filter($relatedid,FILTER_ALPHANUM);
        $module   =string_filter($module   ,FILTER_ALPHANUM);

        //Rein damit
        $query="SELECT id,created,comment,additional FROM ".$this->dbname." WHERE (module=?) and (related=?) ORDER BY created";
        $dbresult=$mysql->doquery($query,array($module,$relatedid));
        if (is_array($dbresult))
            {
            $result=$dbresult;
            }
        else
            {
            $result=FALSE;
            }

        //Fertig
        return ($result);
        }

    /////////////////////////////////////////////////////////////////////////////////
    //Einen Eintrag löschen
    function del($id)
        {
        global $vars;
        global $mysql;

        //Die Page-Variable enthält die ID des aktuell geladenen Moduls
        $module=$vars->page;

        //Text MySQL-tauglich machen
        $id=string_filter($id,FILTER_NUMBER);

        //Rein damit
        $query="DELETE  FROM ".$this->dbname." WHERE id=?";
        $mysql->doquery($query,array($id));

        //Fertig
        return (TRUE);
        }
    }
</script>