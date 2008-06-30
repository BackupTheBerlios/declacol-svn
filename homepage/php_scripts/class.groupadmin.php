<script language="PHP">
//////////////////////////////////////////////////////////////////////////
///
/// Gruppenverwaltung Klasse
///
/// Die Funktionalität beschränkt sich im wesentliche auf das enumerieren
/// und die (de)installation.
///
//////////////////////////////////////////////////////////////////////////
//Versioninfo speichern
define ("CLASS_GROUPADMIN","class_groupadmin");
define ("CLASS_GROUPADMIN_VERSION","0.01");
if (isset($debug)) $debug->add(CLASS_GROUPADMIN,"version ".CLASS_GROUPADMIN_VERSION);


//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
//////////////////////////////////////////////////////////////////////////
class groupadmin
    {
    //Öffentliche Eigenschaftem
    var $active                =FALSE;

    //Private Eigenschaften
    var $internal_index     =0;


    //////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function groupadmin()
        {
        //Alle Werte auf Default setzen
        $this->setdefault();
        }

    //Destruktor
    function destroy()
        {
        }

    //Für den Aktuellen Sessioninhaber die Gruppenzuordnun holen
    function open()
        {
        }

    //Destruktor
    function close()
        {
        }

    //Defaults
    function setdefault()
        {
        $this->active=FALSE;
        $this->internal_index=0;
        }

    //Datenbank erzeugen
    function install()
        {
        //Datenbankzugriff holen
        global $mysql;

        //Alles wegwerfen
        $this->uninstall();

        //Gruppendatenbank anlegen
        $query="CREATE TABLE IF NOT EXISTS ".DB_GROUPS." (id char(64), name char(64), readable int(32),writable int(32),admin int(32), enabled int(32),param1 int(32),param2 int(32),param3 int(32),param4 int(32),param5 int(32),param6 int(32),param7 int(32), PRIMARY KEY (id))";
        $mysql->query($query);

        //Gruppenmodulangehörigkeiten festlegen
        $query="CREATE TABLE IF NOT EXISTS ".DB_GROUPS_OBJECTS." (id int(32) NOT NULL AUTO_INCREMENT, object_id char(64), group_id char(64), PRIMARY KEY (id));";
        $mysql->query($query);

        //Gruppenzuordnung anlegen
        $query="CREATE TABLE IF NOT EXISTS ".DB_GROUPS_MEMBERS." (id int(32) NOT NULL AUTO_INCREMENT, user_id char(64), group_id char(64),  PRIMARY KEY (id))";
        $mysql->query($query);

        //Und ein paar Standardgruppen anlegen
        $this->add(GROUP_ADMIN,TRUE ,TRUE ,TRUE ,TRUE ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);
        $this->add(GROUP_XUSER,TRUE ,TRUE ,FALSE,TRUE ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);
        $this->add(GROUP_USER ,TRUE ,FALSE,FALSE,TRUE ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);
        $this->add(GROUP_GUEST,TRUE ,FALSE,FALSE,TRUE ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);
        $this->add(GROUP_NONE ,TRUE ,FALSE,FALSE,TRUE ,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE);
        }

    //Datenbank zerstören
    function uninstall()
        {
        //Datenbankzugriff holen
        global $mysql;

        //Abfrage aufbauen
        $query="DROP TABLE IF EXISTS ".DB_GROUPS;
        $result=$mysql->query($query);

        $query="DROP TABLE IF EXISTS ".DB_GROUPS_MEMBERS;
        $result|=$mysql->query($query);

        $query="DROP TABLE IF EXISTS ".DB_GROUPS_OBJECTS;
        $result|=$mysql->query($query);

        return($result);
        }

    //////////////////////////////////////////////////////////////////////////
    ///Ab hier die eigentlichen Funktionen
    //////////////////////////////////////////////////////////////////////////

    //Alle verfügbaren Gruppen auflisten und als Objectarray zurückgeben
    function enumerate()
        {
        global $mysql;

        //Alle Gruppen lesen
        $query="SELECT name FROM ".DB_GROUPS." ORDER BY name";
        $dbresult=$mysql->query($query);

        //Was gefunden
        if (!is_array($dbresult))
            {
            return(FALSE);
            }

        //Und durchgehen
        $result=array();
        foreach ($dbresult as $onegroup)
            {
            $mygroup=new group($onegroup["name"]);
            $result[]=$mygroup;
            }
        return($result);
        }

    //Alle Gruppen und Rechte zu dem UserObject holen und entsprechend setzen
    function set_groups(&$userobject)
        {
        global $mysql;

        //Alle Gruppen und Rechte des Users holen
        $query="SELECT A.id,B.object_id,A.readable,A.writable,A.admin,A.enabled,A.param1,A.param2,A.param3,A.param4,A.param5,A.param6,A.param7 FROM ".DB_GROUPS." AS A,".DB_GROUPS_OBJECTS." AS B, ".DB_GROUPS_MEMBERS." AS C WHERE (A.id=B.group_id) AND (B.group_id=C.group_id) AND (C.user_id = '".$userobject->id."')";
        $result=$mysql->query($query);

        //Was gefunden ?
        if (is_array($result))
            {
            //Dann die entsprechenden Gruppen des Users setzen
            //Wir benutzen hier keine Gruppenobjekt, da dies bei hohen
            //Nutzerzahlen den Interpreter ziemlich in die Knie zwingt
            foreach ($result as $group)
                {
                //Alle Objecte einspielen
                $userobject->addgroup($group["object_id"],(bool)$group["readable"],(bool)$group["writable"],(bool)$group["admin"],(bool)$group["param1"],(bool)$group["param2"],(bool)$group["param3"],(bool)$group["param4"],(bool)$group["param5"],(bool)$group["param6"],(bool)$group["param7"],FALSE);
                }
            }
        //Keinen User gefunden, dann der Gruppe NONE zuordnen
        else
            {
            $query="SELECT A.id,B.object_id,A.readable,A.writable,A.admin,A.enabled,A.param1,A.param2,A.param3,A.param4,A.param5,A.param6,A.param7 FROM ".DB_GROUPS." AS A,".DB_GROUPS_OBJECTS." AS B WHERE (A.id=B.group_id) AND (A.name = '".GROUP_NONE."')";
            $result=$mysql->query($query);

            if (is_array($result))
                {
                foreach ($result as $group)
                    {
                    //Alle Objecte einspielen
                    $userobject->addgroup($group["object_id"],(bool)$group["readable"],(bool)$group["writable"],(bool)$group["admin"],(bool)$group["param1"],(bool)$group["param2"],(bool)$group["param3"],(bool)$group["param4"],(bool)$group["param5"],(bool)$group["param6"],(bool)$group["param7"],FALSE);
                    }
                }
            }
        }

    //Einen Gruppe hinzufügen
    function add($groupname,$is_readable,$is_writable,$is_admin,$is_enabled,$param1,$param2,$param3,$param4,$param5,$param6,$param7)
        {
        global $mysql;

        $groupid=crypt_create_hash($groupname);

        $query ="INSERT INTO ".DB_GROUPS." (id,name,readable,writable,admin,enabled,param1,param2,param3,param4,param5,param6,param7) VALUES('".$groupid."','".$groupname."',".(int)$is_readable.",".(int)$is_writable.",".(int)$is_admin.",".(int)$is_enabled.",".(int)$param1.",".(int)$param2.",".(int)$param3.",".(int)$param4.",".(int)$param5.",".(int)$param6.",".(int)$param7.")";
        return(@$mysql->query($query));
        }

    //Eine Gruppe entfernen
    function remove($groupname)
        {
        global $mysql;

        $groupid=crypt_create_hash($groupname);

        //Einfach aus allen Datenbanken rauswerfen
        $query ="DELETE FROM ".DB_GROUPS." WHERE id='".$groupid."'";
        $result=$mysql->query($query);
        $query ="DELETE FROM ".DB_GROUPS_MEMBERS." WHERE group_id='".$groupid."'";
        $mysql->query($query);
        $query ="DELETE FROM ".DB_GROUPS_MEMBERS." WHERE group_id='".$groupid."'";
        $mysql->query($query);

        return($result);
        }

    //Den Namen einer Gruppe holen
    function getname($id)
        {
        global $mysql;

        $query="SELECT name FROM ".DB_GROUPS." WHERE id='".$id."'";
        $result=$mysql->query($query);

        if (is_array($result))
            {
            $result=$result[0]["name"];
            }
        return($result);
        }

    //Die Rechte einer Gruppe anpassen
    function setrights($id,$read=FALSE,$write=FALSE,$admin=FALSE,$enabled=TRUE)
        {
        global $mysql;

        //Und für die Gruppe setzen
        $query="UPDATE ".DB_GROUPS." set readable=".(int)$read.", writable=".(int)$write.", admin=".(int)$admin." , enabled=".(int)$enabled." WHERE id='".$id."'";
        return($mysql->query($query));
        }

    //Die Parameter einer Gruppe anpassen
    function setparams($id,$param1,$param2,$param3,$param4,$param5,$param6,$param7)
        {
        global $mysql;

        //Und für die Gruppe setzen
        $query="UPDATE ".DB_GROUPS." set param1=".(int)$param1.", param2=".(int)$param2.", param3=".(int)$param3." , param4=".(int)$param4." , param5=".(int)$param5." , param6=".(int)$param6." , param7=".(int)$param7." WHERE id='".$id."'";
        return($mysql->query($query));
        }

    //Aus dem Benutzerstatus die zugeordnete Gruppe machen
    function statustogroup($userstatus)
        {
        //Einfach hart zuordnen
        switch ($userstatus)
            {
            case (USER_GUEST)    :    $result=GROUP_GUEST;    break;
            case (USER_USER)    :    $result=GROUP_USER;        break;
            case (USER_XUSER)    :    $result=GROUP_XUSER;    break;
            case (USER_ADMIN)    :    $result=GROUP_ADMIN;    break;
            default             :    $result=GROUP_NONE;        break;
            }
        //Fertig
        return($result);
        }

    //Aus der Gruppe den zugeordneten Benutzerstatus machen
    function grouptostatus($userstatus)
        {
        //Einfach hart zuordnen
        switch ($userstatus)
            {
            case (GROUP_GUEST)    :    $result=USER_GUEST;        break;
            case (GROUP_USER)    :    $result=USER_USER;        break;
            case (GROUP_XUSER)    :    $result=USER_XUSER;        break;
            case (GROUP_ADMIN)    :    $result=USER_ADMIN;        break;
            default             :    $result=USER_NONE;        break;
            }
        //Fertig
        return($result);
        }

    }

</script>