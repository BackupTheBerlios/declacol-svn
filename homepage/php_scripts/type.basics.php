<script language="php">
//////////////////////////////////////////////////////////////////////////////
///
/// Sammlung mit allen selbstdefinierten Typen
///
//////////////////////////////////////////////////////////////////////////////
if (!defined("ID_NONE")) define("ID_NONE",0);

//////////////////////////////////////////////////////////////////////////////
//Einfacher Datenknoten ohne Schnickschnack
class simplenode
    {
    var $id    = ID_NONE;
    var $data  = array();
    var $child = array();

    function simplenode()
        {
        }
        
    function destroy()
        {
        unset($this->data);
        
        //Alle Kinder zerstören
        //Da das rekursive Zerstören nicht ganz trivial ist,
        //ziehen wir uns erst ein Array mit den Keys des
        //Childarrays, um kein Problem mit Foreach und unset zu
        //bekommen
        if (is_array($this->child)==TRUE)
            {
            $keys= array_keys($this->child);
            $key = array_pop($keys);
            while ( $key !== NULL )
                {
                $this->child[$key]->destroy();
                $key = array_pop($keys);
                }
            }
        //Der Welt schönster Destruktor
        unset($this);
        }
    }

//////////////////////////////////////////////////////////////////////////////
/// Ein Standard-Node
/// wird unter anderem verwendet in class.easyxml.php
class node extends simplenode
    {
    var $name       = FALSE;
    var $value      = FALSE;
    var $has_child  = FALSE;
    var $childcount = 0;

    //////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function node($name="",$data="")
        {
        //Evtl. Name und Daten speichern
        if ($name!="")
            {
            $this->name=$name;

            if ($data!="")
                {
                $this->value=$data;
                }
            }
        }

    //////////////////////////////////////////////////////////////////////////////
    //Einen Unterknoten anhängen
    function addchild($newnode)
        {
        //Die ID im Zweig merken
        $newnode->id=count($this->child) + 1;

        //Und den Knoten einfach einhängen
        $this->child[]=$newnode;
        $this->has_child=TRUE;
        $this->childcount++;
        
        //Die ID zurückgeben
        return($newnode->id);
        }
    }
    
//////////////////////////////////////////////////////////////////////////////
/// Kleine Erweiterung des Standardnodes
class xmlnode extends node
    {
    var $key    =  0;
    var $parent = -1;
    var $attributes = array();
    }
</script>