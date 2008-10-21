<script language="php">
////////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Sessionverwaltung
///
////////////////////////////////////////////////////////////////////////////////////////////////////

//Eigentliche Klasse
class session
    {
    //Public
    var $user     = FALSE;
    var $data     = FALSE;
    
    //Private
    var $_registry = FALSE;


    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function session(&$registry)
        {
        //Unsere Registrieung intern ablegen
        $this->_registry=$registry;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        if ( $this->_registry !== FALSE )
            {
            $this->_registry->flush();
            unset($this->_registry);
            }
        unset($this);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die Installfunktion gibt ein Array mit relevanten Daten zur�ck
    function install()
        {
        $result[CLASS_INDEX_ID]       = "session";      //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]     = "session";      //Name der Klasse
        $result[CLASS_INDEX_VERSION]  = "0.1";           //Version der Klasse
        $result[CLASS_INDEX_REGISTRY] = TRUE;            //Wird eine Registry ben�tigt
        $result[CLASS_INDEX_DATABASE] = FALSE;           //Wird eine Datenbank ben�tigt

        $result[CLASS_INDEX_CLEANUP]  = TRUE;            //Soll die Datenbank initialisiert werden ?

        $result[CLASS_INDEX_AUTOLOAD] = TRUE;           //Soll die Klasse beim Systemstart geladen werden ?

        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Eine Session starten
    function start($id)
        {
        //Gibt es die Session schon ?
        echo $id;
        }
    }
</script>