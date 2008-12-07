<script language="php">
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
/// Diese Klasse verwaltet alle Daten, die mit Usern zusammenhängen
/// Ausserdem arbeitet sie als Factory, um Userobjekte zu bauen
///
/// !Es können nur Userobjekte abgespeichert werden, die auch von dieser Klasse erzeugt wurden!
///
///
///    function read($username,$password):$userobjekt
///    function add($realname,$email,$username,$password):Boolean
///    function save($user):Booelan
///    function del($id):Booelan
///    function exists($id):Boolean
///
////////////////////////////////////////////////////////////////////////////////////////////////////
require_once("conf.classes.php");

//Der Userstatus
define ("USER_GUEST"     ,1);
define ("USER_USER"      ,2);
define ("USER_XUSER"     ,4);
define ("USER_ADMIN"     ,8);

//Präfix vor ID und Kennworthash
define ("USER_PREFIX"    ,"%id%");
define ("PASS_PREFIX"    ,"%px%");

//Klasse der Userdaten
class userdata
    {
    //Standarddaten des Users
    var $id       = FALSE;
    var $realname = FALSE;
    var $username = FALSE;
    var $password = FALSE;
    var $email    = FALSE;
    var $status   = FALSE;
    var $active   = FALSE;
    var $data     = FALSE;

    //Welchen Gruppen gehört er an
    var $groups = array();
    }


//Eigentliche Klasse
class user
    {
    //Private
    var $_idbuffer = array();
    var $_states   = array();
    var $_registry = FALSE;

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Konstruktor
    function user(&$registry)
        {
        //Unsere Registrierung intern ablegen
        $this->_registry=$registry;
        
        //Alle zugelassenen Benutzerzustände enumerieren
        $this->_states["guest"] = USER_GUEST;
        $this->_states["user"]  = USER_USER;
        $this->_states["xuser"] = USER_XUSER;
        $this->_states["admin"] = USER_ADMIN;
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
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
        $result[CLASS_INDEX_ID]        = "user";            //ID unserer Klasse, nur alphanumerisch
        $result[CLASS_INDEX_NAME]      = "user";            //Name der Klasse
        $result[CLASS_INDEX_VERSION]   = "0.2";             //Version der Klasse
        $result[CLASS_INDEX_REGISTRY]  = TRUE;              //Wird eine Registry benötigt
        $result[CLASS_INDEX_DATABASE]  = FALSE;             //Wird eine Datenbank benötigt
        $result[CLASS_INDEX_CLEANUP]   = FALSE;             //Soll die Datenbank initialisiert werden ?
        $result[CLASS_INDEX_AUTOLOAD]  = TRUE;              //Soll die Klasse beim Systemstart geladen werden ?
        $result[CLASS_INDEX_COMPRESSED]= TRUE;              //Soll die Datenbank komprimiert werden (gzip)
        $result[CLASS_INDEX_RUNLEVEL]  = 5;                 //In welchen Runlevel soll die Klasse geladen werden
        $result[CLASS_INDEX_RPC_EXPORT]= array("add",   //Welche Methoden der Klasse können per RPC aufgerufen werden
                                               "save");
        return($result);
        }

    //Hier können bei der Installation Daten in die Registry geschrieben werden
    function preset(&$registry)
        {
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen Zeiger auf This liefern
    function getthis()
      {
      $self=&$this;
      return($self);
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die kpl. Daten der Registry löschen
    function clear()
      {
      $this->_registry->clear();
      }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen User lesen (gibt ein Usreobjekt zurück)
    function read($username,$password)
        {
        $user=FALSE;
        //Existiert der Benutzer ?
        $id=$this->createid($username);
        if ($this->exists($id)==TRUE)
            {
            $path=$this->createpath($id);

            //Stimmt das Kennwort ?
            $password=$this->createpass($password);
            $pass=$this->_registry->read($path,"password",ID_NONE);
            if ( $pass == $password )
                {
                //Userobjekt bauen
                $user=$this->createuserobject();
                $user->id       = $id;
                $user->realname = $this->_registry->read($path,"realname","");
                $user->username = $this->_registry->read($path,"username","");
                $user->password = $this->_registry->read($path,"password","");
                $user->email    = $this->_registry->read($path,"email","");
                $user->status   = $this->_registry->read($path,"status",USER_GUEST);
                $user->groups   = $this->_registry->enum($path."groups");
                $user->data     = unserialize( $this->_registry->read($path,"data",serialize( array() ) ) );
                $user->active   = TRUE;
                
                //Die ID merken wir uns um Manipulation zu vermeiden
                $this->_idbuffer[$user->id]=$user->id;
                }
            }
        return($user);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen User zufügen (gibt ein Userobject zurück)
    function add($realname,$email,$username,$password)
        {
        $result=FALSE;

        //Gibt es den User schon ?
        $id = $this->createid($username);
        if ($this->exists($id)==FALSE)
            {
            //Nein, dann anlegen
            $user=$this->createuserobject();
            
            $user->id=$id;
            $user->status=USER_GUEST;
            $user->realname=$realname;
            $user->username=$username;
            $user->password=$this->createpass($password);
            $user->email=$email;

            //Die ID merken wir uns um Manipulation zu vermeiden
            $this->_idbuffer[$user->id]=$user->id;

            //Und abspeichern
            if ($this->save($user)==TRUE)
              {
              $result=$user;
              }
            }
        else
            {
            //User existiert schon
            }
        return($result);
        }
        

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen User abspeichern
    function save($user)
        {
        $result=FALSE;
        
        //Auch wirklich ein Objekt ?
        if ($this->checkuserobject($user)==TRUE)
            {
            //Wurde auch von uns erzeugt ?
            if ( in_array($user->id,$this->_idbuffer)==TRUE)
                {
                //Und abspeichern
                $path=$this->createpath($user->id);
                $this->_registry->write($path,"id"      ,$user->id);
                $this->_registry->write($path,"status"  ,$user->status);
                $this->_registry->write($path,"realname",$user->realname);
                $this->_registry->write($path,"username",$user->username);
                $this->_registry->write($path,"password",$this->createpass($user->password));
                $this->_registry->write($path,"email"   ,$user->email);
                $this->_registry->write($path,"data"    ,serialize($user->email));
                $result=TRUE;
                }
            }
        return($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Einen User entfernen
    function del($input)
        {
        $this->_registry->del($this->createpath($this->createid($input)));
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Prüfen, ob ein Benutzer schon existiert
    //Es wird der Userstatus zurückgeliefert
    function exists($input)
        {
        return ( $this->_registry->read ($this->createpath($this->createid($input)),"status",FALSE) );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Defaultwerte eines Userobjektes setzen und dieses zurückgeben
    function createuserobject()
        {
        $result = new userdata();
        $result->id       = ID_NONE;
        $result->realname = "noone";
        $result->username = "anonymous";
        $result->password = "1234";
        $result->email    = "noone@void.nil";
        $result->status   = USER_GUEST;
        $result->active   = TRUE;
        $result->groups   = array();
        $result->data     = array();

        return ($result);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Prüfen, ob es ein Userobjekt ist
    function checkuserobject($object)
        {
        return ( get_class($object)=="userdata" );
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Die ID eines Benutzers erzeugen
    //Ist es schon eine ID wird diese direkt zurückgegeben
    function createid($input)
        {
        if (strpos($input,USER_PREFIX)===0)
            {
            $result=$input;
            }
        else
            {
            $result=USER_PREFIX.callmethod("crypt","hash",($input));
            }
        return ($result);
        }

    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Kennwort vercrypten
    function createpass($input)
        {
        if (strpos($input,PASS_PREFIX)===0)
            {
            $result=$input;
            }
        else
            {
            $result=PASS_PREFIX.callmethod("crypt","passhash",($input));
            }
        return ($result);
        }
        
    ////////////////////////////////////////////////////////////////////////////////////////////////////
    //Den Pfad zu den User bauen
    function createpath($id)
        {
        return("user/".$id."/");
        }
    }
</script>