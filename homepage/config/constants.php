<script language="php">
/*
 _|    _|            _|                              _|                _|            
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|  
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|    
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|  
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|  
                                                                                     
(c) 2009 Borg@sven-of-nine.de
*/

////////////////////////////////////////////////////////////////////////////////////////////////////////
//Globale Konstanten
@define ("ID_NONE"       ,"0");           //String da ein StrToInt auch null ergibt
@define ("EVERYTHING"    ,"42");

////////////////////////////////////////////////////////////////////////////////////////////////////////
//Messages-Konstanten
@define ("MSG_NONE"     ,ID_NONE);
@define ("MSG_DATA"     ,ID_NONE + 1);    //Allgemeine Datenübertragung
@define ("MSG_KILL"     ,ID_NONE + 2);    //Die empfangene Session wird beendet (der User ausgeloggt)
@define ("MSG_LOGOUT"   ,ID_NONE + 3);    //Der User wird abgemeldet

</script>