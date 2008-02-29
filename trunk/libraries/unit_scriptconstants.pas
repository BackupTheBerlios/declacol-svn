unit unit_scriptconstants;
{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
Author: Sven Lorenz / Borg@Sven-of-Nine.de
}

interface
uses unit_typedefs;
////////////////////////////////////////////////////////////////////////////////
/// Konstanten und Typen des Scriptparsers
////////////////////////////////////////////////////////////////////////////////
const
     //Benutzes Wildcard
     SCR_WILD        = '*-*';

     ////////////////////////////////////////////////////////////////////////////
     //IDs der Token
     //Interne Befehle
     ID_LOOPSTART    = $000001;
     ID_LOOPEND      = $000002;
     ID_IFSTART      = $000004;
     ID_IFEND        = $000008;
     ID_IFELSE       = $000010;
     ID_ECHO         = $000020;
     ID_EXIT         = $000040;
     ID_CLEAR        = $000080;
     ID_SEED         = $000100;
     ID_INC          = $000200;
     ID_DEC          = $000400;
     ID_SET          = $000800;
     ID_RESET        = $001000;

     //Vergleiche
     ID_CMP_GREATER  = $300000;
     ID_CMP_LESSER   = $300001;
     ID_CMP_EQUAL    = $300002;
     ID_CMP_NOTEQUAL = $300004;
     
     //Verbindung zu externen Funktionen
     ID_FUNC_VALUE   = $400000;
     ID_FUNC_TIME    = $400001;

     //Vordefinierte Variablen
     ID_VAR_DATE     = $500001;
     ID_VAR_TIME     = $500002;
     ID_VAR_RND10    = $500004;
     ID_VAR_RND100   = $500008;
     ID_VAR_RND1000  = $500010;
     ID_VAR_RND10000 = $500020;

     //Typen
     ID_VAR_INTEGER  = $510001;
     ID_VAR_STRING   = $510002;
     ID_VAR_BOOL     = $510004;
     ID_VAR_CONST    = $510008;

     //Formatierung des Textes;
     ID_TEXT_REM     = $f00000;
     ID_TEXT_LF      = $f00001;
     ID_TEXT_SPACE   = $f00002;


     //Zusammenfassungen (zur leichteren Filterung von Befehlsgruppen)
     ID_COMMAND      = $000000;
     ID_COMPARE      = $300000;
     ID_FUNCTION     = $400000;
     ID_VAR          = $500000;
     ID_TYPE         = $600000;
     ID_TEXT         = $f00000;

     ////////////////////////////////////////////////////////////////////////////
     //Zuordnun der Tokens zu den IDs
var
     //Da Pascal mit Indizes Problem hat, machen wir einfach zwei Arrays
     //Bei mehr Befehlen sollte das hier aber automatisch ablaufen
     //Wegen der paar erspare ich mir die Arbeit
     aIDList      : array[0..31] of unsigned32 =
                  (
                  ID_TEXT_REM,
                  ID_LOOPSTART,
                  ID_LOOPEND,
                  ID_IFSTART,
                  ID_IFEND,
                  ID_IFELSE,
                  ID_ECHO,
                  ID_EXIT,
                  ID_CLEAR,
                  ID_SEED,
                  ID_INC,
                  ID_DEC,
                  ID_SET,
                  ID_RESET,
                  ID_CMP_GREATER,
                  ID_CMP_LESSER,
                  ID_CMP_EQUAL,
                  ID_CMP_NOTEQUAL,
                  ID_FUNC_VALUE,
                  ID_FUNC_TIME,
                  ID_VAR_DATE,
                  ID_VAR_TIME,
                  ID_VAR_RND10,
                  ID_VAR_RND100,
                  ID_VAR_RND1000,
                  ID_VAR_RND10000,
                  ID_VAR_CONST,
                  ID_VAR_INTEGER,
                  ID_VAR_STRING,
                  ID_VAR_BOOL,
                  ID_TEXT_LF,
                  ID_TEXT_SPACE
                  );
     //Hier werden den IDs die Tokenstrings zugeordnet
     //Jede ID kann maximal 4 Bezeichner haben
     aCommandList : array[0..31] of array[0..3] of Longstring =
                  (
                   (';'+SCR_WILD+#$A,'/*'+SCR_WILD+'*/'     ,''          ,''),
                   ('loop'          ,''        ,''          ,''),
                   ('pool'          ,''         ,''          ,''),
                   ('if'            ,''        ,''          ,''),
                   ('fi'            ,''        ,''          ,''),
                   ('else'          ,''        ,''          ,''),
                   ('echo'          ,''        ,''          ,''),
                   ('exit'          ,''        ,''          ,''),
                   ('clear'         ,''        ,''          ,''),
                   ('seed'          ,''        ,''          ,''),
                   ('inc'           ,''        ,''          ,''),
                   ('dec'           ,''        ,''          ,''),
                   ('set'           ,''        ,''          ,''),
                   ('reset'         ,''        ,''          ,''),
                   ('>'             ,''        ,''          ,''),
                   ('<'             ,''        ,''          ,''),
                   ('=='            ,''        ,''          ,''),
                   ('!='            ,''        ,''          ,''),
                   ('sendvalue'     ,''        ,''          ,''),
                   ('sendtime'      ,''        ,''          ,''),
                   ('%date%'        ,''        ,''          ,''),
                   ('%time%'        ,''        ,''          ,''),
                   ('%rnd10%'       ,''        ,''          ,''),
                   ('%rnd100%'      ,''        ,''          ,''),
                   ('%rnd1000%'     ,''        ,''          ,''),
                   ('%rnd10000%'    ,''        ,''          ,''),
                   ('%'+SCR_WILD+'%',''        ,''          ,''),
                   ('('+SCR_WILD+')',''        ,''          ,''),
                   ('['+SCR_WILD+']',''        ,''          ,''),
                   ('%true%'        ,''        ,''          ,''),           
                   (#10             ,''        ,''          ,''),
                   (#32             ,''        ,''          ,'')
                  );



implementation

end.
