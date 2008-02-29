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

{$IFDEF FPC}
        //Einstellungen für den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
unit Unit_StringSets;
interface
////////////////////////////////////////////////////////////////////////////////
/// Ein paar ganz nützliche Sets
////////////////////////////////////////////////////////////////////////////////
const
     //Alphabet
     ALPHASET  =
               [
                'A','B','C','D','E','F','G','H','I','J',
                'K','L','M','N','O','P','Q','R','S','T',
                'U','V','W','X','Y','Z',
                'a','b','c','d','e','f','g','h','i','j',
                'k','l','m','n','o','p','q','r','s','t',
                'u','v','w','x','y','z'
                ];
     //Zahlen
     NUMBERSET =
               [
                '0','1','2','3','4','5','6','7','8','9'
               ];
     //Buchstaben und Zahlen
     ALPHANUMSET=ALPHASET + NUMBERSET;

     //Alles in Base64
     BASE64SET = ALPHANUMSET +
               [
                '=','/','+','-','_'
                ];
     //Mathematische Zeichen
     MATHSET    =
                [
                '+','-','*','/',':',      //Grundrechenarten
                '.',',',                  //Komma
                '(',')','[',']',          //Klammern
                '^',                      //Hoch
                '<','=','>',              //Vergleich
                '?','!','~'              //Diverses
                 ];
     //Fließkommazahlen
     FLOATSET   = NUMBERSET +
                [
                '-',
                '.',
                ','
                ];
     //Übliche Satzzeichen
     SIGNSET    =
                [
                '.','?',                  //Satzende
                ',',';',':',              //Satzteilung
                '(',')','[',']',          //Klammern
                '&','§',                  //Sonderzeichen
                '@'                       //EMail
                ];

    //Websafe
    URLSET      = ALPHANUMSET +
                [
                '/',':','~',
                '@','.',' ','&','?',
                '-','_','%'
                ];

     //Spaces
     SPACESET   =
                [
                 ' ','_',#9
                ];
     //Linebreaks
     BREAKSET   =
                [
                #10,#13
                ];
     //Vokale
     VOCALSET =
                 [
                 'A','E','I','O','U',
                 'a','e','i','o','u'
                 ];
     //Pfadzeichen
     PATHSET  =
                 [
                 '\',
                 '-',
                 ' ',
                 '+',
                 '#',
                 ',',
                 '~',
                 '^',
                 '!',
                 '§',
                 '$',
                 '%',
                 '&',
                 '/',
                 '(',
                 ')',
                 '=',
                 'ß',
                 '`',
                 '´',
                 '@',
                 ';',
                 '_',
                 '{',
                 '}',
                 '[',
                 ']',
                 '°',
                 '.',
                 ':'
                 ];

implementation

end.

