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
unit Unit_Multilanguage;
////////////////////////////////////////////////////////////////////////////////
///
/// Unit zur Konversion von "fremdländischen Zeichensätzen" ins englische
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
////////////////////////////////////////////////////////////////////////////////

{$mode objfpc}{$H+}

interface
uses
    Unit_Typedefs;

//Konvertiert in den englischen Zeichensatz
function MultiLanguage_ConvertToEnglish(sInput:LongString):LongString;

implementation
uses Unit_Strings;

const
     aFrom : array[0..6] of Shortstring =
           (
           'ä','ü','ö','Ä,','Ü','Ö','ß'
           );
     aTo   : array[0..6] of Shortstring =
           (
           'ae','ue','oe','Ae','Ue','Oe','ss'
           );

////////////////////////////////////////////////////////////////////////////////
function MultiLanguage_ConvertToEnglish(sInput:LongString):Longstring;
var
   u32Index : unsigned32;
begin
     //ergebnis übernehmen
     result:=sInput;

     //Und einfach alle ersetzungen durchgehen
     u32Index:=0;
     while (u32Index < Length(aFrom)) do
           begin
                Result:=String_Replace(result,aFrom[u32Index],aTo[u32Index]);
                writeln(aTo[u32Index]);
                inc(u32Index);
           end;
end;


end.

