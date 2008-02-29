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
        //Einstellungen f�r den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
unit Unit_Log;
/////////////////////////////////////////////////////////////////////////////////
///
/// Unit zur Vereinfachung von Logdatei-Aufzeichnungen
///
///
///
/////////////////////////////////////////////////////////////////////////////////
interface
uses Unit_TypeDefs;

procedure Log_Add   (Filename:LongString;Text:LongString;AddTime:Boolean=TRUE);
procedure Log_Clear (Filename:LongString);

implementation
uses SysUtils,Unit_Time;


/////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag unten anf�gen
procedure Log_Add(Filename:LongString;Text:LongString;AddTime:Boolean=TRUE);
var
   fOut  : TextFile;
begin
     try
     //Wenn gew�nscht Timestamp vorne ankleben
     if (AddTime) then
        begin
             Text:=IntToStr(Time_DateTimeToUnixTimeStamp(Now))+'|'+Text;
        end;

     //Trimmen
//     Text:=Trim(Text);

     //Kanal �ffnen
     AssignFile(fOut,Filename);

     //Wenn die Datei noch nicht existiert, dann erzeugen
     if (not FileExists(Filename)) then
        begin
             Rewrite(fOut);
        end
     else
        begin
             //Ansonsten ans Ende der Datei springen
             Append(fOut);
        end;

     //Text ausgeben
     WriteLn(fOut,Text);
     
     //Kanal schlie�en
     CloseFile(fOut);
     except
     end;
end;

/////////////////////////////////////////////////////////////////////////////////
// Logdatei l�schen
procedure Log_Clear(Filename:LongString);
var
   fOut : Textfile;
begin
     //Datei existiert ?
     if (FileExists(Filename)) then
        begin
             //�ffnen
             AssignFile(fOut,Filename);
             //Leeren
             Rewrite(fOut);
             //Schlie�en
             CloseFile(fOut);
        end;
end;

end.
