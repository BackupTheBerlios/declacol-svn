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
unit Unit_Multiplatform;
////////////////////////////////////////////////////////////////////////////////
///
/// Hilfsunit für leichtere Multiplatformprogrammierung
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
////////////////////////////////////////////////////////////////////////////////
interface

uses
  Unit_Typedefs;

////////////////////////////////////////////////////////////////////////////////
//Dateinamenfunktionen
//Den Namen und Pfad einer Konfigdatei erzeugen
function MultiPlatform_CreateConfigFilename():LongString;


//Slash des Systems zurückgeben
function MultiPlatform_GetSlash():Char;

implementation
uses Sysutils,Unit_Strings;
////////////////////////////////////////////////////////////////////////////////
//Dateinamenfunktionen
//Den Namen und Pfad einer Konfigdatei erzeugen
function MultiPlatform_CreateConfigFilename():LongString;
begin
     //Linux / Unix ?

{$IFDEF LINUX}
     //Unter Unix wird damit auf /home/$user$/projectname verwiesen
     result:=GetAppConfigFile(FALSE);
     //cfg anhängen
     result:=result+'.conf';
{$ENDIF}

{$IFDEF UNIX}
     //Unter Unix wird damit auf /home/$user$/projectname verwiesen
     result:=GetAppConfigFile(FALSE);
     //cfg anhängen
     result:=result+'.conf';
{$ENDIF}

{$IFDEF WIN32}
     //Unter Windows den Anwendungsnamen + cfg benutzen
     //Anwendung holen
     result:=ParamStr(0);
     
     //Extension rausnehmen
     result:=String_Left(result,Pos('.',result));
     
     //Und neue anhängen
     result:=result+'conf';
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
//Slash des Systems zurückgeben
function MultiPlatform_GetSlash():Char;
begin
{$IFDEF LINUX}
     result:='/';
{$ENDIF}

{$IFDEF UNIX}
     result:='/';
{$ENDIF}

{$IFDEF WIN32}
     result:='\';
{$ENDIF}

end;



end.

