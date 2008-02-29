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


/////////////////////////////////////////////////////////////////////////////////
///
/// Wandelt Windows in Klartext-Errorcodes
///
///
///
/////////////////////////////////////////////////////////////////////////////////
unit Unit_ErrorCodes;

interface

uses windows,SysUtils;

function ErrorMessage(ErrorCode:LongWord):String;
function LastErrorMessage():String;
implementation

function ErrorMessage(ErrorCode:LongWord):String;
begin
     Result:=SysErrorMessage(ErrorCode);
end;

function LastErrorMessage():String;
begin
     Result:=SysErrorMessage(GetLastError);
end;

end.
