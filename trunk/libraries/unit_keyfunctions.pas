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
////////////////////////////////////////////////////////////////////////////////////////////////
///
/// Tastaturfunktionen (Fernsteuerung etc)
///
/// v 0.1.5
///
///
///
///
////////////////////////////////////////////////////////////////////////////////////////////////


unit Unit_KeyFunctions;

interface

uses
  Windows, Messages, Classes, Forms, Unit_FormEffects;

const
  OPENBRACE = '{';
  CLOSEBRACE = '}';
  PLUS = '+';
  CARET = '^';
  PERCENT = '%';
  SPACE = ' ';
  TILDE = '~';
  SHIFTKEY = $10;
  CTRLKEY = $11;
  ALTKEY = $12;
  ENTERKEY = $13;
  OPENPARENTHESES = '(';
  CLOSEPARENTHESES = ')';
  NULL = #0;


procedure SendString(MainWindow, TextObject: THandle; sString: string);

procedure SendKey(Target: THandle; Key: Char);
procedure SendReturn(Target: THandle);
procedure ClearField(Target: THandle);

implementation

procedure SendString(MainWindow, TextObject: THandle; sString: string);
var
  z: Integer;
  temp: char;
begin
     //Fenster nach vorne holen
     MakeWindowActive(MainWindow);

     //Textfenster löschen
     ClearField(TextObjetc);

     //Text eingeben
     for z := 1 to Length(sString) do
         begin
              Temp := sString[z];
              SendKey(TextObject, Temp);
              Sleep(100 + Random(100));
         end;

     //Und Return drücken
     SendReturn(TextObject);
end;


procedure sendkey(Target: THandle; Key: Char);
var
  LPara: LongInt;
  KeyScan: Integer;
  OEM: Integer;
begin
  lpara := $00000001;
  SendMessage(Target, WM_CHAR, Ord(Key), lPara);
end;


procedure SendReturn(Target: THandle);
var
  LPara: LongInt;
begin
  lpara := $00000001;

  SendMessage(Target, WM_KEYDOWN, WParam(Chr(13)), lPara);
  Delay(10);
  SendMessage(Target, WM_KEYUP, WParam(Chr(13)), lPara);
end;

procedure ClearField(Target: THandle);
var
  s: string;
  LPara: LongInt;
begin
  lpara := $00000001;

  s := '';
  SendMessage(Target, WM_KEYDOWN, WParam(Chr(32)), lPara);
  SendMessage(Target, WM_SETTEXT, 0, lParam(s));
  SendMessage(Target, WM_KEYUP, WParam(Chr(32)), lPara);
end;


end.
