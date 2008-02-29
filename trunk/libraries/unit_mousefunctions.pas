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

//////////////////////////////////////////////////////////////////////////////////////////
//
// Mausfunktionen (Bewegen etc)
//
//
//////////////////////////////////////////////////////////////////////////////////////////

unit Unit_MouseFunctions;
interface
uses windows, Messages, Forms, graphics;

//Bewegt die Mouse an den definierten Punkt
procedure MoveMouseTo(MouseTarget: TPoint);

//Clickt mit der Mouse
procedure ClickMouse();

//DoppelClickt mit der Mouse
procedure DoubleClickMouse();

//Scrollt ein Scrollfenster um den Wert move
procedure Scroll(Handle: THandle; move: Integer);

//Macht einen Screenshot unter der Maus mit den Maßen X,Y
procedure GetScreenUnderMouse(XSize, YSize: Integer);

//Bewegt die Maus an einen Zufälligen Punkt im Bereich Area
procedure RandomMove(Area: TRect);

//Umfährt den Bereich Area mit der Muas
procedure TraceArea(Area: TRect);

//Fügt einem Punkt eine zufällige Abweichung zu.
function RandomizePoint(Point: TPoint): TPoint;

//Ermittelt den Handle des Objectes unter der Mouse
function GetHandleFromMouse(): THandle;

//Ist die Mouse im Angegebenen Bereich
function MouseInRect(Mouse: TPoint; Area: TRect): Boolean;

var
   //Flag, ob eine Muashandlung durchgeführt wird
  b_MouseActive: Boolean = FALSE;
  MousePic: TBitMap;
implementation
//////////////////////////////////////////////////////////////////////////////////////////////////////
//Bewegt die Mouse an den definierten Punkt

procedure MoveMouseTo(MouseTarget: TPoint);
var
  MousePos: TPoint;
  x, y: Single;
  dx, dy: Single;
  dmax: Integer;
  z: Integer;
begin
     //Mausposition holen
  GetCursorPos(MousePos);
  z := 0;

  x := MousePos.x;
  y := MousePos.y;

     //Entfernung bestimmen
  dx := abs(MousePos.x - MouseTarget.x);
  dy := abs(MousePos.y - MouseTarget.y);

     //Anzahl der Schritte bis zum Ziel
  dmax := 200;

     //Nullwerte verhindern
  if (dx = 0) then dx := 1;
  if (dy = 0) then dy := 1;

     //Steigung bestimmen
  dx := dx / dmax;
  dy := dy / dmax;

     //Richtung bestimmen
  if (MousePos.x > MouseTarget.x) then dx := -dx;
  if (MousePos.y > MouseTarget.y) then dy := -dy;

     //Los gehts
  b_MouseActive := TRUE;

     //Solange, bis wir am Punkt sind, oder z überläuft
  while ((MousePos.x <> MouseTarget.x) or (MousePos.y <> MouseTarget.y)) and (z < 4096) do
  begin
                //Aktuelle Position holen
    GetCursorPos(MousePos);

                //Muessen wir noch was tun ?
    if (MousePos.X <> MouseTarget.X) then x := x + dx;
    if (MousePos.Y <> MouseTarget.Y) then y := y + dy;

                //5 Millisekunden warten
//    delay(5);

                //Und den Cursor setzen
    SetCursorPos(Round(x), Round(y));
    inc(z);
  end;
     //Fertig
  b_MouseActive := FALSE;
end;




//////////////////////////////////////////////////////////////////////////////////////////////////////
//Clickt mit der Mouse

procedure ClickMouse();
var
  pos: LongInt;
  pTemp: TPoint;
  h: THandle;
begin
  b_MouseActive := TRUE;
     //Cursorposition holen
  GetCursorPos(PTemp);

     //Parenthandle holen
  h := WindowFromPoint(PTemp);

     //Coordinaten umrechnen
  ScreenToClient(h, PTemp);
  pos := MakeLong(PTemp.x, PTemp.y);

     //Und die entsprechende Nachricht schicken
  PostMessage(H, WM_LBUTTONDOWN, 0, pos);
  PostMessage(H, WM_LBUTTONUP, 0, pos);

  b_MouseActive := FALSE;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Clickt doppelt mit der Maus

procedure DoubleClickMouse();
var
  pos: LongInt;
  pTemp: TPoint;
  h: THandle;
begin
  b_MouseActive := TRUE;

     //Cursorposition holen
  GetCursorPos(PTemp);

     //Parenthandle holen
  h := WindowFromPoint(PTemp);

     //Coordinaten umrechnen
  ScreenToClient(h, PTemp);
  pos := MakeLong(PTemp.x, PTemp.y);

     //Und die entsprechende Nachricht schicken
  PostMessage(H, WM_LBUTTONDOWN, 0, pos);
  PostMessage(H, WM_LBUTTONUP, 0, pos);
  PostMessage(H, WM_LBUTTONDBLCLK, 0, pos);

  b_MouseActive := FALSE;

end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Scrollt ein Fenster um den Wert move

procedure Scroll(Handle: THandle; move: Integer);
var
  pos: LongInt;
  wheel: LongInt;
  pTemp: TPoint;
  h: THandle;
begin
  b_MouseActive := TRUE;


     //Cursorposition holen
  GetCursorPos(PTemp);

     //Parenthandle holen
  h := WindowFromPoint(PTemp);

     //Coordinaten umrechnen
  ScreenToClient(h, PTemp);
  pos := MakeLong(PTemp.x, PTemp.y);

  wheel := MakeLong(SB_THUMBPOSITION, move);

     //Und die entsprechende Nachricht schicken
  PostMessage(H, WM_VSCROLL, wheel, Handle);

  b_MouseActive := FALSE;
end;



//////////////////////////////////////////////////////////////////////////////////////////////////////
//Bewegt die Maus an einen Zufälligen Punkt im Bereich Area

procedure RandomMove(Area: TRect);
var
  PTemp: TPoint;
  dx, dy: LongInt;
begin
  b_MouseActive := TRUE;

  dx := abs(Area.Right - Area.Left);
  dy := abs(Area.Bottom - Area.Top);

  PTemp.x := Area.Left + Random(dx);
  PTemp.y := Area.Top + Random(dy);

  MoveMouseTo(PTemp);

  b_MouseActive := FALSE;
end;



//////////////////////////////////////////////////////////////////////////////////////////////////////
//Umfährt den Bareich Area mit der Mouse

procedure TraceArea(Area: TRect);
var
  PTemp: TPoint;
begin
  b_MouseActive := TRUE;

  PTemp := Area.TopLeft;
  MoveMouseTo(PTemp);

//  Delay(1000);

  PTemp.x := Area.Right;
  MoveMouseTo(PTemp);

//  Delay(1000);

  PTemp.y := Area.Bottom;
  MoveMouseTo(PTemp);

  PTemp.x := Area.Left;
  MoveMouseTo(PTemp);

  PTemp.y := Area.Top;
  MoveMouseTo(PTemp);

  b_MouseActive := FALSE;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Macht einen Screenshot unter der Maus mit den Maßen X,Y

procedure GetScreenUnderMouse(XSize, YSize: Integer);
var
  H_Screen: THandle;
  MousePos: TPoint;
begin
     //Mouseposition holen
  GetCursorPos(MousePos);

     //Bitmap erzeugen und größe festlegen
  if (MousePic is TBitmap) then else MousePic := TBitMap.Create;

  MousePic.Width := XSize;
  MousePic.Height := YSize;

     //Position des Bildauschnittes festlegen
  MousePos.X := MousePos.x - Round(XSize / 2);
  MousePos.Y := MousePos.Y - Round(YSize / 2);

     //Bildschirmhandle holen
  H_Screen := GetDC(0);

     //Unseren Ausschnitt rausholen
  BitBlt(MousePic.Canvas.Handle, 0, 0, XSize, YSize, H_Screen, MousePos.x, MousePos.y, SRCCOPY);

     //Bildschirm wieder freigeben
  ReleaseDC(0, H_Screen);
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////
//Fügt einem Punkt eine Zufallswert hinzu

function RandomizePoint(Point: TPoint): TPoint;
begin
  Result.x := Point.x + (Random(10) - 5);
  Result.y := Point.y + (Random(10) - 5);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Ermittelt den Handle des Objectes unter der Mouse

function GetHandleFromMouse(): THandle;
var
  MousePos: TPoint;
begin
     //Mouseposition holen
  GetCursorPos(MousePos);
  Result := WindowFromPoint(MousePos);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Mouse in Area ?

function MouseInRect(Mouse: TPoint; Area: TRect): Boolean;
var
  diffX: Integer;
  diffY: Integer;
begin
  diffX := MousePic.Width;
  diffY := MousePic.Height;

     //Mouseposition holen
  if (Mouse.x > (Area.Left + diffX)) and
    (Mouse.x < (Area.Right - diffX)) and
    (Mouse.y > (Area.Top + diffY)) and
    (Mouse.y < (Area.Bottom - diffY)) then Result := TRUE else Result := FALSE;
end;

end.
