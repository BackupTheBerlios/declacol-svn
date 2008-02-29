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

////////////////////////////////////////////////////////////////////////////////
///
/// Unit für Formulareffekte (c) 2005 Borg@Sven-of-Nine.de
///
/// SetTransparentForm(Form.Handle,Wert) macht eine Fenster transparent
/// 
/// NoBorder verhilft Fenstern ohne Rand zu eine Resizefunktion
/// NoBorder_Init(Randbreite) initialisiert die Funktion
///
/// von OnMouseMove auf NoBorder_Move verweisen
/// von OnMouseDown auf NoBorder_Donw verweisen 
///
////////////////////////////////////////////////////////////////////////////////
unit Unit_NoBorder;

interface
uses controls;

     procedure NoBorder_Init(BorderSize:Integer;Resize:Boolean);
    procedure NoBorder_Move(Ctrl:tcontrol; X, Y: Integer);
    procedure NoBorder_Down(Ctrl:Tcontrol; Button: TMouseButton; X, Y: Integer);


implementation
uses Windows,Messages;
const
     //TransForm Konstanten
     WS_EX_LAYERED = $80000;
     LWA_COLORKEY = 1;
     LWA_ALPHA    = 2;

     //SysCOmmand-Konstanten
     sc_DragMove = $f012;
     sc_Leftsize = $f001;
     sc_Rightsize = $f002;
     sc_Upsize = $f003;
     sc_UpLeftsize = $f004;
     sc_DnRightsize = $f005;
     sc_Dnsize = $f006;
     sc_DnLeftsize = $f007;
     sc_UpRightsize = $f008;

var
     iBorderSize : integer = 10;
     bResize     : Boolean = TRUE;

////////////////////////////////////////////////////////////////////////////////
procedure NoBorder_Init(BorderSize:Integer;Resize:Boolean);
begin
     iBorderSize:=10;
     bResize:=Resize;
end;

procedure NoBorder_Move(Ctrl:tcontrol; X, Y: Integer);
var
   cCursor : tCursor;
   bLeft   : Boolean;
   bRight  : Boolean;
   bTop    : Boolean;
   bBottom : Boolean;
begin
     //Min und Max des Rahmens begrenzen
     if (iBorderSize<2) or (iBorderSize>50) then iBorderSize := 8;

     //Sind wir im Rahmen ?
     bTop   := (y < iBorderSize);
     bLeft  := (x < iBorderSize);
     bRight := (ctrl.width-x  <= iBorderSize);
     bBottom:= (ctrl.height-y <= iBorderSize);

     //Cursor setzen
     cCursor := crDefault;

     // Ränder abfragen und den Cursor entsprechend setzen
     if bTop    then cCursor := crSize;
     if (bResize) then
        begin
             if bBottom then cCursor := crSizeNS;
             if (bLeft or bRight) then cCursor := crSizeWE;
             //Ecken
             if ((bLeft  and bTop) or (bRight and bBottom)) then cCursor := crSizeNWSE;
             if ((bRight and bTop) or (bLeft  and bBottom)) then cCursor := crSizeNESW;
        end;

     // change if altered
     if ctrl.cursor <> cCursor then ctrl.cursor := cCursor;
end;

procedure NoBorder_Down(Ctrl:Tcontrol; Button: TMouseButton; X, Y: Integer);
var
   iAction : integer;
begin
     //Links geklickt ?
     if button <> mbLeft then exit;
     //Kein Resize oder so..
     if ctrl.cursor = crdefault then exit;

     iAction := 0;
     //Maus freigeben
     ReleaseCapture;
     if ctrl.cursor = crSizeWE
        then begin
             if X < iBorderSize then
                begin
                     iAction := sc_LeftSize
                end
             else
                begin
                     iAction := sc_RightSize;
                end;
        end;

     if ctrl.cursor = crSizeNS then
        begin
             iAction := sc_DnSize;
        end;

     if ctrl.cursor = crSizeNWSE then
        begin
             if X < iBorderSize then
                begin
                     iAction := sc_UpLeftSize
                end
             else
                begin
                     iAction := sc_UpRightSize;
                end;
        end;
     if ctrl.cursor = crSizeNESW then
        begin
             if X < iBorderSize then
                begin
                     iAction := sc_DnLeftSize
                end
             else
                begin
                    iAction := sc_DnRightSize
                end;
        end;

     if ctrl.cursor = crSize then
        begin
             iAction := sc_DragMove;
        end;

     if iAction<>0 then
        begin
             ctrl.Perform(WM_SYSCOMMAND, iAction, 0);
        end;
end;

end.
 