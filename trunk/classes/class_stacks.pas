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
unit class_stacks;
////////////////////////////////////////////////////////////////////////////////
///
/// Stacks v0.3
/// Borg@Sven-of-Nine.de
///
/// Simple Stacks für ein paar Datentypen
///
////////////////////////////////////////////////////////////////////////////////
interface
uses Unit_TypeDefs;

type TStringStack = class (TObject)
     protected
           aStringStack  : Array of String;
           bLocked       : Boolean;
     private
     public
           constructor create ();
           destructor  free   ();
           //Stringstack
           procedure Flush  ();
           procedure Push   (Value:LongString);
           function  Pop    ()                    :LongString;
           function  Top    ()                    :LongString;
           function  Bottom ()                    :LongString;
           function  Sum    (sGlue:LongString)    :LongString;
           function  Size   ()                    :Unsigned32;
           function  Count  ()                    :Unsigned32;
           procedure Sort   (ASC : Boolean=TRUE);
           function  Lock   ()                    :Boolean;
           function  Unlock ()                    :Boolean;
end;

//Integerstack
type TIntegerStack = class (TObject)
     protected
           aIntegerStack : array of signed64;
           bLocked       : Boolean;
     private
     public
           constructor create ();
           destructor  free   ();
           procedure Flush ();
           procedure Push  (Value:Signed64);
           function  Pop   ()                    :Signed64;
           function  Top   ()                    :Signed64;
           function  Bottom()                    :Signed64;
           function  Sum   ()                    :Signed64;
           function  Size  ()                    :Unsigned32;
           function  Count  ()                    :Unsigned32;
           procedure Sort  (ASC : Boolean=TRUE);
           function  Lock   ()                    :Boolean;
           function  Unlock ()                    :Boolean;
end;

//Pointerstack
type TPointerStack = class (TObject)
     protected
           aPointerStack : array of Pointer;
           bLocked       : Boolean;
     private
     public
           constructor create ();
           destructor  free   ();
           procedure Flush    ();
           procedure Push     (Value:Pointer);
           function  Pop      ()                    :Pointer;
           function  Top      ()                    :Pointer;
           function  Bottom   ()                    :Pointer;
           function  Size     ()                    :Unsigned32;
           function  Count  ()                      :Unsigned32;
           function  Lock   ()                      :Boolean;
           function  Unlock ()                      :Boolean;
     end;


implementation

////////////////////////////////////////////////////////////////////////////////
/// Hilfsfunktionen
////////////////////////////////////////////////////////////////////////////////
//Auf oder Absteigend vergleichen
function SubCompare(data1,data2 : Signed64; bSortmode : boolean):Boolean; overload;
begin
     //Scheinbar schluckt der FPC diese Konstruktion nicht
     // result:=Boolean(data1 > data2) xor bSortMode;

     //Also machen wir's mit IF THEN
     if (bSortMode) then

        begin
             Result:=(data1 < data2);
        end
     else
        begin
             Result:=(data1 > data2);
        end;
end;
//Auf oder Absteigend vergleichen
function SubCompare(data1,data2 : LongString; bSortmode : boolean):Boolean; overload;
begin
     //Scheinbar schluckt der FPC diese Konstruktion nicht
     // result:=Boolean(data1 > data2) xor bSortMode;

     //Also machen wir's mit IF THEN
     if (bSortMode) then

        begin
             Result:=(data1 < data2);
        end
     else
        begin
             Result:=(data1 > data2);
        end;
end;



procedure SubSortInteger(var data : array of Signed64; s64Left,s64Right: signed64; asc : boolean);
//Eigentliche Sortierung
var
   s64LeftNew  : Signed64;
   s64RightNew : Signed64;
   s64Mid      : Signed64;
   s64Swap     : Signed64;
begin
     //Mittleres Segment holen
     s64Mid := data[ ( s64Left + s64Right) shr 1];

     //Positionen merken
     s64LeftNew  := s64Left;
     s64RightNew := s64Right;

     //Sortierschleife
     while ( s64LeftNew < s64RightNew ) do
           begin
                //Links bis zur Kollision gehen
                while (SubCompare(s64Mid,data[s64LeftNew],asc)) do
                      begin
                           inc (s64LeftNew);
                      end;
                //Rechts bis zur Kollision gehen
                while (SubCompare(data[s64RightNew],s64Mid,asc)) do
                      begin
                           dec (s64RightNew);
                      end;
                      
                //Vertauschung notwendig ?
                if (s64LeftNew <= s64RightNew) then
                   begin
                        //Daten vertauschen
                        s64Swap:=data[s64RightNew];
                        data[s64RightNew]:=data[s64LeftNew];
                        data[s64LeftNew]:=s64Swap;

                        //Positionen aktualisieren
                        inc(s64LeftNew);
                        dec(s64RightNew);
                   end;
           end;
     //Noch Sortierungsbedarf ?
     if (s64Left < s64RightNew) then
        begin
             //Untersegment sortieren
             SubSortInteger(data,s64Left,s64RightNew,asc);
        end;
     if (s64LeftNew < s64Right) then
        begin
             //Untersegment sortieren
             SubSortInteger(data,s64LeftNew,s64Right,asc);
        end;

end;


procedure SubSortStrings(var data : array of LongString; s64Left,s64Right: Signed64; asc : Boolean);
var
   s64LeftNew  : Signed64;
   s64RightNew : Signed64;
   sMid        : Longstring;
   sSwap       : Longstring;
begin
     //Mittleres Segment holen
     sMid := data[ ( s64Left + s64Right) shr 1];

     //Positionen merken
     s64LeftNew  := s64Left;
     s64RightNew := s64Right;

     //Sortierschleife
     while ( s64LeftNew < s64RightNew ) do
           begin
                //Links bis zur Kollision gehen
                while (SubCompare(sMid,data[s64LeftNew],asc)) do
                      begin
                           inc (s64LeftNew);
                      end;
                //Rechts bis zur Kollision gehen
                while (SubCompare(data[s64RightNew],sMid,asc)) do
                      begin
                           dec (s64RightNew);
                      end;

                //Vertauschung notwendig ?
                if (s64LeftNew <= s64RightNew) then
                   begin
                        //Daten vertauschen
                        sSwap:=data[s64RightNew];
                        data[s64RightNew]:=data[s64LeftNew];
                        data[s64LeftNew]:=sSwap;

                        //Positionen aktualisieren
                        inc(s64LeftNew);
                        dec(s64RightNew);
                   end;
           end;
     //Noch Sortierungsbedarf ?
     if (s64Left < s64RightNew) then
        begin
             //Untersegment sortieren
             SubSortStrings(data,s64Left,s64RightNew,asc);
        end;
     if (s64LeftNew < s64Right) then
        begin
             //Untersegment sortieren
             SubSortStrings(data,s64LeftNew,s64Right,asc);
        end;

end;



////////////////////////////////////////////////////////////////////////////////
// Stringstacks
////////////////////////////////////////////////////////////////////////////////
constructor TStringStack.Create ();
begin
end;

destructor  TStringStack.Free   ();
begin
     //Alles freigeben
     Self.Flush;
end;

function  TStringStack.Lock   ():Boolean;
begin
     if (Self.bLocked) then
        begin
             result:=FALSE;
        end
     else
        begin
             Self.bLocked:=TRUE;
             result:=TRUE;
        end;
end;

function TStringStack.Unlock ():Boolean;
begin
     if (Self.bLocked) then
        begin
             Self.bLocked:=FALSE;
             result:=TRUE;
        end
     else
        begin
             result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TStringStack.Flush();
begin
     SetLength(aStringStack,0);
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String auf den Stack legen
procedure TStringStack.Push(Value:LongString);
var
   u32Pos : Unsigned32;
begin
     //Alte Größe holen
     u32Pos:=Self.Size();

     //Um eins vergrößern
     SetLength(aStringStack,u32Pos+1);

     //Und "oben" drauf legen
     aStringStack[u32Pos]:=Value;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String vom Stack holen
function TStringStack.Pop():LongString;
var
   u32Pos : Unsigned32;
begin
     result:='';
     if (Self.Size()>0) then
        begin
             //Größe holen
             u32Pos:=Self.Size();
             //String holen
             result:=aStringStack[u32Pos-1];
             //Und eins kleiner machen
             SetLength(aStringStack,u32Pos-1);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Den Obersten String holen, ohne ihn zu poppen
function TStringStack.Top():LongString;
var
   u32Pos : integer;
begin
     result:='';
     if (Self.Size()>0) then
        begin
             //Größe holen
             u32Pos:=Self.Size();
             //String holen
             result:=aStringStack[u32Pos-1];
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Den untersten String holen, ohne ihn zu poppen
function TStringStack.Bottom():LongString;
begin
     result:='';
     if (Self.Size()>0) then
        begin
             //String holen
             result:=aStringStack[0];
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Alle Strings zusammenkleben
function  TStringStack.Sum(sGlue:LongString):LongString;
var
   u32Index : Unsigned32;
begin
     u32Index:=0;
     result:='';
     while (u32Index < Self.Size() ) do
           begin
                result:=result+aStringStack[u32Index]+sGlue;
                inc(u32Index);
           end;

     //Letzten Kleber wieder abziehen
     result:=copy(result,1,length(result)-Length(sGlue));
end;

////////////////////////////////////////////////////////////////////////////////
//Größe des Stacks holen
function TStringStack.Size():Unsigned32;
begin
     result:=Length(aStringStack);
end;

function TStringStack.Count():Unsigned32;
begin
     result:=Self.Size;
end;


////////////////////////////////////////////////////////////////////////////////
/// Die Stackeinträge sortieren
procedure TStringStack.Sort (ASC : Boolean);
begin
     //Über Subfunktione sortieren
     if (Self.Size()>0) then
        begin
             SubSortStrings(aStringStack,0,Self.Size()-1,asc);
        end;
end;





////////////////////////////////////////////////////////////////////////////////
// Integerstacks
////////////////////////////////////////////////////////////////////////////////
constructor TIntegerStack.Create ();
begin
end;

destructor  TIntegerStack.Free   ();
begin
     //Alles freigeben
     Self.Flush;
end;

procedure TIntegerStack.Flush();
begin
     SetLength(aIntegerStack,0);
end;

function  TIntegerStack.Lock   ():Boolean;
begin
     if (Self.bLocked) then
        begin
             result:=FALSE;
        end
     else
        begin
             Self.bLocked:=TRUE;
             result:=TRUE;
        end;
end;

function TIntegerStack.Unlock ():Boolean;
begin
     if (Self.bLocked) then
        begin
             Self.bLocked:=FALSE;
             result:=TRUE;
        end
     else
        begin
             result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Int64 auf den Stack legen
procedure TIntegerStack.Push(Value:Signed64);
var
   u32Pos : unsigned32;
begin
     //Alte Größe holen
     u32Pos:=Self.Size();

     //Um eins vergrößern
     SetLength(aIntegerStack,u32Pos+1);

     //Und "oben" drauf legen
     aIntegerStack[u32Pos]:=Value;
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Int64 vom Stack holen
function TIntegerStack.Pop():Signed64;
var
   u32Pos : Unsigned32;
begin
     result:=0;
     if (Self.Size()>0) then
        begin
             //Größe holen
             u32Pos:=Self.Size();
             //String holen
             result:=aIntegerStack[u32Pos-1];
             //Und eins kleiner machen
             SetLength(aIntegerStack,u32Pos-1);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Oberste Zahl des Stacks holen, ohne sie zu "poppen" :-)
function TIntegerStack.Top():Signed64;
var
   u32Pos : unsigned32;
begin
     result:=0;
     if (Self.Size()>0) then
        begin
             //Größe holen
             u32Pos:=Self.Size();
             //String holen
             result:=aIntegerStack[u32Pos-1];
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Unterste Zahl des Stacks holen
function TIntegerStack.Bottom():Signed64;
begin
     result:=0;
     if (Self.Size()>0) then
        begin
             //String holen
             result:=aIntegerStack[0];
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Summe des Stacks bestimmen
function  TIntegerStack.Sum():Signed64;
var
   u32Index : Unsigned32;
begin
     u32Index:=0;
     result:=0;
     while (u32Index < Self.Size() ) do
           begin
                result:=result+aIntegerStack[u32Index];
                inc(u32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Größe des Stacks holen
function TIntegerStack.Size():Unsigned32;
begin
     result:=Length(aIntegerStack);
end;
function TIntegerStack.Count():Unsigned32;
begin
     result:=Self.Size;
end;

////////////////////////////////////////////////////////////////////////////////
/// Die Stackeinträge sortieren
procedure TIntegerStack.Sort  (ASC : Boolean);
begin
     if (Self.Size()>0) then
        begin
             SubSortInteger(aIntegerStack,0,Self.Size()-1,ASC);
        end;
end;



////////////////////////////////////////////////////////////////////////////////
// Pointerstacks
////////////////////////////////////////////////////////////////////////////////
//Hier wird der Stack zwar geleert, der Speicher der Pointer aber nicht
//freigegeben. Dies MUSS vom Hauptprogramm aus geschehen, da untypisierte Pointer
//kein direktes Dispose zulassen.
constructor TPointerStack.Create ();
begin
end;

destructor  TPointerStack.Free   ();
begin
     //Alles freigeben
     Self.Flush;
end;

procedure TPointerStack.Flush();
begin
     SetLength(aPointerStack,0);
end;

function  TPointerStack.Lock   ():Boolean;
begin
     if (Self.bLocked) then
        begin
             result:=FALSE;
        end
     else
        begin
             Self.bLocked:=TRUE;
             result:=TRUE;
        end;
end;

function TPointerStack.Unlock ():Boolean;
begin
     if (Self.bLocked) then
        begin
             Self.bLocked:=FALSE;
             result:=TRUE;
        end
     else
        begin
             result:=FALSE;
        end;
end;


//Einen Pointer auf den Stack legen
procedure TPointerStack.Push(Value:Pointer);
var
   u32Pos : unsigned32;
begin
     //Alte Größe holen
     u32Pos:=Self.Size();

     //Um eins vergrößern
     SetLength(aPointerStack,u32Pos+1);

     //Und "oben" drauf legen
     aPointerStack[u32Pos]:=Value;
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Pointer vom Stack holen
function TPointerStack.Pop():Pointer;
var
   u32Pos : Unsigned32;
begin
     //Größe holen
     u32Pos:=Self.Size();

     result:=nil;
     if (u32Pos>0) then
        begin
             //String holen
             result:=aPointerStack[u32Pos-1];
             //Und eins kleiner machen
             SetLength(aPointerStack,u32Pos-1);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Obersten Pointer des Stacks holen, ohne ihn zu "poppen" :-)
function TPointerStack.Top():Pointer;
var
   u32Pos : unsigned32;
begin
     result:=nil;
     if (Self.Size()>0) then
        begin
             //Größe holen
             u32Pos:=Self.Size();
             //String holen
             result:=aPointerStack[u32Pos-1];
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Untersten Pointer des Stacks holen
function TPointerStack.Bottom():Pointer;
begin
     result:=nil;
     if (Self.Size()>0) then
        begin
             //String holen
             result:=aPointerStack[0];
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Größe des Stacks holen
function TPointerStack.Size():Unsigned32;
begin
     result:=Length(aPointerStack);
end;
function TPointerStack.Count():Unsigned32;
begin
     result:=Self.Size;
end;


end.
