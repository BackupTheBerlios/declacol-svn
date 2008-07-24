unit class_btree;
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
/// Binärer Baum für bytearrays
///
////////////////////////////////////////////////////////////////////////////////

interface

uses unit_compiler,unit_typedefs,windows;

//Mögliche Blatttypen
type TOrientation = (Left,Right);

//Daten im Blatt
type TLeafData = array of byte;

//Struct eines Blattes
//Binärer Baum => zwei Äste
type pLeaf = ^TLeaf;
     TLeaf = record
     ID      : unsigned32;
     Data    : TLeafData;
     pParent : pLeaf;
     pLeft   : pLeaf;
     pRight  : pLeaf;
end;

type pBTree = ^pBTree;
     TBTree = class (TObject)
     private
      //Datenwurzel
      Root : pLeaf;

     protected
      function AddLeaf     (Root : pLeaf; Orientation : TOrientation; NewLeaf : pLeaf):Boolean;
      function NewLeaf(ID : unsigned32; Data : TLeafData):pLeaf;
      function DelLeaf(Leaf : pLeaf):boolean;

      function AddLeftLeaf (ID : unsigned32; Data : TLeafData):Boolean;
      function AddRightLeaf(ID : unsigned32; Data : TLeafData):Boolean;
     public

      constructor Create();
      destructor  Free();
end;

implementation

constructor TBTree.Create();
begin
end;

destructor  TBTree.Free();
begin
end;

//Blattfunktionen
//Ein neues Blatt anlegen
function TBTree.NewLeaf(ID : unsigned32; Data : TLeafData):pLeaf;
begin
  new (result);

  //ID
  result^.ID:=ID;

  //Daten sind etwas schwieriger
  SetLength(result^.Data,Length(Data));
  CopyMemory( Addr(result^.Data[0]) ,Addr(Data[0]) ,Length(result^.Data) );
end;

//Neues Blatt anhängen
function TBTree.AddLeaf (Root : pLeaf; Orientation : TOrientation; NewLeaf : pLeaf):Boolean;
var
  pHelper : ^pLeaf;
begin
  //Welche Seite ?
  case Orientation of
    Right : Root.pRight:=NewLeaf;
    Left  : Root.pLeft:=NewLeaf;
  end;
end;

//Blatt löschen (leider nicht trivial)
function TBTree.DelLeaf(Leaf : pLeaf):boolean;
begin
  //Root werfen wir nie raus
  if (Leaf^.pParent <> nil) then
    begin
      
    end;
end;

//Links einhängen
function TBTree.AddLeftLeaf (ID : unsigned32; Data : TLeafData):Boolean;
begin
  Self.AddLeaf(nil,Left,Self.NewLeaf(ID,Data));
end;

//Rechts einhängen
function TBTree.AddRightLeaf(ID : unsigned32; Data : TLeafData):Boolean;
begin
  Self.AddLeaf(nil,Right,Self.NewLeaf(ID,Data));
end;

end.
