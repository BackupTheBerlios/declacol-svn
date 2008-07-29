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
     ID      : unsigned32; //Automatisch vergeben ID
     Data    : TLeafData;  //Eigentliche Daten
     pParent : pLeaf;      //Zeiger auf die Verbindungen
     pLeft   : pLeaf;
     pRight  : pLeaf;
end;

type pBTree = ^pBTree;
     TBTree = class (TObject)
     private
      //Datenwurzel
      Root : pLeaf;

      //Link von den IDs auf die Daten
      Link : array of pLeaf;

      //ID-Counter
      u32ID : unsigned32;

      //ReadOnly ?
      bReadOnly : Boolean;

      function  _find(var Root : pLeaf; Data : TLeafData; Add : Boolean):pLeaf;
      function  _new (Data : TLeafData):pLeaf;
      procedure _clear(var Root : pLeaf);
      function  str2data(Input : Longstring):TLeafData;

     protected

     public
      constructor Create();
      destructor  Free();

      function find(Data : TLeafData) :pLeaf; overload;
      function find(Data : LongString):pLeaf; overload;
      function find(ID   : unsigned32):pLeaf; overload;

      function add(Data : TLeafData; var NewLeaf : pLeaf) : Boolean; overload;
      function add(Data : LongString;var NewLeaf : pLeaf) : Boolean; overload;

      function del(Data : TLeafData) :boolean; overload;
      function del(Data : LongString):boolean; overload;

      function test(size : unsigned32):boolean;

      procedure clear();

      property size : unsigned32 read u32ID;
      property readonly : boolean read bReadOnly write bReadOnly;

end;

implementation

////////////////////////////////////////////////////////////////////////////////
//Konstruktor
constructor TBTree.Create();
begin
     Self.Root:=nil;
     Self.u32ID:=0;
     Self.bReadOnly:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////
//Destruktor
destructor  TBTree.Free();
begin
     self.Clear();
end;

////////////////////////////////////////////////////////////////////////////////
//Interner Test
function TBTree.test(size : unsigned32):boolean;
var
   u32Index : unsigned32;
   pTemp    : pLeaf;
begin
     result:=TRUE;

     //Den Baum füllen
     Self.Clear();
     for u32Index:=0 to size do
         begin
              Self.Add( chr( (u32Index shr 0)  and $ff) +
                        chr( (u32Index shr 8)  and $ff) +
                        chr( (u32Index shr 16) and $ff) +
                        chr( (u32Index shr 24) and $ff),pTemp );
              result:=result AND (pTemp <> nil);
         end;

     //Und alle wieder suchen
     for u32Index:=0 to size do
         begin
              pTemp:=Self.find( chr( (u32Index shr 0)  and $ff) +
                                chr( (u32Index shr 8)  and $ff) +
                                chr( (u32Index shr 16) and $ff) +
                                chr( (u32Index shr 24) and $ff) );
              result:=result AND (pTemp <> nil);
         end;
     Self.Clear();
end;

////////////////////////////////////////////////////////////////////////////////
//Eintrag zufügen und Zeiger auf Blatt zurückgeben
function TBTree.add(Data : TLeafData; var NewLeaf : pLeaf) : Boolean;
var
   u32SizeOld : unsigned32;
   u32SizeNew : unsigned32;
   u32Index   : unsigned32;
begin
     if (Length(Data) > 0) AND (not Self.bReadOnly) then
        begin
             newleaf:=Self._find(Self.Root,Data,TRUE);

             //Das Linkarray evtl. erweitern
             u32SizeOld:=Length(Self.Link);
             if (newleaf^.ID >= u32SizeOld) then
                begin
                     u32SizeNew:=newleaf^.ID + 1024;

                     SetLength(Self.Link,u32SizeNew);
                     //Und alle Zeiger sicher löschen
                     for u32Index := u32SizeOld to u32SizeNew -1 do
                         begin
                              Self.Link[u32Index]:=nil;
                         end;
                end;
             //Ist das Blatt neu ?
             result:= Self.Link[newleaf^.ID] = nil;

             //Und einfach den Zeiger ablegen
             Self.Link[newleaf^.ID]:=newleaf;
        end
     else
        begin
             result:=FALSE;
        end; 
end;


////////////////////////////////////////////////////////////////////////////////
//Eintrag suchen und Zeiger auf Blatt zurückgeben (oder nil)
function TBTree.find(Data : TLeafData):pLeaf;
begin
     result:=Self._Find(Self.Root,Data,FALSE);
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Blatt löschen
function TBTree.del(Data : TLeafData):boolean;
var
   pTemp : pLeaf;
begin
     //Gibt es den Eintrag überhaupt ?
     pTemp:=Self.find(Data);

     if (pTemp <> nil) then
        begin
             result:=FALSE;
             //Es gibt drei Fälle
             //1. Blatt hat keine Unterknoten, dann einfach löschen
             //2. Blatt hat nur einen Unterknoten, dann dieses mit Parent verbinden
             //3. Blatt hat zwei Unterknoten, dann müssen wir den Baum umlagern
             if (pTemp^.pLeft = nil) then
                begin
                     if (pTemp^.pRight = nil) then
                        begin
                             //1. Fall
                             if (pTemp^.pParent^.pLeft = pTemp) then
                                begin
                                     pTemp^.pParent^.pLeft:=nil;
                                end
                             else
                                begin
                                     pTemp^.pParent^.pRight:=nil;
                                end;
                        end
                     else
                        begin
                             //2. Fall rechts
                             if (pTemp^.pParent^.pLeft = pTemp) then
                                begin
                                     pTemp^.pParent^.pLeft:=pTemp^.pRight;
                                end
                             else
                                begin
                                     pTemp^.pParent^.pRight:=pTemp^.pRight;
                                end;
                        end; 
                end
             else
                begin
                     if (pTemp^.pRight = nil) then
                        begin
                             //2. Fall links
                             if (pTemp^.pParent^.pLeft = pTemp) then
                                begin
                                     pTemp^.pParent^.pLeft:=pTemp^.pLeft;
                                end
                             else
                                begin
                                     pTemp^.pParent^.pRight:=pTemp^.pLeft;
                                end;
                        end
                     else
                        begin
                             //3. Fall
                             //ToDo
                        end;
                end;
             //Link freigeben
             Self.Link[pTemp^.ID]:=nil;
             //Und das Blatt entleden
             Dispose(pTemp);
        end
     else
        begin
             result:=FALSE;
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
// Und das Ganze nochmal überladen für Strings
////////////////////////////////////////////////////////////////////////////////
function TBTree.add(Data : Longstring; var NewLeaf : pLeaf) : Boolean;
begin
     result:=self.add(self.str2data(Data),NewLeaf);
end;

////////////////////////////////////////////////////////////////////////////////
//Das ganze auch für Strings
function TBTree.Find(Data : LongString):pLeaf;
begin
     //Und die Standardprozedur nehmen
     result:=Self.Find(Self.str2data(Data));
end;

////////////////////////////////////////////////////////////////////////////////
//Daten anhand der ID finden
//Die ID-Suche ist ziemlich einfach. Wenn die ID kleiner als
//die aktuelle Größe des Linkarrays ist, greifen wir direkt darauf zu.
//Ansonsten existiert der Eintrag nicht
function TBTree.find(ID   : unsigned32):pLeaf;
begin
     if (ID < unsigned32(Length(Self.Link))) then
        begin
             result:=Self.Link[ID];
        end
     else
        begin
             result:=nil;
        end; 
end;


////////////////////////////////////////////////////////////////////////////////
//Das ganze auch für Strings
function TBTree.del(Data : LongString):boolean;
begin
     result:=Self.Del(Self.str2data(Data));
end;

////////////////////////////////////////////////////////////////////////////////
//Rekursiv den ganzen Baum löschen
procedure TBTree.clear();
begin
     if (not Self.bReadOnly) then
        begin
             Self._Clear(Self.Root);
             Self.u32ID:=0;

             //Linkarray löschen
             SetLength(Self.Link,0);
        end;
end;

procedure TBTree._clear(var Root : pLeaf);
begin
     if (Root <> nil) then
        begin
             Self._clear(Root^.pLeft);
             Self._clear(Root^.pRight);

             Root^.pLeft:=nil;
             Root^.pRight:=nil;

             Dispose(Root);
             Root:=nil;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String in ein Array wandeln
function TBTree.str2data(Input : Longstring):TLeafData;
var
   u32Size : unsigned32;
begin
     //Ales auf Array umbauen
     u32Size:=Length(Input);
     SetLength(Result,u32Size);
     CopyMemory(Addr(Result[0]),Addr(Input[1]),u32Size);
end;

////////////////////////////////////////////////////////////////////////////////
//Rekursiv den Baum ab "Root" durchsuchen
//Hier nutzen wir, daß Delphi Bool nicht voll auswertet
//Ist die Option Complete Bool Eval an wird die Suche leider viel langsamer
function TBTree._Find(var Root : pLeaf; Data : TLeafData; Add : Boolean):pLeaf;
begin
     if (Root <> nil) then
        begin
             //Schon da ?
             if ( string(root^.Data) = string(Data) ) then
                begin
                     result:=root;
                end
             else
                begin
                     //Nein, dann rechts oder links abbiegen
                     if (string(root^.Data) < string(Data)) then
                        begin
                             result:=Self._Find(Root^.pLeft,Data,Add);
                        end
                     else
                        begin
                             result:=Self._Find(Root^.pRight,Data,Add);
                        end; 
                end; 
        end
     else
        begin
             //Nicht gefunden !
             //Einfügen ?
             if (Add = TRUE) then
                begin
                     Root:=Self._New(Data);
                     result:=Root;                     
                end
             else
                begin
                     result:=nil;
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein neues Blatt erzeugen
function TBTree._New(Data : TLeafData):pLeaf;
begin
  new (result);

  //ID wird automatisch vergeben
  result^.ID:=Self.u32ID;
  inc(Self.u32ID);

  //Zeiger sicher initialisieren
  result^.pParent:=nil;
  result^.pLeft  :=nil;
  result^.pRight :=nil;

  //Daten sind etwas schwieriger
  SetLength(result^.Data,Length(Data));
  CopyMemory( Addr(result^.Data[0]) ,Addr(Data[0]) ,Length(result^.Data) );
end;

end.
