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
unit class_objectbuffer;
////////////////////////////////////////////////////////////////////////////////
///
/// Pointer-Buffer
/// (c) 2004-2006 Borg@Sven-of-Nine.de
///
////////////////////////////////////////////////////////////////////////////////
///
///Der Puffer arbeitet mit verketteten Knoten, wodurch Zugriffe wie "Zufügen" und
///"Entfernen sehr schnell sind. Gespeichert werden können NUR untypisierte
///Zeiger, die aber per Typecasting beliebige Form haben können.
///Da die Bufferverwaltung untypisierte Zeiger nicht entladen kann, muß das
///aufrufende Programm eine CallBackfunktion dafür bereitstellen.
///Gleiches gilt für die Sortierfunktionen.
///
///Achtung :
///
///Unter Lazarus wird aufgrund eines Compilerproblems (FPC0.9.1) bei Aufruf von
///Buffer.Clear nicht der kpl. Speicher freigegeben.
///Dieses Verhalten liegt NICHT an der Software.
///
////////////////////////////////////////////////////////////////////////////////
///Hier ein kleines Beispiel
///
///
///Die Funktion, um den Speicher freizugeben
///function FreeString(MyString:PString):Boolean;
///begin
///     if (MyString<>nil) then
///        begin
///             Dispose(MyString);
///        end;
///     result:=TRUE;
///end;
///
///
///Die Vergleichsfunktion wird zur Sortierung benötigt
///und muß TRUE zurückgeben, wenn Object1 > Object2 ist
///function CompareString(MyString1,MyString2:PString):Boolean;
///begin
///     result:=MyString1 > MyString2;
///end;
///
///Hauptprogramm
///Object erzeugen
///Buffer:=TObjectBuffer.Create;
///Unsere Funktion einbinden
///Buffer.DisposeFunction := TFreeFunction(@FreeString);
///Buffer.SortFunction    := TSortFunction(@CompareString);
///
///Strings zufügen
///     for a:=1 to 1000000 do
///         begin
///              New (kleinerstring);
///              kleinerstring^:='abc'+IntToStr(a);
///              Buffer.Add(kleinerstring);
///         end;
///
///Strings ausgeben
///     for a:=0 to Buffer.Size-1 do
///         begin
///              writeln(PString(Buffer.Get(a)^);
///         end;
///
///Einträge löschen ACHTUNG : Der Index beginnt bei 0 und geht bis Buffer.Size-1
///     Buffer.Delelet(10)
///
///Alles löschen
///     Buffer.Clear;
///
////////////////////////////////////////////////////////////////////////////////
///ToDo :
///
///Sortierung      optimieren
///GetNodeFunktion optimieren
///
////////////////////////////////////////////////////////////////////////////////

interface

uses
    unit_typedefs
    ;
////////////////////////////////////////////////////////////////////////////////
//Free und Comp Funktionen
//Diese Funktion sollte eigentlich den Speicher hinter Pointer freigeben
/// function DummyFree(pObjectToFree      : Pointer):Boolean;
/// begin
///     result:=TRUE;
///end;
///
//Diese Funktion vergleicht und gibt True zurück, wenn Object1 > Object2 ist
///function DummyComp(pObject1, pObject2 : Pointer):Boolean;
///begin
///     result:=TRUE;
///end;
////////////////////////////////////////////////////////////////////////////////
/// Prototypen unserer "externen" Funktionen
type TDisposeFunction = Function (pObjectToFree     : Pointer) : Boolean of Object;
type TCompareFunction = Function (pObject1,pObject2 : Pointer) : Boolean of Object;
type TFindFunction    = Function (pObject           : Pointer;pCompare : Pointer) : Boolean of Object;
//type PDisposeFunction = ^TDisposeFunction;
//type PCompareFunction = ^TCompareFunction;
//type PFindFunction    = ^TFindFunction;


////////////////////////////////////////////////////////////////////////////////
/// Nodes
/// Alle Daten werden in den Nodes gespeichert. Zur schnelleren Verarbeitung
/// werden Sie einfach verkettet. Dadurch entfällt lästiges umkopieren
type PNode = ^TNode;
     TNode = record
     //Eigentlicher Inhalt des Nodes
     Value : Pointer;
     
     //Zeiger auf den vorherigen und nachfolgenden Knoten
     pNext : PNode;
     pPrev : PNode;
end;


////////////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
type TObjectBuffer = class(TObject)
     protected
              //Basisknoten
              pBaseNode    : PNode;

              //Endknoten
              pLastNode    : PNode;

              //Maximal Puffergröße
              u32BufferMax : unsigned32;
              
              //Aktuelle Puffergröße
              u32BufferSize: unsigned32;
              
              //Funktionszeiger
              pDispFunc    : TDisposeFunction;
              pCompFunc    : TCompareFunction;
              pFindFunc    : TFindFunction;

              //Diverses
              bKeepSorted  : Boolean;
     private
            //Property Funktionen
//            function GetObjectBufferSize():unsigned32;
            //Hilfsfunktionen
            function  GetNode    (u32Index     : unsigned32) : pNode;
            procedure InitNode   (var NewNode  : pNode);
            function  DisposeNode(Node         : pNode )     : Boolean;
            procedure SwapNodes  (pNode1,pNode2:pNode);
            procedure SortNodes  (s64Left,s64Right:signed64);
     public
           //Konstruktor / Destruktor
           constructor Create();
           destructor  Free();
           
           //Properties
           property Size            : unsigned32    read u32BufferSize;
           property Maximum         : unsigned32    read u32BufferMax write u32BufferMax;
           property Sorted          : boolean       read bKeepSorted  write bKeepSorted;

           //Zugriffsfunktionen
           function  Add      (pObjectToAdd : Pointer   ) : Boolean;
           function  Insert   (pObjectToAdd : Pointer;u32Index : unsigned32) : Boolean;
           function  Get      (u32Index     : unsigned32) : Pointer;
           function  Delete   (u32Index     : unsigned32) : Boolean;
           function  Find     (pPartToFind  : Pointer   ) : Pointer;
           function  FindIndex(pPartToFind  : Pointer   ) : Signed64;

           procedure Swap     (u32Index1,u32Index2:unsigned32);
           function  Clear    ():Boolean;

           //Externe Zugriffe
           property DisposeFunction : TDisposeFunction read pDispFunc write pDispFunc;
           property CompareFunction : TCompareFunction read pCompFunc write pCompFunc;
           property FindFunction    : TFindFunction read pFindFunc write pFindFunc;

           //Diverses
           procedure Shuffle      ();
           procedure Sort         ();
           function  Full         ():Boolean;
end;



implementation

////////////////////////////////////////////////////////////////////////////////
//Konstruktor / Destruktor
constructor TObjectBuffer.Create();
begin
     //Basisknoten initialisieren
     pBaseNode:=nil;
     Self.InitNode(pBaseNode);
     //Und gleichzeitig als Endknoten setzen
     pLastNode:=pBaseNode;

     //Bufferwerte initialisieren
     Self.Maximum        :=High(unsigned32);

     //Hier direkt auf die Variable zugreifen, da die Property Size readonly ist
     Self.u32BufferSize  :=0;

     //Funktionen initialisieren
     Self.DisposeFunction:=nil;
end;


destructor  TObjectBuffer.Free();
begin
     //Puffer freimachen
     Self.Clear();
end;


////////////////////////////////////////////////////////////////////////////////
// Properties
{
function TObjectBuffer.GetObjectBufferSize():unsigned32;
begin
     result:=Self.u32BufferSize;
end;
}

//Puffer voll ?
function  TObjectBuffer.Full         ():Boolean;
begin
     result:=Self.Size >= Self.Maximum; 
end;

////////////////////////////////////////////////////////////////////////////////
/// Hilfsfunktionen
////////////////////////////////////////////////////////////////////////////////
//#***********************************************
//Hier kann man mit einem Index bestimmt noch Geschwindigkeit rausholen
//#***********************************************
//Ein Node mit dem Index X suchen
function TObjectBuffer.GetNode (u32Index     : unsigned32) : pNode;
begin
     //Fehler annehmen
     result:=nil;

     //Fehler abfangen
     if (u32Index >= Self.Size) then
        begin
             Exit;
        end;

     //Einfach unseren Eintrag suchen
     result:=pBaseNode^.pNext;
     while (u32Index>0) do
           begin
                //Den nächsten Knoten holen
                result:=result^.pNext;
                //Und Zähler erhöhen
                dec(u32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Externe Funktion zum Dispose aufrufen
function TObjectBuffer.DisposeNode(Node : PNode) : Boolean;
begin
     result:=FALSE;

     //Node nicht initialisiert ?
     if (Node=nil) then Exit;

     //Wenn es eine Externe Funktion gibt, dann den Typisierten Pointer dorthin
     //schicken, da nur diese Funktion den Type des Pointers kennt
     if ( addr(pDispFunc) <> nil ) then
        begin
             result:=pDispFunc(Node^.Value);
        end;
        
     //Danach können wir intern den Knoten freigeben
     Dispose(Node);
end;

////////////////////////////////////////////////////////////////////////////////
//Zwei Knoten vertauschen
procedure TObjectBuffer.SwapNodes  (pNode1,pNode2:pNode);
var
   pTemp  : Pointer;
begin
     //Fehler abfangen
     if (pNode1=nil) or (pNode2=nil) then
        begin
             Exit;
        end;

     //Einfach den Inhalt vertauschen
     pTemp:=pNode1^.Value;
     pNode1^.Value:=pNode2^.Value;
     pNode2^.Value:=pTemp;
end;

////////////////////////////////////////////////////////////////////////////////
// Die Knoten durchsortieren
procedure TObjectBuffer.SortNodes  (s64Left,s64Right:signed64);
var
   MidValue   : Pointer;
   pLeftNode  : pNode;
   pRightNode : pNode;
   s64NewLeft : signed64;
   s64NewRight: signed64;
begin
     //Mittleres Element holen
     MidValue :=Self.GetNode( ( s64Left+s64Right) div 2)^.Value;
     
     //Arbeitspositionen initialisieren
     s64NewLeft  := s64Left;
     s64NewRight := s64Right;
     
     //Und los
     while (s64NewLeft <= s64NewRight) do
           begin
                //Linkes Element holen
                pLeftNode:=Self.GetNode( unsigned32( s64NewLeft ));

                //Und solange durcharbeiten bis Left >= Mid
                if ( pCompFunc (MidValue, pLeftNode^.Value)) then
                   begin
                        repeat
                              //Nächsten Knoten holen
                              pLeftNode:=pLeftNode^.pNext;
                              //Zähler erhöhen
                              inc(s64NewLeft);
                        //Bis wir nicht mehr gleich sind
                        until ( not pCompFunc( MidValue , pLeftNode^.Value ) );
                   end;

                //Rechtes Element holen
                pRightNode:=Self.GetNode( unsigned32( s64NewRight ));

                //Und solange durcharbeiten bis Right <= Mid
                if ( pCompFunc (pRightNode^.Value,MidValue)) then
                   begin
                        repeat
                              //Nächsten Knoten holen
                              pRightNode:=pRightNode^.pPrev;

                              //Zähler senken
                              dec(s64NewRight);

                        //Bis mid >= right
                        until ( not pCompFunc( pRightNode^.Value, MidValue ) );
                   end;

                //Werte tauschen
                if (s64NewLeft <= s64NewRight) then
                   begin
                        Self.SwapNodes( Self.GetNode( unsigned32( s64NewRight )),Self.GetNode( unsigned32( s64NewLeft ) ));

                        inc(s64NewLeft);
                        dec(s64NewRight);
                   end;
           end;

     if (s64Left < s64NewRight) then
        begin
             Self.SortNodes(s64Left,s64NewRight);
        end;

     if (s64NewLeft < s64Right) then
        begin
             Self.SortNodes(s64NewLeft,s64Right);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
// Einen Knoten initialisieren
procedure TObjectBuffer.InitNode  (var NewNode : pNode);
begin
     //Ggf. erzeugen
     if (NewNode=nil) then
        begin
             new (NewNode);
        end;

     //Standardwerte setzen
     NewNode^.pPrev :=nil;
     NewNode^.pNext :=nil;
     NewNode^.Value :=nil;
end;

////////////////////////////////////////////////////////////////////////////////
//Zugriffsfunktionen
//Ein neues Object zufügen
function TObjectBuffer.Add (pObjectToAdd : Pointer   ) : Boolean;
var
   pNewNode  : pNode;
begin
     //Fehler annehmen
     result:=FALSE;

     //Maximale Größe des Buffers überschritten ?
     if ( Self.Full() ) then
        begin
             //Dann abbrechen
             exit;
        end;

     //Den neuen Knoten erzeugen
     pNewNode:=nil;

     //Mit Standardwerten füllen
     InitNode(pNewNode);
     with (pNewNode^) do
          begin
               //Zu speichernder Zeiger
               Value :=pObjectToAdd;
               //Den vorherige Knoten anvisieren
               pPrev :=pLastNode;
          end;
          
     //Und Werte des letzten Knotens anpassen
     with (pLastNode^) do
          begin
               //Zeiger auf Nachfolger
               pNext :=pNewNode;
          end;

     //Größe anpassen
     inc(Self.u32BufferSize);
     
     //Neuen letzten Knoten setzen
     pLastNode:=pNewNode;

     //Sortieren ?
     if (bKeepSorted) then
        begin
             Self.Sort;
        end;

     //Fertig
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Object an Position X einfügen
function TObjectBuffer.Insert(pObjectToAdd : Pointer;u32Index : unsigned32) : Boolean;
var
   pWorkNode : pNode;
   pNewNode  : pNode;
begin
     //Fehler annehmen
     result:=FALSE;

     //Maximale Größe des Buffers überschritten ?
     if ( Self.Full() ) then
        begin
             //Dann abbrechen
             exit;
        end;

     //Node mit dem gewünschten Index holen
     pWorkNode:=Self.GetNode(u32Index);
     
     //Was gefunden ?
     if (pWorkNode<>nil) then
        begin
             //Neuen Knoten initialisieren
             pNewNode:=nil;
             Self.InitNode(pNewNode);

             //Werte des neuen Nodes anpassen
             //Der neue Knoten kann nicht der letzte sein,
             //da Insert den neuen wert immer zwischen
             //Index-1 und Index setzt. Dadurch rutscht der alte Eintrag
             //bei Index eins weiter und ist damit der letzte Eintrag
             pNewNode^.Value :=pObjectToAdd;

             //Einfach zwischen Worknode und PreviousNode klemmen
             pNewNode^.pPrev :=pWorkNode^.pPrev;
             pNewNode^.pNext :=pWorkNode;

             //Linken Nachbarn anpassen
             pNewNode^.pPrev^.pNext:=pNewNode;

             //Rechten Nachbarn anpassen
             pNewNode^.pNext^.pPrev:=pNewNode;

             //Puffergröße anpassen
             inc(Self.u32BufferSize);
             
             //Fertig
             result:=TRUE;
        end;
        
     //Sortieren ?
     if (bKeepSorted) then
        begin
             Self.Sort;
        end;

end;


////////////////////////////////////////////////////////////////////////////////
function TObjectBuffer.Get (u32Index     : unsigned32) : Pointer;
begin
     //Einfach GetNode benutzen
     result:=Self.GetNode(u32Index)^.Value;
end;


////////////////////////////////////////////////////////////////////////////////
function  TObjectBuffer.Find   (pPartToFind  : Pointer   ) : Pointer;
var
   pWorkNode : pNode;
begin
     //Fehler annehmen
     result:=nil;
     
{
     //Funktioneprobleme abfangen
     if (pFindFunc=nil) then
        begin
             exit;
        end;
}     
     //Einfach den ganzen Buffer durchgehen und einzeln prüfen
     pWorkNode:=pBaseNode;
     
     repeat
           //Den nächsten Knoten holen
           pWorkNode:=pWorkNode^.pNext;
           //Und diesen vergleichen, wenn er NICHT nil ist
           if (pWorkNode <> nil) then
              begin
                   //Vergleich aufrufen
                   if (pFindFunc (pWorkNode^.Value,pPartToFind) ) then
                      begin
                           //Node merken
                           result:=pWorkNode^.Value;

                           //Und Schleife beenden
                           pWorkNode:=nil;
                      end;
                   
              end;
     until(pWorkNode=nil);
     
     //Fertig
end;

function  TObjectBuffer.FindIndex(pPartToFind  : Pointer   ) : Signed64;
var
   pWorkNode : pNode;
   u32Index  : unsigned32;
begin
     //Fehler annehmen
     result:=-1;
     
{
     //Funktioneprobleme abfangen
     if (pFindFunc=nil) then
        begin
             exit;
        end;
}     
     //Einfach den ganzen Buffer durchgehen und einzeln prüfen
     pWorkNode:=pBaseNode;
     u32Index:=0;
     repeat
           //Den nächsten Knoten holen
           pWorkNode:=pWorkNode^.pNext;
           //Und diesen vergleichen, wenn er NICHT nil ist
           if (pWorkNode <> nil) then
              begin
                   //Vergleich aufrufen
                   if (pFindFunc (pWorkNode^.Value,pPartToFind) ) then
                      begin
                           //Position merken
                           result:=u32Index;
                           //Und Schleife beenden
                           pWorkNode:=nil;
                      end;

              end;
           //Zähler erhöhen
           inc (u32Index);
     until(pWorkNode=nil);
     
     //Fertig
end;

////////////////////////////////////////////////////////////////////////////////
function TObjectBuffer.Delete (u32Index     : unsigned32) : Boolean;
var
   pWorkNode : pNode;
begin
     //Fehler annhemen
     result:=FALSE;
     
     //Index OK ?
     if (u32Index >= Self.Size) then
        begin
             //Nein => Abbruch
             exit;
        end;
        
     //Einfach GetNode() benutzen
     pWorkNode:=Self.GetNode(u32Index);

     //Ist der zu löschende Knoten der letze in der Liste ?
     if (pWorkNode=pLastNode) then
        begin
             //Dann die schnelle Methode:
             //Linke Nachbar ist nun der letzte Knoten
             pWorkNode^.pPrev^.pNext :=nil;

             //Und den Zeiger auf den letzten Knoten neu setzen
             pLastNode:=pWorkNode^.pPrev;
        end
     else
        begin
             //Die Daten der betroffenen Knoten anpassen
             //Linken Nachbarn direkt auf den rechten Nachbarn
             //zeigen lassen
             pWorkNode^.pPrev^.pNext:=pWorkNode^.pNext;

             //Und rechten Nachbarn direkt auf den linke...
             pWorkNode^.pNext^.pPrev:=pWorkNode^.pPrev;
        end;

     //Den Knoten und alles was dazugehört löschen
     Self.DisposeNode(pWorkNode);

     //Größe anpassen
     dec (Self.u32BufferSize);

     //Sortieren ?
     if (bKeepSorted) then
        begin
             Self.Sort;
        end;

     //Fertig
     result:=TRUE;
end;


////////////////////////////////////////////////////////////////////////////////
//Zwei Einträge vertauschen
procedure TObjectBuffer.Swap (u32Index1,u32Index2:unsigned32);
begin
     //Einfach die interne Funktion aufrufen
     Self.SwapNodes(Self.GetNode(u32Index1),Self.GetNode(u32Index2));
end;

////////////////////////////////////////////////////////////////////////////////
//Alle Einträge aus dem Buffer löschen
function TObjectBuffer.Clear     ():Boolean;
var
   pNextNode : pNode;
   pWorkNode : pNode;
begin
     //Erfolg annehmen
     result:=TRUE;

     if (Self.Size=0) then
        begin
             exit;
        end;

     //Den ersten Knoten anvisieren
     pWorkNode:=pBaseNode^.pNext;
     
     //Und solange löschen, bis nichts mehr kommt
     while (pWorkNode<>nil) do
           begin
                //Nächsten Knoten holen
                pNextNode:=pWorkNode^.pNext;
                
                //Den aktuellen Knoten löschen
                DisposeNode(pWorkNode);
                
                //Und nächsten holen
                pWorkNode:=pNextNode;
           end;

     //Und die Statusdaten anpassen
     Self.u32BufferSize:=0;

     //Basisknoten resetten
     Self.InitNode(pBaseNode);

     //Den letzten Knoten wieder anpassen
     pLastNode:=pBaseNode;
end;


////////////////////////////////////////////////////////////////////////////////
//Den Buffer mischen
procedure TObjectBuffer.Shuffle();
var
   u32Count : unsigned32;
begin
     u32Count:=0;
     //Alle Einträge durchgehen
     while (u32Count < Self.Size) do
         begin
              //Einfach zufällig zwei Einträge gegeneinander tauschen
              Self.Swap (u32Count,Random(Self.Size));
              //Nächster Eintrag
              inc(u32Count);
         end;

end;


////////////////////////////////////////////////////////////////////////////////
//Den Buffer sortieren
procedure TObjectBuffer.Sort();
begin
{
     //Sortfunktion definiert ?
     if (pCompFunc=nil) then Exit;
}     
     
     //Ansonsten interne Sortierung aufrufen
     Self.SortNodes(0,Self.Size-1);
end;

end.
