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
///////////////////////////////////////////////////////////////////////////////////////////
//
// div. Stringkompressionsfunktionen
// (c) 2003 Borg@Sven-of-Nine.de
//
// RPC = Reverse Pointer Compression
//       Vom Dateizeiger aus nach hinten wird nach Übereinstimmungen gesucht
//       und bei Treffern der Zeiger gespeichert.
// RLE = Run Length Encodinug
//       Identische Zeichen werden durch Faktoren ersetzt
//
///////////////////////////////////////////////////////////////////////////////////////////
unit Unit_StringCompression;

interface

uses Unit_StringFunctions;

function Str2RPC(Input:String):String;
function RPC2Str(Input:String):String;

function Str2RLE(Input:String):String;
function RLE2Str(Input:String):String;



implementation

Const
     Token  = Chr(255);               //Zeichen, um Compressionsequenzen zu identifizieren
     MaxRad = 250;                    //Maximaler Suchradius bei RPC (Maximal=250);
     MaxSeq = 3;                      //Sequenzlänge bei RPC (Optimal=3);


///////////////////////////////////////////////////////////////////////////////////////////
// Reverse Pointer Compression
///////////////////////////////////////////////////////////////////////////////////////////
function Str2RPC(Input:String):String;
var
   trip: String;  //Enthält das Buchstabentripel, welches ersetzt werden soll
   sub : String;  //Enthält den Teilstring, vom Zeiger z um maximal MaxRad Zeichen zum Anfang hin
   temp: String;  //Hilfstring bei Kompression
   z   : Integer; //Allgemeiner Zähler
   p   : Integer; //Zeiger auf Beginn von Sub
   f   : Integer; //Zeiger auf ein evtl. gefundenes Zahlentripel
   Rad    : Integer;  //Aktueller Radius (Stringlänge von Sub)
begin
     //Maximaler Adressierungsradius
     Result:='';
     z:=1;
     while (z <= Length(Input)) do
         begin
              Temp:=Input[z];

              //Token "Escapen"
              If (Temp=Chr(255)) then
                 begin
                      Temp:=Token+Token;
                 end;

              //Unser Buchstabendreier, den wir komprimieren wollen
              Trip:=Copy(Input,z,3);

              //String mit der maximalen Größe Radius vor dem Zeichenpointer rausschneiden
              //Rausschneiden
              p:=z-MaxRad;
              if (p < 1) then p:=1;
              Rad:=z-p;
              Sub:=Copy(Input,p,Rad);
              Rad:=Length(Sub);

              //Kompression nur versuchen, wenn wir ein Buchstabentripel haben
              if (Length(Trip)=MaxSeq) then
                 begin
                      //Tripel suchen
                      f:=FindCaseString(Sub,Trip);

                      //Was gefunden ?
                      if (f>0) then
                         begin
                              //Aus der relativen Position die absulute im String bestimmen
                              f :=Rad-f+1;

                              //Und den Zeiger im Stringstrom speichern
                              Temp:=Token+Chr(f);

                              //Unser Buchstabentripel überspringen
                              inc(z,2);
                         end;
                 end;
              Result:=Result+Temp;
              inc(z);
         end;
end;

function RPC2Str(Input:String):String;
var
   z    : Integer;
   Temp : String;
begin
     z:=1;
     Result:='';
     //Den kompletten String lesen
     while (z <= Length(Input)) do
           begin
                //Zeichen holen
                Temp:=Input[z];

                //Ein Kompressionstoken ?
                if (Input[z]=Token) then
                   begin
                        //Zeiger holen
                        inc(z);
                        Temp:=Input[z];

                        //Kein Escape Token ?
                        if (Temp<>Token) then
                           begin
                                //Buchstabentripel aus dem schon dekomprimierten String
                                //herausholen
                                Temp:=Copy(Result,(Length(Result))-Ord(Input[z])+1,MaxSeq);
                           end;
                   end;
                //Nächstes Zeichen schon mal adressieren
                inc(z);
                //Ausgabestring zusammensetzen
                Result:=Result+Temp;
           end;
end;


///////////////////////////////////////////////////////////////////////////////////////////
// Run Length Encoding
///////////////////////////////////////////////////////////////////////////////////////////
function Str2RLE(Input:String):String;
var
   z,c  :  Integer;
   Temp :  String;
begin
     z:=1;
     Result:='';
     while (z <= Length(Input) ) do
           begin
                Temp:=Input[z];
                c:=1;
                //Mehrfache Zeichen ? Dann durchzählen (Aber nur maximal 254 Wiederholungen
                while (Input[z]=Input[z+c]) and (c < 255) do
                      begin
                           inc(c);
                      end;

                //Mehr als 2 Wiederholungen gefunden ?
                if (c>2) then
                   begin
                        //Dann unseren Faktor ansetzen und kompletten String schreiben
                        Temp:=Token+Chr(c)+Temp;

                        //Widerholungen überspringen
                        dec(c);
                        inc(z,c);
                   end
                else
                   begin
                        //Ist das aktuelle Zeichen ein Token, dann "Escapen"
                        if Temp=Token then
                           begin
                                Temp:=Token+Chr(1)+Temp;
                           end;
                   end;
                Result:=Result+Temp;
                inc(z);
           end;
end;

function RLE2Str(Input:String):String;
var
   z,c  : Integer;
   Temp : String;
begin
     Result:='';
     z:=1;
     while (z <= Length(Input)) do
           begin
                Temp:=Input[z];

                //Kompressionssequenz gefunden ?
                if (Temp=Token) then
                   begin
                        //Counter holen
                        inc(z);
                        c:=Ord(Input[z]);

                        //Zeichen holen
                        inc(z);
                        Temp:=Input[z];

                        //Und Wiederholungen ausgeben
                        Temp:=FillString(Temp,c-1);
                   end;
                inc(z);
                Result:=Result+Temp;
           end;
end;

end.
