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
/// Klasse um Streams zu komprimieren.
/// In dieser Unit wird das RLE-Verfahren umgesetzt. Ist zwar schön schnell
/// hat aber leider keinen hohen Kompressionsfaktor
///
////////////////////////////////////////////////////////////////////////////////

unit class_tinyrle;

interface
uses unit_typedefs,classes,sysutils;

type TTinyRLE = class(TObject)
     private
     protected
     public
           function test(Size : unsigned32):boolean;
           procedure compress  (input,output : TStream);
           procedure uncompress(input,output : TStream);
end;


implementation

////////////////////////////////////////////////////////////////////////////////
//Interner Test der Kompression
////////////////////////////////////////////////////////////////////////////////
function TTinyRLE.test(size : unsigned32):boolean;
var
   Temp1    : TMemoryStream;
   Temp2    : TMemoryStream;
   Temp3    : TMemoryStream;
   u32Count : unsigned32;
   u8Data   : unsigned8;
   u8Data1  : unsigned8;
begin
     result:=TRUE;

     Temp1:=TMemoryStream.Create();
     Temp2:=TMemoryStream.Create();
     Temp3:=TMemoryStream.Create();

     while (Size > 0) do
           begin
                //Zufallsstring bauen
                Temp1.Size:=0;
                Temp1.Position:=0;
                
                randomize();
                u32Count:=Size;
                while (u32Count > 0) do
                      begin
                           u8Data:=random(255);
                           u8Data:=65;
                           Temp1.Write(u8Data,SizeOf(u8Data));
                           dec (u32Count);
                      end;

                Temp1.Position:=0;
                Temp2.Position:=0;
                Temp2.Size:=0;
                Temp3.Position:=0;
                Temp3.Size:=0;

                Self.Compress(Temp1,Temp2);
                Self.UnCompress(Temp2,Temp3);

                //Und nun Stream 1 und Stream 2 vergleichen
                Temp1.Position:=0;
                Temp3.Position:=0;

                while (Temp1.position < Temp1.Size) do
                      begin
                           Temp1.Read(u8Data,SizeOf(u8Data));
                           Temp3.Read(u8Data1,SizeOf(u8Data1));

                           result:=result AND ( u8Data = u8Data1);
                      end;

                size:=size shr 1;
           end;

     Temp1.Free();
     Temp2.Free();
     Temp3.Free();
end;

////////////////////////////////////////////////////////////////////////////////
/// Einfache RLE-Kompression durchführen
////////////////////////////////////////////////////////////////////////////////
//Dabei wird auf ein Escape zeichen verzichtet sondern
//Das zu komprimierende Zeichen selbst als Escape benutzt.
//
//
//Kommt ein Zeichen mehrfach vor so wir es zweimal ausgegeben, gefolgt vom
//Multiplikator wir oft das Zeichen wiederholt wird. Kommt ein Zeichen nur
//einfach vor wird es durchgereicht
//aus
//
// aus aaabbcdegghhhhij wird aa#02bb#01cdefgg#01hh#03ij
//
////////////////////////////////////////////////////////////////////////////////

procedure TTinyRLE.compress(input,output : TStream);
var
   u8Counter  : unsigned8;
   u8Current  : unsigned8;
   u8Last     : unsigned8;
begin
     u8Counter:=0;
     Input.Position:=0;

     if (input.size > 1) then
        begin
             //Beim zweiten Zeichen anfangen
             input.Read(u8Last,SizeOf(u8Last));

             while (Input.Position < Input.Size) do
                   begin
                   input.read(u8Current,SizeOf(u8Current));

                   //Zeichenwiederholung ?
                   //Oder der Counter läuft über ?
                   if (u8Current = u8Last) AND (u8Counter < 255) then
                      begin
                           inc(u8Counter);
                      end
                   else
                      begin
                           //Zeichen schreiben
                           Output.Write(u8Last,SizeOf(u8Last));

                           //Sequenz vorrüber ?
                           if (u8Counter > 0) then
                              begin
                                   //Ja => Multiplikator anhängen
                                   Output.Write(u8Last,SizeOf(u8Last));
                                   Output.Write(u8Counter,SizeOf(u8Counter));
                                   u8Counter:=0;
                              end;
                      end;

                   //Für die nächste Runde merken
                   u8Last:=u8Current;
             end;

             //Der Rest
             Output.Write(u8Last,SizeOf(u8Last));
             if (u8Counter > 0) then
                begin
                     Output.Write(u8Last,SizeOf(u8Last));
                     Output.Write(u8Counter,SizeOf(u8Current));
                end;
        end
     else
        begin
             //String lohnt nicht zu komprimieren
             Output.CopyFrom(Input,Input.Size);
        end; 

end;

////////////////////////////////////////////////////////////////////////////////
/// Einfache RLE-Dekompression durchführen
////////////////////////////////////////////////////////////////////////////////
//Sind zwei Identische Zeichen hintereinander im Datenstrom wird das erste Zeichen
//direkt ausgegeben, daß zweite Zeichen aber so oft wiederholt wie hinter dem
//zweiten Zeichen angegeben ist. Kommt ein Zeichen nur einmal vor, wird es
//einfach durchgereicht.
procedure TTinyRLE.uncompress(input,output : TStream);
var
   u8Counter  : unsigned8;
   u8Last     : unsigned8;
   u8Current  : unsigned8;

begin
     //Beim zweiten Zeichen anfangen
     Input.Position:=0;
     Input.Read(u8Current,SizeOf(u8Current));

     while (input.position < input.size) do
        begin
             //Für die nächste Runde merken
             u8Last:=u8Current;

             //Erstes Zeichen können wir immer ausgeben
             Output.Write(u8Last,SizeOf(u8Last));

             //Kommen zwei identische Zeichen muß das nächste Zeichen ein
             //Multiplikator sein
             Input.Read(u8Current,SizeOf(u8Current));
             if (u8Last = u8Current) then
                begin
                     //Multiplikator lesen und String auffüllen
                     Input.Read(u8Counter,SizeOf(u8Counter));
                     while (u8Counter > 0) do
                           begin
                                Output.Write(u8Last,SizeOf(u8Last));
                                dec(u8Counter);
                           end;

                     //Wieder mit dem Eingangsstrom synchronisieren
                     Input.Read(u8Current,SizeOf(u8Current));
                end;
        end;

     //Letztes Zeichen anhängen
     if (u8Last <> u8Current) then
        begin
             Output.Write(u8Current,SizeOf(u8Current));
        end;
end;


end.
