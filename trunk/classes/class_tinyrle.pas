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
unit class_tinyrle;

interface
uses unit_typedefs,classes,sysutils;

type TTinyRLE = class(TObject)
     private
     protected
     public
           function test(Size : unsigned32):boolean;
           function compress  (input : longstring):longstring;
           function uncompress(input : longstring):longstring;
end;


implementation


function TTinyRLE.test(size : unsigned32):boolean;
var
   sTemp1   : longstring;
   sTemp2   : longstring;
   sTemp3   : longstring;
begin
     sTemp1:='';
     while (Size > 0) do
           begin
                sTemp1:=sTemp1 + chr(random(255));
                dec (Size);
           end;

     sTemp2:=Self.Compress(sTemp1);
     sTemp3:=Self.UnCompress(sTemp2);

     result:= sTemp1 = sTemp3;
end;

function TTinyRLE.compress(input : longstring):longstring;
var
   u8Counter  : unsigned8;
   u32Index   : unsigned32;
   u32Len     : unsigned32;
   u8Last     : char;
begin
     result:='';

     u8Counter:=0;
     u32Len:=length(input);

     if (u32Len > 1) then
        begin
             //Beim zweiten Zeichen anfangen
             u32Index:=1;
             u8Last:=input[u32Index];
             inc(u32Index);

             repeat
                   //Zeichenwiederholung ?
                   //Oder der Counter läuft über ?
                   if (input[u32Index] = u8Last) AND (u8Counter < 255) then
                      begin
                           inc(u8Counter);
                      end
                   else
                      begin
                           //Nein, aber eine Sequenz vorrüber ?
                           if (u8Counter > 0) then
                              begin
                                   //Ja => Multiplikator anhängen
                                   //Beim Dekomprimieren müssen wir den Counter um eins runtersetzen.
                                   //Hier lassen wir aber den Wert 1 um kein #0 in den Strom einzufügen
                                   result:=result + u8Last + u8Last + Chr(u8Counter);
                                   u8Counter:=0;
                              end
                           else
                              begin
                                   //Nein => Zeichen anhängen
                                   result:=result + u8Last;
                              end;
                      end;

                   //Für die nächste Runde merken
                   u8Last:=input[u32Index];
                   inc(u32Index);
             until (u32Len < u32Index);

             //Der Rest
             if (u8Counter > 0) then
                begin
                     //Ja => Multiplikator anhängen
                     result:=result + u8Last + u8Last + Chr(u8Counter);
                end
             else
                begin
                     //Nein => Zeichen anhängen
                     result:=result + u8Last;
                end;
        end
     else
        begin
             //String lohnt nicht zu komprimieren
             result:=input;
        end; 

end;

function TTinyRLE.uncompress(input : longstring):longstring;
var
   u32Counter : unsigned32;
   u32Index   : unsigned32;
   u32Len     : unsigned32;
   u8Last     : char;
begin
     result:='';
     u32Len:=length(input);

     //Beim zweiten Zeichen anfangen
     u32Index:=1;
     u8Last:=input[u32Index];
     inc(u32Index);

     if (u32Index <= u32Len) then
        begin
             repeat
                   //Kommen zwei identische Zeichen muß das nächste Zeichen ein
                   //Multiplikator sein
                   if (u8Last = input[u32Index]) then
                      begin
                           //Multiplikator lesen und String auffüllen
                           inc(u32Index);
                           //Überlauf abfangen
                           if (u32Index <= u32Len) then
                              begin
                                   u32Counter:=Ord(input[u32Index]);
                                   result:=result + u8Last + StringOfChar(u8Last,u32Counter);
                              end;
                              
                           inc(u32Index);
                      end
                   else
                      begin
                           result:=result + u8Last;
                      end;

                   //Überlauf abfangen
                   if (u32Index <= u32Len) then
                      begin
                           u8Last:=input[u32Index];
                      end;

                      inc(u32Index);

             until (u32Index > u32Len);

             //Letztes Zeichen ?
             if (u8Last = input[u32Len]) then
                 begin
                      result:=result + u8Last;
                 end;
        end
     else
        begin
             //String zu kurz
             result:=input;
        end; 

end;


end.
