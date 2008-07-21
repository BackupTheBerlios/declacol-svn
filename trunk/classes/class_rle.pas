unit class_rle;
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
/// Klasse um Speicherbereiche mit REL zu komprimieren
/// Alle anderen Klassen sollten hierauf zugreifen.
/// Die öffentlichen Eigenschaften/Methoden sind für alle Kompressionsklassen gleich
/// Werden aber nicht für jede Klasse benutzt
///
/// Um die Kompression zu initialisieren wird sie einmal mit dem Pointer nil und
/// Size 0 aufgerufen. Gleiches gilt für die Dekompression
/// Output enthält einen Pointer auf den Beginn der Ausgabe. 
///
/// Der Speicher auf den Output verweist liegt intern in einem Memorystream.
/// Eine weiterer Aufruf der (Un)Pack-Funktion überschreibt alte Daten 
/// Free zerstört die Memorystreams. Damit werden die Outputpointer ungültig
///
////////////////////////////////////////////////////////////////////////////////

interface

uses unit_compiler,unit_typedefs,classes;

type TRLE = class(TObject)
     private
            u32Mode   : unsigned32;
            u32Buffer : unsigned32;

            //Zum Puffern benutzen wir einfach Memorystreams
            PackBuffer   : TMemoryStream;
            UnpackBuffer : TMemoryStream;    

     protected
           function UnPackInit():Boolean;
           function PackInit():Boolean;

           function _test   (input : pointer; insize : unsigned32):Boolean;
           function _pack   (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
           function _unpack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;

     public
           constructor Create();
           constructor Free();

           function test   (size : unsigned32):Boolean;
           function pack   (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
           function unpack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;

           //Eigenschaften
           //Werden in dieser Klasse nicht benutzt
           property buffersize : unsigned32 read u32Buffer write u32Buffer;
           property mode       : unsigned32 read u32Mode   write u32Mode;
end;


implementation

////////////////////////////////////////////////////////////////////////////////
constructor TRLE.Create();
begin
     PackBuffer:=TMemoryStream.Create();
     UnpackBuffer:=TMemoryStream.Create();

     //Buffer setzen
     Self.BufferSize:=8192;
     Self.UnpackInit();
     Self.PackInit();
end;

////////////////////////////////////////////////////////////////////////////////
constructor TRLE.Free();
begin
     PackBuffer.Free();
     UnpackBuffer.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Initialisieren des Entpackens
function TRLE.UnpackInit():Boolean;
begin
     UnpackBuffer.Clear();
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Initialisieren des Packens
function TRLE.PackInit():Boolean;
begin
     PackBuffer.Clear();
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Interne Testfunktion
function TRLE.test    (size : unsigned32):Boolean;
var
   u32Index : unsigned32;
   aTemp    : Array of Byte;
begin
     result:=TRUE;

     SetLength(aTemp,Size);

     //Speicher mit Zufallsdaten füllen
     randomize();
     u32Index := 0;
     While (u32Index < unsigned32(Length(aTemp))) do
           begin
                aTemp[u32Index]:=Byte(random(High(Byte)));
                inc(u32Index);
           end;

     //Testen
     Result:=Result AND Self._test(Addr(aTemp[0]),Length(aTemp));

     //Speicher mit Daten füllen
     randomize();
     u32Index := 0;
     While (u32Index < unsigned32(Length(aTemp))) do
           begin
                aTemp[u32Index]:=10;
                inc(u32Index);
           end;

     //Testen
     Result:=Result AND Self._test(Addr(aTemp[0]),Length(aTemp));

     //Speicher freigeben
     SetLength(aTemp,0);
end;


function TRLE._test(input : pointer; insize : unsigned32):Boolean;
var
   pPack    : ^Byte;
   pUnpack  : ^Byte;
   pData    : ^Byte;
   u32Size  : unsigned32;
   u32Index : unsigned32;
begin
     result:=TRUE;

     pData:=input;

    //Init
     Self.Pack(nil,0,Pointer(pPack),u32Size);


     //Komprimieren
     if (Self.Pack(input,insize,pointer(pPack),u32Size) = TRUE) then
        begin
             //Dekomprimieren
             Self.Unpack(nil,0,pointer(pUnpack),u32Size);
             if ( Self.Unpack(pointer(pPack),u32Size,pointer(pUnpack),u32Size) = TRUE) then
                begin
                     //Daten vergleichen
                     if (u32Size = insize) then
                        begin
                             u32Index:=0;
                             while (u32Index < u32Size) do
                                   begin
                                        //Bei Fehler abbrechen
                                        if (pData^ <> pUnpack^) then
                                           begin
                                                result:=FALSE;
                                                u32Index:=u32Size - 1;
                                           end;

                                        inc(pUnpack);
                                        inc(pData);
                                        inc(u32Index);
                                   end;

                        end;
                end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Packen
function TRLE.Pack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
begin
     //Initialisierung machen
     if (Input = nil) then
        begin
             result:=Self.PackInit();
        end
     else
        begin
             //Komprimieren
             if (insize > 0) then
                begin
                     result:=Self._Pack(input,insize,output,outsize);
                end
             else
                begin
                     result:=FALSE;
                end; 
        end;
end;

function TRLE._Pack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
var
   u8Counter  : unsigned8;
   u8Current  : unsigned8;
   u8Last     : unsigned8;
   u32Index   : unsigned32;
   pData      : ^Byte;
begin
     //Zähler für Sequenzen 
     u8Counter:=0;

     //Zähler für Eingabe
     u32Index:=1;

     //Den Pointer mappen wie einfach auf ein Byte,
     //damit wird der Zugriff viel einfacher und das iterieren
     //nit von Exceptions gestört.
     pData:=Input;

     //Ausgabestream initialisieren
     PackBuffer.Size:=Self.BufferSize;
     PackBuffer.Position:=0;


     //Erstes Zeichen in den Schleifenpuffer laden
     u8Last:=pData^;
     inc(pData);

     //Und alle Weiteren verarbeiten
     while (u32Index < InSize) do
           begin
                //Zeichen laden
                u8Current:=pData^;
                inc(pData);

                //Zeichenwiederholung ?
                //Oder der Counter läuft über ?
                if (u8Current = u8Last) AND (u8Counter < 255) then
                   begin
                        inc(u8Counter);
                   end
                else
                    begin
                         //Zeichen schreiben
                         PackBuffer.Write(u8Last,SizeOf(u8Last));

                         //Sequenz vorrüber ?
                         if (u8Counter > 0) then
                            begin
                                 //Ja => Multiplikator anhängen
                                 PackBuffer.Write(u8Last,SizeOf(u8Last));
                                 PackBuffer.Write(u8Counter,SizeOf(u8Counter));
                                 u8Counter:=0;
                            end;
                      end;

                   //Für die nächste Runde merken
                   u8Last:=u8Current;

                   inc(u32Index);
             end;

     //Der Rest
     PackBuffer.Write(u8Last,SizeOf(u8Last));
     if (u8Counter > 0) then
        begin
             PackBuffer.Write(u8Last,SizeOf(u8Last));
             PackBuffer.Write(u8Counter,SizeOf(u8Current));
        end;


     //Nun sollte alles gepackte im DeBuffer-Stream sein
     outsize:=unsigned32(PackBuffer.Position);
     output:=PackBuffer.Memory;

     //Diese Kompression hat immer Erfolg
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Entpacken
function TRLE.Unpack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
begin
     if (Input = nil) then
        begin
             result:=Self.UnpackInit();
        end
     else
        begin
             //Komprimieren
             if (insize > 0) then
                begin
                     result:=Self._Unpack(input,insize,output,outsize);
                end
             else
                begin
                     result:=FALSE;
                end; 
        end;
end;

function TRLE._Unpack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
var
   u8Counter  : unsigned8;
   u8Last     : unsigned8;
   u8Current  : unsigned8;
   pData      : ^Byte;
   u32Index   : unsigned32;
begin
     //Zähler für Eingabe
     u32Index:=1;

     //Den Pointer mappen wie einfach auf ein Byte,
     //damit wird der Zugriff viel einfacher und das iterieren
     //nit von Exceptions gestört.
     pData:=Input;

     //Ausgabestream initialisieren
     UnpackBuffer.Size:=Self.BufferSize;
     UnpackBuffer.Position:=0;

     //Erstes Zeichen in den Schleifenpuffer laden
     u8Current:=pData^;
     inc(pData);

     while (u32Index < insize) do
        begin
             //Für die nächste Runde merken
             u8Last:=u8Current;

             //Erstes Zeichen können wir immer ausgeben
             UnpackBuffer.Write(u8Last,SizeOf(u8Last));

             //Kommen zwei identische Zeichen muß das nächste Zeichen ein
             //Multiplikator sein
             u8Current:=pData^;
             inc(pData);
             inc(u32Index);

             if (u8Last = u8Current) then
                begin
                     //Multiplikator lesen und String auffüllen
                     u8Counter:=pData^;
                     inc(pData);
                     inc(u32Index);

                     while (u8Counter > 0) do
                           begin
                                UnpackBuffer.Write(u8Last,SizeOf(u8Last));
                                dec(u8Counter);
                           end;

                     //Wieder mit dem Eingangsstrom synchronisieren
                     u8Current:=pData^;
                     inc(pData);
                     inc(u32Index);
                end;

        end;

     //Letztes Zeichen anhängen
     if (u8Last <> u8Current) then
        begin
             UnpackBuffer.Write(u8Current,SizeOf(u8Current));
        end;

     //Ausgabe mappen
     outsize:=unsigned32(UnpackBuffer.Position);
     output :=UnpackBuffer.Memory;

     //Diese DeKompression hat immer Erfolg
     result:=TRUE;
end;

end.
