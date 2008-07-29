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
///
/// Alle Kompressionsmethoden werden extern eingebunden um den Wrapper
/// möglichst allgemein zu halten
///
/// Da je nach Kompressionmethode Probleme durch die Streams entstehen können,
/// Werden immer Blöcke ind "buffersize"-Größe komprimiert und einzeln geschrieben
/// Vor jedem Block steht als 32BitZahl der Größendeskriptor des Paketes um
/// bei der Dekompression das volle Paket ansprechen zu können
////////////////////////////////////////////////////////////////////////////////

unit class_streamcompression;

interface
uses unit_typedefs,classes,class_lzw;

type TStreamCompression = class(TObject)
     private
            Packer    : TLZW;
            u32buffer : unsigned32;
            u32mode   : unsigned32;
     protected
     public

           constructor create();
           destructor  free();

           function test  (Size : unsigned32):boolean;
           function pack  (input,output : TStream):boolean;
           function unpack(input,output : TStream):boolean;

           property buffersize : unsigned32 read u32buffer write u32buffer;
           property mode       : unsigned32 read u32mode   write u32mode;
end;


implementation

////////////////////////////////////////////////////////////////////////////////
/// Konstruktor / Destruktor
////////////////////////////////////////////////////////////////////////////////
constructor TStreamCompression.create();
begin
     self.buffersize:=81920;

     Packer:=TLZW.Create();
end;

destructor  TStreamCompression.free();
begin
     Packer.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Interner Test der Kompression
////////////////////////////////////////////////////////////////////////////////
function TStreamCompression.test(size : unsigned32):boolean;
var
   Temp1    : TMemoryStream;
   Temp2    : TMemoryStream;
   Temp3    : TMemoryStream;
   u32Count : unsigned32;
   u8Data   : unsigned8;
   u8Data1  : unsigned8;
   sTemp    : longstring;
begin
     Temp1:=TMemoryStream.Create();
     Temp2:=TMemoryStream.Create();
     Temp3:=TMemoryStream.Create();

     //Interne Testfunktion des Packers
     Result:=Self.Packer.test(Size);

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
                           Temp1.Write(u8Data,SizeOf(u8Data));
                           dec (u32Count);
                      end;

                //Komprimieren
                Temp1.Position:=0;
                Temp2.Position:=0;
                Temp2.Size:=0;
                Temp3.Position:=0;
                Temp3.Size:=0;

                Self.pack(Temp1,Temp2);

                //Dekomprimieren
                Temp2.Position:=0;
                Temp3.Position:=0;
                Temp3.Size:=0;

                Self.unpack(Temp2,Temp3);

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

     //Cleanup
     Temp1.Free();
     Temp2.Free();
     Temp3.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Stream komprimieren
function TStreamCompression.pack(input,output : TStream):boolean;
var
   InBuffer  : Array of Byte;
   pPacked   : ^Byte;
   u32Packed : unsigned32;
   u32Size   : unsigned32;
begin
     result:=TRUE;

     //Packer initialisieren
     Self.Packer.BufferSize:=Self.BufferSize;
     Self.Packer.Mode:=Self.Mode;
     Self.Packer.pack(nil,0,pointer(pPacked),u32Size);

     //Und den ganzen eingabestream packen
     SetLength(InBuffer,Self.BufferSize);
     repeat
       u32Size:=Input.Read(InBuffer[0],Length(InBuffer));
       if (u32Size > 0) then
          begin
               //Daten packen
               if (Self.Packer.pack(Addr(InBuffer[0]),u32Size,pointer(pPacked),u32Packed) = TRUE) then
                  begin
                       //Größe schreiben
                       Output.Write(u32Packed,SizeOf(u32Packed));

                       //Und in Ausgabe schreiben
                       Output.Write(pPacked^,u32Packed);
                  end
               else
                  begin
                       //Packen schiefgegangen
                       u32Size:=0;
                       result:=FALSE;
                  end; 
          end;
     until (u32Size = 0);

     SetLength(InBuffer,0);
end;

////////////////////////////////////////////////////////////////////////////////
/// Dekompression durchführen
////////////////////////////////////////////////////////////////////////////////
function TStreamCompression.unpack(input,output : TStream):boolean;
var
   InBuffer    : Array of Byte;
   pUnPacked   : ^Byte;
   u32UnPacked : unsigned32;
   u32Size     : unsigned32;
   sTemp       : longstring;
begin
     result:=TRUE;

     //Packer initialisieren
     Self.Packer.BufferSize:=Self.BufferSize;
     Self.Packer.Mode:=Self.Mode;
     Self.Packer.unpack(nil,0,pointer(pUnPacked),u32Size);

     //Und den ganzen eingabestream entpacken
     repeat
       //Größendeskriptor lesen    
       if ( Input.Read(u32Size,SizeOf(u32Size)) = SizeOf(u32Size) ) then
          begin
               //Puffer richtig setzen
               SetLength(InBuffer,u32Size);

               //Packet einlesen
               u32Size:=Input.Read(InBuffer[0],u32Size);

               //OK ?
               if (u32Size = unsigned32(Length(InBuffer))) then
                  begin
                       //Daten packen
                       if (Self.Packer.unpack(Addr(InBuffer[0]),u32Size,pointer(pUnPacked),u32UnPacked) = TRUE) then
                          begin
                               //Und in Ausgabe schreiben
                               Output.Write(pUnPacked^,u32UnPacked);
                          end
                       else
                          begin
                               //Packen schiefgegangen
                               u32Size:=0;
                               result:=FALSE;
                          end;
                  end
               else
                  begin
                       //Defektes Datenpaket
                       u32Size:=0;
                       result:=FALSE;
                  end;
          end
       else
          begin
               //Defektes Datenpaket
               u32Size:=0;
               result:=FALSE;
          end;
     until (u32Size = 0);

     SetLength(InBuffer,0);
end;


end.
