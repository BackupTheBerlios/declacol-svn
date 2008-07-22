unit class_blockio;
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
////////////////////////////////////////////////////////////////////////////////
///
/// Klasse um Dateien komprimiert schreiben oder lesen zu können
/// Ein gemischter Zugriff oder ein bytegenaues Lesen ist nicht möglich
///
/// Blocksize wird zwar automatisch bestimmt, aber es erhöht die Geschwindigkeit
/// sehr, wenn die Blockgröße richtig vorgegeben ist
///
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,class_rle,windows;

type  TBlockReader = class(TObject)
      private
             Packer       : TRLE;
             hFile        : THandle;
             u32blocksize : unsigned32;
             bcompressed  : boolean;

             aBuffer      : array of byte;
             //Dummies für die Initialisierung
             pDummy       : Pointer;
             u32Dummy     : unsigned32;

      protected
      public
            Constructor Create();
            Destructor  free();
            function open (filename : longstring):Boolean;
            function read (var output : pointer; var outsize : unsigned32):Boolean;
            function close():Boolean;

            property compressed: boolean    read bcompressed  write bcompressed;
            property blocksize : unsigned32 read u32blocksize write u32blocksize;  
end;

type  TBlockWriter = class(TObject)
      private
             Packer       : TRLE;
             hFile        : THandle;
             bcompressed  : boolean;
             //Dummies für die Initialisierung
             pDummy       : Pointer;
             u32Dummy     : unsigned32;
      protected
      public
            Constructor Create();
            Destructor  free();
            function open (filename : longstring):Boolean;
            function write(output : pointer; outsize : unsigned32):Boolean;
            function close():Boolean;

            property compressed: boolean    read bcompressed  write bcompressed;
end;



implementation

////////////////////////////////////////////////////////////////////////////////
/// Der Blockreader
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
constructor TBlockReader.create();
begin
     Self.Packer:=TRLE.Create();
     Self.BlockSize:=8192;

     //Handle auf jeden Fall initialisieren
     Self.Close();
end;

////////////////////////////////////////////////////////////////////////////////
destructor TBlockReader.free();
begin
     //Datei auf jeden Fall schließen
     Self.Close();

     Self.Packer.Free();
end;

////////////////////////////////////////////////////////////////////////////////
function TBlockReader.open (filename : longstring):Boolean;
begin
     if (hFile = INVALID_HANDLE_VALUE) then
        begin
             Self.hFile:=CreateFile( pChar(filename),
                                     GENERIC_READ,
                                     FILE_SHARE_READ OR FILE_SHARE_WRITE,
                                     nil,
                                     OPEN_EXISTING,
                                     FILE_ATTRIBUTE_NORMAL,
                                     0);
             //Wenn der Aufruf fehlschlägt ist hFILE = INVALID_HANDLE_VALUE
             result:=Self.hFILE <> INVALID_HANDLE_VALUE;
        end
     else
        begin
             result:=FALSE;
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
function TBlockReader.read (var output : pointer; var outsize : unsigned32):Boolean;
var
   u32Buffer : unsigned32;
begin
     result:=FALSE;

     if (Self.hFile <> INVALID_HANDLE_VALUE) then
        begin
             //Komprimiert müssen wir erst die Paketgröße holen
             if (Self.bCompressed=TRUE) then
                begin
                     ReadFile(Self.hFile,u32Buffer,SizeOf(u32Buffer),Cardinal(Self.u32Dummy),nil);

                     //Gelesen ?
                     if (Self.u32Dummy <> SizeOf(u32Buffer))then
                        begin
                             //PackHeader invalid
                             u32Buffer:=0;
                             result:=FALSE;
                        end;
                end
             else
                begin
                     u32Buffer:=Self.BlockSize;
                end;

             //Buffer immer neu initialisieren, falls der User diesen
             //dynamisch anpassen möchte oder die Kompression dies erzwingt
             //dadurch halten wir den Speicherverbrauch schön niedrig
             SetLength(Self.aBuffer,u32Buffer);

             //Einmal den Buffer füllen
             ReadFile(Self.hFile,Self.aBuffer[0],Length(Self.aBuffer),Cardinal(outsize),nil);

             if (Outsize > 0) then
                begin
                     //Entpacken ?
                     if (Self.bcompressed=TRUE) then
                        begin
                             //Da auch gemischter Betrieb möglich wäre initialisieren wir die Kompression vor
                             //jedem Packvorgang neu
                             Self.Packer.unpack(nil,0,Self.pDummy,Self.u32Dummy);

                             //Entpacken
                             if (Self.Packer.unpack(Addr(Self.aBuffer[0]),outsize,output,outsize)<>TRUE) then
                                begin
                                     //Entpacken fehlgeschlagen
                                     outsize:=0;
                                     result:=FALSE;
                                end
                             else
                                begin
                                     result:=TRUE;
                                end;
                        end
                     else
                        begin
                             //Unkomprimiert mappen
                             output:=Addr(aBuffer[0]);
                             result:=TRUE;
                        end;
                end
             else
                begin
                     //Es konnte nicht mehr gelesen werden
                     Result:=FALSE;
                end;
        end
     else
        begin
             //Datei nicht geöffnet
             result:=FALSE;
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
function TBlockReader.close():Boolean;
begin
     if (Self.hFile <> INVALID_HANDLE_VALUE) then
        begin
             CloseHandle(Self.hFile);
             result:=TRUE;
        end
     else
        begin
             result:=FALSE;
        end;

     //Wie auch immer es gelaufen ist,
     //der Handle ist invalide
     Self.hFile:=INVALID_HANDLE_VALUE;
end;


////////////////////////////////////////////////////////////////////////////////
/// Der Blockwrite
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
constructor TBlockWriter.create();
begin
     Self.Packer:=TRLE.Create();

     //Handle auf jeden Fall initialisieren
     Self.Close();
end;

////////////////////////////////////////////////////////////////////////////////
destructor TBlockWriter.free();
begin
     //Datei auf jeden Fall schließen
     Self.Close();

     Self.Packer.Free();
end;

////////////////////////////////////////////////////////////////////////////////
function TBlockWriter.open (filename : longstring):Boolean;
begin
     if (hFile = INVALID_HANDLE_VALUE) then
        begin
             Self.hFile:=CreateFile( pChar(filename),
                                     GENERIC_WRITE,
                                     0, //No Share
                                     nil,
                                     CREATE_ALWAYS,
                                     FILE_ATTRIBUTE_NORMAL,
                                     0);
             //Wenn der Aufruf fehlschlägt ist hFILE = INVALID_HANDLE_VALUE
             result:=Self.hFILE <> INVALID_HANDLE_VALUE;
        end
     else
        begin
             result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TBlockWriter.write (output : pointer; outsize : unsigned32):Boolean;
var
   u32write : unsigned32;
   pdata    : ^Byte;
begin
     result:=FALSE;
     if (Self.hFile <> INVALID_HANDLE_VALUE) then
        begin
             //Komprimiert müssen wir erst die Paketgröße holen
             if (Self.bCompressed=TRUE) then
                begin
                     //Da auch gemischter Betrieb möglich wäre initialisieren wir die Kompression vor
                     //jedem Packvorgang neu
                     Self.Packer.pack(nil,0,Self.pDummy,Self.u32Dummy);


                     if (Self.Packer.pack(output,outsize,pointer(pData),outsize) = TRUE) then
                        begin
                             //Blockgröße schreiben
                             WriteFile(Self.hFile,outsize,SizeOf(outsize),cardinal(u32write),nil);
                        end
                     else
                        begin
                             //Packen fehlgeschlagen!
                             outsize:=0;
                             result:=FALSE;
                        end;

                end
             else
                begin
                     //Einfach mappen
                     pData:=output;
                end;

             if (outsize > 0) then
                begin
                     WriteFile(Self.hFile,pData^,outsize,cardinal(u32write),nil);
                     result:=outsize = u32write;
                end;
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
function TBlockWriter.close():Boolean;
begin
     if (Self.hFile <> INVALID_HANDLE_VALUE) then
        begin
             CloseHandle(Self.hFile);
             result:=TRUE;
        end
     else
        begin
             result:=FALSE;
        end;

     //Wie auch immer es gelaufen ist,
     //der Handle ist invalide
     Self.hFile:=INVALID_HANDLE_VALUE;
end;


end.
