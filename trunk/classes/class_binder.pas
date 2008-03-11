unit class_binder;
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
/// Eine Streamklasse die abgeleitet von Filestream
/// einen Schreiblesezugriff auf einen Datenbereich am Ende einer Exe-Datei
/// zulässt.
///
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,windows,classes,sysutils;

type
   pIMAGE_DOS_HEADER      = ^IMAGE_DOS_HEADER;
   pIMAGE_FILE_HEADER     = ^IMAGE_FILE_HEADER;
   pIMAGE_OPTIONAL_HEADER = ^IMAGE_OPTIONAL_HEADER;
   pIMAGE_SECTION_HEADER  = ^IMAGE_SECTION_HEADER;


type tbinderstream = class (TFileStream)
     private
            u32ExeOffset : unsigned32;
            sExeName     : longstring;

            bReadOnly    : Boolean;

            function     GetStreamPosition():unsigned64;
            procedure    SetStreamPosition(Value:unsigned64);
            function     GetStreamSize():unsigned64;
            procedure    SetStreamSize(Value:unsigned64);

            function     getexesize(ExeHandle:unsigned32):unsigned64;
            function     getexename():longstring;
            function     iswritable(Filename:longstring):boolean;

     public
            constructor  create();

            function     Seek(Offset: Longint; Origin: Word): Longint;

            property     Position       : unsigned64 read GetStreamPosition write SetStreamPosition;
            property     Size           : unsigned64 read GetStreamSize     write SetStreamSize;
            property     WriteProtected : boolean    read bReadOnly;
end;


implementation


constructor tbinderstream.create();
begin
     Self.u32ExeOffset:=Self.getexesize(hInstance);
     Self.sExeName:=Self.getexename();

     //Einen Stream auf uns öffnen
     bReadOnly:=not Self.iswritable(Self.sExeName);
     if (bReadOnly) then
        begin
             inherited Create(Self.sExeName,fmOpenRead or fmShareDenyNone);
        end
     else
        begin
             inherited Create(Self.sExeName,fmOpenReadWrite or fmShareDenyNone);
        end;

     //Und Dateizeiger zur Sicherheit positionieren
     Self.Seek(0,soFromBeginning);
end;

////////////////////////////////////////////////////////////////////////////////
//Getter und Setter um vor dem Benutzer die waren Werte zu verstecken
function tbinderstream.GetStreamPosition():unsigned64;
begin
     result:=unsigned32(inherited Position) - Self.u32ExeOffset;
end;

procedure tbinderstream.SetStreamPosition(Value:unsigned64);
begin
     inherited Position := Value + Self.u32ExeOffset;
end;

function tbinderstream.GetStreamSize():unsigned64;
begin
     result:=unsigned32(inherited Size) - Self.u32ExeOffset;
end;

procedure tbinderstream.SetStreamSize(Value:unsigned64);
begin
     inherited Size := Value + Self.u32ExeOffset;
end;

function TBinderStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
     result:=inherited Seek(Offset + Self.u32ExeOffset,Origin);
     result:=unsigned32(unsigned32(result) - u32ExeOffset);
end;


////////////////////////////////////////////////////////////////////////////////
//Die Dateigröße aus dem EXE-Header holen
function tbinderstream.getexesize(ExeHandle:Unsigned32):unsigned64;
var
   pData    : ^Byte;
   u32Count : unsigned32;
   u32Size  : unsigned32;
begin
     result:=0;

     //hInstance verweist auf den eigenen virtuellen Speicher und
     //erlaubt damit ein auslesen der Exe-Daten
     pData := Pointer(ExeHandle);

     //Zum PE-Header springen
     inc( pData, pIMAGE_DOS_HEADER(pData)^._lfanew + SizeOf(Unsigned16));

     //PE-Kennung überspringen
     //Brauchen wir nicht zu checken, da wir immer eine PE-Anwendung sind
     inc( pData, SizeOf(IMAGE_NT_SIGNATURE));

     //Anzahl der Sektionen lesen
     u32Count := pIMAGE_FILE_HEADER(pData)^.NumberOfSections;

     //Zu den Sektionstabellen springen
     inc(pData,IMAGE_SIZEOF_FILE_HEADER + IMAGE_SIZEOF_NT_OPTIONAL_HEADER);

     //Alle Sektionen enumerieren und die Größe aufaddieren
     while (u32Count > 0) do
           begin
                //Evtl. Größe lesen
                u32Size:=pIMAGE_SECTION_HEADER(pData)^.SizeOfRawData + pIMAGE_SECTION_HEADER(pData)^.PointerToRawData;

                //Ist OK ?
                if ( u32Size > Result ) then
                              begin
                                   Result:=u32Size;
                              end;

                //Nächste Sektion addressieren
                inc(pData,SizeOf(IMAGE_SECTION_HEADER));
                dec(u32Count);
           end;
end;


////////////////////////////////////////////////////////////////////////////////
//Den Dateinamen unseres Prozesses holen
function tbinderstream.getexename():Longstring;
var
   aPath : array[0..MAX_PATH] of Char;
begin
     GetModuleFileName(0,aPath,Length(aPath));
     result:=String(aPath);
end;

////////////////////////////////////////////////////////////////////////////////
//Prüfen, ob eine Datei schreibfähig ist.
function tbinderstream.iswritable(filename:longstring):boolean;
var
   hFile : TFileHandle;
begin
     hFile:=FileOpen(filename,fmOpenReadWrite);
     if (hFile <> INVALID_HANDLE_VALUE) then
        begin
             CloseHandle(hFile);
             result:=TRUE;
        end
     else
        begin
             result:=FALSE;
        end; 
end;


end.
