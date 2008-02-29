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

/////////////////////////////////////////////////////////////////////////////////
///
/// Unit zur Hardwareerkennung
///
/// v 0.6.1
///
/////////////////////////////////////////////////////////////////////////////////
unit Unit_Hardware;

interface

uses Windows,Classes,SysUtils,WinInet,WinSock,Messages;


type THDStatus=Record
     sSerialNumber  : String;
     lwSerialNumber : INT64;
     sLabel         : String;
     lwHDLoad       : LongWord;
     sHDLoad        : String;
     lwTotalPhys    : INT64;
     lwTotalAvail   : INT64;
     lwTotalUsed    : INT64;

     sTotalPhys     : String;
     sTotalAvail    : String;
     sTotalUsed     : String;

     bCompressed    : Boolean;
     bReady         : Boolean;
     end;

type TCPUStatus=Record
     sSerialNumber  : String;
     lwSerialNumber : INT64;
     lwFrequency    : INT64;
     sFrequency     : String;
     sVendor        : String;
     end;


///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
function MemInfo():MemoryStatus;
function HDInfo(Drive:Char):THDStatus;
function CPUInfo():TCPUStatus;
function MacInfo: string;
function IPInfo():TStrings;


///////////////////////////////////////////////////////////////////////////////////////////////////
//procedure MonitorOff();
//procedure MonitorOn();

implementation
///////////////////////////////////////////////////////////////////////////////////////////////////
function MemInfo():MemoryStatus;
begin
   //Speicherstand abfragen (WinAPI)
   GlobalMemoryStatus(Result);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////
function HDInfo(Drive:Char):THDStatus;
var
   Name : Array[0..64]of Char;
   FS   : Array[0..64]of Char;
   dw   : DWord;
   cm   : Cardinal;
begin
     //Alles Initialisieren
     Result.sSerialNumber  :='';
     Result.lwSerialNumber :=0;
     Result.sLabel         :='none';
     Result.lwHDLoad       :=0;
     Result.sHDLoad        :='';
     Result.lwTotalPhys    :=0;
     Result.lwTotalAvail   :=0;
     Result.lwTotalUsed    :=0;
     Result.bCompressed    :=FALSE;
     Result.bReady         :=FALSE;

     //Daten holen und fehlende Werte bestimmen
     if (GetVolumeInformation ( PChar(Drive+':\'),Name,SizeOf(Name),@Result.lwSerialNumber,dw,cm,FS,SizeOf(FS))=TRUE) then
        begin
             Result.sSerialNumber:=IntToStr(Result.lwSerialNumber);

             //Laufwerkgröße lesen
             Result.lwTotalPhys :=SysUtils.DiskSize(Byte(Drive)-$40);
             Result.lwTotalAvail:=SysUtils.DiskFree(Byte(Drive)-$40);
             Result.lwTotalUsed :=Result.lwTotalPhys-Result.lwTotalAvail;
             Result.sTotalPhys  :=IntToStr(Result.lwTotalPhys);
             Result.sTotalAvail :=IntToStr(Result.lwTotalAvail);
             Result.sTotalUsed  :=IntToStr(Result.lwTotalUsed);

             //Prozent bestimmen
             Result.lwHDLoad:=100-Round(Result.lwTotalAvail/Result.lwTotalPhys*100);
             Result.sHDLoad:=IntToStr(Result.lwHDLoad);

             //Name holen
             Result.sLabel:=Name;

             //Komprimiert ?
             if (CM and FS_VOL_IS_Compressed)=FS_VOL_IS_COMPRESSED then Result.bCompressed:=TRUE;

             Result.bReady:=TRUE;
        end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////
function CPUInfo():TCPUStatus;
var
  f,tsc,pc : Int64;

   function RDQPC : Int64;
      begin
           QueryPerformanceCounter(result);
      end;
   function RDTSC : Int64; assembler;
      asm
         db $0F, $31  // opcode for RDTSC
      end;

begin
     //Werte initialisieren
     Result.sSerialNumber  :='';
     Result.lwSerialNumber := 0;
     Result.lwFrequency    := 0;
     Result.sFrequency     := '';
     Result.sVendor        := '';

     if QueryPerformanceFrequency(f) then
        begin
             Sleep(0);
             pc := RDQPC;
             tsc := RDTSC;
             Sleep(100);
             pc := RDQPC-pc;
             tsc := RDTSC-tsc;
             pc := round(tsc*f/(pc*1000000));
        end
     else
        begin
             pc := -1;
        end;

     Result.sFrequency     := IntToStr(pc);
end;


function  MacInfo(): string; 
begin
  Result := '';
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Eigene IP-Adresse holen
function IPInfo():TStrings;
type
    TaPInAddr = array[0..10] of PInAddr;
    PaPInAddr = ^TaPInAddr;
var
   phe: PHostEnt;
   pptr: PaPInAddr;
   Buffer: array[0..63] of Char;
   I: Integer;
   GInitData: TWSAData;
begin
     WSAStartup($101, GInitData);
     Result := TstringList.Create;
     Result.Add('127.0.0.1');
     GetHostName(Buffer, SizeOf(Buffer));
     phe := GetHostByName(buffer);
     if phe = nil then Exit;

     pPtr := PaPInAddr(phe^.h_addr_list);
     I    := 0;

     Result.Clear;
     while pPtr^[I] <> nil do
           begin
                Result.Add(inet_ntoa(pptr^[I]^));
                Inc(I);
           end;
     WSACleanup;
end;




end.
