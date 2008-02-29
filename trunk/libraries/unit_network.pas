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
//////////////////////////////////////////////////////////////////////////////////////////
//
// Auslesen diverser Netzwerkinformationen
//
//////////////////////////////////////////////////////////////////////////////////////////
unit Unit_NetWork;

interface
uses Windows,WinInet,SysUtils,Classes,WinSock,NB30,Class_CheckSum;

const
     RAS_MaxDeviceType = 16;
     RAS_MaxDeviceName = 128;

type
   TRasDevInfoA = record
     dwSize: Longint;
     szDeviceType: Array[0..RAS_MaxDeviceType] of AnsiChar;
     szDeviceName: Array[0..RAS_MaxDeviceName] of AnsiChar;
   end;
   TRasDevInfo = TRasDevInfoA;
   LPRasDevInfo = ^TRasDevInfo;
   TMACAddress = packed array[0..5] of Byte;
   ENetBiosError = class(Exception);





function GetPCId():string;
function NetWork():Boolean;
function GetLocalIPs():TStrings;
function EnumModems():TStringList;
function GetMacAddress():String;

//Externe Functionen
function RasEnumDevices(lpBuff: LPRasDevInfo; var lpcbSize: Longint; var lpcDevices: Longint): Longint; stdcall;
function RasEnumDevices; external 'rasapi32.dll' name 'RasEnumDevicesA';



implementation

///////////////////////////////////////////////////////////////////////////////////////////////////
function NetWork():Boolean;
var
   INet : LongWord;
begin
     Result:=TRUE;
     //Sind wir über einen Router Online ?
     if (InternetGetConnectedState(@INet,0)) then
        begin
             if (INet and INTERNET_CONNECTION_LAN)<> INTERNET_CONNECTION_LAN then
                begin
                     Result:=FALSE;
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Netzwerkname des PC's lesen
function GetPCId():string;
var
   pcName : pchar;   // Holds the computer name
   dwSize : LongWord;   // Size of the buffer holding the name
   z      : integer;
begin
     //PC-Name holen
     try
        dwSize := MAX_COMPUTERNAME_LENGTH + 1;
        pcName := strAlloc( dwSize );

        if getComputerName( pcName, dwSize )=FALSE then pcName:='NA';
     finally
     end;

     //Aus dem Buffer einen String machen
     for z:=0 to dwSize do
         begin
              if pcName[z]='\' then pcName[z]:=' ';
         end;

     //Fertig
     Result:=string( trim (pcName) );
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Eigene IP-Adresse holen
function GetLocalIPs():TStrings;
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


function EnumModems():TStringList;
var
   Device    : Array[0..64] of TRASDEVINFO;
   DeviceNum : Integer;
   Size      : LongInt;
begin
     Result:=TStringList.Create;
     Result.Clear;

     DeviceNum:=0;
     Size:=SizeOf(TRASDEVINFO)*64;
     Device[0].dwSize:=SizeOf(TRASDEVINFO);

     if (RasEnumDevices(@Device[0],Size,DeviceNum)=0) then
        begin
             Dec(DeviceNum);
             While (DeviceNum>=0) do
                   begin
                        Result.Add(Device[DeviceNum].szDeviceName);
                        Dec(DeviceNum);
                   end;
        end
     else
        begin
             Result.Add('None');
        end;

end;


function GetMACAddress: string;
var
   sTemp : String;
   iTemp : Integer;
   NCB: PNCB;
   Adapter: PAdapterStatus;

   URetCode: PChar;
   RetCode: char;
   I: integer;
   Lenum: PlanaEnum;
   _SystemID: string;
   TMPSTR: string;
begin
  Result    := '';
  _SystemID := '';
  Getmem(NCB, SizeOf(TNCB));
  Fillchar(NCB^, SizeOf(TNCB), 0);

  Getmem(Lenum, SizeOf(TLanaEnum));
  Fillchar(Lenum^, SizeOf(TLanaEnum), 0);

  Getmem(Adapter, SizeOf(TAdapterStatus));
  Fillchar(Adapter^, SizeOf(TAdapterStatus), 0);

  Lenum.Length    := chr(0);
  NCB.ncb_command := chr(NCBENUM);
  NCB.ncb_buffer  := Pointer(Lenum);
  NCB.ncb_length  := SizeOf(Lenum);
  RetCode         := Netbios(NCB);

  i := 0;
  repeat
    Fillchar(NCB^, SizeOf(TNCB), 0);
    Ncb.ncb_command  := chr(NCBRESET);
    Ncb.ncb_lana_num := lenum.lana[I];
    RetCode          := Netbios(Ncb);

    Fillchar(NCB^, SizeOf(TNCB), 0);
    Ncb.ncb_command  := chr(NCBASTAT);
    Ncb.ncb_lana_num := lenum.lana[I];
    // Must be 16
    Ncb.ncb_callname := '*               ';

    Ncb.ncb_buffer := Pointer(Adapter);

    Ncb.ncb_length := SizeOf(TAdapterStatus);
    RetCode        := Netbios(Ncb);
    //---- calc _systemId from mac-address[2-5] XOR mac-address[1]...
    if (RetCode = chr(0)) or (RetCode = chr(6)) then
    begin
      _SystemId := IntToHex(Ord(Adapter.adapter_address[0]), 2) + '-' +
        IntToHex(Ord(Adapter.adapter_address[1]), 2) + '-' +
        IntToHex(Ord(Adapter.adapter_address[2]), 2) + '-' +
        IntToHex(Ord(Adapter.adapter_address[3]), 2) + '-' +
        IntToHex(Ord(Adapter.adapter_address[4]), 2) + '-' +
        IntToHex(Ord(Adapter.adapter_address[5]), 2);
    end;
    Inc(i);
  until (I >= Ord(Lenum.Length)) or (_SystemID <> '00-00-00-00-00-00');
  FreeMem(NCB);
  FreeMem(Adapter);
  FreeMem(Lenum);

  //Keine MAC gelesen, dann eine Erzeugen
  Result := trim(_SystemID);
{
  if (Length(Result)<>Length('00-00-00-00-00-00')) then
     begin
          sTemp:=CHK_StringToAdler32(GetPCId);

          result:=sTemp;
          while (length(result)< 20) do
                begin
                     sTemp:=CHK_StringToAdler32(result);
                     result:=result+Copy(sTemp,1,2);
                end;
          sTemp:=result;
          //Und in eine MAC basteln
          sTemp[3]:='-';
          sTemp[6]:='-';
          sTemp[9]:='-';
          sTemp[12]:='-';
          sTemp[15]:='-';
          result:=copy(sTemp,1,17);
     end;
}
end;




end.
