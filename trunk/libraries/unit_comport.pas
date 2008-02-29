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
unit Unit_ComPort;
////////////////////////////////////////////////////////////////////////////////
///
/// EasyComport Borg@Sven-of-Nine.de
///
/// ComPort auf die Harte Tour.
///
///Prototypen
/// Gibt die verfügbaren (nicht installierten) Ports zurück
/// ComPortsEnumerateActive():TStringList
///
/// Gibt die installierten Ports zurück
/// ComPortsEnumerate():TStringList
///
/// Öffnet das generische ConfigFenster für Comport (String)
/// ComPortConfig         (Comport):Bool
///
/// Setzt die Übergebene Configuration
/// ComPortSetConfig      (Comport,Baud,Bits,Parity,Stop,Flow):Bool
///
/// Setzt Timeoutwerte für den Comport
/// ComPortTimeOut        (Comport,IntRead,TotalRead,TotalWrite):Bool
///
/// Schreibt die durch den Pointer übergebenen Bytes
/// ComportWriteRaw(Comport,Data,Size):Bool
///
/// Versucht Size-Bytes aus dem Comport zu lesen.
/// ComportReadRwaw(Comport,Data,Size):Bool
///
////////////////////////////////////////////////////////////////////////////////
///
/// History
///
/// v 0.25 CommClearError scheint nicht notwendig zu sein und wurde aus allen Funktionen entfernt.
/// v 0.2  Enumfunktion durch EnumActive ersetzt, da nur die verfügbaren wichtig sind
///        der Enumerator ist zwar unelegant aber er funktioniert scheinbar problemlos
/// v 0.14 SetTimeouts eingefügt, um ReadRaw zu vereinfachen
/// v 0.1  So weit fertig
///
////////////////////////////////////////////////////////////////////////////////
interface
uses Windows,Classes,unit_typedefs;

///Konfigurationen des ComPorts
function ComPortsEnumerate():TStringList;
function ComPortsEnumerateActive():TStringList;
function ComPortConfig(sComPort:string):Boolean;
function ComPortSetConfig(sComPort:string;Config:TCommConfig):Boolean;
function ComPortGetConfig(sComPort:string;var Config:TCommConfig):Boolean;

function ComPortTimeOuts(Comport:String;IntervalRead,TotalRead:unsigned32;TotalWrite:unsigned32):Boolean;

///Lesen und schreiben
function ComportWriteRaw(Comport:String;Data:Pointer;Size:unsigned32;ClearBuff:Boolean=FALSE):Boolean;
function ComportReadRaw(Comport:String;Data:Pointer;var Size:unsigned32;ClearBuff:Boolean=FALSE):Boolean;

//Schreiben und auf eine Antwort warten
function ComportPollRaw(Comport:String;OutData:Pointer;OutSize:unsigned32;InData:Pointer;var InSize:unsigned32;ClearBuff:Boolean=FALSE):Boolean;

//Direkter Zugriff auf die Schnittstellen
function ComOpen (ComPort:String):THND;

function ComClear(ComHandle:thnd):Boolean;

function ComSetConfig(ComHandle:thnd;Config:DCB):Boolean;

function ComGetConfig(ComHandle:thnd;var Config:DCB):Boolean;

function ComSetTimeOuts(ComHandle:thnd;IntervalRead,TotalRead:unsigned32;TotalWrite:unsigned32):Boolean;

function ComWrite (ComHandle:thnd;Data:Pointer;    Size:unsigned32;ClearBuff:Boolean=FALSE):unsigned32;

function ComRead  (ComHandle:thnd;Data:Pointer;var Size:unsigned32;ClearBuff:Boolean=FALSE):Boolean;

function ComClose(ComHandle:thnd):Boolean;

function ComInFlush(ComHandle:thnd):Boolean;

function ComOutFlush(ComHandle:thnd):Boolean;


//Diverses
//ESC-Zeichen in eine String wandeln, der das Kürzel wiederspiegelt
function Esc2Str(Input:Char):String;

////////////////////////////////////////////////////////////////////////////////
///Nützliche Konstanten
////////////////////////////////////////////////////////////////////////////////
Const
     RxBufferSize = 256;
     TxBufferSize = 256;

     NULL= $00;       // NUll
     SOH = $01;       // Start of Headin
     STX = $02;       // Start of Text
     ETX = $03;       // End of Text
     EOT = $04;       // End of Transmission
     ENQ = $05;       // Enquiry
     ACK = $06;       // Acknowledge
     BEL = $07;       // Bell
     BS  = $08;       // Backspace
     TAB = $09;       // Horizontal Tab
     LF  = $0a;       // Linefeed
     VT  = $0b;       // Vertical Tab
     FF  = $0c;       // Next page (Formfeed)
     CR  = $0d;       // Carriage Return
     SO  = $0e;       // Shift out;
     SI  = $0f;       // Shift in;
     DLE = $10;       // Datalink escape
     XON = $11;       // Devicecontrole 1
     DC2 = $12;       // Devicecontrole 2
     XOff= $13;       // Devicecontrole 3
     DC4 = $14;       // Devicecontrole 4
     NAK = $15;       // Negative Acknowledge
     SYN = $16;       // Synchronous Idle
     ETB = $17;       // End of Transfer Block
     CAN = $18;       // Cancel
     EM  = $19;       // End of Medium
     SUB = $1a;       // Substitude
     ESC = $1b;       // Escape
     FS  = $1c;       // File separator
     GS  = $1d;       // Group separator
     RS  = $1e;       // Record Separator
     US  = $1f;       // Unit Separator

     //Comport-Flags
     dcb_Binary           = $00000001;
     dcb_Parity           = $00000002;
     dcb_OutxCTSFlow      = $00000004;
     dcb_OutxDSRFlow      = $00000008;
     dcb_DTRControl       = $00000030;
     dcb_DSRSensivity     = $00000040;
     dcb_TxContinueOnXoff = $00000080;
     dcb_OutX             = $00000100;
     dcb_InX              = $00000200;
     dcb_ErrorChar        = $00000400;
     dcb_Null             = $00000800;
     dcb_RTSControl       = $00003000;
     dcb_AbortOnError     = $00004000;

{
     //Speed ist definiert als
     CBR_110
     CBR_300
     CBR_600
     CBR_1200
     CBR_2400
     CBR_4800
     CBR_9600
     CBR_14400
     CBR_19200
     CBR_38400
     CBR_56000
     CBR_57600
     CBR_115200
     CBR_128000
     CBR_256000

     //Parity ist definiert als
     NONEPARITY
     EVENPARITY
     ODDPARITY

     //Stopbits ist definiert als
     ONESTOPBIT
     ONE5STOPBITS
     TWOSTOPBITS

     //Weitere Konstanten
     RTS_CONTROL_DISABLE
     RTS_CONTROL_ENABLE
     RTS_CONTROL_HANDSHAKE
     RTS_CONTROL_TOGGLE
     DTR_CONTROL_DISABLE
     DTR_CONTROL_ENABLE
     DTR_CONTROL_HANDSHAKE
}
implementation
uses Registry,SysUtils;
////////////////////////////////////////////////////////////////////////////////
/// Alle verfügbaren Comports anzeigen
////////////////////////////////////////////////////////////////////////////////
function ComPortsEnumerateActive():TStringList;
var
   hComHandle : thnd;
   sTemp      : TStringList;
begin
     Result:=TStringList.Create;
     sTemp:=ComPortsEnumerate;

     //ComPorts durchchecken
     //Einfach alle auf die harte Tour öffnen und die Rückgabe testen.
     //Dadurch erhalte ich alle "verfügbaren" COMPorts, nicht alle installierten
     while (sTemp.Count>0) do
           begin
                //ComPort öffnen
                hComHandle:=CreateFile(PChar(sTemp[0]),GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
                //Handle online ?
                if (hComHandle<>INVALID_HANDLE_VALUE) then
                   begin
                        Result.Add(sTemp[0]);
                        CloseHandle(hComHandle);
                   end;
                sTemp.Delete(0);
         end;
end;


////////////////////////////////////////////////////////////////////////////////
/// Alle Comports enumerieren
////////////////////////////////////////////////////////////////////////////////
function ComportsEnumerate():TStringList;
var
  reg   : TRegistry;
  sTemp : TStringList;
begin
      reg := TRegistry.Create;
      Result := TStringList.Create;
      sTemp  := TStringList.Create;

      try
         reg.RootKey := HKEY_LOCAL_MACHINE;
         reg.OpenKey('hardware\devicemap\serialcomm', False);
         reg.GetValueNames(sTemp);

         //Und nun alle Werte lesen
         while (sTemp.Count>0) do
               begin
                    Result.Add(Reg.ReadString(sTemp[0]));
                    sTemp.Delete(0);
               end;
      finally
         reg.CloseKey;
         sTemp.Free;
         reg.Free;
      end;
end;


////////////////////////////////////////////////////////////////////////////////
/// Configurationsdialog für den gewünschten Comport öffnen
////////////////////////////////////////////////////////////////////////////////
function ComPortConfig(sComPort:string):Boolean;
var
   hComHandle : thnd;
   Buffer    :  TCommConfig;
   cSize      : unsigned32;
begin
     cSize   :=0;
     Result  :=FALSE;
     //Comport öffnen
     hComHandle:=CreateFile(PChar(sComPort),GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

     //Handle online ?
     if (hComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Konfiguration ziehen
             GetCommConfig(hComHandle,Buffer,Cardinal(cSize));

             //Und jetzt die Daten wirklich holen
             GetCommConfig(hComHandle,Buffer,Cardinal(cSize));

             //KonfigFenster machen
             if (CommConfigDialog(PChar(sComPort),0,Buffer)=TRUE) then
                begin
                     //Alles gut gelaufen, dan ComPort setzen
                     Result:=SetCommConfig(hComHandle,Buffer,cSize);
                end;

             //Und Handle schließen
             CloseHandle(hComHandle);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Comport Einstellungen lesen
////////////////////////////////////////////////////////////////////////////////
function ComPortGetConfig(sComPort:string;var Config:TCommConfig):Boolean;
var
   hComHandle : thnd;
   cSize      : unsigned32;
begin
     cSize   :=0;
     Result  :=FALSE;
     //Comport öffnen
     hComHandle:=CreateFile(PChar(sComPort),GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

     //Handle online ?
     if (hComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Konfiguration ziehen
             GetCommConfig(hComHandle,Config,Cardinal(cSize));

             //Und jetzt die Daten wirklich holen
             Result:=GetCommConfig(hComHandle,Config,Cardinal(cSize));

             //Und Handle schließen
             CloseHandle(hComHandle);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Konfigurationsblock direkt schreiben
////////////////////////////////////////////////////////////////////////////////
function ComPortSetConfig(sComPort:string;Config:TCommConfig):Boolean;
var
   hComHandle : thnd;
   cSize      : unsigned32;
begin
     Result  :=FALSE;
     //Comport öffnen
     hComHandle:=CreateFile(PChar(sComPort),GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

     //Handle online ?
     if (hComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Größe setzen
             cSize:=Sizeof(Config);
             Config.dwSize:=cSize;

             //Alles gut gelaufen, dan ComPort setzen
             Result:=SetCommConfig(hComHandle,Config,cSize);

             //Und Handle schließen
             CloseHandle(hComHandle);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
///Comport Timeouts setzen
////////////////////////////////////////////////////////////////////////////////
function ComPortTimeOuts(Comport:String;IntervalRead,TotalRead:unsigned32;TotalWrite:unsigned32):Boolean;
var
   hComHandle : thnd;
   pBuffer    : pCommTimeOuts;
begin
     Result  :=FALSE;
     //Comport öffnen
     hComHandle:=CreateFile(PChar(ComPort),GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

     //Handle online ?
     if (hComHandle<>INVALID_HANDLE_VALUE) then
        begin
             GetMem(pBuffer,SizeOf(TCommTimeOuts));
             if (GetCommTimeOuts(hComHandle,pBuffer^)) then
                begin
                     //Maximale Zeit zwischen zwei gelesenen Bytes (ms)
                     pBuffer.ReadIntervalTimeout         :=IntervalRead;
                     //Multiplikator, um den max. Timeout zu bestimmen Max:=Size*TimeoutMultiplier
                     pBuffer.ReadTotalTimeoutMultiplier  :=TotalRead;
                     //Gesamtes Timeout (Wird auf Max-Timeout addiert
                     pBuffer.ReadTotalTimeoutConstant    :=TotalRead;

                     //Identisch mit Read
                     pBuffer.WriteTotalTimeoutMultiplier :=TotalWrite;
                     //Gesamtes Timeout
                     pBuffer.WriteTotalTimeoutConstant   :=TotalWrite;

                     //Und setzen
                     Result:=SetCommTimeOuts(hComHandle,pBuffer^);
                end;
             FreeMem(pBuffer);
             CloseHandle(hComHandle);
        end;
end;
////////////////////////////////////////////////////////////////////////////////
/// Auf den Comport schreiben
////////////////////////////////////////////////////////////////////////////////
function ComportWriteRaw(Comport:String;Data:Pointer;Size:unsigned32;ClearBuff:Boolean=FALSE):Boolean;
var
   hComHandle : thnd;
   pBuffer    : PByte;
   cSize      : unsigned32;
begin
     Result  :=FALSE;
     //Comport öffnen
     hComHandle:=CreateFile(PChar(ComPort),GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

     //Zeiger übernehmen
     pBuffer:=Data;

     //Handle online ?
     if (hComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Reinschreiben
             WriteFile(hComHandle,pBuffer^,Size,Cardinal(cSize),nil);

             //Bufferpurge angefordert ?
             if (ClearBuff) then PurgeComm(hComHandle,PURGE_TXCLEAR);

             //Alles geschrieben ?
             if (Size=cSize) then Result:=TRUE;

             //Fertig
             CloseHandle(hComHandle);
        end;
end;



////////////////////////////////////////////////////////////////////////////////
/// Vom den Comport lesen
////////////////////////////////////////////////////////////////////////////////
function ComportReadRaw(Comport:String;Data:Pointer;var Size:unsigned32;ClearBuff:Boolean=FALSE):Boolean;
var
   hComHandle : thnd;
   pBuffer    : PByte;
   cSize      : unsigned32;

begin
     Result  :=FALSE;
     //Comport öffnen
     hComHandle:=CreateFile(PChar(ComPort),GENERIC_READ,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

     pBuffer:=Data;

     //Handle online ?
     if (hComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Daten lesen
             ReadFile(hComHandle,pBuffer^,Size,Cardinal(cSize),nil);

             //Kein Fehler und alles gelesen ?
             if (GetLastError()=NO_ERROR) and (cSize=Size) then Result:=TRUE;

             //Bufferpurge angefordert ?
             if (ClearBuff) then PurgeComm(hComHandle,PURGE_RXCLEAR);

             //Und gelesene Menge zurückmelden
             Size:=cSize;

             //Fertig
             CloseHandle(hComHandle);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
/// Schreiben und auf eine Antwort warten
////////////////////////////////////////////////////////////////////////////////
function ComportPollRaw(Comport:String;OutData:Pointer;OutSize:unsigned32;InData:Pointer;var InSize:unsigned32;ClearBuff:Boolean=FALSE):Boolean;
var
   hComHandle : thnd;
   pInBuffer  : PByte;
   pOutBuffer : PByte;
   cSize      : unsigned32;

begin
     Result  :=FALSE;
     //Comport öffnen
     hComHandle:=CreateFile(PChar(ComPort),GENERIC_READ or GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

     pOutBuffer :=OutData;
     pInBuffer  :=InData;

     //Handle online ?
     if (hComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Bufferpurge angefordert ?
             if (ClearBuff) then PurgeComm(hComHandle,PURGE_TXCLEAR);

             //Daten schreiben
             WriteFile(hComHandle,pOutBuffer^,OutSize,Cardinal(cSize),nil);

             //Daten lesen
             ReadFile(hComHandle,pInBuffer^,InSize,Cardinal(cSize),nil);

             //Kein Fehler und alles gelesen ?
             if (GetLastError()=NO_ERROR) and (cSize=InSize) then Result:=TRUE;

             //Bufferpurge angefordert ?
             if (ClearBuff) then PurgeComm(hComHandle,PURGE_RXCLEAR);

             //Und gelesene Menge zurückmelden
             InSize:=cSize;

             //Fertig
             CloseHandle(hComHandle);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
function ComOpen (ComPort:String):thnd;
begin
     //Comport öffnen
     Result:=CreateFile(PChar(ComPort),GENERIC_READ or GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_WRITE_THROUGH,0);
//     Result:=CreateFile(PChar(ComPort),GENERIC_READ or GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
end;


////////////////////////////////////////////////////////////////////////////////
function ComClear(ComHandle:thnd):Boolean;
begin
     result := PurgeComm(ComHandle,PURGE_RXCLEAR) and
               PurgeComm(ComHandle,PURGE_TXCLEAR);
end;
////////////////////////////////////////////////////////////////////////////////
function ComSetConfig(ComHandle:thnd;Config:DCB):Boolean;
begin
     Result:=FALSE;
     Config.DCBlength:=SizeOf(Config);
     if (ComHandle <> INVALID_HANDLE_VALUE) then
        begin
             Result:=SetCommState(ComHandle,Config);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function ComGetConfig(ComHandle:thnd;var Config:DCB):Boolean;
begin
     Result:=FALSE;
     if (ComHandle <> INVALID_HANDLE_VALUE) then
        begin
             Result:=GetCommState(ComHandle,Config);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function ComSetTimeOuts(ComHandle:thnd;IntervalRead,TotalRead:unsigned32;TotalWrite:unsigned32):Boolean;
var
   Buffer    : CommTimeOuts;
begin
     Result  :=FALSE;

     //Handle online ?
     if (ComHandle<>INVALID_HANDLE_VALUE) then
        begin
             if (GetCommTimeOuts(ComHandle,Buffer)) then
                begin
                     //Maximale Zeit zwischen zwei gelesenen Bytes (ms)
                     Buffer.ReadIntervalTimeout         :=IntervalRead;
                     //Multiplikator, um den max. Timeout zu bestimmen Max:=Size*TimeoutMultiplier
                     Buffer.ReadTotalTimeoutMultiplier  :=0;
                     //Gesamtes Timeout (Wird auf Max-Timeout addiert
                     Buffer.ReadTotalTimeoutConstant    :=TotalRead;

                     //Identisch mit Read
                     Buffer.WriteTotalTimeoutMultiplier :=0;
                     //Gesamtes Timeout
                     Buffer.WriteTotalTimeoutConstant   :=TotalWrite;

                     //Und setzen
                     Result:=SetCommTimeOuts(ComHandle,Buffer);
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function ComWrite (ComHandle:thnd;Data:Pointer;Size:unsigned32;ClearBuff:Boolean=FALSE):unsigned32;
var
   pBuffer : PByte;
   cSize      : unsigned32;
begin
     pBuffer :=Data;
     result  :=0;
     //Handle online ?
     if (ComHandle<>INVALID_HANDLE_VALUE) then
        begin
             WriteFile(ComHandle,pBuffer^,Size,Cardinal(cSize),nil);
             FlushFileBuffers(ComHandle);

             //Bufferpurge angefordert ?
             if (ClearBuff) then PurgeComm(ComHandle,PURGE_RXCLEAR);

             //Und gelesene Menge zurückmelden
             Result:=cSize;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function ComRead (ComHandle:thnd;Data:Pointer;var Size:unsigned32;ClearBuff:Boolean=FALSE):Boolean;
var
   pBuffer : PByte;
   cSize      : unsigned32;
begin
     Result  :=FALSE;
     pBuffer :=Data;

     //Handle online ?
     if (ComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Daten lesen
             cSize:=0;
             ReadFile(ComHandle,pBuffer^,Size,Cardinal(cSize),nil);

             //Bufferpurge angefordert ?
             if (ClearBuff) then PurgeComm(ComHandle,PURGE_TXCLEAR);

             //Fehler ?
             if (Size = cSize) then Result:=TRUE;
             Size:=cSize
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function ComClose(ComHandle:thnd):Boolean;
begin
     Result:=FALSE;
     if (ComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Fertig
             result:=CloseHandle(ComHandle);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
function ComInFlush(ComHandle:thnd):Boolean;
begin
     Result:=FALSE;
     //Handle online ?
     if (ComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Bufferpurge angefordert ?
             PurgeComm(ComHandle,PURGE_RXABORT);
             PurgeComm(ComHandle,PURGE_RXCLEAR);
             Result:=TRUE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function ComOutFlush(ComHandle:thnd):Boolean;
begin
     Result:=FALSE;
     //Handle online ?
     if (ComHandle<>INVALID_HANDLE_VALUE) then
        begin
             //Bufferpurge angefordert ?
             PurgeComm(ComHandle,PURGE_TXABORT);
             PurgeComm(ComHandle,PURGE_TXCLEAR);
             Result:=TRUE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Diverse sonstige Funktionen
////////////////////////////////////////////////////////////////////////////////
function Esc2Str(Input:Char):String;
begin
     Result:='';
     case (Ord(Input)) of
          NULL : Result:= '<NULL>';               // NUll
          SOH  : Result:= '<SOH>';                // Start of Headin
          STX  : Result:= '<STX>';                // Start of Text
          ETX  : Result:= '<ETX>';                // End of Text
          EOT  : Result:= '<EOT>';                // End of Transmission
          ENQ  : Result:= '<ENQ>';                // Enquiry
          ACK  : Result:= '<ACK>';                // Acknowledge
          BEL  : Result:= '<BEL>';                // Bell
          BS   : Result:= '<BS>';                 // Backspace
          TAB  : Result:= '<TAB>';                // Horizontal Tab
          LF   : Result:= '<LF>';                 // Linefeed
          VT   : Result:= '<VT>';                 // Vertical Tab
          FF   : Result:= '<FF>';                 // Next page (Formfeed)
          CR   : Result:= '<CR>';                 // Carriage Return
          SO   : Result:= '<SO>';                 // Shift out;
          SI   : Result:= '<SI>';                 // Shift in;
          DLE  : Result:= '<DLE>';                // Datalink escape
          XON  : Result:= '<XON>';                // Devicecontrole 1
          DC2  : Result:= '<DC2>';                // Devicecontrole 2
          XOff : Result:= '<XOff>';               // Devicecontrole 3
          DC4  : Result:= '<DC4>';                // Devicecontrole 4
          NAK  : Result:= '<NAK>';                // Negative Acknowledge
          SYN  : Result:= '<SYN>';                // Synchronous Idle
          ETB  : Result:= '<ETB>';                // End of Transfer Block
          CAN  : Result:= '<CAN>';                // Cancel
          EM   : Result:= '<EM>';                 // End of Medium
          SUB  : Result:= '<SUB>';                // Substitude
          ESC  : Result:= '<ESC>';                // Escape
          FS   : Result:= '<FS>';                 // File separator
          GS   : Result:= '<GS>';                 // Group separator
          RS   : Result:= '<RS>';                 // Record Separator
          US   : Result:= '<US>';                 // Unit Separator
     else
          Result:=Input;
    end;
end;


end.
