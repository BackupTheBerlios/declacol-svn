unit class_comport;
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
/// Comportklasse die Threadbasiert einzelne Bytes aus dem Comport liest
///
///
///
////////////////////////////////////////////////////////////////////////////////
interface

uses unit_typedefs,unit_comport,classes,sysutils,windows,class_highperftimer;

const
     //Windowsinterner ReceiveBuffer
     MAX_BUFFER = 256;

//Struktur der Workflow-Flags
type pFlags = ^TFlags;
     TFlags = record
     //Index eine neuen Datensatzes oder FLAG_NONE
     NewData : unsigned32;
end;

//Struktur eines Comport-Paketes
//packed ist zwar langsamer, das Speichern wird aber sehr vereinfacht
type pComPacket = ^TComPacket;
     TComPacket = packed record
     Byte       : unsigned8;
     Timestamp  : unsigned64;
end;

//Struktur mit allen Daten zusammen
type pDataPack = ^TDatapack;
     TDatapack = record
     Data      : TComPacket;
     //Flags     : TFlags;
end;

//Callback bei Empfang
//Gibt den Index um Buffer an, der eingelesen wurde.
//Es können aber auch mehrere Bytes eingelesen worden sein
TComportCallback = procedure(BufferIndex : unsigned64) of Object;


type TThreadedComPort = Class(TThread)
     private
           //Handle zum offenen Comport
           hComport : THandle;
           //Alle gesammelten Daten
           aData    : TList;
           //Name des ComPorts
           sComport : Longstring;

           //TimeOutStructur
           rTimeout : COMMTIMEOUTS;
           rConfig  : COMMCONFIG;

           //Callback-Funktionen
           fReceive : TComportCallback;

           bOnline  : Boolean;
           bDoIt    : Boolean;
           bBusy    : Boolean;

           rTimer   : THighPerfTimer;

           u64LastGet   : unsigned64;

           u32Error     : unsigned32;

           function adddata(Data : TDatapack):unsigned64;
           function getdata(Index : unsigned64; var Data : TDataPack):Boolean;

           procedure SetComport(Value:longstring);
           function  checkport(Name : longstring):boolean;
           procedure setportdefaults();
           procedure execute(); override;

           procedure seterror(ErrorCode:unsigned32);
     public
           constructor create();
           function  open(Comport:longstring=''):Boolean;
           procedure close();
           procedure clearinput();
           procedure clearoutput();

           //Einlesen Starten und Beenden
           procedure Start();
           procedure Stop();

           //Auf die gelesenen Daten zugreifen
           function  shift(var Data:TComPacket):Boolean;
           function  get  (index : unsigned64; var Data : TComPacket):Boolean;
           function  pop  (var Data:TComPacket):Boolean;
           function  count():unsigned64;
           procedure clear();

           //Datenbereiche suchen
           function getfirst(var Data:TComPacket):Boolean;
           function getnext (var Data:TComPacket):Boolean;
           function getlast (var Data:TComPacket):Boolean;

           function findbytime(Timestamp : unsigned64; var Data:TComPacket):Boolean;

           //Comport konfigurieren
           procedure showconfigdialog();
           procedure applyconfig();

           function setconfig(configstring : longstring):boolean;
           function getconfig():longstring;

           //Name des Comports
           property Comport     : Longstring  read sComport write SetComport;
           //Haben wir eine Verbindung ?
           property Online      : Boolean     read bOnline;
           //Comport-Konfiguration
           property Config      : TCommconfig read rConfig write rConfig;
           //Ist der Lesethread aktiv ?
           property Busy        : boolean     read bBusy;
           //Fehlerzustand
           property Error       : unsigned32 read u32Error;
           //Timerzugriff
           property Timer       : THighPerfTimer read rTimer;
           //OnReceiveCallback
           property OnReceive   : TComportCallback read fReceive write fReceive;

end;


implementation

////////////////////////////////////////////////////////////////////////////////
constructor TThreadedComport.create();

begin
     //PerformanceCounter initialisieren
     Self.Priority:=tpTimeCritical;
     Self.rTimer:=THighPerfTimer.Create();
     Self.rTimer.Start(TRUE);
     Self.Priority:=tpNormal;

     Self.aData:=TList.Create();

     //MainThread anhalten
     Self.bDoIt:=FALSE;
     Self.bBusy:=FALSE;
     Self.hComport:=INVALID_HANDLE_VALUE;
     Self.u64LastGet:=0;

     //Nicht Suspended starten
     inherited create(FALSE);
     Self.FreeOnTerminate:=TRUE;         

     //Standardkonfiguration laden
     Self.SetPortDefaults();

     //Defaulttimeouts
     rTimeout.ReadIntervalTimeout:=MAXDWORD;
     rTimeout.ReadTotalTimeoutMultiplier:=MAXDWORD;
     rTimeout.ReadTotalTimeoutConstant:=1;
     rTimeout.ReadTotalTimeoutConstant:=1;
     rTimeout.WriteTotalTimeoutMultiplier:=1;
     rTimeout.WriteTotalTimeoutConstant:=1;

     Self.SetError(NO_ERROR);
end;

procedure TThreadedComPort.setportdefaults();
var
   u32Size : unsigned32;
   sName   : longstring;
   Config  : TCommConfig;
begin
     //Einen Namen erzwingen
     if (Self.sComport ='') then
        begin
             sName:='COM1';
        end
     else
        begin
             sName:=Self.sComport;
        end;

     u32Size:=0;
     GetDefaultCommConfig(PChar(sName),Config,cardinal(u32Size));
     if (GetDefaultCommConfig(PChar(sName),Config,cardinal(u32Size))) then
        begin
             //Längen setzen
             Config.DCB.DCBlength:=SizeOf(Config.DCB);
             Config.dwSize:=SizeOf(Config);
             //Und abspeichern
             Self.rConfig:=Config;
             Self.ApplyConfig();
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TThreadedComPort.open(Comport:longstring=''):boolean;
begin
     result:=FALSE;

     //Name als Property übernehmen
     if (Comport <> '') then
        begin
             Self.sComport:=Comport;
        end;

     Self.Close();
     Self.hComport:=CreateFile ( PChar(Self.sComport),
                                 GENERIC_READ or GENERIC_WRITE,
                                 0, //No Share
                                 nil,
                                 OPEN_EXISTING,
                                 FILE_FLAG_NO_BUFFERING,
                                 NULL);
                                 
     if (Self.hComport <> INVALID_HANDLE_VALUE) then
        begin
             Self.bOnline:=TRUE;

             Self.ApplyConfig();
             Self.ClearInput();
             Self.ClearOutput();
             result:=TRUE;
        end
     else
        begin
             Self.bOnline:=FALSE;
        end;
     Self.u32Error:=GetLastError();
end;

////////////////////////////////////////////////////////////////////////////////
procedure TThreadedComPort.close();
begin
     if (Self.hComport <> INVALID_HANDLE_VALUE) then
        begin
             CloseHandle(Self.hComport);
             Self.hComport:=INVALID_HANDLE_VALUE;
        end;
     Self.u32Error:=GetLastError();
end;

////////////////////////////////////////////////////////////////////////////////
//Alle Eingabepuffer löschen und weitere Ausgaben abbrechen
procedure TThreadedComPort.ClearInput();
begin
     //Buffer löschen
     if (Self.hComport <> INVALID_HANDLE_VALUE) then
        begin
             PurgeComm(Self.hComport,PURGE_TXCLEAR or PURGE_TXABORT);
             Self.SetError(GetLastError());
        end
     else
        begin
             Self.SetError(ERROR_IO_DEVICE);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Alle Ausgabepuffer löschen und weitere Ausgaben abbrechen
procedure TThreadedComPort.ClearOutput();
begin
     //Buffer löschen
     if (Self.hComport <> INVALID_HANDLE_VALUE) then
        begin
             PurgeComm(Self.hComport,PURGE_RXCLEAR or PURGE_RXABORT);
             Self.SetError(GetLastError());
        end
     else
        begin
             Self.SetError(ERROR_IO_DEVICE);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Datensatz holen
function TThreadedComport.getdata(Index : unsigned64; var Data : TDatapack):boolean;
begin
     if (Index < unsigned64(Self.aData.Count)) then
        begin
             Data:=pDataPack(Self.aData[Index])^;
             //letzten Datensatzindex merken
             Self.u64LastGet:=Index;
             result:=TRUE;
             Self.SetError(NO_ERROR);
        end
     else
        begin
             result:=FALSE;
             Self.SetError(ERROR_INVALID_INDEX);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Datensatz zufügen
function TThreadedComport.adddata(Data : TDatapack):unsigned64;
var
   pData : pDatapack;
begin
     pData := new (pDatapack);
     pData^:=Data;
     result:=Self.aData.Add(pData);
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Datensatz holen
function  TThreadedComPort.get  (index : unsigned64; var Data : TComPacket):Boolean;
begin
     if (index < unsigned64(Self.aData.Count)) then
        begin
             data:=pComPacket(Self.aData[Index])^;
             Self.u64LastGet:=Index;
             Self.SetError(NO_ERROR);
             result:=TRUE;
        end
     else
        begin
             Self.SetError(ERROR_INVALID_INDEX);
             result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Datensätze zählen
function TThreadedComport.Count():unsigned64;
begin
     result:=unsigned64(Self.aData.Count);
     Self.SetError(NO_ERROR);
end;

////////////////////////////////////////////////////////////////////////////////
//Ältesten Eintrag holen
function TThreadedComport.shift(var Data :TComPacket):Boolean;
var
   MyData : TDataPack;
begin
     result:=Self.GetData(0,MyData);
     if (Result) then
        begin
             //Eintrag entfernen
             Dispose(Self.aData[0]);
             Self.aData.Delete(0);
             //Nur die ComPortDaten zurückgeben
             Data:=MyData.Data;
             Self.SetError(NO_ERROR);
        end
     else
        begin
             Self.SetError(ERROR_NO_DATA);
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Neuesten Eintrag holen
function TThreadedComport.pop(var Data :TComPacket):Boolean;
var
   u32Size : unsigned32;
   MyData : TDataPack;
begin
     Self.SetError(ERROR_NO_DATA);

     result:=FALSE;
     u32Size:=Self.aData.Count;
     if (u32Size > 0) then
        begin
             dec(u32Size);
             result:=Self.GetData(u32Size,MyData);
             if (Result) then
                begin
                     //Eintrag entfernen
                     Dispose(Self.aData[u32Size]);
                     Self.aData.Delete(u32Size);

                     //Nur die ComPortDaten zurückgeben
                     Data:=MyData.Data;

                     Self.SetError(NO_ERROR);
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TThreadedComport.getfirst(var Data:TComPacket):Boolean;
begin
     result:=Self.get(0,Data);
     if (result = FALSE) then
        begin
             Self.SetError(ERROR_NO_DATA);
        end;
     Self.u64LastGet:=0;
end;

////////////////////////////////////////////////////////////////////////////////
function TThreadedComport.getnext (var Data:TComPacket):Boolean;
begin
     inc(Self.u64LastGet);
     result:=Self.get(Self.u64LastGet,Data);
     if (result = FALSE) then
        begin
             Self.SetError(ERROR_NO_MORE_ITEMS);
        end;
     result:=Self.get(Self.u64LastGet,Data);
end;

////////////////////////////////////////////////////////////////////////////////
function TThreadedComport.getlast (var Data:TComPacket):Boolean;
begin
     if (Self.aData.Count > 0) then
        begin
             Self.u64LastGet:=Self.aData.Count - 1;
             result:=Self.Get(Self.u64LastGet,Data);
        end
     else
        begin
             Self.SetError(ERROR_NO_DATA);
             result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TThreadedComport.findbytime(Timestamp : unsigned64; var Data:TComPacket):Boolean;
var
   u64Index : unsigned64;
begin
     Self.seterror(ERROR_NO_DATA);
     result:=FALSE;
     u64Index:=0;
     while (u64Index < unsigned64(Self.aData.Count)) do
           begin
                //Timestamp OK ?
                if ( pDataPack(Self.aData[u64Index])^.Data.Timestamp = TimeStamp) then
                   begin
                        //Daten übernehmen
                        Data:=pDataPack(Self.aData[u64Index])^.Data;
                        //Und last get setzen
                        Self.u64LastGet:=u64Index;
                        Self.SetError(NO_ERROR);
                        result:=TRUE;
                        u64Index:=unsigned64(Self.aData.Count);
                   end
                else
                   begin
                        //Da die Zeit ansteigt und damit die Liste IMMER sortiert ist,
                        //können wir einen Einfachen Abbruch definieren
                        if (pDataPack(Self.aData[u64Index])^.Data.Timestamp > Timestamp) then
                           begin
                                u64Index:=unsigned64(Self.aData.Count);
                           end;
                   end;

                inc(u64Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Alle gelesenen Daten löschen
procedure TThreadedComport.clear();
begin
     if (not Self.bDoIt) then
        begin
             Self.SetError(NO_ERROR);
             while (self.aData.Count > 0) do
                   begin
                        Dispose(Self.aData[0]);
                        Self.aData.Delete(0);
                   end;
        end
     else
        begin
             Self.SetError(ERROR_BUSY);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Auslese starten
procedure TThreadedComport.Start();
begin
     if (Self.hComport <> INVALID_HANDLE_VALUE) then
        begin
             Self.applyconfig();
             Self.clearinput();
             Self.clearoutput();
             Self.bDoIt:=TRUE;
             Self.bBusy:=TRUE;

             if (Self.Suspended) then
                begin
                     Self.Resume();
                end;

             Self.SetError(NO_ERROR);
        end
     else
        begin
             Self.SetError(ERROR_BAD_DEVICE);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Auslese anhalten
procedure TThreadedComport.Stop();
begin
     Self.bDoIt:=FALSE;
     Self.SetError(NO_ERROR);
end;

////////////////////////////////////////////////////////////////////////////////
//Den Comport setzen
procedure TThreadedComport.SetComport(Value:longstring);
begin
     if (Self.checkport(Value)) then
        begin
             Self.sComport:=Value;
             Self.SetError(NO_ERROR);
        end
     else
        begin
             Self.SetError(ERROR_BAD_DEVICE);
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Konfiguration öffnen (Nur wenn wir keinen Connect haben)
procedure TThreadedComport.showconfigdialog();
var
   Buffer  : TCommConfig;
   u32Size : unsigned32;
   hPort   : THandle;
begin
     Self.SetError(ERROR_CAN_NOT_COMPLETE);

     //Öffnen wenn wir nicht online sind
     if (not Self.bOnline) then
        begin
             //Comport öffnen
             hPort:=CreateFile(PChar(sComPort),GENERIC_WRITE,0,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
        end
     else
        begin
             hPort:=Self.hComport;
        end;

     //Handle online aber Auslese nicht aktiv ?
     if (hPort<>INVALID_HANDLE_VALUE) and (not Self.bDoIt) then
        begin
             //Konfiguration ziehen
             u32Size:=0;
             GetCommConfig(hPort,Buffer,Cardinal(u32Size));

             //Und jetzt die Daten wirklich holen
             GetCommConfig(hPort,Buffer,Cardinal(u32Size));

             //KonfigFenster machen
             if (CommConfigDialog(PChar(Self.sComport),0,Buffer)=TRUE) then
                begin
                     //Alles gut gelaufen, dann ComPort setzen
                     SetCommConfig(hPort,Buffer,SizeOf(Buffer));
                     Self.SetError(NO_ERROR);
                end;
        end;

     //Handle wieder schließen
     if (not Self.bOnline) then
        begin
             CloseHandle(hPort);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Konfiguration anwenden
procedure TThreadedComport.applyconfig();
var
   u32Size : unsigned32;
   bTest   : boolean;
begin
     if (Self.bOnline) and (not Self.bDoIt) then
        begin
             //Größe setzen
             u32Size:=Sizeof(Self.rConfig);
             Self.rConfig.dwSize:=u32Size;
             Self.rConfig.dcb.DCBlength:=SizeOf(Self.rConfig.dcb);

             //Alles gut gelaufen, dann ComPort setzen
             bTest:=SetCommConfig(Self.hComport,Self.rConfig,u32Size);

             //Und die Timeouts setzen
             bTest:=bTest AND SetCommTimeouts(Self.hComport,Self.rTimeout);

             //Buffer
             bTest:=bTest AND SetupComm(Self.hComport,MAX_BUFFER,MAX_BUFFER);

             if (bTest) then
             Self.SetError(GetLastError());
        end
     else
        begin
             Self.SetError(ERROR_IO_DEVICE);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Hauptschleife
//Hier wird so schnell es geht der Puffer des Comports gelesen und
//abgelegt. Die Verarbeitung ist dann kein Problem mehr
procedure TThreadedComport.Execute();
var
   rData    : TDataPack;
   u32Size  : unsigned32;
   u32Read  : unsigned32;
begin
//     Self.Priority:=tpHighest;

     while (not Self.Terminated) do
           begin
                if (Self.bDoIt) then
                   begin
                        u32Size:=SizeOf(unsigned8);
                        ReadFile(Self.hComport,rData.Data.Byte,u32Size,Cardinal(u32Read),nil);
                        if (u32Read = u32Size) then
                           begin
                                rData.Data.Timestamp:=Self.rTimer.Timestamp;

                                //Mit Callback oder ohne
                                if (assigned(Self.fReceive)) then
                                   begin
                                        Self.fReceive( Self.adddata(rData) );
                                   end
                                else
                                   begin
                                        Self.adddata(rData);
                                   end;
                           end;
                   end
                else
                   begin
                        Self.bBusy:=FALSE;
                        Self.Suspend();
                   end;
           end;

     Self.Priority:=tpNormal;
     Self.Close();
end;




////////////////////////////////////////////////////////////////////////////////
//Die Konfiguration laden
function TThreadedComport.setconfig(configstring : longstring):boolean;
var
   pByte    : ^Byte;
   u32Count : unsigned32;
   u32Len   : unsigned32;
begin
     result:=FALSE;
     u32Len:=unsigned32(Length(configstring)) shr 1;
     if (u32Len = SizeOf(Self.rConfig)) then
        begin
             //Der Konfigstring ist einfach der kpl. Configbuffer als Hexwerte dargestellt
             pByte:=Addr(Self.rConfig);
             u32Count:=1;
             while (u32Count < ( unsigned32 ( Length(configstring) ) shr 1)) do
                   begin
                        pByte^:=StrToInt('$' + configstring[u32Count] + ConfigString[u32Count+1]);
                        inc(pByte);
                        inc(u32Count,2);
                   end;
             result:=TRUE;
        end;
     Self.SetError(NO_ERROR);
end;

////////////////////////////////////////////////////////////////////////////////
//Konfiguration des Ports lesen
function TThreadedComport.getconfig():longstring;
var
   pByte    : ^Byte;
   u32Count : unsigned32;
begin
     //Der Konfigstring ist einfach der kpl. Configbuffer als Hexwerte dargestellt
     result:='';
     pByte:=Addr(Self.rConfig);
     u32Count:=SizeOf(Self.rConfig);
     while (u32Count > 0) do
           begin
                Result:=Result + IntToHex(pByte^,2);
                inc(pByte);
                dec(u32Count);
           end;
     Self.SetError(NO_ERROR);
end;


////////////////////////////////////////////////////////////////////////////////
//Den Comport auf Zugriff prüfen
function TThreadedComport.checkport(Name : longstring):boolean;
var
   hPort : THandle;
begin
     result:=FALSE;
     hPort :=CreateFile ( PChar(Name),
                          GENERIC_READ or GENERIC_WRITE,
                          0, //No Share
                          nil,
                          OPEN_EXISTING,
                          0,
                          NULL);
     //Checken
     if (Self.hComport <> INVALID_HANDLE_VALUE) then
        begin
             CloseHandle(hPort);
             result:=TRUE;
        end;
end;


procedure TThreadedComPort.seterror(ErrorCode:unsigned32);
begin
     Self.u32Error:=ErrorCode;
end;

end.
