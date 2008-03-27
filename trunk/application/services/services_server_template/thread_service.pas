unit thread_service;
/////////////////////////////////////////////////////////////////////////////////
///
/// (c) 2007 Borg@Sven-of-Nine.de / http://www.sven-of-nine.de
///
/// Hier läuft der eigentlich Service ab
///
/// Alle Arbeiten des Services werden in der Methode Execute ausgeführt.
/// Um den Service steuerbar zu halten, MUSS Execute das Flag "terminated"
/// überwachen darauf reagieren.
///
/// Der Entropyserver nimmt einfach Systemereignisse auf und speichert sie in
/// einer S-Box ab. Mittels eines RC4-Stroms werden daraus Zufallsbytes erzeugt
/// Durch den Einfluß der normalen Betriebsparamter des PCs werden Wiederholungen
/// der Zufallszahlen ausgeschlossen
///
/////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,sysutils,classes,windows,class_threadedsocket;

/////////////////////////////////////////////////////////////////////////////////
//Hier definieren wir uns einen Thread der einzig zur Ausführung des
//services da ist.
const
     SBOX_SIZE      = 8192;


/////////////////////////////////////////////////////////////////////////////////
//Der Servicethread
type tsvcthread = class(tthread)
     private
           sBox      : Array[0..SBOX_SIZE - 1] of unsigned8;
           Server    : TThreadedServer;
     protected

           procedure Execute(); override;
           procedure Collect();
           function  GetByte():unsigned8;

           procedure ClientDispatcher(var Data:Longstring);

     public
           constructor Create();
           destructor  Free();

           property Byte : unsigned8 read GetByte;
end;

implementation

/////////////////////////////////////////////////////////////////////////////////
//Konstruktor
constructor tsvcthread.Create();
var
   u32Index : unsigned32;
begin
     //unseren Thread immer suspended startem
     Inherited Create(TRUE);

     //Server initialisieren
     Self.Server:=TThreadedServer.Create();
     Self.Server.Port:=23;
     Self.Server.BufferSize:=4;
     Self.Server.TimeOut:=60000;
     Self.Server.Name:='entropyserver 1.0';
     Self.Server.OnData:=Self.ClientDispatcher;

     //Mit relativem Zufall initialisieren
     randomize();
     for u32Index:=0 to (SBOX_SIZE - 1) do
         begin
              Self.sBox[u32Index]:=Random( High(unsigned8) + 1 );
         end;
end;

destructor  tsvcthread.Free();
begin
     inherited Free();
end;

/////////////////////////////////////////////////////////////////////////////////
//Main
procedure tsvcthread.Execute();
begin
     Server.Active:=TRUE;
     //In einer Endlosschleife einfach immer die SBox aktualisieren
     while (self.Terminated = FALSE) do
           begin
                Self.Collect();
                Sleep(Self.GetByte());
           end;

     Server.Active:=FALSE;
end;

/////////////////////////////////////////////////////////////////////////////////
//ein Byte aus dem Puffer lesen
function tsvcthread.GetByte():unsigned8;
var
   u32Index1 : unsigned32;
   u32Index2 : unsigned32;
   u8Temp    : unsigned8;
begin
     //Paar zufällig auswählen
     u32Index1:=random( SBOX_SIZE );
     u32Index2:=random( SBOX_SIZE );

     //Zwei Bytes in der Box tauschen
     u8Temp:=Self.sBox[u32Index1];
     Self.sBox[u32Index1]:=Self.sBox[u32Index2];
     Self.sBox[u32Index2]:=u8Temp;

     //Ergebnis bestimmen
     result:=( Self.sBox[u32Index1] +  Self.sBox[u32Index2] ) mod 256;
end;

/////////////////////////////////////////////////////////////////////////////////
//Ein Zufallsbyte holen und ablegen
procedure tsvcthread.Collect();
var
   u32Index  : unsigned32;
   u32Data   : unsigned32;
   rMemInfo  : MemoryStatus;
   rMousePos : TPoint;
begin
     //Den gewünschten Index in der SBox holen
     u32Index:=GetTickCount() mod SBOX_SIZE;

     //Aktuelle Mausposition holen
     GetCursorPos(rMousePos);

     //Aktuelle Speicherbelegung holen
     GlobalMemoryStatus(rMemInfo);

     //Daten verknüpfen
     u32Data:=unsigned32(rMousePos.x) xor unsigned32(rMousePos.y) xor
              rMemInfo.dwAvailPageFile xor rMemInfo.dwAvailPhys xor
              GetTickCount();

     //Und in die SBox legen
     //Mit XOR um ein versehentliches auffüllen mit Nullen zu vermeiden
     Self.sBox[u32Index]:=Self.sBox[u32Index] xor unsigned8(u32Data AND MASK_UNSIGNED8);
end;

/////////////////////////////////////////////////////////////////////////////////
//Ein Client möchte Daten haben
procedure tsvcthread.ClientDispatcher(var Data:Longstring);
var
   u32Size : unsigned32;
begin
     //Und die gewünschte Anzahl antworten
     u32Size:=StrToIntDef(Copy(Data,1,4),1);

     Data:=IntToStr(u32Size)+':';

     while (u32Size > 0) do
           begin
                Data:=Data + IntToHex(Self.Byte,2)+' ';
                dec(u32Size);
           end;
     Data:=Trim(Data) + #13 + #10;
end;

end.
