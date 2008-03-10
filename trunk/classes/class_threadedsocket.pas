unit class_threadedsocket;

interface
uses unit_typedefs,scktcomp,sysutils,windows;


const
     SERVER_TEXT_HI      = 'connected'+#13+#10;
     SERVER_TEXT_BYE     = 'disconnected'+#13+#10;
     SERVER_TEXT_TIMEOUT = 'timeout'+#13+#10;


/////////////////////////////////////////////////////////////////////////////////
//Die eigentliche Serverthread
//muß nur die ExecuteMethode überschreiben
//Wer initialisierung braucht natürllich auch noch Create und Destroy
type TServerCallBack = procedure(var Text:LongString) of Object;

type TThreadedServer = class(TObject)
     private
           u16Port      : unsigned16;
           sName        : Longstring;
           u32Timeout   : unsigned32;
           u32Buffer    : unsigned32;

           Server       : TServerSocket;
           OnRequest    : TServerCallBack;
     protected

           procedure ClientThreadInit(Sender:TObject;Socket:TServerClientWinSocket;var SocketThread:TServerClientThread);

           procedure SetPort(value:unsigned16);

           function  IsActive():Boolean;
           procedure DoActive(value:Boolean);
     public
           constructor Create();
           destructor  Free();
           procedure   Start();
           procedure   Stop();

           property    Active     : Boolean read IsActive write DoActive;
           property    Port       : unsigned16 read u16Port write SetPort;
           property    Timeout    : unsigned32 read u32Timeout write u32Timeout;
           property    BufferSize : unsigned32 read u32Buffer write u32Buffer;
           property    Name       : longstring read sName write sName;
           property    OnData     : TServerCallBack read OnRequest write OnRequest;
end;


//Thread zur Verarbeitung der Serveranfragen
type TEntropyServerThread = class(TServerClientThread)
     protected
           fSendData : TServerCallBack;
           u32Buffer : unsigned32;
           u32Timeout: unsigned32;

     public
           procedure ClientExecute(); override;
           property  OnRequest : TServerCallBack read fSendData write fSendData;
           property  BufferSize: unsigned32 read u32Buffer  write u32Buffer;
           property  Timeout   : unsigned32 read u32Timeout write u32Timeout;
end;


implementation
/////////////////////////////////////////////////////////////////////////////////
//Die Steuerklasse welche den Serverthread kontrolliert
constructor TThreadedServer.Create();
begin
     //Serversockets initialisieren
     Self.Server            := TServerSocket.Create(nil);
     Server.ServerType      := stThreadBlocking;
     Server.ThreadCacheSize := 32;
     Server.OnGetThread     := Self.ClientThreadInit;

     //Defaults setzen (wegen Lazarus)
     Self.Timeout:=5000;
     Self.Port:=100;
     Self.BufferSize:=8192;
     Self.Name:='server';
end;

//Alle bestehenden Threads beenden
destructor TThreadedServer.Free();
begin
     while (Self.Server.Socket.ActiveConnections > 0) do
           begin
                Self.Server.Socket.Connections[0].Close();
           end;
     inherited Free();
end;

//Server starten und beenden
procedure TThreadedServer.Start();
begin
     if (Self.Server.Active=FALSE) then
        begin
             Self.Server.Active:=TRUE;
        end;
end;

procedure TThreadedServer.Stop();
begin
     if (Self.Server.Active=TRUE) then
        begin
             Self.Server.Active:=FALSE;
        end;
end;

/////////////////////////////////////////////////////////////////////////////////
//ServerThread Init
procedure TThreadedServer.ClientThreadInit(Sender:TObject;Socket:TServerClientWinSocket;var SocketThread:TServerClientThread);
begin
     //Den Serverthread initialisieren
     SocketThread:=TEntropyServerThread.Create(TRUE,Socket);

     //Leider müssen wir hier casten um an die Eigenschaften zu kommen
     TEntropyServerThread(SocketThread).Timeout    :=Self.TimeOut;
     TEntropyServerThread(SocketThread).BufferSize :=Self.BufferSize;
     TEntropyServerThread(SocketThread).OnRequest  :=Self.OnRequest;

     //Kennung übertragen
     Socket.SendText(Self.sName+#10+#13);

     //Und los
     SocketThread.Resume();
end;


/////////////////////////////////////////////////////////////////////////////////
//Getter und Setter
procedure TThreadedServer.SetPort(value: unsigned16);
begin
     Self.Server.Port:=Value;
end;

function TThreadedServer.isactive():Boolean;
begin
     result:=Self.Server.Active;
end;

procedure TThreadedServer.DoActive(value: Boolean);
begin
     if (Value = TRUE) then
        begin
             Self.Start();
        end
     else
        begin
             Self.Stop();
        end; 
end;

/////////////////////////////////////////////////////////////////////////////////
//Hier wird alles erledigt
procedure TEntropyServerThread.ClientExecute();
var
   u32Size    : unsigned32;
   aBuffer    : array of Char;
   sBuffer    : Longstring;
   IPStream   : TWinSocketStream;
begin
     //Speicherfreigabe erzwingen
     inherited FreeOnTerminate:=TRUE;
     SetLength(aBuffer,Self.u32Buffer);

     Self.ClientSocket.SendText(SERVER_TEXT_HI);
     //Wir warten in einer Endlosschleife auf Daten und beenden, wenn sich
     //der Client abmeldet oder ein Timeout auftritt
     while (Not Self.Terminated) and
           (Self.ClientSocket.Connected) do
           begin
                IPStream:=TWinSocketStream.Create(Self.ClientSocket,Self.Timeout);
                //Und Daten verarbeiten
                repeat
                      //Um nicht zu viel Last zu erzeugen schalfen wir ein wenig
                      if (IPStream.WaitForData(Self.TimeOut)) then
                         begin
                              u32Size:=Self.ClientSocket.ReceiveBuf(aBuffer[0],Self.u32Buffer);

                              //Callback anspringen
                              if (Assigned(Self.OnRequest)) then
                                 begin
                                      sBuffer:=Copy(String(aBuffer),1,u32Size);
                                      Self.OnRequest(sBuffer);
                                 end;

                              //Und das Ergebnis zurückschicken
                              Self.ClientSocket.SendText(sBuffer);
                         end
                      else
                         begin
                              //Bei einem Timeout beenden
                              Self.ClientSocket.SendText(SERVER_TEXT_TIMEOUT);
                              Self.ClientSocket.Close;
                         end;
                until (Self.ClientSocket.Connected=FALSE);

                IPStream.Free();
           end;
end;



end.
