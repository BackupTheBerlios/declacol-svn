unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdTelnet, StdCtrls, ScktComp, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ClientSocket: TClientSocket;
    Memo1: TMemo;
    cbAutoswitch: TCheckBox;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ClientSocketConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure SendText(text:string);
    procedure ClientSocketError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  bSignal  : Boolean;
  bDone    : Boolean;

implementation

{$R *.dfm}
procedure TForm1.Button1Click(Sender: TObject);
begin
  ClientSocket.Host:='localhost';
  ClientSocket.Port:=9051;
  ClientSocket.Open();
  bSignal :=FALSE;
  bDone:=FALSE;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  Randomize();
  if (ParamStr(1)<>'') then
    begin
      bDone:=FALSE;
      Button1Click(Self);
      while (bDone=FALSE) do
        begin
          Sleep(100);
          Application.ProcessMessages();
        end;
      Self.Close();
    end;
end;

procedure TForm1.SendText(text:string);
begin
  Memo1.Lines.Add('send :'+text);
  ClientSocket.Socket.SendText(text+#10);
end;

procedure TForm1.ClientSocketConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  //Bei einem Connect immer Authentifizieren
  SendText('AUTHENTICATE ');
end;

procedure TForm1.ClientSocketRead(Sender: TObject;
  Socket: TCustomWinSocket);
var
  buffer : string;
begin
  buffer:=trim(Socket.ReceiveText());
  Memo1.Lines.Add(buffer);

  //Fehler aufgetreten?
  if (buffer <> '250 OK') then
    begin
      Memo1.Lines.Add('closing');
      Socket.Close();
      bDone:=TRUE;
    end
  else
    begin
      if (bSignal=FALSE) then
        begin
          bSignal:=TRUE;
          SendText('SIGNAL NEWNYM');
        end
      else
        begin
          Memo1.Lines.Add('closing');
          Socket.Close();
          bDone:=TRUE;
        end;
    end;
end;

procedure TForm1.ClientSocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  bDone:=TRUE;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  u32time : cardinal;
begin
  if (cbAutoSwitch.Checked = TRUE) and
     (bDone = FALSE) then
    begin
          bDone:=TRUE;
          Button1Click(Self);
          //Zufällig warten
          u32Time:=Random( 5 * 60 * 1000 ) + (1 * 60 * 1000);
          Timer1.Interval := u32Time;
          Memo1.Lines.Add('next switch in '+IntToStr(u32Time div 1000) + 'sec');
    end;
end;

end.
