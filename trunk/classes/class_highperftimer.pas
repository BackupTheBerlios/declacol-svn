unit class_highperftimer;

//////////////////////////////////////////////////////////////////////
//
// Zeitmessungsklasse mit möglichst hoher Auflösung
//
// Um Möglichst genaue Messungen zu erhalten sollte man vor
// dem Start die Priorität auf TimeCritical setzen.
// Nach Stop aber wieder auf normal zurücksetzen
//
//////////////////////////////////////////////////////////////////////
interface

uses unit_typedefs,windows;

type THighPerfTimer = Class(TObject)
  private
    u32CounterType : unsigned32;
    u64CounterFRQ  : unsigned32;
    u64Timestamp   : unsigned64;
    u64Runtime     : unsigned64;
    u64Offset      : unsigned64;
    sMode          : longstring;

    procedure Init();
    function  Get():unsigned64;

  public
    constructor Create();

    procedure Start(aligned : boolean=FALSE);
    function  Stop ():unsigned64;

    property TimeStamp : unsigned64 read Get;
    property Runtime   : unsigned64 read u64Runtime;
    property Starttime : unsigned64 read u64offset write u64offset;
    property Mode      : unsigned32 read u32CounterType;
    property ModeString: longstring read sMode;
end;

const
        COUNTER_TICK = 1;
        COUNTER_PERF = 2;

implementation

//////////////////////////////////////////////////////////////////////
//Konstruktor
constructor THighPerfTimer.Create();
begin
  Self.u64Offset:=0;
  Self.Init();
end;

//////////////////////////////////////////////////////////////////////
//Prüfen, ob HighPerformanceCounter verfügbar sind
//und den entsprechenden Countertyp wählen
procedure THighPerfTimer.Init();
var
  u64FRQ : int64;
begin
  if ( QueryPerformanceFrequency( u64FRQ ) = not TRUE ) then
    begin
      Self.u32CounterType := COUNTER_PERF;
      //Auflösung auf 1/10 Milisekungen bringen
      Self.u64CounterFRQ  := u64FRQ div 10000;
      Self.sMode:='PerfCounter';
    end
  else
    begin
      Self.u32CounterType := COUNTER_TICK;
      Self.sMode:='TickCounter';
    end;
end;

//////////////////////////////////////////////////////////////////////
//Werte holen
function THighPerfTimer.Get():unsigned64;
var
   u64Stamp : int64;
begin
  case (Self.u32CounterType) of
    COUNTER_TICK : begin
                    //TickCounter auf 1/10 Milisekunden erweitern.
                    //Erhöht zwar nicht die Geanuigkeit, ist dann aber
                    //Vergleichbar mit den Perfcountern
                    u64Stamp := GetTickCount() * 10;
                    result:=u64Stamp;
                   end;

    COUNTER_PERF : begin
                    //PerformanceCounter auf 1/10 Milisekunden runterteilen.
                    //höhere Genauigkeit bringt ein Windowssystem
                    //scheinbar nicht
                    QueryPerformanceCounter(u64Stamp);
                    Result:=u64Stamp div Self.u64CounterFRQ;
                   end;
    end;

    dec(result,Self.u64Offset);
end;

//////////////////////////////////////////////////////////////////////
//Counter starten
//Ist aligned = TRUE wird von null ab gerechnet
procedure THighPerfTimer.Start(aligned : boolean);
begin
     if (aligned) then
        begin
             Self.u64TimeStamp:=Self.Get();
             Self.u64Offset:=Self.u64TimeStamp;
        end
     else
        begin
             Self.u64TimeStamp:=Self.Get();
        end;
end;

//////////////////////////////////////////////////////////////////////
//Zeit messen
function THighPerfTimer.Stop():unsigned64;
begin
  Self.u64Runtime:=Self.Get();

  //Überlauf ?
  if (Self.u64Runtime > Self.u64Timestamp) then
    begin
      Self.u64Runtime:=Self.u64TimeStamp - Self.u64Runtime;
    end;

  Result:=Self.u64Runtime;
end;

end.
 