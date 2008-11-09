unit class_renderengine;


interface
uses unit_typedefs,windows;

const
  //Windows Farbcodes umrubeln
  //Primärfarben
  TEXT_BLACK      = 0;
  TEXT_BLUE       = FOREGROUND_BLUE;
  TEXT_GREEN      = FOREGROUND_GREEN;
  TEXT_RED        = FOREGROUND_RED;
  TEXT_LIGHTBLUE  = FOREGROUND_BLUE or FOREGROUND_INTENSITY;
  TEXT_LIGHTGREEN = FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  TEXT_LIGHTRED   = FOREGROUND_RED or FOREGROUND_INTENSITY;

  //Mischfarben
  TEXT_PURPLE     = FOREGROUND_BLUE or FOREGROUND_GREEN;
  TEXT_LIGHTPURPLE= FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  TEXT_YELLOW     = FOREGROUND_RED or FOREGROUND_GREEN;
  TEXT_LIGHTYELLOW= FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  TEXT_CYAN       = FOREGROUND_BLUE or FOREGROUND_GREEN;
  TEXT_LIGHTCYAN  = FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_INTENSITY;

  TEXT_GRAY       = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE;
  TEXT_WHITE      = FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;

  BACK_BLACK      = 0;
  BACK_BLUE       = BACKGROUND_BLUE;
  BACK_GREEN      = BACKGROUND_GREEN;
  BACK_RED        = BACKGROUND_RED;
  BACK_LIGHTBLUE  = BACKGROUND_BLUE or FOREGROUND_INTENSITY;
  BACK_LIGHTGREEN = BACKGROUND_GREEN or FOREGROUND_INTENSITY;
  BACK_LIGHTRED   = BACKGROUND_RED or FOREGROUND_INTENSITY;

  BACK_PURPLE     = BACKGROUND_BLUE or BACKGROUND_RED;
  BACK_LIGHTPURPLE= BACKGROUND_BLUE or BACKGROUND_RED or BACKGROUND_INTENSITY;
  BACK_YELLOW     = BACKGROUND_RED or BACKGROUND_GREEN;
  BACK_LIGHTYELLOW= BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_INTENSITY;
  BACK_CYAN       = BACKGROUND_BLUE or BACKGROUND_GREEN;
  BACK_LIGHTCYAN= BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_INTENSITY;

  BACK_GRAY       = BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_BLUE;
  BACK_WHITE      = BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY;

type TPixel = packed record
  character : byte;
  attribute : byte;
end;

type TCoord = packed record
  xPos      : unsigned16;
  yPos      : unsigned16;
end;

type TRender = class(TObject)
    //Screenbuffer
    aScreen : array of array of TPixel;

    u16MaxX : unsigned16;
    u16MaxY : unsigned16;
  protected
  private
  public
    constructor create(xSize : unsigned16; ySize : unsigned16);

    procedure clear(value : byte; Attrib : Byte);

    procedure setPixel(xPos : unsigned16; yPos : unsigned16; Value : char; Attrib : Byte); overload;
    procedure setPixel(xPos : unsigned16; yPos : unsigned16; Value : byte; Attrib : Byte); overload;

    function  getDiff(xPos1 : unsigned16; yPOs1 : unsigned16; xPos2 : unsigned16; yPos2 : unsigned16) : signed32;

    procedure flush();

    property xSize : unsigned16 read u16MaxX;
    property ySize : unsigned16 read u16MaxY;
end;

implementation

constructor TRender.create(xSize : unsigned16; ySize : unsigned16);
var
  u16IndexX : unsigned32;
begin
  Self.u16MaxX:=xSize - 1;
  Self.u16MaxY:=ySize - 1;

  //Den "Bildschirm" initialisieren
  SetLength(aScreen,xSize);
  for u16IndexX := 0 to xSize - 1 do
    begin
      SetLength(aScreen[u16IndexX],ySize);
    end;

  //Und gleich löschen
  Self.Clear(0,0);
end;

//Bildschirm dimensionieren und löschen
procedure TRender.Clear(value : byte; attrib : byte);
var
  u16IndexX : unsigned16;
  u16IndexY : unsigned16;
begin
  //Den "Bildschirm" initialisieren
  for u16IndexX := 0 to Self.u16MaxX do
    begin
      for u16IndexY := 0 to Self.u16MaxY do
        begin
          Self.aScreen[u16IndexX][u16IndexY].character:=value;
          Self.aScreen[u16IndexX][u16IndexY].attribute:=value;
        end;
    end;
end;


//Ein Pixel setzen
procedure TRender.setPixel(xPos : unsigned16; yPos : unsigned16; Value : char; Attrib : Byte);
begin
  Self.setPixel(xPos,yPos,Ord(Value),Attrib);
end;

procedure TRender.setPixel(xPos : unsigned16; yPos : unsigned16; Value : byte; Attrib : Byte);
begin
  if (yPos <= Self.u16MaxY) and (xPos < Self.u16MaxX) then
    begin
      Self.aScreen[xPos][yPos].character:=Value;
      Self.aScreen[xPos][yPos].attribute:=Attrib;
    end;
end;

//direkten Abstand zwischen zwei Punkten messen
function  TRender.getDiff(xPos1 : unsigned16; yPOs1 : unsigned16; xPos2 : unsigned16; yPos2 : unsigned16) : signed32;
var
  s32prefix : signed32;
  u16xDiff  : unsigned16;
  u16yDiff  : unsigned16;
begin
  s32prefix:= 1;

  if ( xPos1 > xPos2 ) then
    begin
      u16xDiff:=xPos1 - xPos2;
    end
  else
    begin
      s32prefix:= - 1;
      u16xDiff:=xPos2 - xPos1;
    end;

  if ( yPos1 > yPos2 ) then
    begin
      u16yDiff:=yPos1 - yPos2;
    end
  else
    begin
      s32prefix:= - 1;
      u16yDiff:=yPos2 - yPos1;
    end;
  result:=trunc( sqrt( u16xDiff * u16xDiff + u16yDiff * u16yDiff ) ) * s32Prefix;
end;

//Den Speicher auf die Konsole ausgeben
procedure TRender.flush();
var
  u16IndexX : unsigned16;
  u16IndexY : unsigned16;
  u32Out    : unsigned32;
  hCon      : THandle;
  rPos      : _COORD;
begin
  hCon := GetStdHandle(STD_OUTPUT_HANDLE);

  if (hCon <> INVALID_HANDLE_VALUE) then
    begin
      for u16IndexX := 0 to Self.u16MaxX do
        begin
          for u16IndexY := 0 to Self.u16MaxY do
            begin
              rPos.X:=u16IndexX;
              rPos.Y:=u16IndexY;

              WriteConsoleOutputCharacter(hCon,@Self.aScreen[u16IndexX][u16IndexY].character,1,rPos,Cardinal(u32Out));
              WriteConsoleOutputAttribute(hCon,@Self.aScreen[u16IndexX][u16IndexY].attribute,1,rPos,Cardinal(u32Out));
            end;
        end;
      closehandle(hCon);
    end;
end;

end.
