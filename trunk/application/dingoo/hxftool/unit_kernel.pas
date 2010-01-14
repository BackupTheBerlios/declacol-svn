unit unit_kernel;

interface

uses sysutils,unit_typedefs;

const
  ID_POWEROFF   = 0;
  ID_BRIGHTNESS = 1;
  ID_VOLUME     = 2;
  ID_BACKLIGHT  = 3;
  ID_VIDEOPLAY  = 4;
  ID_VIDEOTIME  = 5;
  ID_VIDEOZOOM  = 6;
  ID_FONTCOLOR  = 7;
  ID_LANGUAGE   = 8;
  

type TDefaultData = record
  Name : string[32];
  Data : unsigned8;
end;

type TDefaultOptions = record
  Name    : string;                //Name der Einstellung
  Offset  : unsigned32;            //Offset in der Defaultdatein
  Options : array of TDefaultData; //Array aller Optionen
end;

var
  //Array mit allen verfügbaren Optionen
  DefaultOptions : array of TDefaultOptions;

implementation
var
  u32temp : unsigned32;



initialization

////////////////////////////////////////////////////////////////////////////////
//Hier das Array mit den Optionen setzen
SetLength(DefaultOptions,3);


////////////////////////////////////////////////////////////////////////////////
//Die Automatische Ausschaltung
SetLength(DefaultOptions[ID_POWEROFF].Options,4);
DefaultOptions[ID_POWEROFF].Name:='auto poweroff';
DefaultOptions[ID_POWEROFF].Offset:=$1e;
DefaultOptions[ID_POWEROFF].Options[0].Name:='10 Minutes';
DefaultOptions[ID_POWEROFF].Options[0].Data:=0;

DefaultOptions[ID_POWEROFF].Options[1].Name:='30 Minutes';
DefaultOptions[ID_POWEROFF].Options[1].Data:=1;

DefaultOptions[ID_POWEROFF].Options[2].Name:='1 Hour';
DefaultOptions[ID_POWEROFF].Options[2].Data:=2;

DefaultOptions[ID_POWEROFF].Options[3].Name:='Off';
DefaultOptions[ID_POWEROFF].Options[3].Data:=3;

////////////////////////////////////////////////////////////////////////////////
//Helligkeit
SetLength(DefaultOptions[ID_BRIGHTNESS].Options,5);
DefaultOptions[ID_BRIGHTNESS].Name:='brightness';
DefaultOptions[ID_BRIGHTNESS].Offset:=$2f;
DefaultOptions[ID_BRIGHTNESS].Options[0].Name:='1';
DefaultOptions[ID_BRIGHTNESS].Options[0].Data:=0;

DefaultOptions[ID_BRIGHTNESS].Options[1].Name:='2';
DefaultOptions[ID_BRIGHTNESS].Options[1].Data:=1;

DefaultOptions[ID_BRIGHTNESS].Options[2].Name:='3';
DefaultOptions[ID_BRIGHTNESS].Options[2].Data:=2;

DefaultOptions[ID_BRIGHTNESS].Options[3].Name:='4';
DefaultOptions[ID_BRIGHTNESS].Options[3].Data:=3;

DefaultOptions[ID_BRIGHTNESS].Options[4].Name:='5';
DefaultOptions[ID_BRIGHTNESS].Options[4].Data:=4;

////////////////////////////////////////////////////////////////////////////////
//Lautstärke
SetLength(DefaultOptions[ID_VOLUME].Options,30);
DefaultOptions[ID_VOLUME].Name:='volume';
DefaultOptions[ID_VOLUME].Offset:=$2f;
for u32Temp:=0 to 30 do
  begin
    defaultOptions[ID_VOLUME].Options[0].Name:=IntToStr(u32Temp);
    DefaultOptions[ID_VOLUME].Options[0].Data:=u32Temp;
  end;

////////////////////////////////////////////////////////////////////////////////
//Backlight
SetLength(DefaultOptions[ID_BACKLIGHT].Options,6);
DefaultOptions[ID_BACKLIGHT].Name:='Backlight Timeout';
DefaultOptions[ID_BACKLIGHT].Offset:=$05;
DefaultOptions[ID_BACKLIGHT].Options[0].Name:='5 seconds';
DefaultOptions[ID_BACKLIGHT].Options[0].Data:=0;

DefaultOptions[ID_BACKLIGHT].Options[0].Name:='10 seconds';
DefaultOptions[ID_BACKLIGHT].Options[0].Data:=1;

DefaultOptions[ID_BACKLIGHT].Options[0].Name:='15 seconds';
DefaultOptions[ID_BACKLIGHT].Options[0].Data:=2;

DefaultOptions[ID_BACKLIGHT].Options[0].Name:='20 seconds';
DefaultOptions[ID_BACKLIGHT].Options[0].Data:=3;

DefaultOptions[ID_BACKLIGHT].Options[0].Name:='30 seconds';
DefaultOptions[ID_BACKLIGHT].Options[0].Data:=4;

DefaultOptions[ID_BACKLIGHT].Options[0].Name:='Allways on';
DefaultOptions[ID_BACKLIGHT].Options[0].Data:=5;

////////////////////////////////////////////////////////////////////////////////
//Continue Videoplay
SetLength(DefaultOptions[ID_VIDEOPLAY].Options,2);
DefaultOptions[ID_VIDEOPLAY].Name:='remember videoposition';
DefaultOptions[ID_VIDEOPLAY].Offset:=$34;
DefaultOptions[ID_VIDEOPLAY].Options[0].Name:='Off';
DefaultOptions[ID_VIDEOPLAY].Options[0].Data:=0;

DefaultOptions[ID_VIDEOPLAY].Options[1].Name:='On';
DefaultOptions[ID_VIDEOPLAY].Options[1].Data:=1;

////////////////////////////////////////////////////////////////////////////////
//Videotime
SetLength(DefaultOptions[ID_VIDEOTIME].Options,2);
DefaultOptions[ID_VIDEOTIME].Name:='show videotime';
DefaultOptions[ID_VIDEOTIME].Offset:=$18;
DefaultOptions[ID_VIDEOTIME].Options[0].Name:='Off';
DefaultOptions[ID_VIDEOTIME].Options[0].Data:=0;

DefaultOptions[ID_VIDEOTIME].Options[1].Name:='On';
DefaultOptions[ID_VIDEOTIME].Options[1].Data:=1;

////////////////////////////////////////////////////////////////////////////////
//Videozoom
SetLength(DefaultOptions[ID_VIDEOZOOM].Options,2);
DefaultOptions[ID_VIDEOZOOM].Name:='show videotime';
DefaultOptions[ID_VIDEOZOOM].Offset:=$3d;
DefaultOptions[ID_VIDEOZOOM].Options[0].Name:='normal';
DefaultOptions[ID_VIDEOZOOM].Options[0].Data:=0;

DefaultOptions[ID_VIDEOZOOM].Options[1].Name:='fillbox';
DefaultOptions[ID_VIDEOZOOM].Options[1].Data:=1;

DefaultOptions[ID_VIDEOZOOM].Options[1].Name:='fullscreen';
DefaultOptions[ID_VIDEOZOOM].Options[1].Data:=2;

////////////////////////////////////////////////////////////////////////////////
//Menufontcolor
SetLength(DefaultOptions[ID_FONTCOLOR].Options,2);
DefaultOptions[ID_FONTCOLOR].Name:='menu fontcolor';
DefaultOptions[ID_FONTCOLOR].Offset:=$08;
DefaultOptions[ID_FONTCOLOR].Options[0].Name:='white';
DefaultOptions[ID_FONTCOLOR].Options[0].Data:=0;

DefaultOptions[ID_FONTCOLOR].Options[1].Name:='red';
DefaultOptions[ID_FONTCOLOR].Options[1].Data:=1;

DefaultOptions[ID_FONTCOLOR].Options[2].Name:='dark red';
DefaultOptions[ID_FONTCOLOR].Options[2].Data:=2;

DefaultOptions[ID_FONTCOLOR].Options[3].Name:='green';
DefaultOptions[ID_FONTCOLOR].Options[3].Data:=3;

DefaultOptions[ID_FONTCOLOR].Options[4].Name:='dark green';
DefaultOptions[ID_FONTCOLOR].Options[4].Data:=4;

DefaultOptions[ID_FONTCOLOR].Options[5].Name:='blue';
DefaultOptions[ID_FONTCOLOR].Options[5].Data:=5;

DefaultOptions[ID_FONTCOLOR].Options[6].Name:='dark blue';
DefaultOptions[ID_FONTCOLOR].Options[6].Data:=6;

DefaultOptions[ID_FONTCOLOR].Options[7].Name:='dark white';
DefaultOptions[ID_FONTCOLOR].Options[7].Data:=7;

DefaultOptions[ID_FONTCOLOR].Options[8].Name:='greenblue';
DefaultOptions[ID_FONTCOLOR].Options[8].Data:=8;

DefaultOptions[ID_FONTCOLOR].Options[9].Name:='violet';
DefaultOptions[ID_FONTCOLOR].Options[9].Data:=9;

DefaultOptions[ID_FONTCOLOR].Options[10].Name:='dark violet';
DefaultOptions[ID_FONTCOLOR].Options[10].Data:=10;

DefaultOptions[ID_FONTCOLOR].Options[11].Name:='yellow';
DefaultOptions[ID_FONTCOLOR].Options[11].Data:=11;

DefaultOptions[ID_FONTCOLOR].Options[12].Name:='dark yellow';
DefaultOptions[ID_FONTCOLOR].Options[12].Data:=12;

DefaultOptions[ID_FONTCOLOR].Options[13].Name:='gray';
DefaultOptions[ID_FONTCOLOR].Options[13].Data:=13;

DefaultOptions[ID_FONTCOLOR].Options[14].Name:='black';
DefaultOptions[ID_FONTCOLOR].Options[14].Data:=14;


////////////////////////////////////////////////////////////////////////////////
//Sprachen (muß nach analyse der hxf und ccpmp gesetzt werden)
SetLength(DefaultOptions[ID_LANGUAGE].Options,0);
DefaultOptions[ID_LANGUAGE].Name:='language';
DefaultOptions[ID_LANGUAGE].Offset:=$2e;

end.
