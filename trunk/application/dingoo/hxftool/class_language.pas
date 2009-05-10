unit class_language;
////////////////////////////////////////////////////////////////////////////////
///
/// Read and Write within language-files
///
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,sysutils,classes,windows;

const
  HEADER_SIZE    = 32;
  HEADER_ID      = $2d584c44; // => DLX-
  HEADER_VERSION = $00000001; //?
  LNG_MARKER     = $19811108; //?

  //IDs f黵 die Sprachen
  ID_ENGLISH1      = 0;
  ID_ENGLISH2      = 1;
  ID_CHINESE1      = 2;
  ID_CHINESE2      = 3;
  ID_POLISH        = 4;
  ID_FRENCH        = 5;
  ID_GERMAN        = 6;
  ID_ITALIAN       = 7;
  ID_SPANISH       = 8;
  ID_LNG_MAX       = 9;

  //Die Dateinamen
  NAME_ENGLISH     = '英文';
  NAME_CHINESE1    = '简体中文';
  NAME_CHINESE2    = '繁体中文';
  NAME_POLISH      = '波兰语';
  NAME_FRENCH      = '法语';
  NAME_GERMAN      = '德语';
  NAME_ITALIAN     = '意大利语';
  NAME_SPANISH     = '西班牙';

  //Die zugeh鰎ige Kodierung der BMF-Dateien
  ENC_ENGLISH1   = #$b6 + #$03 + #$00 + #$00;
  ENC_ENGLISH2   = #$e4 + #$04 + #$00 + #$00;
  ENC_CHINESE1   = #$a8 + #$03 + #$00 + #$00;
  ENC_CHINESE2   = #$b6 + #$03 + #$00 + #$00;
  ENC_POLISH     = #$e2 + #$04 + #$00 + #$00;
  ENC_FRENCH     = #$e4 + #$04 + #$00 + #$00;
  ENC_GERMAN     = #$e4 + #$04 + #$00 + #$00;
  ENC_ITALIAN    = #$e4 + #$04 + #$00 + #$00;
  ENC_SPANISH    = #$e4 + #$04 + #$00 + #$00;

  //Pfad zur Sprachdatie
  FILE_ENGLISH1   = 'system\nls\'+NAME_ENGLISH+'.dlx';
  FILE_ENGLISH2   = 'system\nls\'+NAME_ENGLISH+'.dlx';
  FILE_CHINESE1   = 'system\nls\'+NAME_CHINESE1+'.dlx';
  FILE_CHINESE2   = 'system\nls\'+NAME_CHINESE2+'.dlx';
  FILE_POLISH     = 'system\nls\'+NAME_POLISH+'.dlx';
  FILE_FRENCH     = 'system\nls\'+NAME_FRENCH+'.dlx';
  FILE_GERMAN     = 'system\nls\'+NAME_GERMAN+'.dlx';
  FILE_ITALIAN    = 'system\nls\'+NAME_ITALIAN+'.dlx';
  FILE_SPANISH    = 'system\nls\'+NAME_SPANISH+'.dlx';

  //Identifikationsarray in ccpmp.bin
  CFG_ENGLISH1    = '英文.dlx' + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + ENC_ENGLISH1;
  CFG_ENGLISH2    = '英文.dlx' + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + ENC_ENGLISH2;
  CFG_CHINESE1    = '简体中文.dlx'  + #0 + #0 + #0 + #0 + ENC_CHINESE1;
  CFG_CHINESE2    = '繁体中文.dlx'  + #0 + #0 + #0 + #0 + ENC_CHINESE2;
  CFG_POLISH      = '波兰语.dlx'    + #0 + #0 + #0 + #0 + #0 + #0 + ENC_POLISH;
  CFG_FRENCH      = '法语.dlx' + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + ENC_FRENCH;
  CFG_GERMAN      = '德语.dlx' + #0 + #0 + #0 + #0 + #0 + #0 + #0 + #0 + ENC_GERMAN;
  CFG_ITALIAN     = '意大利语.dlx'  + #0 + #0 + #0 + #0 + ENC_ITALIAN;
  CFG_SPANISH     = '西班牙.dlx'    + #0 + #0 + #0 + #0 + #0 + #0 + ENC_SPANISH;

var
  //Konfiguration der Sprachen
  aLangConf : array [0..8] of array[0..19] of byte;
  //Namen der Sprachen
  aLangName : array [0..8] of string;

type tlngheader = packed record
  id        : unsigned32;
  version   : unsigned32;
  count     : unsigned32;
  marker    : unsigned32;
  stringoff : unsigned32;
  filename  : array[0..11] of char;
end;

type tlngrecord = packed record
  marker : unsigned32;
  size   : unsigned32;
  offset : unsigned32;
end;

type tlanguageencoder = class(tobject)
  private
    u32MemNeeded : unsigned32;
  protected
  public
    function pack   (var data : array of byte;    strings : tstringlist):boolean;
    function unpack (    data : array of byte;var strings : tstringlist):boolean;

    property memneeded : unsigned32 read u32MemNeeded;
end;


implementation

////////////////////////////////////////////////////////////////////////////////
// Aus einer Stringliste einen validen Sprachfile machen
////////////////////////////////////////////////////////////////////////////////
function tlanguageencoder.pack   (var data : array of byte;strings : tstringlist):boolean;
var
  header  : tlngheader;
  entry   : tlngrecord;
  u32Off  : unsigned32;
  u32Index: unsigned32;
  u32Pos  : unsigned32;
  u32Size : unsigned32;
  sTemp   : Widestring;
begin
  //Die Datemetriken bestimmen
  fillmemory(addr(header),sizeof(header),0);
  header.id        :=HEADER_ID;
  header.version   :=HEADER_VERSION;
  header.count     :=strings.count;
  header.marker    :=LNG_MARKER;
  header.stringoff :=sizeof(header) + (sizeof(entry) * header.count);
  header.filename  :='英文';

  //Ben鰐igten Platz bestimmen
  u32Size:=header.stringoff;
  u32Index:=0;
  while (u32Index < unsigned32(Strings.Count)) do
    begin
      //2Byte Zeichensatz
      inc(u32Size,Length(Strings[u32Index]) shl 1);
      inc(u32Index);
    end;

  Self.u32MemNeeded:=u32Size;

  //Soviel Platz brauchen die records 
  if (unsigned32(length(data)) >= u32Size) then
    begin
      result:=TRUE;
      
      //Alles leermachen
      fillmemory(addr(data[0]),length(data),00);

      //Header rein
      copymemory(addr(data[0]),addr(header),sizeof(header));

      //Die Daten schreiben
      u32Index:=0;
      u32Off  :=0;
      u32Pos  :=SizeOf(Header);
      while (u32Index < header.count) do
        begin
          entry.marker :=LNG_MARKER;
          entry.size   :=length(strings[u32Index]) shl 1;
          entry.offset :=u32Off;

          //Eintrag reinschreiben
          copymemory(addr(data[u32Pos]),addr(entry),sizeof(entry));

          //String ablegen
          sTemp:=strings[u32Index];

          copymemory(addr(data[header.stringoff + u32Off]),addr(sTemp[1]),entry.size);

          inc(u32Pos,sizeof(entry));
          inc(u32Off,entry.size);
          inc(u32Index);
        end;
    end
  else
    begin
      result:=FALSE;
    end;

    
end;

////////////////////////////////////////////////////////////////////////////////
// Eine Sprachdatei in eine Stringliste entpacken
////////////////////////////////////////////////////////////////////////////////
function tlanguageencoder.unpack (data : array of byte;var strings : tstringlist):boolean;
var
  header  : tlngheader;
  entry   : tlngrecord;
  u32Size : unsigned32;
  u32Index: unsigned32;
  aTemp   : array of widechar;
  u32Slice: unsigned32;
  sTemp   : string;
begin
  result:=FALSE;

  copymemory(addr(header),addr(data[0]),SizeOf(header));

  if (header.id = HEADER_ID) then
    begin
      u32Size :=header.count;
      u32Index:=HEADER_SIZE;
      while (u32Size > 0) do
        begin
          copymemory(addr(entry),addr(data[u32index]),sizeof(entry));

          //Speicher belegen (in Unicode 2Byte)
          setlength(aTemp,entry.size shr 1);
          fillmemory(addr(aTemp[0]),entry.size,0);

          //Rauskopieren und in der Tabelle ablegen
          copymemory(addr(aTemp[0]),addr(data[header.stringoff+entry.offset]),entry.size);

          sTemp:=WideCharLenToString(addr(aTemp[0]),entry.size shr 1);

          //Dank TeamDingo nullen am Ende eines Strings entfernen
          u32slice:=length(sTemp);
          if (u32SLice > 0) then
            begin
              while (sTemp[u32Slice]=#0) do
                begin
                  dec(u32slice);
                  sTemp:=copy(sTemp,1,u32Slice);
                end;
            end;

          strings.Add(sTemp);


          inc(u32index,sizeof(entry));
          dec(u32Size);
        end;
      result:=TRUE;
    end;
end;

initialization
  //Die dynamische Sprachtabelle aufbauen
  move(CFG_ENGLISH1[1],aLangConf[ID_ENGLISH1][0],length(aLangConf[ID_ENGLISH1]));
  move(CFG_ENGLISH2[1],aLangConf[ID_ENGLISH2][0],length(aLangConf[ID_ENGLISH2]));
  move(CFG_CHINESE1[1],aLangConf[ID_CHINESE1][0],length(aLangConf[ID_CHINESE1]));
  move(CFG_CHINESE2[1],aLangConf[ID_CHINESE2][0],length(aLangConf[ID_CHINESE2]));
  move(CFG_GERMAN[1],aLangConf[ID_GERMAN][0],length(aLangConf[ID_GERMAN]));
  move(CFG_POLISH[1],aLangConf[ID_POLISH][0],length(aLangConf[ID_POLISH]));
  move(CFG_SPANISH[1],aLangConf[ID_SPANISH][0],length(aLangConf[ID_SPANISH]));
  move(CFG_ITALIAN[1],aLangConf[ID_ITALIAN][0],length(aLangConf[ID_ITALIAN]));
  move(CFG_FRENCH[1],aLangConf[ID_FRENCH][0],length(aLangConf[ID_FRENCH]));

  aLangName[ID_ENGLISH1]:='english (ascii)';
  aLangName[ID_ENGLISH2]:='english (utf)';
  aLangName[ID_CHINESE1]:='chinese 1';
  aLangName[ID_CHINESE2]:='chinese 2';
  aLangName[ID_GERMAN]:='german';
  aLangName[ID_POLISH]:='polish';
  aLangName[ID_FRENCH]:='french';
  aLangName[ID_SPANISH]:='spanish';
  aLangName[ID_ITALIAN]:='italian';





end.
