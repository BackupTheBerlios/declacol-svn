unit class_language;
////////////////////////////////////////////////////////////////////////////////
///
/// Read and Write within language-files
///
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,sysutils,classes,windows;

const
  //IDs f�r die Sprachen MU� mit dem LanguageNameArray synchron sein!!!!!
  ID_ENGLISH1      = 0;
  ID_ENGLISH2      = 1;
  ID_CHINESE1      = 2;
  ID_CHINESE2      = 3;
  ID_POLISH        = 4;
  ID_FRENCH        = 5;
  ID_GERMAN        = 6;
  ID_ITALIAN       = 7;
  ID_SPANISH       = 8;
  ID_RUSSIAN       = 9;
  LANGUAGE_COUNT   = 10;

  SIZEOF_ENCODING  = 4;
  SIZEOF_CONFIG    = 20;
type
  TLanguageData = record
    id       : unsigned32;
    realname : longstring;
    filename : longstring;
    dlxname  : longstring;
    filepath : longstring;
    encoding : array[0..SIZEOF_ENCODING-1] of char;
    config   : array[0..SIZEOF_CONFIG - 1] of char;
    configb  : array[0..SIZEOF_CONFIG - 1] of byte;
  end;


var
  //Array mit allen relevanten Daten (wird zur Laufzeit initialisiert)
  LanguageData  : array[0..LANGUAGE_COUNT - 1] of TLanguageData;

  //Namen aller Sprachen
  LanguageNames : array[0..LANGUAGE_COUNT - 1] of longstring =
      (
      'english (ascii)',
      'english (utf)',
      'chinese 1',
      'chinese 2',
      'polish',
      'french',
      'german',
      'italian',
      'spanish',
      'russian'
      );

  LanguageFiles : array[0..LANGUAGE_COUNT - 1] of longstring =
      (
      'Ӣ��',         // english
      'Ӣ��',         // english
      '��������',     // chinese 1
      '��������',     // chinese 2
      '������',       // polish
      '����',         // french
      '����',         // german
      '�������',     // italian
      '������',       // spanish
      '����'          // russian
      );

  LanguageEncoding : array[0..LANGUAGE_COUNT - 1] of array[0..3] of char =
      (
      #$b6 + #$03 + #$00 + #$00,    //English 1
      #$e4 + #$04 + #$00 + #$00,    //English 2
      #$a8 + #$03 + #$00 + #$00,    //Chinese 1
      #$b6 + #$03 + #$00 + #$00,    //Chinese 2
      #$e2 + #$04 + #$00 + #$00,    //Polish
      #$e4 + #$04 + #$00 + #$00,    //French
      #$e4 + #$04 + #$00 + #$00,    //German
      #$e4 + #$04 + #$00 + #$00,    //Italian
      #$e4 + #$04 + #$00 + #$00,    //Spanish
      #$e3 + #$04 + #$00 + #$00     //Russian
      );


const
  HEADER_SIZE    = 32;
  HEADER_ID      = $2d584c44; // => DLX-
  HEADER_VERSION = $00000001; //?
  LNG_MARKER     = $19811108; //?

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

var
  u32index : unsigned32;
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
  header.filename  :='Ӣ��';

  //Ben�tigten Platz bestimmen
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

  //Die Datenarrays f�r die Sprachen f�llen
  u32index:=0;
  while (u32index < unsigned32(length(LanguageNames))) do
    begin
      fillmemory(Addr(LanguageData[u32Index]),SizeOf(LanguageData[u32Index]),0);

      //Hilfreiche Infos
      LanguageData[u32Index].id:=u32Index;
      LanguageData[u32Index].realname:=LanguageNames[u32Index];
      LanguageData[u32Index].filename:=LanguageFiles[u32Index];
      LanguageData[u32Index].dlxname:=LanguageFiles[u32Index]+'.dlx';
      LanguageData[u32Index].filepath:='system\nls\'+LanguageFiles[u32Index]+'.dlx';

      //UTF-Encoding als CharArray
      LanguageData[u32Index].encoding[0]:=LanguageEncoding[u32Index][0];
      LanguageData[u32Index].encoding[1]:=LanguageEncoding[u32Index][1];
      LanguageData[u32Index].encoding[2]:=LanguageEncoding[u32Index][2];
      LanguageData[u32Index].encoding[3]:=LanguageEncoding[u32Index][3];

      //UTF-Config-Sequenz als CharArray
      move(LanguageData[u32Index].dlxname[1],LanguageData[u32Index].config[0],Length(LanguageData[u32Index].dlxname));
      LanguageData[u32Index].config[16]:=LanguageEncoding[u32Index][0];
      LanguageData[u32Index].config[17]:=LanguageEncoding[u32Index][1];
      LanguageData[u32Index].config[18]:=LanguageEncoding[u32Index][2];
      LanguageData[u32Index].config[19]:=LanguageEncoding[u32Index][3];

      //UTF-Config-Sequenz als ByteArray
      move(LanguageData[u32Index].config[0],LanguageData[u32Index].configb[0],Length(LanguageData[u32Index].configb));


      inc(u32Index);
    end;



end.
