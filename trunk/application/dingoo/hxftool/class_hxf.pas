unit class_hxf;
////////////////////////////////////////////////////////////////////////////////
///
/// Read and Write within hxf-files
///
////////////////////////////////////////////////////////////////////////////////
interface

uses unit_typedefs,sysutils,windows;

const
  HEADER_SIZE = 64;
  HEADER_ID   = 'WADF';
  HEADER_ID1  = 'Chinachip PMP firmware V1.0';

  HXF_ERROR_NONE      = NO_ERROR;
  HXF_ERROR_NO_HANDLE = INVALID_HANDLE_VALUE;
  HXF_ERROR_NO_ID     = 1024;
  HXF_ERROR_NO_CRC    = 2048;

////////////////////////////////////////////////////////////////////////////////
type Thxfheader = packed record
  id       : array[0..3] of char;
  version  : array[0..3] of char; //?
  timestamp: array[0..11] of char;
  size     : unsigned32;
  crc      : unsigned32;
  a1       : unsigned32;
  id1      : array[0..31] of char;
end;

type Thxfrecord = record
  id       : unsigned32;    //Unique ID
  filename : longstring;    //relative Filename in HXF-Package
  offset   : unsigned32;    //Offset in HXF-Package
  size     : unsigned32;    //Size of Buffer
  buffer   : array of byte; //Data
end;

////////////////////////////////////////////////////////////////////////////////
type Thxfreader = class (TObject)
  private
    aFAT       : array of Thxfrecord;
    u32FatSize : unsigned32;
    u32FatIndex: unsigned32; 
    hFile      : THandle;
    aHeader    : Thxfheader;
    u32hxfsize : unsigned32;
    u32hxfcrc  : unsigned32;
    u32Error   : unsigned32;
    bIgnoreErrors : boolean;
  protected

  public
    constructor create(filename : longstring);
    destructor  free();

    function    analyze():boolean;
    function    getfirst(var data : Thxfrecord):boolean;
    function    getnext (var data : Thxfrecord):boolean;
    function    exists(filename : longstring):boolean;
    function    get(index : unsigned32; var data : Thxfrecord):boolean; overload;
    function    get(filename : longstring; var data : Thxfrecord):boolean; overload;
    function    put(data : Thxfrecord):boolean;

    function    createchecksum():unsigned32;
    function    writeheader():boolean;

    property    count : unsigned32 read u32FATSize;
    property    size  : unsigned32 read u32HXFSize;
    property    crc   : unsigned32 read u32HXFCrc;
    property    error : unsigned32 read u32Error;

    property    ignoreerrors : boolean read bIgnoreErrors write bIgnoreErrors;
    property    header : thxfheader read aheader;
end;

implementation

////////////////////////////////////////////////////////////////////////////////
// Konstruktor / Destruktor
////////////////////////////////////////////////////////////////////////////////
constructor Thxfreader.create(filename : longstring);
begin
  self.u32FATSize:=0;
  bIgnoreErrors:=TRUE;

  hFile:=fileopen(filename,fmOpenReadWrite);

  if (hFile = INVALID_HANDLE_VALUE) then
    begin
      u32Error:=HXF_ERROR_NO_HANDLE;
    end
  else
    begin
      u32Error:=HXF_ERROR_NONE;
      self.u32hxfcrc:=createchecksum();
      Self.analyze();
    end;

end;

destructor  Thxfreader.free();
begin
  if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      writeheader();
      closehandle(hFile);
    end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Checksumme bilden
/// Einfach alle DoubleWords aufaddieren
////////////////////////////////////////////////////////////////////////////////
function    thxfreader.createchecksum():unsigned32;
var
  u32Read  : unsigned32;
  u32Index : unsigned32;
  aBuffer  : array[0..8191] of unsigned32;
begin
  result:=0;
  if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      fileseek(hFile,SizeOf(aHeader),0);

      repeat
        u32read:=fileread(hFile,aBuffer[0],SizeOf(aBuffer));

        u32Index:=0;
        while (u32Index <  (u32read shr 2)) do
          begin
            inc(result,aBuffer[u32Index]);
            inc(u32Index);
          end;
      until u32Read=0;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// Die komplette Struktur analysieren und der Geschwindigkeit wegen
// als Array ablegen
////////////////////////////////////////////////////////////////////////////////
//Data
//
// 0 - 68 Header
//
//FileRecord
// name 0x20 size (unsigned32) data
//
//
////////////////////////////////////////////////////////////////////////////////
function    Thxfreader.analyze():boolean;
var
  u8Temp  : Byte;
  u32Size : unsigned32;
  u32Pos  : unsigned32;
  aData   : Thxfrecord;
begin
  setlength(aFat,0);

  if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      u32HXFSize:=getfilesize(hFile,nil);
      u32HXFCrc:=createchecksum();

      fileseek(hFile,0,0);
      fileread(hFile,aHeader,sizeof(aHeader));

      //Ist der Header OK?
      if (aHeader.id  = HEADER_ID ) then
        begin
          if (aHeader.crc = u32HXFCrc) OR (IgnoreErrors=TRUE) then
            begin
              // bis zum ende der Datei analysieren
              u32Pos:=0;
              while (u32Pos < u32HXFSize) do
                begin
                 //Datensatz vorbereiten
                 aData.id:=length(aFAT);
                 aData.filename:='';
                 setlength(aData.buffer,0);

                 //SizeOf Name holen
                 FileRead(hFile,u32Size,4);

                 while (FileRead(hFile,u8Temp,1) = 1) and
                       (u32Size > 0) do
                   begin
                     aData.filename:=aData.filename + Chr(u8Temp);
                     dec(u32Size);
                   end;

                 //Immer kleinschrift
                 aData.filename:=LowerCase(aData.filename);

                 //Dateigröße lesen
                 Fileread(hFile,aData.Size,4);

                 //Unsere Position holen
                 aData.offset:=FileSeek(hFile,0,1);

                 SetLength(aFAT,aData.id + 1);
                 aFAT[aData.id]:=aData;

                 //Daten überspringen
                 u32Pos:=FileSeek(hFile,aData.size,1);
                end;
            end
          else
            begin
              //Checksummenfehler
              u32Error:=HXF_ERROR_NO_CRC;
            end;
          end
        else
          begin
              //HeaderID falsch
              u32Error:=HXF_ERROR_NO_ID;
          end;
      end
    else
      begin
        //Keine Datei geöffnet
        u32Error:=HXF_ERROR_NO_HANDLE;
      end;

  //Anzahl der Einträge merken
  u32FatSize:=length(aFat);

  //Was gefunden?
  result:=u32FatSize > 0;
end;


////////////////////////////////////////////////////////////////////////////////
// Einen Eintrag im HXF-Paket holen
////////////////////////////////////////////////////////////////////////////////
function    Thxfreader.get(index : unsigned32; var data : Thxfrecord):boolean;
var
  u32read : unsigned32;
begin
  if (index < u32FATsize) then
    begin
      data:=aFAT[index];
      u32read:=0;

      //Neuen allokieren und beschreiben
      setlength(data.buffer,data.size);
      if ( unsigned32(fileseek(hFile,data.offset,0)) = data.offset) then
        begin
          u32read:=fileread(hFile,data.buffer[0],data.size);
        end;

      result:=u32read = data.size;
    end
  else
    begin
      result:=FALSE;
    end;
end;


function    Thxfreader.get(filename : longstring; var data : Thxfrecord):boolean;
var
  u32Index : unsigned32;
begin
  result:=FALSE;
  u32Index:=0;
  while (u32Index < u32FatSize) and (result=FALSE) do
    begin
      if ( aFAT[u32Index].filename = filename ) then
        begin
          result := self.get(u32index,data);
        end;
      inc(u32Index);
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// Prüfen, ob eine Datei existiert
////////////////////////////////////////////////////////////////////////////////
function    Thxfreader.exists(filename : longstring):boolean;
var
  u32Index : unsigned32;
begin
  result:=FALSE;
  u32Index:=0;
  while (u32Index < u32FatSize) and (result=FALSE) do
    begin
      result:=aFAT[u32Index].filename = filename;
      inc(u32Index);
    end;
end;


////////////////////////////////////////////////////////////////////////////////
// Den Buffer an die Stelle der Datei im Paket schreiben
////////////////////////////////////////////////////////////////////////////////
function    Thxfreader.put(data : Thxfrecord):boolean;
var
  u32write : unsigned32; 
begin
  result:=FALSE;
  //Nur identische Dateien überschreiben
  if (data.filename = aFAT[data.id].filename) then
    begin
      //Datenposition suchen
      if (unsigned32(fileseek(hFile,data.offset,0))=data.offset) then
        begin
          //Und Buffer schreiben
          u32write:=filewrite(hFile,data.buffer[0],data.size);
          result:=u32write = data.size;
        end;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// Rebuild Header
////////////////////////////////////////////////////////////////////////////////
function    Thxfreader.writeheader():boolean;
var
  sTemp : longstring;
begin
  //Headerdaten setzen
  fillmemory(addr(aHeader),SizeOf(aHeader),0);

  aHeader.id:=HEADER_ID;
  aHeader.version:='0100';
  DateTimeToString(sTemp,'yyyymmddhhnn',Now());
  move(sTemp[1],aHeader.timestamp[0],SizeOf(aHeader.timestamp));
  aHeader.timestamp:='200801291107';
  aHeader.crc:=createchecksum();
  aHeader.size:=getfilesize(hFile,nil);
  aHeader.id1:=HEADER_ID1;

  if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      fileseek(hFile,0,0);
      filewrite(hFile,aHeader,SizeOf(aHeader));
      result:=TRUE;
    end
  else
    begin
      result:=FALSE;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// Ersten Eintrag im HXF-Paket holen
////////////////////////////////////////////////////////////////////////////////
function    Thxfreader.getfirst(var data : Thxfrecord):boolean;
begin
  u32FatIndex:=0;
  result:=self.get(u32FATIndex,data);
end;

////////////////////////////////////////////////////////////////////////////////
// Nächsten Eintrag im HXF-Paket holen
////////////////////////////////////////////////////////////////////////////////
function    Thxfreader.getnext(var data : Thxfrecord):boolean;
begin
  inc(u32FatIndex);
  result:=self.get(u32FATIndex,data);
end;

end.
