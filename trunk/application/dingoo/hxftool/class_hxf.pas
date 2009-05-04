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
  HEADER_ID   = 'WAD';

////////////////////////////////////////////////////////////////////////////////
type Thxfrecord = record
  id       : unsigned32;    //Unique ID
  filename : longstring;    //relative Filename in HXF-Package
  offset   : unsigned32;    //Offset in HXF-Package
  buffer   : array of byte; //Data
  size     : unsigned32;    //Size of Buffer
end;

////////////////////////////////////////////////////////////////////////////////
type Thxfreader = class (TObject)
  private
    aFAT       : array of Thxfrecord;
    u32FatSize : unsigned32;
    u32FatIndex: unsigned32; 
    hFile      : THandle;
    aHeader    : array [0..HEADER_SIZE - 1] of Byte;
    u32hxfsize : unsigned32;
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

    property    count : unsigned32 read u32FATSize;
    property    size  : unsigned32 read u32HXFSize;
end;

implementation

////////////////////////////////////////////////////////////////////////////////
// Konstruktor / Destruktor
////////////////////////////////////////////////////////////////////////////////
constructor Thxfreader.create(filename : longstring);
begin
  self.u32FATSize:=0;

  hFile:=fileopen(filename,fmOpenReadWrite);

  Self.analyze();
end;

destructor  Thxfreader.free();
begin
  if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      closehandle(hFile);
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
      u32size:=fileseek(hFile,0,0);
      fileread(hFile,aHeader[0],HEADER_SIZE - u32size);

      u32HXFSize:=getfilesize(hFile,nil);

      //Ist der Header OK?
      if ( aHeader[0]=ord(HEADER_ID[1])) and
         ( aHeader[1]=ord(HEADER_ID[2])) and
         ( aHeader[2]=ord(HEADER_ID[3])) then
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
              Fileread(hFile,aData.size,4);

              //Unsere Position holen
              aData.offset:=FileSeek(hFile,0,1);

              SetLength(aFAT,aData.id + 1);
              aFAT[aData.id]:=aData;

              //Daten überspringen
              u32Pos:=FileSeek(hFile,aData.size,1);
          end;
        end;
    end;

  //Anzah der Einträge merken
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
