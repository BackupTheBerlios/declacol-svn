unit class_diskio;
{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
Author: Sven Lorenz / Borg@Sven-of-Nine.de
}

{$IFDEF FPC}
        //Einstellungen für den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
////////////////////////////////////////////////////////////////////////////////
/// DiskIO-Klasse (c) 2007 Borg@Sven-of-Nine.de
///
///
/// Unit um direkt auf Laufwerke zuzugreifen
/// (ACHTUNG) damit ist ruckzuck Laufwerk C zerstört
///
/// Die Funktion sollte einigermaßen selbsterklärend sein.
/// Wichtig ist nur,daß bei Benutzung der Methoden RawRead/RawWrite
/// In Sectorengröße addressiert wird, da sonst manche Devices
/// Probleme bekommen.
///
/// RealSize prüft auf die echte Größe eines Datenträgers da Windows die
/// Laufwerksgröße leider in CHS-Parametern meldet und diese ungenau sein können
/// Allerdings ist diese Funktion sehr langsam.
////////////////////////////////////////////////////////////////////////////////
/// Achtung :
/// Festplatten erhalten automatisch das Flag WriteProtected und müssen
/// erst freigegeben werden
///
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,windows,sysutils,unit_errorcodes;


//Leider ist die Deklaration von SetFilePointer falsch, daher hier der richtige Prototyp
function MySetFilePointer(hFile: THandle; lDistanceToMove: unsigned32;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: DWORD): DWORD; stdcall;

function MySetFilePointer; external kernel32 name 'SetFilePointer';

////////////////////////////////////////////////////////////////////////////////
/// Einige Hilfskonstanten
////////////////////////////////////////////////////////////////////////////////
//Mediatypen
  (*typedef enum _MEDIA_TYPE {
     Unknown,                // Format is unknown
     F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
     F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
     F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
     F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
     F3_720_512,             // 3.5",  720KB,  512 bytes/sector
     F5_360_512,             // 5.25", 360KB,  512 bytes/sector
     F5_320_512,             // 5.25", 320KB,  512 bytes/sector
     F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
     F5_180_512,             // 5.25", 180KB,  512 bytes/sector
     F5_160_512,             // 5.25", 160KB,  512 bytes/sector
     RemovableMedia,         // Removable media other than floppy
     FixedMedia,             // Fixed hard disk media
     F3_120M_512,            // 3.5", 120M Floppy
     F3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
     F5_640_512,             // 5.25",  640KB,  512 bytes/sector
     F5_720_512,             // 5.25",  720KB,  512 bytes/sector
     F3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
     F3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
     F5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
     F3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
     F3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
     F8_256_128              // 8",     256KB,  128 bytes/sector
  } MEDIA_TYPE, *PMEDIA_TYPE;*)
const
  MEDIA_TYPE_REMOVABLE       = 11;
  MEDIA_TYPE_FIXED           = 12;

  IFLAG_HANDLES_DMA_BOUNDARY = 1;
  IFLAG_GEOMETRY_VALID       = 2;
  IFLAG_REMOVABLE            = 4;
  IFLAG_VERIFY_SUPPORT       = 8;
  IFLAG_CHANGE_LINE_SUPPORT  = 16;
  IFLAG_IS_LOCKABLE          = 32;
  IFLAG_NO_MEDIA_PRESENT     = 64;

  //Definitionen die in Delhpi leider fehlen
  FILE_DEVICE_DISK               = $00000007;
  FILE_DEVICE_DISK_FILE_SYSTEM   = $00000008;
  FILE_DEVICE_FILE_SYSTEM         = $00000009;

  FILE_ANY_ACCESS                = $0000;
  FILE_READ_ACCESS               = $0001;
  FILE_WRITE_ACCESS              = $0002;
  FILE_ALL_ACCESS                = FILE_READ_ACCESS OR FILE_WRITE_ACCESS;

  METHOD_BUFFERED                = 0;
  IOCTL_DISK_BASE                = FILE_DEVICE_DISK;
  IOCTL_DISK_GET_DRIVE_GEOMETRY  = ( ((IOCTL_DISK_BASE) SHL 16) OR ((FILE_ANY_ACCESS)  SHL 14) OR (($0000) SHL 2) OR (METHOD_BUFFERED) );

  IOCTL_DISK_GET_DRIVE_LAYOUT    = ( ((IOCTL_DISK_BASE) SHL 16) OR ((FILE_READ_ACCESS) SHL 14) OR (($0040) SHL 2) OR (METHOD_BUFFERED) );
  IOCTL_DISK_SET_DRIVE_LAYOUT    = ( ((IOCTL_DISK_BASE) SHL 16) OR ((FILE_ALL_ACCESS)  SHL 14) OR (($0004) SHL 2) OR (METHOD_BUFFERED) );
  IOCTL_DISK_DELETE_DRIVE_LAYOUT = ( ((IOCTL_DISK_BASE) SHL 16) OR ((FILE_ALL_ACCESS)  SHL 14) OR (($0003) SHL 2) OR (METHOD_BUFFERED) );

  IOCTL_DISK_CREATE_DISK         = ( ((IOCTL_DISK_BASE) SHL 16) OR ((FILE_ALL_ACCESS)  SHL 14) or (($0016) SHL 2) OR (METHOD_BUFFERED) );

  FSCTL_DISMOUNT_VOLUME          = ( (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (8 shl 2) or METHOD_BUFFERED);
  FSCTL_LOCK_VOLUME              = ( (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (6 shl 2) or METHOD_BUFFERED);
  FSCTL_UNLOCK_VOLUME            = ( (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (7 shl 2) or METHOD_BUFFERED);

  //Partitionstypen
  PARTITION_ENTRY_UNUSED         = $00;
  PARTITION_EXTENDED             = $05;
  PARTITION_FAT_12               = $01;
  PARTITION_FAT_16               = $04;
  PARTITION_FAT32                = $0B;
  PARTITION_IFS                  = $07;
  PARTITION_LDM                  = $42;
  PARTITION_NTFT                 = $80;
  VALID_NTFT                     = $C0;


//Typ für die Laufwerksgeometry
//aus Winioctl.h
type
  TDISK_GEOMETRY = packed record
    Cylinders         : unsigned64;
    MediaType         : unsigned32;
    TracksPerCylinder : unsigned32;
    SectorsPerTrack   : unsigned32;
    BytesPerSector    : unsigned32;
end;

//Typ für das Partitionslayout
type
  PPARTITION_INFORMATION = ^TPARTITION_INFORMATION;
  TPARTITION_INFORMATION = record
    StartingOffset      : unsigned64;
    PartitionLength     : unsigned64;
    HiddenSectors       : unsigned32;
    PartitionNumber     : unsigned32;
    PartitionType       : unsigned8;
    BootIndicator       : ByteBool;
    RecognizedPartition : ByteBool;
    RewritePartition    : ByteBool;
end;


//MBR Layout
type
  PPARTITION_INFORMATION_MBR = ^TPARTITION_INFORMATION_MBR;
  TPARTITION_INFORMATION_MBR = record
    PartitionType       : unsigned8;
    BootIndicator       : ByteBool;
    RecognizedPartition : ByteBool;
    HiddenSectors       : unsigned32;
end;


//Strukturen für die initialisierung von Devices
type
  TPARTITION_STYLE = (
    PARTITION_STYLE_MBR,
    PARTITION_STYLE_GPT,
    PARTITION_STYLE_RAW);

type
  PCREATE_DISK_MBR = ^TCREATE_DISK_MBR;
  TCREATE_DISK_MBR = record
    Signature           : unsigned32;
end;

type
  PCREATE_DISK_GPT = ^TCREATE_DISK_GPT;
  TCREATE_DISK_GPT = record
    DiskId              : TGUID;
    MaxPartitionCount   : unsigned32;
  end;

type
  PCREATE_DISK = ^TCREATE_DISK;
  TCREATE_DISK = record
    PartitionStyle: TPARTITION_STYLE;
    case Integer of
      0: (Mbr: TCREATE_DISK_MBR);
      1: (Gpt: TCREATE_DISK_GPT);
end;


type
  PDRIVE_LAYOUT_INFORMATION = ^TDRIVE_LAYOUT_INFORMATION;
  TDRIVE_LAYOUT_INFORMATION = record
    PartitionCount      : unsigned32;
    Signature           : unsigned32;
    PartitionEntry: array of TPARTITION_INFORMATION;
end;

type pDiskIO = ^TDiskIO;
  TDiskIO = class(TObject)
  private
    u8Drive       : Unsigned8;
    diskgeo       : TDISK_GEOMETRY;
    u64Size       : unsigned64;
    u64chssize    : unsigned64;
    u64SectorCount: unsigned64;
    sSize         : String;
    bRealSize     : Boolean;
    bValid        : Boolean;
    bRemovable    : Boolean;
    bNoWrite      : Boolean;
    fSpeed        : Single;
    sdata         : longstring;
  protected
    procedure SetDrive(Drive:unsigned8);
    procedure SetRealSize(Flag:Boolean);
    procedure Refresh();
    function  RescanSize():unsigned64;
  public
    //Konstruktor
    constructor Create(Drive:Byte=0);

    function GetGeometry(var disk_geometry : TDISK_GEOMETRY): boolean;
    //Direkt auf das Gerät addressieren
    //Funktioniert aber nur in SektorenSchritten
    function RawRead (Offset : unsigned64; Buffer:Pointer; BytesToRead:unsigned32; var BytesRead:unsigned32):Boolean;
    function RawWrite(Offset : unsigned64; Buffer:Pointer; BytesToWrite:unsigned32; var BytesWritten:unsigned32):Boolean;

    //Sektoren lesen / schreiben
    function Read(StartSector : unsigned64; Buffer : Pointer; SectorsToRead:unsigned32; var SectorsRead: unsigned32):Boolean;
    function Write(StartSector : unsigned64; Buffer : Pointer; SectorsToWrite:unsigned32; var SectorsWritten: unsigned32):Boolean;

    //Den kpl. Datenträger partitionieren und FAT32 formatieren
    function CreatePartition():Boolean;


    //Laufwerksbezeichner (0-25)
    property Devicenumber: unsigned8 read u8Drive write SetDrive;
    //Struktur mit Geometry-Daten
    property Geometry   : TDISK_GEOMETRY read diskgeo;
    //Die Sektorengröße weisen wir einzeln aus, da sie seht häufig benutzt wird
    property SectorSize : unsigned32 read diskgeo.BytesPerSector;
    property SectorCount: unsigned64 read u64SectorCount;

    //Echte größe des Datenträgers bestimmen ?
    property RealSize   : Boolean read bRealSize write SetRealSize;
    //Größe des Datenträgers
    property CHSSize    : unsigned64 read u64chssize;
    property Size       : unsigned64 read u64size;
    property SizeString : String read sSize;
    //Alles OK
    property Valid      : boolean read bValid;
    //Wechseldatenträger ?
    property Removable  : boolean read bRemovable;
    //Soll das Laufwerk als Schreibgeschützt behandelt werden ?
    property WriteProtected : boolean read bNoWrite write bNoWrite;
    //Geschwindigkeit des letzten Schreib/Lesevorganges in KB/s
    property Speed      : Single read fSpeed;

    property Data       : longstring read sdata write sdata; 
end;




implementation

////////////////////////////////////////////////////////////////////////////////
//Der Konstruktor initialisiert alle nötigen Daten
constructor TDiskIO.Create(Drive:Byte);
begin
  //Nicht automatisch die echte Größe bestimmen
  Self.bRealSize:=FALSE;

  //Auf jeden Fall mit Laufwerk 0 initialisieren
  Self.SetDrive(Drive);
end;

//Beim Laufwerkswechsel alles initialisieren
procedure TDiskIO.SetDrive(Drive:unsigned8);
begin
    Self.u8Drive:=Drive;
    Self.Refresh();
end;

//Beim Wechsel von RealSize evtl. Scannen
procedure TDiskIO.SetRealSize(Flag:Boolean);
begin
  Self.bRealSize:=Flag;
  Self.Refresh();
end;

////////////////////////////////////////////////////////////////////////////////
//Alle wichtigen Daten bestimmen
procedure TDiskIO.refresh();
var
  SizePref : Array [0..5] of Char;
  TempSize : Unsigned64;
  Divisor  : unsigned32;
  Index    : unsigned8;
begin
    //Leider lassen sich Arrays lokal nicht vorbelegen
    SizePref[0]:=' ';
    SizePref[1]:='K';
    SizePref[2]:='M';
    SizePref[3]:='G';
    SizePref[4]:='T';
    SizePref[5]:='P';

    //Können wir eine Diskgeometry lesen existiert das Gerät
    Self.bValid:=Self.GetGeometry(Self.diskgeo);

    //Größe ausrechnen
    if (Self.bValid) then
      begin
          //CHS-Größe bestimmen
          Self.u64CHSSize:= Self.diskgeo.BytesPerSector    *
                            Self.diskgeo.SectorsPerTrack   *
                            Self.DiskGeo.TracksPerCylinder *
                            Self.Diskgeo.Cylinders;
          //Die echte Größe nur auf Anfrage bestimmen
          if (Self.RealSize = TRUE) then
            begin
              Self.u64Size:=Self.RescanSize;
            end
          else
            begin
              Self.u64Size:=Self.u64chssize;
            end;
          Self.u64SectorCount:=Self.u64Size div Self.Diskgeo.BytesPerSector;

          //Einige nützliche Flags setzen
          Self.bRemovable:=( (Geometry.MediaType and MEDIA_TYPE_REMOVABLE) = MEDIA_TYPE_REMOVABLE);

          //Fixed-Drives immer automatisch schreibschützen, um den Anwender vor sich selbst zu schützen
          Self.WriteProtected:=not Self.Removable;          
      end
    else
      begin
          Self.u64Size:=0;
          Self.u64SectorCount:=0;
      end;

    //Da sich Int64 nicht in Floats wandlen lassen,
    //Ist die größenberechnung unter Delphi 5 eher aufwändig
    Index:=0;
    Divisor:=0;
    TempSize:=Self.Size;
    //Runterteilen bis wir im richtigen Größenbereich sind
    while ( TempSize > KA ) do
      begin
        TempSize:=TempSize shr WIDTH_OF_KA;
        inc(Divisor,WIDTH_OF_KA);
        inc(Index);
      end;

    //Bytes einzeln abfangen
    if (Divisor > 0)then
      begin
        TempSize:=Self.Size shr (Divisor - WIDTH_OF_KA);
      end
    else
      begin
        TempSize:=Self.Size shl WIDTH_OF_KA;
      end;

    //Für Delphi5 (Kein 64Bit in Format) nochmal casten
    Self.sSize:=Format('%f %sByte',[ (unsigned32(TempSize) / KA) ,SizePref[Index]]);
end;


////////////////////////////////////////////////////////////////////////////////
//Da Windows NTx leider die Geometrie in CHS-Parametern ausgibt,
//Ist die wahre größe des Laufwerkes eine andere als gemeldet.
//Um dennoch die richtige Größe zu finden lesen wir über die
//maximale Sektorzahl hinaus bis zum ersten Fehler
//leider ist dieses vorgehen seeeehr langsam aber SetFilePointer /FILE_END
//scheint nicht zu funktionieren
function TDiskIO.rescansize():unsigned64;
var
  hDevice          : THandle;
  DistanceToMoveHi : unsigned32;
  DistanceToMoveLo : unsigned32;
  bDone            : Boolean;
  TempBuffer       : array of byte;
begin
    result:=Self.Size;

    //Device Öffnen
    hDevice:=CreateFile(pchar('\\.\PhysicalDrive'+IntToStr(Self.u8Drive)),
                        GENERIC_READ,
                        FILE_SHARE_READ OR FILE_SHARE_WRITE,
                        nil, OPEN_EXISTING,
                        0,
                        0);

    //Handle OK
    if (hDevice<> INVALID_HANDLE_VALUE) then
      begin
        //Bummybuffer setzen
        SetLength(TempBuffer,Self.SectorSize);
        bDone:=FALSE;
        while (not bDone) do
          begin
            //Filepointer setzen
            DistanceToMoveLo:=unsigned32(Result and MASK_UNSIGNED32);
            DistanceToMoveHi:=unsigned32(Result shr WIDTH_OF_UNSIGNED32);

            //Und los
            MySetFilePointer(hDevice,DistanceToMoveLo,@DistanceToMoveHi,FILE_BEGIN);

            //(SetFilepointer geht immer, daher lesen wir zur Sicherheit den Sektor)
            bDone:=not ReadFile(hDevice,
                                TempBuffer[0],
                                cardinal(Self.SectorSize),
                                cardinal(DistanceToMoveLo),
                                nil);
            inc (result,Self.SectorSize);
          end;
        //Inkrement anpassen
        dec (result,Self.SectorSize);

        CloseHandle(hDevice);
    end;

    //Und auf Sektorengröße abrunden
    Result:=(Result div Self.SectorSize) * Self.SectorSize;
end;

////////////////////////////////////////////////////////////////////////////////
//Die Geometry des gewählten Laufwerkes holen
function TDiskIO.GetGeometry(var disk_geometry : TDISK_GEOMETRY): boolean;
var
  hDevice : THandle;
  Dummy   : Cardinal;
begin
    Result:=FALSE;

    //Zugriff auf das Laufwerk holen
    hDevice := CreateFile( pchar('\\.\PhysicalDrive'+IntToStr(Self.u8Drive)),
                           0,
                           FILE_SHARE_READ OR FILE_SHARE_WRITE,
                           nil,
                           OPEN_EXISTING,
                           0,
                           0);

    //Kanal offen ?
    if (hDevice <> INVALID_HANDLE_VALUE) then
    begin
        //Geometrie vom Treiber holen
        Result := DeviceIoControl(hDevice,
                                  IOCTL_DISK_GET_DRIVE_GEOMETRY,
                                  nil,
                                  0,
                                  @disk_geometry,
                                  sizeof(TDISK_GEOMETRY),
                                  Dummy,
                                  nil);
        CloseHandle(hDevice);
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//Sektoren vom Laufwerk lesen
function TDiskIO.Read(StartSector : unsigned64; Buffer : Pointer; SectorsToRead:unsigned32; var SectorsRead: unsigned32):Boolean;
begin
  //Fehler annehmen
  result:=FALSE;
  SectorsRead:=0;

  //Falschadressierung abfangen
  if (StartSector < Self.SectorCount) then
    begin
    //Wenn zuviele Sektoren angefragt sind stutzten wir zurecht,
    //da das System sonst alle Zugriffe abweisen würde
    if ( (StartSector + SectorsToRead) >= Self.SectorCount) then
      begin
        SectorsToRead := Self.SectorCount - StartSector;
      end;

    //Einfach die Sektoren umeumeln
    Result:=Self.RawRead(StartSector * Self.Diskgeo.BytesPerSector,
                         Buffer,
                         SectorsToRead * Self.diskgeo.BytesPerSector,
                         SectorsRead);

    //Und wieder in Sektoren umrechnen
    SectorsRead:=SectorsRead div Self.diskgeo.BytesPerSector;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//Sektoren auf das Laufwerk schreiben
function TDiskIO.Write(StartSector : unsigned64; Buffer : Pointer; SectorsToWrite:unsigned32; var SectorsWritten: unsigned32):Boolean;
begin
  //Fehler annehmen
  result:=FALSE;
  SectorsWritten:=0;

  //Falschadressierung abfangen
  if (StartSector < Self.SectorCount) then
    begin
    //Wenn zuviele Sektoren angefragt sind stutzten wir zurecht,
    //da das System sonst alle Zugriffe abweisen würde
    if ( (StartSector + SectorsToWrite) >= Self.SectorCount) then
      begin
        SectorsToWrite := Self.SectorCount - StartSector;
      end;

    //Einfach die Sektoren umeumeln
    Result:=Self.RawWrite(StartSector * Self.Diskgeo.BytesPerSector,
                         Buffer,
                         SectorsToWrite * Self.diskgeo.BytesPerSector,
                         SectorsWritten);

    //Und wieder in Sektoren umrechnen
    SectorsWritten:=SectorsWritten div Self.diskgeo.BytesPerSector;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//Den kpl. Datenträger partitionieren und FAT32 formatieren
//ToDo
function TDiskIO.CreatePartition():Boolean;
var
  hDevice     : THandle;
  Dummy       : Cardinal;
  NewDisk     : TCREATE_DISK;
begin
    Result:=FALSE;

    //Zugriff auf das Laufwerk holen
    hDevice := CreateFile( pchar('\\.\PhysicalDrive'+IntToStr(Self.u8Drive)),
                           GENERIC_READ OR GENERIC_WRITE,
                           FILE_SHARE_READ OR FILE_SHARE_WRITE OR FILE_SHARE_DELETE,
                           nil,
                           OPEN_EXISTING,
                           0,
                           0);

    //Kanal offen ?
    if (hDevice <> INVALID_HANDLE_VALUE) AND (Self.WriteProtected = FALSE) then
       begin
            //Disk initialisieren
            Result := DeviceIoControl( hDevice,
                                       IOCTL_DISK_DELETE_DRIVE_LAYOUT,
                                       nil,
                                       0,
                                       nil,
                                       0,
                                       Dummy,
                                       nil);


            MessageBox(0,PChar(LastErrorMessage()),'error',MB_OK);


            NewDisk.PartitionStyle:=PARTITION_STYLE_MBR;
            NewDisk.Mbr.Signature :=PARTITION_FAT32;

            //Partition anlegen
            Result := DeviceIoControl( hDevice,
                                       IOCTL_DISK_CREATE_DISK,
                                       @NewDisk,
                                       SizeOf(NewDisk),
                                       nil,
                                       0,
                                       Dummy,
                                       nil);

            MessageBox(0,PChar(LastErrorMessage()),'error',MB_OK);
                                       

            CloseHandle(hDevice);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//LowLevel vom Laufwerk lesen
//Es muß IMMER mindestens ein Sektor gelesen werden, da es sonst nicht funktioniert
function TDiskIO.RawRead(Offset : unsigned64; Buffer:Pointer; BytesToRead:unsigned32; var BytesRead:unsigned32):Boolean;
var
  hdevice          : THandle;
  DistanceToMoveHi : unsigned32;
  DistanceToMoveLo : unsigned32;
  TempBuffer       : array of byte;
  u32Tick          : unsigned32;
begin
    Result:=FALSE;
    Self.fSpeed:=0;

    //Device Öffnen
    hDevice:=CreateFile(pchar('\\.\PhysicalDrive'+IntToStr(Self.u8Drive)),
                        GENERIC_READ,
                        FILE_SHARE_READ OR FILE_SHARE_WRITE,
                        nil, OPEN_EXISTING,
                        FILE_FLAG_WRITE_THROUGH,
                        0);

    //OK ?
    if (hDevice <> INVALID_HANDLE_VALUE) then
      begin
        //Offset bestimmen
        DistanceToMoveLo:=unsigned32(Offset and MASK_UNSIGNED32);
        DistanceToMoveHi:=unsigned32(Offset shr WIDTH_OF_UNSIGNED32);

        //Und bewegen
        MySetFilePointer(hdevice,DistanceToMoveLo, @DistanceToMoveHi, FILE_BEGIN);
        if (GetLastError = NO_ERROR) then
            begin
               //Benchmark starten
               u32Tick:=GetTickCount();

                //Da die Definition in Delphi VAR als Buffer erwartet müssen wir hier mit einem
                //kleinen Zwischenpuffer arbeiten sonst gibt es Ärger mit dem Speicher
                SetLength(TempBuffer,BytesToRead);
                Result:=ReadFile(hDevice,
                                 TempBuffer[0],
                                 cardinal(BytesToRead),
                                 cardinal(BytesRead),
                                 nil);
                Move(TempBuffer[0],Buffer^,BytesToRead);

                //Benchmark bestimmen
                if (Result=TRUE) then
                  begin
                    u32Tick:=(GetTickCount() - u32Tick) + 1;
                    Self.fSpeed:=BytesRead / u32Tick;
                  end;
            end;
        CloseHandle(hDevice);
      end;
end;


////////////////////////////////////////////////////////////////////////////////
//LowLevel auf das Laufwerk schreiben
//Es muß IMMER mindestens ein Sektor gelesen werden, da es sonst nicht funktioniert
function TDiskIO.RawWrite(Offset : unsigned64; Buffer:Pointer; BytesToWrite:unsigned32; var BytesWritten:unsigned32):Boolean;
var
  hdevice          : THandle;
  DistanceToMoveHi : unsigned32;
  DistanceToMoveLo : unsigned32;
  u32Tick          : unsigned32;
begin
    Result:=FALSE;
    Self.fSpeed:=0;

    //Dummheit der User abfangen
    if (Self.WriteProtected=FALSE) then
      begin
      //Device Öffnen
      hDevice:=CreateFile(pchar('\\.\PhysicalDrive'+IntToStr(Self.u8Drive)),
                          GENERIC_WRITE,
                          FILE_SHARE_READ OR FILE_SHARE_WRITE,
                          nil, OPEN_EXISTING,
                          FILE_FLAG_WRITE_THROUGH,
                          0);

      //OK ?
      if (hDevice <> INVALID_HANDLE_VALUE) then
        begin
          //Offset bestimmen
          DistanceToMoveLo:=unsigned32(Offset and MASK_UNSIGNED32);
          DistanceToMoveHi:=unsigned32(Offset shr WIDTH_OF_UNSIGNED32);

          //Und bewegen
          MySetFilePointer(hdevice,DistanceToMoveLo, @DistanceToMoveHi, FILE_BEGIN);
          if (GetLastError = NO_ERROR) then
              begin
                  //Benchmark starten
                  u32Tick:=GetTickCount();

                  Result:=WriteFile(hDevice,
                                    Buffer^,
                                    cardinal(BytesToWrite),
                                    cardinal(BytesWritten),
                                   nil);

                //Benchmark bestimmen
                if (Result=TRUE) then
                  begin
                    u32Tick:=(GetTickCount() - u32Tick) + 1;
                    Self.fSpeed:=BytesWritten / u32Tick;
                  end;
              end;
          CloseHandle(hDevice);
        end;
      end;
end;

end.
