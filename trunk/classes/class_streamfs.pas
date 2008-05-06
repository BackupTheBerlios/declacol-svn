unit class_streamfs;
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

////////////////////////////////////////////////////////////////////////////////
///
/// Klasse um auf ein Pseudo-Dateisystem zuzugreifen welches in einer Datei kpl.
/// abgelegt wird. Damit können z.B. alle Resourcen
/// (Grafiken,Sounds,etc) in einer Datei abgelegt und zur Laufzeit geladen werden.
/// Eine einfache XOR-Verschlüsselung hält neugierige Augen erstmal fern.
/// Aus Geschwindigkeitsgründen ist eine Kompression nicht implementiert
///
/// ToDo :
/// Bei jedem Schreibvorgang wird die Verschlüsselung neu initialisiert. Damit
/// bilden identische Dateisequenzen eine Angriffsmöglichkeit. XOR-Stream muß
/// überarbeitet werden
/// FileCheck nicht mit Export machen sondern direkt aus der Datenbank prüfen
/// Prüfung auf freien Speicher bei Export
/// Defragmentierung kann auch im DB-File selbst geschehen. Sollte überarbeitet
/// werden.
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,windows,sysutils,classes,class_binder;

const
     //Maximale länge der Dateinamen
     MAX_FILENAME = 64;

     //Standargröße der Schreib/Lese-Puffer
     MAX_BUFFER   = 8192 * 10;

     //Speichermethoden
     CRYPT_NONE    = 0;
     CRYPT_XOR     = 1;

     //Richtung der Verschlüsselung
     CRYPT_ENCODE  = 1 shl 17;
     CRYPT_DECODE  = 1 shl 18;

     //Streamtype die benutzt werden können
     STREAM_FILE   = 1;
     STREAM_MEMORY = 2;
     STREAM_SOCKET = 3;
     STREAM_BINDER = 4;

     //Header der Dateien kann beliebig aussehen
     FSDB_HEADER = '[$fsdb1$]';

     //Magic-Number eines FAT-Eintrages (unsigned16)
     FAT_HEADER   = $1661;

     //Test-Werte für den Hash-Algorithmus
     ADLER_CHECK_STRING = 'the quick brown fox jumps over the lazy dog';
     ADLER_CHECK_VALUE  = 1631326202;

     //Fehlercodes
     FSDB_FAT_FILE_NOT_FOUND = $ffffffff;
     FSDB_FAT_FILE_EXISTS    = $fffffffe;

     //Eigene Fehler auf Windows-Codes gemappt
     FSDB_ERROR_NONE               = NO_ERROR;                    //Kein Fehler
     FSDB_ERROR_NO_ARCHIVE         = ERROR_UNRECOGNIZED_MEDIA;    //Die gewünschte Datei ist kein FSArchiv
     FSDB_ERROR_WRITEPROTECTED     = ERROR_WRITE_PROTECT;       //Kann nicht schreiben
     FSDB_ERROR_NO_ARCHIVE_MOUNTED = ERROR_NOT_READY;             //Es ist kein Archiv gemounted
     FSDB_ERROR_FILE_NOT_FOUND     = ERROR_FILE_NOT_FOUND;        //Die Datei wurde nicht gefunden
     FSDB_ERROR_NO_ACCESS          = ERROR_ACCESS_DENIED;         //Zugriff auf die Datei verweigert
     FSDB_ERROR_ARCHIVE_MOUNTED    = ERROR_DEVICE_IN_USE;         //Es ist schon ein Archiv gemounted
     FSDB_ERROR_INVALID_FILESIZE   = ERROR_FILE_CORRUPT;          //Datei zu groß oder zu klein
     FSDB_ERROR_WRITE              = ERROR_WRITE_FAULT;           //Es ist ein Fehler beim schreiben aufgetreten
     FSDB_ERROR_FAT_TABLE_INVALID  = ERROR_DISK_CORRUPT;          //Es konnte kein FAT-Eintrag erzeugt werden
     FSDB_ERROR_FAT_ENTRY_INVALID  = ERROR_INVALID_INDEX;         //Es konnte kein FAT-Eintrag erzeugt werden
     FSDB_ERROR_FILE_EXISTS        = ERROR_FILE_EXISTS;           //Datei existiert schon im System
     FSDB_ERROR_READ               = ERROR_READ_FAULT;            //Es konnten nicht alle Daten gelesen werden
     FSDB_ERROR_FILEHASH           = ERROR_CRC;                   //Die Prüfsumme einer Datei ist falsch
     FSDB_ERROR_BUSY               = ERROR_BUSY_DRIVE;            //Es wird schon auf das System zugegriffen
     FSDB_ERROR_LOCKED             = 1234;                        //System ist gelockt (Wiederholung erfolgt automatisch)      

//Aufbau eines FAT-Eintrages
type pFSEntry = ^TFSEntry;
     TFSEntry = packed Record

     u16Magic    : unsigned16;          //Header eines Eintrages
     aFilename   : packed array[0..MAX_FILENAME] of Char;
     u64Position : unsigned32;          //Position im Dateisystem   
     u64FileSize : unsigned64;          //Größe der Datei
     u64Offset   : unsigned64;          //Sprungweite zum nächsten Dateibeginn (gerechnet ab dem Ende des Headers)
     u32Method   : unsigned32;          //Speichermethode (gepackt / verschlüsselt / etc)
     u32Attrib   : unsigned32;          //Dateiattribute (werden im StreamFS nicht benutzt und stehen frei zur Verfügung)
     u32Hash     : unsigned32;          //Prüfsumme (http://de.wikipedia.org/wiki/Adler-32)
     bEnabled    : Boolean;             //Ist der Eintrag aktiviert (Zum einfachen Verabeitung bei Dateilöschen)
     u32Check    : unsigned32;          //Prüfsumme der Headers (Generisch)
end;


type
    //CallBack on Progress
    TCallback = procedure(Data : unsigned32; Text : Longstring) of Object;

    //Die eigentliche Klasse
    TStreamFS = Class(TObject)
     private
            tStream   : TStream;
            sStream   : Longstring;
            u32Stream : unsigned32;

            //Dateiliste (einziger begrenzender Faktor der Dateizahl)
            tFAT      : TList;

            //Simples locking
            bLocked   : Boolean;

            //Aktuelle Speichermethode
            u32Method : unsigned32;
            sPassword : longstring;

            //Sollen auch Header verschlüsselt werden ?
            bCryptHead: boolean;

            //Letzter aufgetretener Fehler
            u32Error  : unsigned32;

            //Zugriffsmodus
            bReadable : boolean;
            bWritable : boolean;
            bMounted  : boolean;

            //Callbackzeiger
            fOnProgress : TCallback;
            fOnLocked   : TCallback;

            //Buffer für Zufallswerte
            Scrambler   : array[0..High(unsigned8)] of unsigned8;
            ScramblerX  : unsigned8;
            ScramblerY  : unsigned8;

            //Streamfunktionen
            function  openfilestream  (DBFile : longstring):boolean;
            function  openmemorystream():boolean;
            function  opensocketstream(DBFile : longstring):boolean;
            function  openbinderstream():boolean;
            function  getstreamsize():unsigned64;

            //FAT-Funktionen
            procedure clearfat();
            function  loadfat():boolean;
            function  delfat(index  : unsigned32):boolean; overload;
            function  delfat(fsname : longstring):boolean; overload;
            function  seekfat(fsname:longstring):unsigned32;

            function  addfat(Data:TFSEntry):unsigned32;  //Fügt einen FAT-Eintrag in die interne Liste ein
            function  findfreespace(FIleSize : unsigned64):unsigned32; //Sucht nach einer freien Stelle in der FAT 

            //Prüfsumme erzeugen
            function hash      (Data:TStream):unsigned32;
            function headerhash(fsEntry : TFSEntry):unsigned32;

            //Zugriffe auf eine Datei prüfen
            function iswritable(filename : longstring):boolean;
            function isreadable(filename : longstring):boolean;

            //Locking
            procedure getlock();
            procedure releaselock();

            //Temporäre Dateinamen erzeugen
            function createtempfilename():longstring;

            //Aus einem Stream in einen anderen kopieren
            function copystream(Source : TStream; SourceOffset:unsigned64; SourceSize : unsigned64; Target:TStream; TargetOffset : unsigned64; encode : unsigned32; streamname : longstring):unsigned64;

            //Einen FAT-Eintrag schreiben/Lesen
            function writeheader(fsEntry     : TFSEntry; OutStream : TStream; encode : unsigned32):Boolean;
            function readheader(var fsEntry : TFSEntry; InStream  : TStream; encode : unsigned32):Boolean;

            //Fehler setzen
            procedure seterror(Error : unsigned32);

            //Einen string in ein FS-konformes Array wandeln
            procedure validatename(FSName:Longstring;pArray : Pointer);

            //Verschlüsselungsinterface
            procedure cryptinit(method:unsigned32);
            procedure cryptdo(Method:unsigned32; pBuffer :pointer; Size : unsigned32);

            //Zufallsgenerator  (nach http://de.wikipedia.org/wiki/Rc4)
            procedure rndinit(Key:longstring);
            function  rndbyte():unsigned8;

            //Die Verschlüsselungsprovider für XOR-PseudeOneTimePad
            procedure init_enc_xor();
            procedure init_dec_xor();
            procedure enc_xor(var pBuffer : Pointer; var Size : unsigned32);
            procedure dec_xor(var pBuffer : Pointer; var Size : unsigned32);

            //Fortschritt ausgeben
            procedure DoProgress(Data:unsigned64; MaxData:unsigned64; Text : Longstring);
            procedure DoLock(Text : Longstring);
     public
            constructor create();
            destructor  free();

            //Ein Dateisystem mounten
            function    open(DBFile : longstring):boolean;
            //System zurücksetzen (neu mounten)
            procedure   reset();
            //Dateisystem unmounten
            procedure   close();

            //Dateifunktionen
            function  save(fsname:longstring; Data:TStream):boolean;
            function  load(fsname:longstring):TStream;
            function  exists(fsname:longstring):boolean;
            function  enumfiles  ():TStringList;
            function  info(fsname:longstring):TFSEntry;
            function  delete(fsname:longstring):boolean;

            //Stringfunktionen (Strings werden im System wie Dateien behandelt)
            function    loadstring(fsname:longstring):longstring;
            function    savestring(fsname:longstring; data:longstring):boolean;

            //Prüf und Reinigungsfunktionen
            function  check(fsname:longstring):boolean;
            function  defrag():boolean;
            function  format():boolean;

            //Eine Datei von der Festplatte ins archiv schreiben
            function  store (filename :longstring;fsname:longstring):boolean;
            //Eine Datei auf die Festplatte schreiben
            function  restore (fsname :longstring;filename:longstring):boolean;

            //Selbsttest der Klasse
            function systemtest():boolean;

            //Letzter aufgetretener Fehler
            property error       : unsigned32 read u32error;
            //Welche Verschlüsselungsmethode soll gewählt werden
            property crypt       : unsigned32 read u32method  write u32method;
            property password    : longstring read sPassword  write sPassword;
            property cryptheader : boolean    read bcrypthead write bcrypthead;

            //Ist das Dateisystem geladen
            property mounted     : boolean    read bMounted;
            //Welche Rechte haben wir
            property writable    : boolean    read bWritable;
            property readable    : boolean    read bReadable;

            //Was für ein Stream soll benutzt werden ?
            property streamtype  : unsigned32 read u32Stream write u32Stream;
            property streamsize  : unsigned64 read GetStreamSize;
            property streamname  : longstring read sStream;

            //Fortschritt
            property OnProgress  : TCallback read fOnProgress write fOnProgress;
            property OnLocked    : TCallback read fOnLocked   write fOnLocked;
end;


implementation

////////////////////////////////////////////////////////////////////////////////
//Konstruktor
constructor TStreamFS.create();
begin
     inherited Create();

     Self.tFAT:=TList.Create();

     //Default ist immer verschlüsseln
     Self.sPassWord:='hasnabopf';
     Self.crypt:=CRYPT_XOR;
     Self.cryptheader:=FALSE;

     //Default ist Dateistream 
     Self.StreamType:=STREAM_FILE;

     Self.bLocked:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////
//Destruktor
destructor  TStreamFS.free();
begin
     Self.Close();
     Self.clearfat();
     Self.tFAT.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Den Stream zurücksetzen
procedure   TStreamFS.reset();
begin
     Self.Close();

     if (Self.sStream <> '') then
        begin
             Self.Open(Self.sStream);
        end;

     //Unlock erzwingen
     Self.bLocked:=FALSE;
end;


function TStreamFS.open(DBFile : longstring):boolean;
begin
     case (Self.StreamType) of
          STREAM_FILE     : result:=Self.openfilestream(DBFile);
          STREAM_MEMORY   : result:=Self.openmemorystream();
          STREAM_BINDER   : result:=Self.openbinderstream();
          STREAM_SOCKET   : result:=Self.openfilestream(DBFile);
     else
         begin
              result:=Self.openfilestream(DBFile);
         end;
     end;
end;


////////////////////////////////////////////////////////////////////////////////
//Getter für StreamSize
function  TStreamFS.getstreamsize():unsigned64;
begin
     if (bMounted) then
        begin
             result:=Self.tStream.Size;
        end
     else
        begin
             result:=0;
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Stream auf die Datenbank erzeugen oder eine neue Datenbank erstellen
function    TStreamFS.openfilestream(DBFile : longstring):boolean;
var
   hFile    : TFileHandle;
   aHeader  : array[0..Length(FSDB_HEADER)-1] of char;
begin
     Self.GetLock();

     Self.SetError(FSDB_ERROR_NONE);
     result:=FALSE;

     //Nicht noch ein anderer Zugriff offen ?
     if (Self.tStream = nil) then
        begin
             Self.bReadable := FALSE;
             Self.bWritable := FALSE;

             //Wenn die Datei nicht existiert versuchen wir sie zu erstellen
             if (not fileexists(DBFile)) then
                begin
                     hFile:=FileCreate(DBFile);
                     if (hFile <> INVALID_HANDLE_VALUE) then
                        begin
                             //Header schreiben
                             FileWrite(hFile,FSDB_HEADER[1],Length(FSDB_HEADER));
                             FileClose(hFile);
                        end;
                end;

             //Gibt es die Datei ?
             if (fileexists(DBFile)) then
                begin
                     //Zugriff erlaubt ?
                     if (Self.isreadable(DBFile)) then
                        begin
                             if (Self.iswritable(DBFile)) then
                                begin
                                     Self.tStream:=TFileStream.Create(DBFile,fmOpenReadWrite);
                                     Self.bReadable:=TRUE;
                                     Self.bWritable:=TRUE;
                                end
                             else
                                begin
                                     Self.tStream:=TFileStream.Create(DBFile,fmOpenRead);
                                     Self.bReadable:=TRUE;
                                     Self.bWritable:=FALSE;
                                end;

                             //Header checken und evlt Zugriff killen
                             Self.tStream.Position:=0;
                             Self.tStream.Read(aHeader[0],Length(FSDB_HEADER));
                             if (string(aHeader)<> FSDB_HEADER) then
                                begin
                                     Self.tStream.Free();
                                     Self.tStream:=nil;
                                     Self.bReadable:=FALSE;
                                     Self.bWritable:=FALSE;
                                end;

                        end
                     else
                        begin
                             Self.SetError(FSDB_ERROR_NO_ACCESS);
                        end;
                end
             else
                begin
                     Self.SetError(FSDB_ERROR_FILE_NOT_FOUND);
                end;

             //Gemounted ?
             Self.bMounted:=Self.tStream <> nil;
             result:=Self.bMounted;

             //FAT laden
             if (bMounted) AND (bReadable) then
                begin
                     //Name merken
                     Self.sStream:=DBFile;
                     Self.ReleaseLock();
                     Self.LoadFat;
                     Self.GetLock();
                end;
        end
     else
        begin
             Self.SetError(FSDB_ERROR_ARCHIVE_MOUNTED);
        end;

     Self.ReleaseLock();
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Speicherstream mounten
function TStreamFS.openmemorystream():boolean;
begin
     Self.GetLock();

     Self.SetError(FSDB_ERROR_NONE);
     result:=FALSE;

     //Nicht noch ein anderer Zugriff offen ?
     if (Self.tStream = nil) then
        begin
             Self.tStream:=TMemoryStream.Create();
             Self.tStream.Write(FSDB_HEADER[1],SizeOf(FSDB_HEADER));

             Self.bReadable:=TRUE;
             Self.bWritable:=TRUE;
             Self.bMounted :=TRUE;
             result:=Self.bMounted;

             //FAT laden
             if (bMounted) AND (bReadable) then
                begin
                     //Name merken
                     Self.sStream:='memorystream';
                     Self.ReleaseLock();
                     Self.LoadFat;
                     Self.GetLock();
                end;
        end
     else
        begin
             Self.SetError(FSDB_ERROR_ARCHIVE_MOUNTED);
        end;

     Self.ReleaseLock();
end;


////////////////////////////////////////////////////////////////////////////////
//Einen Socketstream mounten
//So9Todo
function    TStreamFS.opensocketstream(DBFile : longstring):boolean;
var
   hFile    : TFileHandle;
   aHeader  : array[0..Length(FSDB_HEADER)-1] of char;
begin
{
     Self.GetLock();

     Self.SetError(FSDB_ERROR_NONE);
     result:=FALSE;

     //Nicht noch ein anderer Zugriff offen ?
     if (Self.tStream = nil) then
        begin
             Self.tStream:=TFileStream.Create(DBFile,fmOpenReadWrite);
             Self.bReadable:=TRUE;
             Self.bWritable:=TRUE;

             //Gemounted ?
             Self.bMounted:=Self.tStream <> nil;
             result:=bMounted;


             //FAT laden
             if (bMounted) AND (bReadable) then
                begin
                     //Name merken
                     Self.sStream:=DBFile;
                     Self.ReleaseLock();
                     Self.LoadFat;
                     Self.GetLock();
                end;
        end
     else
        begin
             Self.SetError(FSDB_ERROR_ARCHIVE_MOUNTED);
        end;

     Self.ReleaseLock();
}
 result:=FALSE;
end;

function  TStreamFS.openbinderstream():boolean;
var
   aHeader  : array[0..Length(FSDB_HEADER)-1] of char;
begin
     Self.tStream:=TBinderStream.Create();

     //Header checken und evlt Zugriff killen
     Self.tStream.Seek(0,soFromBeginning);
     Self.tStream.Read(aHeader[0],Length(FSDB_HEADER));

     aHeader[0]:=Char(Self.tStream.Position);

     if (string(aHeader)<> FSDB_HEADER) then
        begin
             Self.tStream.Free();
             Self.tStream:=nil;
             Self.bReadable:=FALSE;
             Self.bWritable:=FALSE;
        end
     else
        begin
             Self.bReadable:=TRUE;
             Self.bWritable:=FALSE;
             Self.bMounted :=TRUE;
             Self.sStream:='binderstream';
             Self.ReleaseLock();
             Self.LoadFat;
             Self.GetLock();
        end;
        
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Einfach den Stream löschen und alles wieder freigeben
procedure   TStreamFS.close();
begin
     if (Self.tStream <> nil) then
        begin
             GetLock();
             Self.tStream.Free();
             Self.tStream:=nil;

             Self.bMounted :=FALSE;
             Self.bReadable:=FALSE;
             Self.bWritable:=FALSE;

             ReleaseLock();

             //Die FAT-Tabelle entladen
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Dateifunktionen
function  TStreamFS.save(fsname:longstring; Data : TStream):boolean;
var
   u32Index  : unsigned32;
   pEntry    : pFSEntry; 
begin
     result:=FALSE;
     //Dateisystem verfügbaer
     if (bMounted) then
        begin
          //Schreibberechtigung
          if (bWritable) then
             begin
                  //Datei mit Inhalt
                  if (Data.Size > 0) then
                     begin
                          //Name existiert noch nicht
                          if (Self.seekfat(fsName)=FSDB_FAT_FILE_NOT_FOUND) then
                             begin
                                  //Die nächste freie Stelle im Dateisystem suchen
                                  u32Index:=Self.FindFreeSpace(Data.Size);

                                  if (u32Index <> FSDB_FAT_FILE_NOT_FOUND) then
                                     begin
                                          //Zur vereinfachung die Daten ziehen
                                          pEntry:=pFSEntry(Self.tFAT[u32Index]);

                                          //Dateiname setzen
                                          Self.ValidateName(fsName,Addr(pEntry^.aFilename[0]));
                                          pEntry^.bEnabled    :=TRUE;
                                          pEntry^.u64FileSize :=unsigned64(Data.Size);
                                          pEntry^.u32Method   :=Self.u32Method;
                                          pEntry^.u32Hash     :=Self.Hash(Data);

                                          //Hinspringen und einfach alle Daten abspeichern
                                          Self.tStream.Seek(pEntry^.u64Position,soFromBeginning);
                                          Data.Seek(0,soFromBeginning);

                                          //Header schreiben
                                          Self.writeheader(pEntry^,Self.tStream,pEntry^.u32Method);

                                          //Und die Daten hinterher
                                          if (Self.copystream( Data,
                                                               0,
                                                               Data.Size,
                                                               Self.tStream,
                                                               pEntry^.u64Position + SizeOf(TFSEntry),
                                                               pEntry^.u32Method OR CRYPT_ENCODE,
                                                               pEntry^.aFilename
                                                               ) = Data.Size) then
                                             begin
                                                  SetError(FSDB_ERROR_NONE);
                                                  result:=TRUE;
                                             end
                                          else
                                             begin
                                                  SetError(FSDB_ERROR_WRITE);
                                             end;
                                     end
                                  else
                                     begin
                                          SetError(FSDB_ERROR_FILE_EXISTS);
                                     end;
                             end
                          else
                             begin
                                  SetError(FSDB_ERROR_FAT_TABLE_INVALID);
                             end;
                     end
                  else
                     begin
                          //Dateigröße = 0
                          Self.SetError(FSDB_ERROR_INVALID_FILESIZE);
                     end;
                end
             else
                begin
                     //Dateisystem readonly gemountd
                     Self.SetError(FSDB_ERROR_WRITEPROTECTED);
                end;
        end
     else
        begin
             //Kein Dateisystem verfügbar
             Self.SetError(FSDB_ERROR_NO_ARCHIVE_MOUNTED);
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Datei aus der Datenbank ziehen
//dazu kopieren wir die Datei in einen Memorystream und liefern diesen zurück
function  TStreamFS.load(fsname:longstring):TStream;
var
   u32Index : unsigned32;
   fsEntry  : TFSEntry;
begin
     //Den Eintrag im FAT suchen
     u32Index:=Self.SeekFat(fsname);

     Result:=TMemoryStream.Create();

     if (u32Index <> FSDB_FAT_FILE_NOT_FOUND) then
        begin
             fsEntry:=pFSEntry(Self.tFAT[u32Index])^;
             Self.GetLock();

             if ( Self.copystream( Self.tStream,
                                   (fsEntry.u64Position + SizeOf(TFSEntry)),
                                   fsEntry.u64FileSize,
                                   Result,
                                   0,
                                   fsEntry.u32Method OR CRYPT_DECODE,
                                   fsEntry.aFilename
                                   ) = fsEntry.u64FileSize) then
                begin
                     Self.SetError(FSDB_ERROR_NONE);
                end
             else
                begin
                     Self.SetError(FSDB_ERROR_READ);
                end;
             Self.ReleaseLock();
        end
     else
        begin
             Self.SetError(FSDB_ERROR_FILE_NOT_FOUND);
        end;

     //Memorystream zurückspulen
     Result.Seek(0,soFromBeginning);
end;


////////////////////////////////////////////////////////////////////////////////
//Stringfunktionen einfach auf die Streamfunktionen abbilden             
function    TStreamFS.loadstring(fsname:longstring):longstring;
var
   MemStr   : TMemoryStream;
begin
     MemStr:=TMemoryStream(Self.load(fsName));
     result:=StringOfChar(#00,MemStr.Size);
     MemStr.Seek(0,soFromBeginning);
     MemStr.Read(Result[1],MemStr.Size);
     MemStr.Free();
end;

function    TStreamFS.savestring(fsname:longstring; Data : Longstring):boolean;
var
   MemStr : TMemoryStream;
begin
     MemStr:=TMemoryStream.Create();
     MemStr.WriteBuffer(Data[1],Length(Data));
     Result:=Self.save(fsName,MemStr);
     MemStr.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Prüfen, ob eine Datei existiert
function  TStreamFS.exists(fsname:longstring):boolean;
begin
     result:=Self.SeekFat(fsName) <> FSDB_FAT_FILE_NOT_FOUND;
end;


////////////////////////////////////////////////////////////////////////////////
//Alle Dateien im Archiv zurückliefern
function    TStreamFS.enumfiles():TStringList;
var
   u32Index : unsigned32;
begin
     u32Index:=0;
     result:=TStringList.Create();

     if (Self.bMounted) then
        begin
             while (u32Index < unsigned32(Self.tFAT.Count)) do
                   begin
                        if (pFSEntry(Self.tFAT[u32Index])^.bEnabled) then
                           begin
                                Result.Add( string( pFSEntry(Self.tFAT[u32Index])^.aFilename ) );
                           end;
                        inc(u32Index);
                   end;
           end;
     Result.Sort();
end;

////////////////////////////////////////////////////////////////////////////////
//Infostrukt für eine Datei ziehen
function  TStreamFS.info(fsname:longstring):TFSEntry;
var
   u32Index : unsigned32;
begin
     u32Index:=Self.seekfat(fsname);
     if (u32Index <> FSDB_FAT_FILE_NOT_FOUND) then
        begin
             result:=pFSEntry(Self.tFAT[u32Index])^;
             Self.SetError(FSDB_ERROR_NONE);
        end
     else
        begin
             Self.SetError(FSDB_ERROR_FILE_NOT_FOUND);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Datei löschen
function  TStreamFS.delete(fsname:longstring):boolean;
begin
     result:=Self.delfat(fsName);
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Datei dem Archiv zufügen
function TStreamFS.store (filename:longstring;fsname:longstring):boolean;
var
   FileStr  : TFileStream;
begin
     result :=FALSE;

     if (fileexists(filename)) then
        begin
             if (Self.bWritable) then
                begin
                     FileStr:=TFileStream.Create(filename,fmOpenRead OR fmShareDenyNone);
                     result:=Self.save(fsname,FileStr);
                     FileStr.Free();
                end
             else
                begin
                     Self.SetError(FSDB_ERROR_WRITEPROTECTED);
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Datei aus dem Archiv auf die Festplatte speichern
//Um den Vorgang zu Beschleunigen und nicht so viel Speicher zu verbrauchen
//umgehen wird die Load Methode und schreiben direkt aus dem Datnbankstream
function    TStreamFS.restore (fsname:longstring;filename:longstring):boolean;
var
   FileStr   : TFileStream;
   u32Index  : unsigned32;
   fsEntry   : pFSEntry;
begin
     result :=FALSE;
     u32Index:=Self.seekfat(fsname);
     if (u32Index <> FSDB_FAT_FILE_NOT_FOUND) then
        begin
             //Eintrag ziehen
             fsEntry:=Self.tFat[u32Index];

             try
                FileStr:=TFileSTream.Create(filename,fmCreate);

                if (Self.CopyStream( Self.tStream,
                                     fsEntry^.u64Position + SizeOf(TFSEntry),
                                     fsEntry^.u64FileSize,
                                     FileStr,
                                     0,
                                     fsEntry^.u32Method OR CRYPT_DECODE,
                                     fsEntry^.aFilename
                                     )=fsEntry^.u64FileSize) then
                   begin
                        Self.SetError(FSDB_ERROR_NONE);
                        result:=TRUE;
                   end
                else
                   begin
                        Self.SetError(FSDB_ERROR_WRITE);
                   end;
                FileStr.Free();
             except
             end;
        end
     else
        begin
             Self.SetError(FSDB_ERROR_FILE_NOT_FOUND);
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Die Tabelle aus der Datenbank extrahieren
function TStreamFS.loadfat():boolean;
var
   bOK     : boolean;
   fsEntry : TFSEntry;
begin
     if (bReadable) then
        begin
             //Erfolg annehmen
              Self.SetError(FSDB_ERROR_NONE);

             //Alten FAT löschen
             Self.ClearFat();

             GetLock();

             //An der Anfang springen
             Self.tStream.Seek(Length(FSDB_HEADER),soFromBeginning);

             //Die Datenbank MUß mit einem Dateiheader beginnen
             bOK:=Self.readheader(fsEntry,Self.tStream,Self.u32Method);  
             while (bOK) do
                   begin
                        //Position im Datenstrom puffern
                        FSEntry.u64Position:=unsigned64(Self.tStream.Position) - SizeOf(fsEntry);

                        //Und abspeichern
                        Self.AddFat(FSEntry);

                        //Auf den nächsten Eintrag vorspulen
                        Self.tStream.Seek(fsEntry.u64Offset,soFromCurrent);

                        //Kommt noch was ?
                        bOK:=Self.readheader(fsEntry,Self.tStream,Self.u32Method);
                   end;
             //Ein Fehler ausser FAT_FILE_NOT_FOUND aufgetreten ?
             if (Self.u32Error = FSDB_FAT_FILE_NOT_FOUND) then
                begin
                     Self.SetError(FSDB_ERROR_NONE);
                end;

             ReleaseLock();
        end
     else
        begin
             Self.SetError(FSDB_ERROR_READ);
        end; 

     result:=Self.u32Error = FSDB_ERROR_NONE;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag aus der FAT nehmen
function  TStreamFS.delfat(fsname : longstring):boolean;
begin
     result:=Self.delfat(Self.seekfat(fsname));
end;

////////////////////////////////////////////////////////////////////////////////
//Dabei werden nicht die Dateien gelöscht sonder einfach nur der Eintrag deaktiviert
function  TStreamFS.delfat(index  : unsigned32):boolean;
var
   pEntry : pFSEntry;
begin
     result:=FALSE;

     if (index < unsigned32(Self.tFAT.Count)) then
        begin
             pEntry:=Self.tFAT[index];
             //Eintrag deaktivieren
             if (pEntry^.bEnabled) then
                begin
                     pEntry^.bEnabled:=FALSE;

                     //Und Änderungen auf die Platte schreiben
                     Self.GetLock();
                     Self.tStream.Seek(pEntry^.u64Position,soFROMBeginning);
                     Self.writeheader(pEntry^,Self.tStream,pEntry.u32Method);
                     Self.ReleaseLock();

                     result:=TRUE;
                end
             else
                begin
                     SetError(FSDB_ERROR_FILE_NOT_FOUND);
                end;
        end
     else
        begin
             SetError(FSDB_ERROR_FILE_NOT_FOUND);
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Nach einer Datei im FAT suchen
function TStreamFS.seekfat (fsname:longstring):unsigned32;
var
   u32Index : unsigned32;
begin
     //Fehler annehmen
     result:=FSDB_FAT_FILE_NOT_FOUND;

     fsName:=copy(fsName,1,MAX_FILENAME);

     //Nach dem Index suchen und dann über die Indexfunktion deaktivieren
     if (fsname <> '') then
        begin
             u32Index:=0;
             while ( (u32Index < unsigned32(Self.tFAT.Count)) and (result = FSDB_FAT_FILE_NOT_FOUND) ) do
                   begin
                        //Eintrag aktiv ?
                        if ( pFSEntry(Self.tFAT[u32Index])^.benabled ) then
                           begin
                                //Name OK ?
                                if (pFSEntry(Self.tFAT[u32Index])^.afilename[0] = fsname[1]) then
                                   begin
                                        if (string(pFSEntry(Self.tFAT[u32Index])^.afilename) = fsname) then
                                           begin
                                                result:=u32Index;
                                           end;
                                   end;
                           end;
                        inc(u32Index);
                   end;
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Ale Einträge aus der FAT löschen
procedure TStreamFS.clearfat();
begin
     Self.GetLock();

     //FAT freigeben
     while (Self.tFat.Count > 0) do
           begin
                //Speicher freigeben
                Dispose(Self.tFAT[0]);
                //Item entfernen
                Self.tFAT.Delete(0);
           end;
     Self.tFAT.Pack();
     Self.ReleaseLock();
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag in die FAT machen
function TStreamFS.AddFat(Data:TFSEntry):unsigned32;
var
   pFileEntry : pFSEntry;
begin
     pFileEntry:=new (pFSEntry);

     FillMemory(pFileEntry,SizeOf(TFSEntry),0);
     pFileEntry^.u16Magic   := FAT_HEADER;                   
     pFileEntry^.aFilename  := Data.aFilename;
     pFileEntry^.u64Position:= Data.u64Position;
     pFileEntry^.u64filesize:= Data.u64filesize;
     pFileEntry^.u64offset  := Data.u64offset;
     pFileEntry^.u32hash    := Data.u32hash;
     pFileEntry^.u32attrib  := Data.u32attrib;
     pFileEntry^.u32method  := Data.u32method;
     pFileEntry^.benabled   := Data.benabled;

     //Den Index zurückliefern
     result:=unsigned32 ( Self.tFAT.Add(pFileEntry) );
end;

////////////////////////////////////////////////////////////////////////////////
//Nach einer Lücke im Dateisystem suchen
function TStreamFS.findfreespace(FileSize:unsigned64):unsigned32;
var
   u32Index : unsigned32;
   u64Space : unsigned64;
   fsEntry  : TFSEntry;
begin
     Self.SetError(FSDB_ERROR_NONE);

     //Fehler annehmen
     result:=FSDB_FAT_FILE_NOT_FOUND;

     //Die aktuelle FAT durchsuchen, ob etwas passendes dabei ist
     u32Index:=0;
     u64Space:=high(unsigned64);
     while (u32Index < unsigned32(Self.tFAT.Count)) AND (u64Space <> 0) do
           begin
                //Ein nutzloser Eintrag ?
                if (not pFSEntry(Self.tFAT[u32Index])^.bEnabled) then
                   begin
                        //Dateigröße passt ?
                        if (pFSEntry(Self.tFAT[u32Index])^.u64FileSize >= FileSize) then
                           begin
                                //Restspielraum schön klein ?
                                if ( (pFSEntry(Self.tFAT[u32Index])^.u64Offset - FileSize) < u64Space ) then
                                   begin
                                        u64Space:=pFSEntry(Self.tFAT[u32Index])^.u64Offset - Filesize;
                                        result:=u32Index;
                                   end;
                           end;
                   end;
                inc(u32Index);
           end;

     //Wenn wir keinen passenden Eintrag gefunden haben erstellen wir einen neuen
     //am Ende der Datenbank
     if (result = FSDB_FAT_FILE_NOT_FOUND) then
        begin
             //Wir geben nur den Offset und die Position vor, der Rest interessiert nicht
             //und wird später von der Save-Funktion gesetzte
             fsEntry.u64Position:=Self.tStream.Size;
             fsEntry.u64Offset  :=Filesize;
             fsEntry.bEnabled   :=TRUE;

             result:=Self.addfat(fsEntry);
        end
     else
        begin
             //Eintrag aktivieren
             pFSEntry(Self.tFAT[result])^.bEnabled:=TRUE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Datei im System prüfen
//um auch in einem ReadOnly-System den Check zuzulassen lesen wir die Datei in
//einen MemoryStream und prüfen dort.
function TStreamFS.check(fsname:longstring):boolean;
var
   FileStr  : TFileStream;
   sTemp    : longstring;
   fsEntry  : TFSEntry;
begin
     result:=FALSE;

     fsEntry:=Self.info(fsname);
     if (Self.Error = FSDB_ERROR_NONE) then
        begin
             sTemp:=Self.createtempfilename();

             if (Self.Restore(fsname,sTemp) ) then
                begin
                     FileStr:=TFileStream.Create(sTemp,fmOpenRead);
                     FileStr.Position:=0;

                     //Hash checken
                     if (Self.Hash(FileStr) = fsEntry.u32Hash) then
                        begin
                             SetError(FSDB_ERROR_NONE);
                             result:=TRUE;
                        end
                     else
                        begin
                             Self.SetError(FSDB_ERROR_FILEHASH);
                        end;

                     FileStr.Free();
                     DeleteFile(sTemp);
                end;
        end
end;

////////////////////////////////////////////////////////////////////////////////
//Defragmentieren ist ganz leicht, wir kopieren alle aktiven Blöcken um
//und ersatzen dann das kpl. Dateisystem
function TStreamFS.defrag():boolean;
var
   u32Index : unsigned32;
   fsEntry  : TFSEntry;
   FileStr  : TFileStream;
   sTemp    : Longstring;
begin
     result:=FALSE;
     Self.SetError(FSDB_ERROR_NONE);

     if (Self.bMounted) then
        begin
             if (Self.bWritable) then
                begin
                     try
                        Self.GetLock();

                        //Temp Datei erzeugen
                        sTemp:=Self.createtempfilename();
                        FileStr:=TFileStream.Create(sTemp,fmCreate);

                        //Header schreiben
                        FileStr.Seek(0,soFromBeginning);
                        FileStr.Write(FSDB_HEADER[1],Length(FSDB_HEADER));

                        //Alle FAT-Einträge durchgehen
                        u32Index:=0;
                        result:=TRUE;
                        while (u32Index < unsigned32(Self.tFAT.Count)) do
                              begin
                                   fsEntry:=pFSEntry(Self.tFAT[u32Index])^;

                                   Self.DoProgress(u32Index,Self.tFat.Count,fsEntry.aFileName);

                                   //Nur aktive Einträge benutzen
                                   if (fsEntry.bEnabled) then
                                      begin
                                           //FAT-Eintrag anpassen und schreiben

                                           //Um Lücken zu entfernen wird der Offset angepasst
                                           fsEntry.u64Offset  :=fsEntry.u64FileSize;
                                           Self.writeheader(fsEntry,FileStr,fsEntry.u32Method);

                                           //Und die Daten hinterher
                                           //Ohne verschlüsselung, um eine evtl. schon vorliegende
                                           //verschlüsselung nicht zu ändern
                                           result:=Self.CopyStream( Self.tStream,
                                                                    fsEntry.u64Position + SizeOf(TFSEntry),
                                                                    fsEntry.u64FileSize,
                                                                    FileStr,
                                                                    FileStr.Position,
                                                                    CRYPT_NONE,
                                                                    fsEntry.aFilename
                                                                    ) = fsEntry.u64FileSize;

                                           //Bei einem Fehler brechen wir sofort ab
                                           if (result=FALSE) then
                                              begin
                                                   u32Index:=High(unsigned32) - 1;
                                                   SetError(FSDB_ERROR_WRITE);
                                                   result:=FALSE;
                                              end;
                                      end;
                                   inc(u32Index);
                             end;
                             
                        //Alles OK, dann Datei umkopieren
                        if (result = TRUE) then
                           begin
                                Self.tStream.Size:=0;
                                Self.CopyStream(FileStr,0,FileStr.Size,Self.tStream,0,CRYPT_NONE,'moving tempfile');

                                //Alles neu einlesen
                                Self.ReleaseLock();
                                Self.Close();
                                Self.open(Self.sStream);
                                Self.GetLock();
                           end;
                        //Alles wieder freigeben
                        Self.ReleaseLock();
                        FileStr.Free();
                        DeleteFile(sTemp);
                     except
                        //Nix machen   
                     end;
                end
             else
                begin
                     Self.SetError(FSDB_ERROR_WRITEPROTECTED);
                end;
        end
     else
        begin
             Self.SetError(FSDB_ERROR_NO_ARCHIVE_MOUNTED);
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Interner Test
function    TStreamFS.systemtest():boolean;
var
   u32Size  : unsigned32;
   u32Count : unsigned32;
   MyStream : TMemoryStream;
   sTemp    : longstring;
   u32Do    : unsigned32;
begin
     u32Do:=8192;

     //Adler32 checken
     MyStream:=TMemoryStream.Create();
     MyStream.Write(ADLER_CHECK_STRING,Length(ADLER_CHECK_STRING));
     u32Count:=Self.Hash(MyStream);
     result:=ADLER_CHECK_VALUE = u32Count;
     MyStream.Free();

     //SaveString Checken
     u32Size:=0;
     while (u32Size < u32Do) do
           begin
                //Zufallsstring bauen
                sTemp:='';
                for u32Count:=0 to u32Size do
                    begin
                         sTemp:=sTemp + Chr( Random(255) );
                    end;

                //Speichern
                Self.savestring(IntToStr(u32Size)+'???',sTemp);
                result:=result AND (Self.loadstring(IntToStr(u32Size)+'???') = sTemp);

                inc(u32Size,16);
           end;

     //Alle Einträge wieder löschen
     u32Size:=0;
     while (u32Size < u32Do) do
           begin
                result:=result AND Self.delete(IntToStr(u32Size)+'???');
                inc(u32Size,16);
           end;

     //Prüfen ob sie wirklich gelöscht sind
     u32Size:=0;
     while (u32Size > 0) do
           begin
                result:=result AND Not Self.exists(IntToStr(u32Size)+'???');
                inc(u32Size,16);
           end;

     //FindFreeSpace checken, indem wir Rückwarts der Größ nach abspeichern
     u32Size:=u32Do;
     while (u32Size > 0) do
           begin
                //Zufallsstring bauen
                sTemp:='';
                for u32Count:=0 to u32Size do
                    begin
                         sTemp:=sTemp + Chr( Random(255) );
                    end;

                //Speichern
                Self.savestring(IntToStr(u32Size)+'???',sTemp);
                result:=result AND (Self.loadstring(IntToStr(u32Size)+'???') = sTemp);

                dec(u32Size,16);
           end;

     //Alle Einträge wieder löschen
     u32Size:=u32Do;
     while (u32Size > 0) do
           begin
                result:=result AND Self.delete(IntToStr(u32Size)+'???');
                dec(u32Size,16);
           end;

     //Prüfen ob sie wirklich gelöscht sind
     u32Size:=u32Do;
     while (u32Size > 0) do
           begin
                result:=result AND Not Self.exists(IntToStr(u32Size)+'???');
                dec(u32Size,16);
           end;

     //Aufräumen
     Self.Defrag();
end;



//Das Dateisystem klarmachen
function TStreamFS.format():boolean;
begin
     if (Self.bMounted) then
        begin
             if (Self.bWritable) then
                begin
                     Self.ClearFAT();
                     Self.tStream.Size:=0;
                     Self.tStream.Write(FSDB_HEADER[1],Length(FSDB_HEADER));
                end
             else
                begin
                     Self.SetError(FSDB_ERROR_WRITEPROTECTED);
                end;
        end
     else
        begin
             Self.SetError(FSDB_ERROR_NO_ARCHIVE_MOUNTED);
        end;

     result:=Self.Error = FSDB_ERROR_NONE;
end;

////////////////////////////////////////////////////////////////////////////////
//Prüfsumme eines Stream erzeugen
//Im Moment eine optimierte Form von Adler32
function    TStreamFS.hash(Data:TStream):unsigned32;
var
   u32Adler1   : unsigned32;
   u32Adler2   : unsigned32;
   u32Size     : unsigned32;
   u32Index    : unsigned32;
   aBuffer     : Array [0..5400] of Byte; //Schwelle bis zum Overflow ist 5500
begin
     u32Adler1:=1;
     u32Adler2:=0;
     Data.Seek(0,soFromBeginning);

     u32Size:=Data.Read(aBuffer[0],Length(aBuffer));
     while ( u32Size > 0) do
           begin
                dec(u32Size);
                for u32Index:=0 to u32Size do
                    begin
                         u32Adler1:=u32Adler1 + aBuffer[u32Index];
                         u32Adler2:=u32Adler2 + u32Adler1;
                    end;

                //Vor dem Overflow schon Mod bestimmen
                u32Adler1 := u32Adler1 mod 65521;
                u32Adler2 := u32Adler2 mod 65521;

                Self.DoProgress(Data.Position,Data.Size,'hashing');

                //Neue Daten lesen
                u32Size:=Data.Read(aBuffer[0],Length(aBuffer));
           end;

     result:=( (u32Adler2 AND MASK_UNSIGNED16) shl 16) OR u32Adler1;
end;


//Für den Header eine einfache Prüfsumme bilden
function TStreamFS.headerhash(fsEntry : TFSEntry):unsigned32;
begin
     result := fsEntry.u64Position AND MASK_UNSIGNED32 XOR
               fsEntry.u64Offset   AND MASK_UNSIGNED32 XOR
               fsEntry.u32Method                       XOR
               fsEntry.u32Attrib                       XOR
               fsEntry.u32Hash;
end;

////////////////////////////////////////////////////////////////////////////////
//Zugrifssmöglichkeiten auf eine Datei bestimmen
function TSTreamFS.iswritable(filename : longstring):boolean;
var
   hFile : TFileHandle;
begin
     hFile:=FileOpen(filename,fmOpenReadWrite or fmShareDenyNone);
     FileCLose(hFIle);

     result:=hFile <> INVALID_HANDLE_VALUE;
end;

function TStreamFS.isreadable(filename : longstring):boolean;
var
   hFile : TFileHandle;
begin
     hFile:=FileOpen(filename,fmOpenRead or fmShareDenyNone);
     FileClose(hFIle);

     result:=hFile <> INVALID_HANDLE_VALUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Fehlerbehandlung
procedure TStreamFS.seterror(Error : unsigned32);
begin
     Self.u32Error:=Error;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen String in ein passendes Array bringen
procedure TSTreamFS.validatename(FSName:Longstring;pArray : Pointer);
begin
     fsName:=copy(fsName,1,MAX_FILENAME);
     FillMemory(pArray,MAX_FILENAME,0);
     CopyMemory(pArray,Addr(fsName[1]),Length(fsName));
end;

////////////////////////////////////////////////////////////////////////////////
//Simples Locking (aber leider auch etwas fehlerträchtig)
//daher ist aufwändiges Debuggen unumgänglich
procedure   TStreamFS.getlock();
begin
     while (Self.bLocked) do
           begin
                Self.DoLock('locked for 100ms');
                sleep(100);
           end;
     Self.bLocked:=TRUE;
end;

procedure   TStreamFS.releaselock();
begin
     Self.bLocked:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Temporäre Dateiname erzeugen
function TStreamFS.createtempfilename():Longstring;
var
   aTemp : array[0..MAX_PATH] of Char;
begin
     if (GetTempPath(MAX_PATH,aTemp) > 0) then
        begin
             if (GetTempFileName(aTemp,'sf_',0,aTemp) > 0) then
                begin
                     result:=trim(string(aTemp));
                end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Aus einem Stream in einen anderen kopieren
//zurückgegeben wird die Zahl der kopierten Bytes
//Mit encode kann eine Enkodiermethode mitübergeben werden

function TStreamFS.copystream(Source : TStream; SourceOffset:unsigned64; SourceSize : unsigned64; Target:TStream; TargetOffset : unsigned64; encode : unsigned32; streamname : longstring):unsigned64;
var
   aBuffer : array[0..MAX_BUFFER] of Byte;
   u32Loop : unsigned32;
   u32Mod  : unsigned32;
   u32Size : unsigned32;
begin
     result:=0;
     if ( (Source.Seek(SourceOffset,soFromBeginning) = SourceOffset) AND
          (Target.Seek(TargetOffset,soFromBeginning) = TargetOffset)) then
          begin
               //Anzahl der vollen Buffer
               u32Loop:=SourceSize div Length(aBuffer);
               u32Mod :=SourceSize mod Length(aBuffer);

               Self.cryptinit(encode);

               //Einfach die Daten in den Stream kopieren
               while (u32Loop > 0) do
                     begin
                          u32Size:=Source.Read(aBuffer[0],Length(aBuffer) );

                          Self.cryptdo(encode, Addr(aBuffer[0]) ,u32Size);

                          Target.Write(aBuffer[0] ,u32Size);
                          inc(result,u32Size);

                          //Fortschritt ausgeben
                          Self.DoProgress(result,SourceSize,streamname);

                          dec(u32Loop);
                     end;

               //Der Rest
               u32Size:=Source.Read(aBuffer[0],u32Mod );

               Self.cryptdo(encode, Addr(aBuffer[0]) ,u32Size);

               Target.Write(aBuffer[0] ,u32Size);
               inc(result,u32Size);

               //Fehler setzen
               if (result = SourceSize) then
                  begin
                       SetError(FSDB_ERROR_NONE);
                  end
               else
                  begin
                       SetError(FSDB_ERROR_WRITE);
                  end; 
         end
     else
         begin
               SetError(FSDB_ERROR_READ OR FSDB_ERROR_WRITE);
         end;

     //100% melden
     Self.DoProgress(100,100,streamname);
end;

////////////////////////////////////////////////////////////////////////////////
//Einen FAT-Header schreiben und evtl. kodieren
function TStreamFS.writeheader(fsEntry : TFSEntry; OutStream : TStream; encode : unsigned32):Boolean;
begin
     //Prüfsumme bilden
     fsEntry.u32Check:=Self.headerhash(fsEntry);

     //Verschlüsseln ?
     if (Self.bCryptHead) then
        begin
             Self.cryptinit(encode OR CRYPT_ENCODE);
             Self.cryptdo  (encode OR CRYPT_ENCODE , Addr(fsEntry) ,SizeOf(TFSEntry));
        end;

     result:=OutStream.Write(fsEntry,SizeOf(fsEntry)) = SizeOf(fsEntry);

     if (result = FALSE) then
        begin
             Self.SetError(FSDB_ERROR_WRITE);
        end
     else
        begin
             Self.SetError(FSDB_ERROR_NONE);
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Einen FAT-Header lesen und auf Konsistenz analysieren
function TStreamFS.readheader(var fsEntry : TFSEntry; InStream : TStream; encode : unsigned32):Boolean;
var
   u32Size : unsigned32;
begin
     //Header lesen
     u32Size:=InStream.Read(fsEntry,SizeOf(TFSEntry));

     if (u32Size = SizeOf(TFSENtry)) then
        begin
             //Entschlüsseln ?
             if (Self.bCryptHead) then
                begin
                     Self.cryptinit(encode OR CRYPT_DECODE);
                     Self.cryptdo(encode OR CRYPT_DECODE , Addr(fsEntry) ,Sizeof(TFSEntry));
                end;

             //Header und Hash OK ?
             if (fsEntry.u16Magic <> FAT_HEADER) OR
                (fsEntry.u32Check <> Self.headerhash(fsEntry) )then
                begin
                     //FAT-Eintrag defekt
                     Self.SetError(FSDB_ERROR_FAT_ENTRY_INVALID);
                     result:=FALSE;
                end
             else
                begin
                     Self.SetError(FSDB_ERROR_NONE);
                     result:=TRUE;
                end;
        end
     else
        begin
             //Kein FAT-Eintrag gefunden
             Self.SetError(FSDB_FAT_FILE_NOT_FOUND);
             result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Datenblock anhand der Eigenschaft Method verschlüsseln
procedure  TStreamFS.cryptinit(Method : unsigned32);
begin
     //Soll ver oder entschlüsselt werden ?
     if ( (Method AND CRYPT_ENCODE) <> 0 ) then
        begin
             case (Method AND MASK_UNSIGNED16) of
                  CRYPT_XOR  : Self.init_enc_xor();
             end;
        end
     else
        begin
             case (Method AND MASK_UNSIGNED16) of
                  CRYPT_XOR  : Self.init_dec_xor();
             end;
        end;
end;

procedure   TStreamFS.cryptdo(Method:unsigned32; pBuffer :pointer; Size : unsigned32);
begin
     //Soll ver oder entschlüsselt werden ?
     if ( (Method AND CRYPT_ENCODE) <> 0 ) then
        begin
             case (Method AND MASK_UNSIGNED16) of
                  CRYPT_XOR  : Self.enc_xor(pBuffer,Size);
             end;
        end
     else
        begin
             case (Method AND MASK_UNSIGNED16) of
                  CRYPT_XOR  : Self.dec_xor(pBuffer,Size);
             end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Zufallsgenerator  (nach http://de.wikipedia.org/wiki/Rc4)
procedure TStreamFS.rndinit(Key:longstring);
var
   u8Index  : unsigned8;
   u8swap   : unsigned8;
   u32Size  : unsigned8;
   u8Temp   : unsigned8;
begin
     u32Size:=Length(Key);

     //Scrambler initialisieren
     for u8Index:=0 to High(unsigned8) do
         begin
              Self.Scrambler[u8Index]:=u8Index;
         end;

     //Schlüssel einkneten
     u8Swap:=0;
     for u8Index:=0 to High(unsigned8) do
         begin
              u8Swap:=( (u8Swap + Self.Scrambler[u8Index] + Ord(Key[u8Index mod u32Size])) mod 256);

              u8Temp:=Self.Scrambler[u8Index];
              Self.Scrambler[u8Index]:=Self.Scrambler[u8Swap];
              Self.Scrambler[u8Swap]:=u8Temp;
         end;

     //Startvektoren initialisieren
     Self.ScramblerX:=0;
     Self.ScramblerY:=0;
end;

function  TStreamFS.rndbyte():unsigned8;
var
   u8Temp : unsigned8;
begin
     Self.ScramblerX:=( Self.ScramblerX + 1 ) mod 256;
     Self.ScramblerY:=( Self.ScramblerY + Ord(Self.Scrambler[Self.ScramblerX]) ) mod 256;

     u8Temp:=Self.Scrambler[Self.ScramblerX];
     Self.Scrambler[Self.ScramblerX]:=Self.Scrambler[Self.ScramblerY];
     Self.Scrambler[Self.ScramblerX]:=u8Temp;

     result:=Self.Scrambler[ (Self.Scrambler[Self.ScramblerX] + Self.Scrambler[Self.ScramblerY]) mod 256 ] 
end;

////////////////////////////////////////////////////////////////////////////////
//XOR verschlüsselung mit einem Bytestrom aus dem Zufallsgenerator
//Dabei wird jedes Byte mit seinem vorherigen Verknüpft wodurch eine Manipulation
//an einem Byte den kpl. restlichen Datenstrom zerstört.
procedure   TStreamFS.init_enc_xor();
begin
     Self.rndinit(Self.sPassword);
end;

procedure   TStreamFS.init_dec_xor();
begin
     Self.rndinit(Self.sPassword);
end;

procedure   TStreamFS.enc_xor(var pBuffer : Pointer; var Size : unsigned32);
var
   pByte : ^Byte;
   u8Buff: unsigned8;
   u8Temp: unsigned8;
begin
     u8Buff:=0;

     //Mit ein wenig Zeigerbasteln geht es entschieden schneller
     pByte:=pBuffer;

     while (Size > 0) do
           begin
                //Klartextbyte merken
                u8Temp:=pByte^;

                //Verschlüsseln
                pByte^:=pByte^ xor Self.rndbyte();
                pByte^:=pByte^ xor u8Buff;

                //Das Klartext-Byte für die nächste Runde merken
                u8Buff:=u8Temp;

                inc(pByte);
                dec(Size);
           end;
end;

procedure   TStreamFS.dec_xor(var pBuffer : Pointer; var Size : unsigned32);
var
   pByte : ^Byte;
   u8Buff: unsigned8;
begin
     //Switchbyte initialisieren
     u8Buff:=0;

     //Mit ein wenig Zeigerbasteln geht es entschieden schneller
     pByte:=pBuffer;

     while (Size > 0) do
           begin
                //Byte dekodieren
                pByte^:=pByte^ xor self.rndbyte();
                pByte^:=pByte^ xor u8Buff;

                //Das dekodierte Byte für die nächste Runde merken
                u8Buff:=pByte^;

                inc(pByte);
                dec(Size);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Fortschritt melden (in Prozent)
procedure TStreamFS.DoProgress(Data:unsigned64; MaxData:unsigned64; Text:longstring);
begin
     if (Assigned(Self.fOnProgress)) then
        begin
             if (MaxData = 0) then
                begin
                     Self.fOnProgress(0,Text);
                end
             else
                begin
                     Self.fOnProgress( (Data * 100) div MaxData,Text);
                end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Lock-Zustand melden
procedure TStreamFS.DoLock(Text:longstring);
begin
     if (Assigned(Self.fOnLocked)) then
        begin
            Self.fOnLocked(FSDB_ERROR_LOCKED,Text);
        end;
end;

end.




