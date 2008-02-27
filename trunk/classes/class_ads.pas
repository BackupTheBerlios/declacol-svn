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

unit class_ads;

interface
uses unit_typedefs,windows;

const
     //Die StreamIDs ein wenig umrubeln, um mit der Backup-Syntax nicht
     //zu kollidieren
     STREAM_INVALID            = $00000000;
     STREAM_STANDARD           = $00000001;
     STREAM_EXTENDED_ATTRIBUTE = $00000002;
     STREAM_DATA               = $00000004;
     STREAM_LINK               = $00000005;
     STREAM_OBJECT             = $00000007;
     STREAM_PROPERTY           = $00000006;
     STREAM_REPARSE            = $00000008;
     STREAM_SECURITY           = $00000003;
     STREAM_SPARSE             = $00000009;

//Ein paar bekannte Streams
//Windows Voransicht
//     INDEX_ID = '{4c8cc155-6c1e-11d1-8e41-00c04fb9386d}';
//     THUMB_ID = #05+'Q30lsldxJoudresxAaaqpcawXc';



//Definition eines Streameintrages
type TStreamRecord = record
     sStreamName    : LongString;               //Name des Streams
     u32StreamType  : unsigned32;               //Type des Stream
     sStreamType    : Longstring;               //Type des Streams als "Name"
     s64StreamSize  : signed64;                 //Größe des Streams
end;

//Unsere eigentliche Klasse
type TADSObject = class(TObject)
     private
            aStreamRecords : array of TStreamRecord;
            u32StreamCount : unsigned32;
            sFilename      : Longstring;

            function  GetStreamName  (Index:unsigned32) : Longstring;
            function  GetStreamRecord(Index:unsigned32) : TStreamRecord;
            function  ID2Str         (u32ID:unsigned32) : Longstring;
            procedure Clear();

     protected
            procedure Add(rStream:TStreamRecord);

     public
          //Ergebnisse
          property  Stream     [Index : unsigned32] : TStreamRecord     read GetStreamRecord;
          property  StreamName [Index : unsigned32] : LongString        read GetStreamName;
          property  StreamCount                     : unsigned32        read u32StreamCount;
          property  Filename                        : Longstring        read sFilename;

          //Konstruktor / Destructor
          constructor create(sFilename:Longstring='');
          destructor  free();

          //Streams enumerieren
          function  Enum(sFilename:Longstring):Boolean;
end;


//Da die Deklaration in Windows.Pas falsch ist, hier die richtige
//hardlinked. Funktioniert nicht bei Win9X
function BackupSeek( hFile: THandle;
                     dwLowBytesToSeek, dwHighBytesToSeek: DWORD;
                     var lpdwLowByteSeeked, lpdwHighByteSeeked: DWORD;
                     var lpContext: Pointer): BOOL; stdcall; external 'Kernel32.dll' name 'BackupSeek'; 

implementation

////////////////////////////////////////////////////////////////////////////////
/// Konstruktor / Destruktor
////////////////////////////////////////////////////////////////////////////////
constructor TADSObject.create(sFilename:Longstring);
begin
     //Wenn ein Name übergeben wurde, diese Datei direkt scannen
     if (sFilename<>'') then
        begin
             Self.Enum(sFilename);
        end;
end;

destructor TADSObject.free();
begin
     Self.Clear;
end;

////////////////////////////////////////////////////////////////////////////////
/// Methoden / Properties
////////////////////////////////////////////////////////////////////////////////
//Den Record eines Streams lesen
function TADSObject.GetStreamRecord(Index:unsigned32):TStreamRecord;
begin
     //Defaultwerte setzen
     result.sStreamName     := '';
     result.u32StreamType  := STREAM_INVALID;
     result.sStreamType    := Self.ID2Str(STREAM_INVALID);
     result.s64StreamSize     := 0;

     //Nur im zugelassenen Bereich lesen
     if (Index < Self.u32StreamCount) then
        begin
             result:=Self.aStreamRecords[Index];
        end;
end;


//Den Namen eines Streams lesen
function TADSObject.GetStreamName(Index:unsigned32):Longstring;
begin
     result:='';
     //Nur im zugelassenen Bereich lesen
     if (Index < Self.u32StreamCount) then
        begin
             result:=Self.aStreamRecords[Index].sStreamName;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Interne Funktionen
////////////////////////////////////////////////////////////////////////////////

//Alle Ströme einer Datei enumerieren
function  TADSObject.Enum(sFilename:Longstring):Boolean;
var
   rStreamHeader : WIN32_STREAM_ID;
   u32BytesToRead: unsigned32;
   u32Read       : DWORD;
   u32High       : DWORD;
   u32Low        : DWORD;
   Handle        : TFilehandle;
   pContext      : Pointer;
   MyStream      : TStreamRecord;
   wStreamName   : Array of WideChar;
   bStream       : Boolean;
begin
     result:=FALSE;

     //Alte Daten löschen
     Self.Clear();

     //Dateiname merken
     Self.sFilename:=sFilename;

     //Datei öffnen
     Handle:=CreateFile( PChar(sFilename),
                         GENERIC_READ,
                         FILE_SHARE_READ OR FILE_SHARE_WRITE,
                         nil,
                         OPEN_EXISTING,
                         FILE_FLAG_BACKUP_SEMANTICS,
                         0);
     //Los gehts
     if (Handle <> INVALID_HANDLE_VALUE) then
        begin
             pContext :=nil;
             bStream  :=TRUE;

             //Leseweite bestimmen
             //(Nur den Header ohne Namen)
             u32BytesToRead:=unsigned32(@rStreamHeader.cStreamName[0]) - unsigned32(@rStreamHeader.dwStreamId);

             //Header lesen
             while (BackupRead(Handle,(@rStreamHeader),u32BytesToRead,u32Read,FALSE,FALSE,pContext)) and
                   (bStream) do
                   begin
                        //Was gelesen ?
                        if (u32Read>0) then
                           begin
                                //Der Stream hat einen Namen ?
                                if (rStreamHeader.dwStreamNameSize>0) then
                                   begin
                                        //Speicher anlegen
                                        SetLength(wStreamName,rStreamHeader.dwStreamNameSize+SizeOf(wChar));

                                        //Und Name lesen
                                        BackupRead(Handle,@wStreamName[0],Length(wStreamName),u32Read,FALSE,FALSE,pContext);

                                        //StreamDaten speichern
                                        u32Read:=rStreamHeader.dwStreamNameSize-rStreamHeader.Size;
                                        MyStream.s64StreamSize:=rStreamHeader.Size;
                                        MyStream.u32StreamType:=rStreamHeader.dwStreamId;

                                        //Den WideChar in Ansi wandlen und dabei das erste Zeichen ':' ignorieren
                                        MyStream.sStreamName:=WideCharToString(@wStreamName[1]);
                                        //Und am zweiten Doppelpunkt abschneiden
                                        MyStream.sStreamName:=Copy(MyStream.sStreamName,1,Pos(':',MyStream.sStreamName)-1);

                                        //Dem Array zufügen
                                        Self.Add(MyStream);

                                        //Speicher freigeben
                                        SetLength(wStreamName,0);

                                        result:=TRUE;
                                   end;

                                //Über alle Grenzen suchen...
                                //Die Funktion bleibt von selbst am nächsten StreamHeader stehen
                                BackupSeek(Handle,$ffffffff,$0fffffff,u32Low,u32High,pContext);

                                //Wenn aber eine Größenangabe zurückgegeben wurde, stehen wir
                                //vor dem nächsten Stream
                                bStream:=(u32Low>0) or (u32High>0);

                           end
                        else
                           begin
                                //EOF
                                break;
                           end;
                   end;

             //Backup-Heap freigeben
             BackupRead(Handle,Addr(rStreamHeader),SizeOf(rStreamHeader),u32Read,TRUE,FALSE,pContext);

             //Datei schließen
             CloseHandle(Handle);
        end;
end;


//Alle Namen im Array löschen
procedure TADSObject.Clear();
var
   u32Count : unsigned32;
begin
     //Wegen Lazarus die String löschen
     u32Count:=0;
     while (u32Count < Self.u32StreamCount) do
           begin
                Self.aStreamRecords[u32Count].sStreamName:='';
                inc(u32Count);
           end;

     //Zähler löschen
     Self.u32StreamCount:=0;

     //Speicher freigeben
     SetLength(Self.aStreamRecords,0);
end;

//Einen Record zum Array zufügen
procedure TADSObject.Add(rStream:TStreamRecord);
begin
     //Neu setzen
     SetLength(aStreamRecords,u32StreamCount+1);

     //Typname dekodieren
     rStream.sStreamType:=Self.ID2Str(rStream.u32StreamType);

     //Und Daten anfügen
     Self.aStreamRecords[Self.u32StreamCount]:=rStream;

     //Größevariable anpassen
     inc(Self.u32StreamCount);
end;

//Eine TypeID in einen Namen wandeln
function TADSObject.Id2Str(u32ID:unsigned32):Longstring;
begin
     case (u32ID) of
          STREAM_INVALID            : result:='invalid stream';
          STREAM_STANDARD           : result:='standard data';
          STREAM_EXTENDED_ATTRIBUTE : result:='extended attribute';
          STREAM_DATA               : result:='alternate data';
          STREAM_LINK               : result:='link';
          STREAM_OBJECT             : result:='object';
          STREAM_PROPERTY           : result:='property';
          STREAM_REPARSE            : result:='reparse';
          STREAM_SECURITY           : result:='security data';
          STREAM_SPARSE             : result:='sparse data';
     else
          result:='unknown';
     end;
end;

end.
