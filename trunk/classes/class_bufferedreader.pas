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
/// Buffered Reader (c) 2006 Borg@Sven-of-Nine.de
///
/// Unit zum simplen einlesen von Dateien. Komplett portierbarer Code
///
///
/// Wichtige Hinweise :
///
/// -Alle Leseoperationen bewegen den Dateizeiger
/// -Ge�ffnete Dateien sind f�r andere Prozesse gesperrt
/// -Kein schreiben m�glich
///
/// wichtige Prototypen :
///
/// Reset            Setzt den Dateizeiger auf den Anfang
///
/// ReadFirstLine    Liest die erste durch <linefeed> begrenzte Zeile ab der Dateiposition.
/// ReadLine(index)  Liest die "index'te" Zeile.
/// ReadNextLine     Liest die n�chste "Zeile" ab der aktuellen Dateiposition.
///
/// ReadFirstByte    Liest das erste Byte der Datei
/// ReadNextByte     Liest das n�chste Byte in der Datei
/// ReadByte(index)  Liest Byte an position <index> ein.
///
/// ReadBuffer(var Pointer, Size)     F�llt den Puffer und gibt die entsprechenden Werte zur�ck
///
/// EOF:Boolean      Ende der Datei erreicht. Flag wird gesetzt, nachdem das letzte
///                  Zeichen gelesen wurde.
///
/// Linefeed:String  Definiert, nach welchem Zeilenumbruch gesucht werden soll
///
////////////////////////////////////////////////////////////////////////////////
/// Beispiel : Alle Bytes einer Datei lesen
///
/// Reader:=TBufferedReader.Create
/// Reader.Open('meinedatei.exe');
/// Reader.Reset();  //Nicht zwangsweise notwendig, aber schadet nicht
/// while ( not Reader.EOF) do
///       begin
///            writeln( Reader.ReadNextByte() );
///       end;
///
/// Beispiel : Alle Zeilen einer Unix-Datei lesen
///
/// Reader:=TBufferedReader.Create
/// Reader.Open('meinedatei.txt');
/// Reader.Reset();  //Nicht zwangsweise notwendig, aber schadet nicht
/// Reader.Linefeed:=lfUNIX;
/// while ( not Reader.EOF) do
///       begin
///            writeln( Reader.ReadNextLine() );
///       end;
///
////////////////////////////////////////////////////////////////////////////////
{$IFDEF FPC}
        //Einstellungen f�r den GPC
        {$mode objfpc}{$H+}
{$ENDIF}

unit class_bufferedreader;
////////////////////////////////////////////////////////////////////////////////
interface
uses Unit_TypeDefs; //Wegen Fileoperationen und THandle

const
     //Flags f�r die Linefeedbehandlung
     lfDOS        = #13+#10;
     lfUNIX       = #10;

     //Defaultgr��e des Lesepuffers in Byte
     READBUFFSIZE = 64 * 1024;

     //Ein paar Fehler definieren
     READER_ERROR_INVALID_HANDLE     =-1;
     READER_ERROR_SUCCESS            = 0;
     READER_ERROR_FILE_NOT_FOUND     = 1;
     READER_ERROR_NOT_ENOUGH_MEMORY  = 2;
     READER_ERROR_EOF                = 4;
     READER_ERROR_UNKNOWN            = 1024;


////////////////////////////////////////////////////////////////////////////////
// Die eigentliche Klasse
////////////////////////////////////////////////////////////////////////////////
type TBufferedReader = Class(TObject)
     private
        //Linefeedmodus
        sLineFeed   : LongString;

        //Dateihandle
        RawFile     : File of Byte;
        //Flag, ob Datei offen ist, da Assign
        //nur mit TryExcept eine Abfrage zul�sst
        bRawFile    : Boolean;

        //EOF-Flags
        bFileEOF    : Boolean;
        bBufferEOF  : Boolean;
        bEOF        : Boolean;

        //Puffergr��e
        u32BufferSize : Unsigned32;
        //Lesepuffer
        aReadBuffer : array of Byte;

        //Lesepointer
        u32ReadBuffer : Unsigned32;

        //Obere Marke des Lesepuffers
        u32BufferMax  : Unsigned32;

        //Errorstatus
        s32ErrorStatus: Signed32;

        //Fehlerbehandlung
        procedure SetError(s32Error:Signed32);

        //Puffer setzen (Nicht direkt, um Fehler abzufangen)
        procedure SetBufferSize(value:Unsigned32);

        //F�llt dern Puffer wenn n�tig und gibt die anzahl der noch im
        //Puffer verf�gbaren Bytes zur�ck
        function FillBuffer():Unsigned32;

        //Eine Zeile aus dem Puffer lesen
        function GetLine():LongString;

        //Ein Byte aus dem Puffer lesen
        function GetByte():Byte;

     public
           //Funktionen
           Constructor Create();
           Destructor  Free();

           function Open(Filename:LongString):Boolean;

           //Gibt es kein Ergebnis, wird '' zur�ckgegeben (Bei jeder Funktion)
           //Erste Zeile lesen (Setzt den Pointer wieder zur�ck);
           function ReadFirstLine():LongString;

           //N�chste Zeile lesen (erh�ht den Zeilenpointer und gibt das Ergebnis zur�ck);
           function ReadNextLine ():LongString;

           //Liest Zeile iIndex
           //Liegt der Index h�her als die tats�chliche Zeilenzahl wird
           //die letzte Zeile ausgegeben und die EOF-Flags gesetzt
           function ReadLine(s64Index:Signed64):LongString;

           //Ein Byte lesen (Analog zu ReadLine)
           function ReadFirstByte():Byte;
           function ReadNextByte():Byte;
           function ReadByte(u64Index:unsigned64):Byte;

           //Zeiger auf Puffer zur�ckgeben
           function ReadBuffer(var pByteBuffer : Pointer; var u32Size : unsigned32):Boolean;

           //An den Anfang der Datei springen
           function Reset():Boolean;

           //Datei schlie�en (Dateihandles werden ung�ltig
           function Close():Boolean;


           //Properties
           //Modus der Linefeedbehandlung
           property Linefeed : LongString read sLineFeed write sLineFeed;

           //EOF
           property EOF : Boolean read bEOF default FALSE;

           //Puffergr��e
           property BufferSize : Unsigned32 read u32BufferSize write SetBufferSize;

           //Fehlerstatus
           property Error : Signed32 read s32ErrorStatus write s32ErrorStatus default READER_ERROR_SUCCESS;
end;


implementation
uses Sysutils       //Wegen FileExists
     ;
////////////////////////////////////////////////////////////////////////////////
//Alles Initialisieren
Constructor TBufferedReader.Create();
begin
     //Linefeed default setzen
     sLineFeed:=lfDOS;

     //Puffer gro�z�gig dimensionieren
     SetBufferSize(READBUFFSIZE);

     //Dateiflag l�schen
     bRawFile:=FALSE;

     //Alles initialisieren
     Self.Reset();
end;
////////////////////////////////////////////////////////////////////////////////
//Alles freimachen
Destructor  TBufferedReader.Free();
begin
     //Handle schlie�en
     Self.Close();
     
     //Speicher freimachen
     SetLength(aReadBuffer,0);
end;


////////////////////////////////////////////////////////////////////////////////
//Datei �ffnen und den Filehandle in hFile ablegen
function TBufferedReader.Open(Filename:LongString):Boolean;
begin
     //Alten Handle schlie�en
     Self.Close();

     //Fehler annehmen
     result   :=FALSE;
     bEOF     :=TRUE;
     Self.SetError(READER_ERROR_INVALID_HANDLE);

     //Datei existiert ?
     if (not FileExists(Filename)) then
        begin
             //Ansonsten abbrechen
             exit;
        end;

     try
        //Hier Zugriff auf die Datei holen
        AssignFile(RawFile,Filename);

        //Alles OK
        bRawFile:=TRUE;
     except
           //Fehler
           bRawFile:=FALSE;
     end;


     //Alles initialisieren
     Self.Reset();

     //Fertig ?
     result:=bRawFile;
end;

////////////////////////////////////////////////////////////////////////////////
//Alles Resetten und mit dem Lesen wieder vorne anfangen
function TBufferedReader.Reset():Boolean;
begin
     //Problem annehmen     
     result:=FALSE;

     //Fehler abfangen
     if (not bRawFile) then
        begin
             Exit;
        end;

     //Zeiger resetten
     u32ReadBuffer:=0;
     u32BufferMax :=0;

     //EOF-Flags l�schen
     bFileEOF :=FALSE;
     bEOF     :=FALSE;

     //An den Dateianfang springen
     try
        System.Reset(RawFile);
     except
           //Fehlerflags setzen
            bRawFile :=FALSE;
            bEOF     :=TRUE;
     end;

     //Ergebnis setzen
     Result:=bRawFile;

     //Und den Puffer evtl. schon mal f�llen
     if (result) then Self.FillBuffer();
end;

////////////////////////////////////////////////////////////////////////////////
//Handle schlie�en und Datei freigeben
function TBufferedReader.Close():Boolean;
begin
     //Fehler annehmen
     result   :=FALSE;
     SetError(READER_ERROR_INVALID_HANDLE);

     //Gar keine Datei offen ?
     if (bRawFile<>FALSE) then
        begin
             //Einfach den Filehandle verwerfen
             CloseFile(RawFile);
        end;


     //Flag l�schen
     bRawFile:=FALSE;

     //Puffer flushen
     FillChar(aReadBuffer[0],u32BufferSize,0);
end;

////////////////////////////////////////////////////////////////////////////////
//Den Puffer f�llen, wenn es n�tig ist
function TBufferedReader.FillBuffer():Unsigned32;
var
   u32BytesToRead : Unsigned32;
   u32BytesRead   : Unsigned32;
begin
     //Fehler annehmen
     result:=0;


     //Fehler abfangen
     if (bRawFile = FALSE) then
        begin
             //Fehlerflags setzen
             bFileEOF   := TRUE;
             bBufferEOF := TRUE;

             //Und abbrechen
             exit;
        end;

     //Ist der Puffer schon aufgebraucht ?
     if (u32ReadBuffer >= u32BufferMax) or (bBufferEOF) then
        begin
             //Anzahl der zu lesenden Bytes holen
             u32BytesToRead := u32BufferSize;
             u32BytesRead   := 0;

             //Neu f�llen
             while (
                   //Dateiende nicht erreicht
                   (not System.EOF(RawFile)) and
                   //Puffer noch nicht voll
                   (u32BytesToRead > 0)
                   )do
                   begin
                        //Ist zwar bei der Byteweisen eingabe langsam,
                        //aber komplett portierbar und ohne API-Zugriffe
                        System.Read(RawFile,aReadBuffer[u32BytesRead]);

                        //N�chstes Byte lesen
                        inc(u32BytesRead);
                        
                        //Pufferz�hler anpassen
                        dec(u32BytesToRead);
                   end;

             //EOF-Flags setzen
             bFileEOF   := u32BytesToRead <> u32BytesRead;
             bBufferEOF := u32BytesRead = 0;

             //Maximaler Pegel des Puffers merken
             u32BufferMax := u32BytesRead;

             //Und Zeiger auf Start
             u32ReadBuffer:=0;
        end;
     //Anzahl der Maximalen Bytes im Puffer zur�ckgeben
     result:=u32BufferMax;
end;



////////////////////////////////////////////////////////////////////////////////
//Ein Byte lesen und wenn der Puffer durchgelesen ist, diesen neu f�llen
function TBufferedReader.GetByte():Byte;
begin
     //Nullbyte setzen
     result:=0;

     //Puffer neu f�llen (Macht er nur wenn notwendig)
     //Keine Bytes verf�gbar bedeutet Ende der Datei und der Puffers
     if (FillBuffer()> 0) then
        begin
             //Und nun einfach ein Byte holen
             result:=aReadBuffer[u32ReadBuffer];
             //Schon mal das n�chste Byte anvisieren
             inc(u32ReadBuffer);
        end;

     //Ende des Puffers erreicht ? dann gleich auff�llen
     //Passiert dies hier nicht, wird beim zusammenfallen von Pufferende und Dateiende
     //einmal zuviel gelesen, bevor EOF gesetzt wird
     FillBuffer();

     //Ende aller Daten ?
     bEOF:=bFileEOF and bBufferEOF;
end;



////////////////////////////////////////////////////////////////////////////////
//Eine Zeile bis zum Zeilenende lesen (nur intern verwendet)
function TBufferedReader.GetLine():LongString;
begin
     //Leer initialisieren
     result:='';

     //Eine Zeile bis zum Definierten Linefeed (oder EOF) lesen
     while (pos(sLineFeed,Result)=0) and (not bEOF) do
           begin
                result:=result+chr(GetByte);
           end;
end;



////////////////////////////////////////////////////////////////////////////////
//Filepointer resetten und erste Zeile lesen
function TBufferedReader.ReadFirstLine():LongString;
begin
     //Filepointer resetten
     Self.Reset();
     //Und Zeile lesen
     result:=GetLine();
end;


////////////////////////////////////////////////////////////////////////////////
//Einfach n�chste Zeile lesen
function TBufferedReader.ReadNextLine ():LongString;
begin
     result:=GetLine();
end;


////////////////////////////////////////////////////////////////////////////////
//Zeile iIndex lesen
function TBufferedReader.ReadLine(s64Index:Signed64):LongString;
begin
     //Vorne anfangen
     result:=ReadFirstLine();

     //Einfach solange lesen, bis wir da sind.
     while (s64Index > 0) do
           begin
                result:=GetLine();
                dec(s64Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Bytereader
function TBufferedReader.ReadFirstByte():Byte;
begin
     //Filepointer resetten
     Self.Reset();
     //Und Byte lesen
     result:=GetByte();
end;

////////////////////////////////////////////////////////////////////////////////
//N�chstes Byte lesen
function TBufferedReader.ReadNextByte():Byte;
begin
     //Und Byte lesen
     result:=GetByte();
end;


////////////////////////////////////////////////////////////////////////////////
//Um auch richtig gro�e Dateien zu bearbeiten
//wird hier als Index signed64 benutzt.
//32BitPascal unterst�tzt keine unsigned64 Typen
function TBufferedReader.ReadByte(u64Index:unsigned64):Byte;
begin
     //Vorne anfangen
     result:=ReadFirstByte();

     //Einfach solange lesen, bis wir da sind.
     while (u64Index > 0) do
           begin
                result:=GetByte();
                dec(u64Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
// Den aktuellen Pufferzeiger und die noch verf�gbaren Bytes zur�ckgeben
function TBufferedReader.ReadBuffer(var pByteBuffer : Pointer; var u32Size : unsigned32):Boolean;
begin
     //Noch verf�gbare Bytes holen und den Puffer f�llen, wenn es n�tig ist
     u32Size:=Self.FillBuffer()-u32ReadBuffer;

     //Zeiger auf aktuellen Pufferstand
     pByteBuffer:= Addr( aReadBuffer[u32ReadBuffer] );

     //Den ausgegebenen Buffer als burned markieren
     u32ReadBuffer:=u32BufferMax;
     
     //Erfolg signalisieren
     result:=u32Size <> 0;
end;


////////////////////////////////////////////////////////////////////////////////
//Fehlerhandling
////////////////////////////////////////////////////////////////////////////////
procedure TBufferedReader.SetError(s32Error:Signed32);
begin
     s32ErrorStatus:=s32Error;
end;


////////////////////////////////////////////////////////////////////////////////
//Properties
////////////////////////////////////////////////////////////////////////////////
//Die Puffergr��e setzen
procedure TBufferedReader.SetBufferSize(value:Unsigned32);
begin
     //Keine Null
     if (Value <         4) then Value:=4;
     //Nicht gr��er als ein Megabyte
     if (Value > 1024*1024) then Value:=1024*1024;

     //Wert auf Basis zwei setzen
     Value:=(Value div 2) * 2;

     //Speicher neu sezten
     u32BufferSize:=Value;

     //Fehler l�schen
     SetError(READER_ERROR_SUCCESS);

     //Und Speicher anfordern
     try
        SetLength(aReadBuffer,u32BufferSize);
     except
           SetError(READER_ERROR_NOT_ENOUGH_MEMORY);
     end;

     //Alles initialisieren
end;

end.
