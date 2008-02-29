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

unit class_CheckSum;
{$IFDEF FPC}
        //Einstellungen für den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
////////////////////////////////////////////////////////////////////////////////
/// Prüfsummen/Hash Klasse (c) 2006 Borg@Sven-of-Nine.de
///
/// CRC32 / Adler32 / MD5
///
///
////////////////////////////////////////////////////////////////////////////////

interface

uses
    unit_typedefs,          //Allgemeine Typendefinitionen
    sysutils,               //Wegen IntToHex
    class_bufferedreader,    //Um Dateien auszulesen
    class_checksum_tables,   //Hilfstabellen zur schnelleren Berechnung
    unit_bits,               //Roll-Befehle in SHA1
    windows
    ;


const BUFFER_SIZE = 1024 * 8;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Datenstrukturen
//////////////////////////////////////////////////////////////////////////////////////////////////////
//Hilfstypen für CRC32
type TCRC32Struct   = record
                 //Hilfsvariable
                 u32CRC32    : Unsigned32;
                 //Ergebnisse als Hexstring und Unsigned32
                 sChecksum   : String;
                 u32Checksum : unsigned32;
end;

//Hilfstypen für Adler32
type TAdler32Struct = record
                 //Hilfsvariablen
                 u32Adler1   : unsigned32;
                 u32Adler2   : unsigned32;
                 //Ergebnisse als Hexstring und Unsigned32
                 sChecksum   : LongString;
                 u32Checksum : unsigned32;
end;

//Hilfstypen für MD5
type
        TMD5Count = array[0..1]  of unsigned32;
        TMD5State = array[0..3]  of unsigned32;
        TMD5Block = array[0..15] of unsigned32;
        TMD5CBits = array[0..7]  of byte;
        TMD5Digest = array[0..15]of byte;
        TMD5Buffer = array[0..63]of byte;
        TMD5Struct = record
                   State     : TMD5State;
                   Count     : TMD5Count;
                   Buffer    : TMD5Buffer;
                   aChecksum : TMD5Digest;
                   sChecksum : Longstring;
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////
//Adler32-Klasse
//////////////////////////////////////////////////////////////////////////////////////////////////////
type TAdler32 = class (TObject)
          private
                 MyCheck : TAdler32Struct;
          protected
                //Die eigentliche Berechnung
                procedure Calc (p: pointer; u32Size: unsigned32);
          public
                constructor create();
                //Die Prüfsummen initialisieren
                procedure init();

                //Einen String bestimmen und der Prüfsumme zufügen
                procedure add(sInput  : Longstring); overload;
                procedure add(u8Input : Byte);       overload;
                procedure add(pbuffer : Pointer;Size:unsigned32); overload;


                //Die Endgültige Prüfsumme berechnen
                function  finalize():TAdler32Struct;

                //Einen String in eine Prüfsumme umrechnen
                function fromstring(sInput:Longstring):TAdler32Struct;

                //Die Prüfsumme einer Datei berechnen
                function fromfile(sInput:Longstring):TAdler32Struct;

                //Das Ergebnis
                property Result : TAdler32Struct read MyCheck;
     end;


//////////////////////////////////////////////////////////////////////////////////////////////////////
//CRC32-Klasse
//////////////////////////////////////////////////////////////////////////////////////////////////////
type TCRC32 = class (TObject)
          private
                MyCheck : TCRC32Struct;
          protected
                //Die eigentliche Berechnung
                procedure Calc (p: pointer; u32Size: unsigned32);
          public
                constructor create();
                //Die Prüfsummen initialisieren
                procedure init();

                //Einen String bestimmen und der Prüfsumme zufügen
                procedure add(sInput  : Longstring); overload;
                procedure add(u8Input : Byte);       overload;
                procedure add(pbuffer : Pointer;Size:unsigned32); overload;

                //Die Endgültige Prüfsumme berechnen
                function  finalize():TCRC32Struct;

                //Einen String in eine Prüfsumme umrechnen
                function fromstring(sInput:Longstring):TCRC32Struct;

                //Die Prüfsumme einer Datei berechnen
                function fromfile(sInput:Longstring):TCRC32Struct;

                //Das Ergebnis
                property Result : TCRC32Struct read MyCheck;
     end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Tiger-Klasse
//////////////////////////////////////////////////////////////////////////////////////////////////////
type TMD5 = class (TObject)
          private
                MyCheck : TMD5Struct;
                PADDING : TMD5Buffer;
          protected
                //Die eigentliche Berechnung
                procedure Calc (p: pointer; u32Size: unsigned32);

                //Hilfsfunktionen
                procedure Transform(Buffer: pointer; var State: TMD5State);
                function  F(x, y, z: unsigned32): unsigned32;
                function  G(x, y, z: unsigned32): unsigned32;
                function  H(x, y, z: unsigned32): unsigned32;
                function  I(x, y, z: unsigned32): unsigned32;
                procedure rot(var x: unsigned32; n: BYTE);
                procedure FF(var a: unsigned32; b, c, d, x: unsigned32; s: BYTE; ac: unsigned32);
                procedure GG(var a: unsigned32; b, c, d, x: unsigned32; s: BYTE; ac: unsigned32);
                procedure HH(var a: unsigned32; b, c, d, x: unsigned32; s: BYTE; ac: unsigned32);
                procedure II(var a: unsigned32; b, c, d, x: unsigned32; s: BYTE; ac: unsigned32);
                procedure Encode(Source, Target: pointer; Count: longword);
                procedure Decode(Source, Target: pointer; Count: longword);

          public
                constructor create();
                //Die Prüfsummen initialisieren
                procedure init();

                //Einen String bestimmen und der Prüfsumme zufügen
                procedure add(sInput  : Longstring); overload;
                procedure add(u8Input : Byte);       overload;
                procedure add(pbuffer : Pointer;Size:unsigned32);       overload;

                //Die Endgültige Prüfsumme berechnen
                function  finalize():TMD5Struct;

                //Einen String in eine Prüfsumme umrechnen
                function fromstring(sInput:Longstring):TMD5Struct;

                //Die Prüfsumme einer Datei berechnen
                function fromfile(sInput:Longstring):TMD5Struct;

                //Das Ergebnis
                property Result : TMD5Struct read MyCheck;
     end;


//////////////////////////////////////////////////////////////////////////////////////////////////////

implementation


//////////////////////////////////////////////////////////////////////////////////////////////////////
/// Den ADLER32-Wert für einen Datensatz erzeugen
//////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TAdler32.create();
begin
     Self.Init();
end;

//Checksumme initialisieren
procedure TAdler32.Init();
begin
     //ADLER32-Struktur initialisieren
     with Self.MyCheck do
          begin
               u32Adler1   := 1;
               u32Adler2   := 0;
               u32Checksum := 0;
               sChecksum   := '';
          end;
end;

//Einen String bestimmen und der Prüfsumme zufügen
procedure TAdler32.add(sInput : Longstring);
begin
     //Den kompletten String checken
     Self.Calc(Addr(sInput[1]),Length(sInput));
end;

procedure TAdler32.add(u8Input : Byte);
begin
     //Nur ein Byte
     Self.Calc(Addr(u8Input),1);
end;

procedure TAdler32.add(pbuffer : Pointer;Size:unsigned32);
begin
     Self.Calc(pBuffer,Size);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Checksumme bestimmen
procedure TAdler32.Calc (p: pointer; u32Size: unsigned32);
var
   pt      : ^Byte;
begin
     //Zeiger zuweisen
     pt:=p;

     //Kompletten Bereich durchackern
     while (u32Size > 0) do
           begin
                //Aus den CRC-Tabellen Prüfsumme bestimmen
                with Self.MyCheck do
                     begin
                          u32Adler1:=(u32Adler1 + pt^)       mod 65521;
                          u32Adler2:=(u32Adler2 + u32Adler1) mod 65521;
                     end;

                //Bytepointer erhöhen
                inc(pt);

                //Zähler erhöhen
                dec(u32Size);
           end;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Endberechnung
function TAdler32.Finalize():TAdler32Struct;
begin
     with Self.MyCheck do
          begin
                 //Endberechnung machen
                 u32Checksum :=(u32Adler2 shl 16) xor + u32Adler1;

                 //Hexstring erzeugen
                 sChecksum   :=IntToHex(u32Checksum,8);
          end;
     result:=Self.Result;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Prüfsumme aus einem String berechnene
function TAdler32.FromString(sInput:LongString):TAdler32Struct;
begin
     Self.Init();

     //Den kompletten String checken
     Self.Add(sInput);

     //Ergebnis bestimmen
     Result:=Self.Finalize();
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Prüfsumme aus einer Datei berechnene
function TAdler32.FromFile (sInput:LongString) : TAdler32Struct;
var
   Reader  : TBufferedReader;
   pBuff   : Pointer;
   u32Size : unsigned32;
begin
     //ADLER32 Initialisieren
     Self.Init();

     //Variablen initialisieren
     //eigentlich nicht nötig, aber der Compiler warnt dann nicht
     pBuff   :=nil;
     u32Size :=0;

     //Reader-Objet erzeugen
     Reader:=TBufferedReader.Create();
     Reader.BufferSize:=BUFFER_SIZE;

     //Datei öffnen
     if (Reader.Open(sInput)) then
        begin
             //Filepointer an den Anfang setzen
             Reader.Reset;

             //Solange den Puffer abarbeiten, bis nichts mehr komm
             while reader.ReadBuffer(pBuff,u32Size) do
                   begin
                        Self.Calc(pBuff,u32Size);
                   end;
        end;

     //Reader entladen
     Reader.Free;

     //Ergebnis bestimmen
     Result:=Self.Finalize();
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
/// Den CRC32-Wert für einen Datensatz erzeugen
//////////////////////////////////////////////////////////////////////////////////////////////////////
//Checksumme initialisieren

//////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TCRC32.Create();
begin
     Self.Init();
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TCRC32.Init();
begin
     //CRC32-Struktur initialisieren
     with Self.MyCheck do
          begin
               u32CRC32    := $ffffffff;
               u32Checksum := 0;
               sChecksum   := '';
          end;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen String bestimmen und der Prüfsumme zufügen
procedure TCRC32.add(sInput : Longstring);
begin
     //Den kompletten String checken
     Self.Calc(Addr(sInput[1]),Length(sInput));

end;

procedure TCRC32.add(u8Input : Byte);
begin
     //Nur ein Byte
     Self.Calc(Addr(u8Input),1);
end;

procedure TCRC32.add(pbuffer : Pointer;Size:unsigned32);
begin
     Self.Calc(pBuffer,Size);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Checksumme bestimmen
procedure TCRC32.Calc (p: pointer; u32Size: unsigned32);
var
   pt      : ^Byte;
begin
     //Zeiger zuweisen
     pt:=p;

     //Kompletten Bereich durchackern
     while (u32Size > 0) do
           begin
                //Aus den CRC-Tabellen Prüfsumme bestimmen
                with Self.MyCheck do
                     u32CRC32:= ( u32CRC32 shr 8 ) xor crc32table[ ( u32CRC32 xor pt^ ) and $ff];

                //Bytepointer erhöhen
                inc(pt);

                //Zähler erhöhen
                dec(u32Size);
           end;
end;

//Endberechnung
function TCRC32.Finalize():TCRC32Struct;
begin
     with Self.MyCheck do
          begin
                 //Endberechnung machen
                 u32Checksum :=u32CRC32 xor $FFFFFFFF;

                 //Hexstring erzeugen
                 sChecksum   :=IntToHex(u32Checksum,8);
          end;
     result:=Self.MyCheck;
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////
//Prüfsumme aus einem String berechnene
function TCRC32.FromString(sInput:LongString):TCRC32Struct;
begin
     Self.Init();

     Self.Add(sInput);
     //Ergebnis bestimmen
     Result:=Self.Finalize();
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Prüfsumme aus einer Datei berechnene
function TCRC32.FromFile (sInput:LongString) : TCRC32Struct;
var
   Reader  : TBufferedReader;
   pBuff   : Pointer;
   u32Size : unsigned32;
begin
     //Initialisieren
     Self.Init();

     //Variablen initialisieren
     //eigentlich nicht nötig, aber der Compiler warnt dann nicht
     pBuff   :=nil;
     u32Size :=0;

     //Reader-Objet erzeugen
     Reader:=TBufferedReader.Create();
     Reader.BufferSize:=BUFFER_SIZE;

     //Datei öffnen
     if (Reader.Open(sInput)) then
        begin
             //Filepointer an den Anfang setzen
             Reader.Reset;

             //Solange den Puffer abarbeiten, bis nichts mehr komm
             while reader.ReadBuffer(pBuff,u32Size) do
                   begin
                        Self.Calc(pBuff,u32Size);
                   end;
        end;

     //Reader entladen
     Reader.Free;

     //Ergebnis bestimmen
     Result:=Self.Finalize();
end;



//////////////////////////////////////////////////////////////////////////////////////////////////////
/// Den MD5-Wert für einen Datensatz erzeugen
//////////////////////////////////////////////////////////////////////////////////////////////////////
//Checksumme initialisieren

//Hilfsfunktionen
function TMD5.F(x, y, z: unsigned32): unsigned32;
begin
        Result := (x and y) or ((not x) and z);
end;

function TMD5.G(x, y, z: unsigned32): unsigned32;
begin
        Result := (x and z) or (y and (not z));
end;

function TMD5.H(x, y, z: unsigned32): unsigned32;
begin
        Result := x xor y xor z;
end;

function TMD5.I(x, y, z: unsigned32): unsigned32;
begin
        Result := y xor (x or (not z));
end;

procedure TMD5.rot(var x: unsigned32; n: BYTE);
begin
        x := (x shl n) or (x shr (32 - n));
end;

procedure TMD5.FF(var a: unsigned32; b, c, d, x: unsigned32; s: BYTE; ac: unsigned32);
begin
        inc(a, F(b, c, d) + x + ac);
        rot(a, s);
        inc(a, b);
end;

procedure TMD5.GG(var a: unsigned32; b, c, d, x: unsigned32; s: BYTE; ac: unsigned32);
begin
        inc(a, G(b, c, d) + x + ac);
        rot(a, s);
        inc(a, b);
end;

procedure TMD5.HH(var a: unsigned32; b, c, d, x: unsigned32; s: BYTE; ac: unsigned32);
begin
        inc(a, H(b, c, d) + x + ac);
        rot(a, s);
        inc(a, b);
end;

procedure TMD5.II(var a: unsigned32; b, c, d, x: unsigned32; s: BYTE; ac: unsigned32);
begin
        inc(a, I(b, c, d) + x + ac);
        rot(a, s);
        inc(a, b);
end;

// Encode Count bytes at Source into (Count / 4) unsigned32s at Target
procedure TMD5.Encode(Source, Target: pointer; Count: longword);
var
        S: ^Byte;
        T: ^unsigned32;
        I: longword;
begin
        S := Source;
        T := Target;
        for I := 1 to Count div 4 do begin
                T^ := S^;
                inc(S);
                T^ := T^ or (S^ shl 8);
                inc(S);
                T^ := T^ or (S^ shl 16);
                inc(S);
                T^ := T^ or (S^ shl 24);
                inc(S);
                inc(T);
        end;
end;

// Decode Count unsigned32s at Source into (Count * 4) Bytes at Target
procedure TMD5.Decode(Source, Target: pointer; Count: longword);
var
        S: ^unsigned32;
        T: ^Byte;
        I: longword;
begin
        S := Source;
        T := Target;
        for I := 1 to Count do begin
                T^ := S^ and $ff;
                inc(T);
                T^ := (S^ shr 8) and $ff;
                inc(T);
                T^ := (S^ shr 16) and $ff;
                inc(T);
                T^ := (S^ shr 24) and $ff;
                inc(T);
                inc(S);
        end;
end;

// Transform State according to first 64 bytes at Buffer
procedure TMD5.Transform(Buffer: pointer; var State: TMD5State);
var
        a, b, c, d: unsigned32;
        Block: TMD5Block;
begin
        Encode(Buffer, @Block, 64);
        a := State[0];
        b := State[1];
        c := State[2];
        d := State[3];
        FF (a, b, c, d, Block[ 0],  7, $d76aa478);
        FF (d, a, b, c, Block[ 1], 12, $e8c7b756);
        FF (c, d, a, b, Block[ 2], 17, $242070db);
        FF (b, c, d, a, Block[ 3], 22, $c1bdceee);
        FF (a, b, c, d, Block[ 4],  7, $f57c0faf);
        FF (d, a, b, c, Block[ 5], 12, $4787c62a);
        FF (c, d, a, b, Block[ 6], 17, $a8304613);
        FF (b, c, d, a, Block[ 7], 22, $fd469501);
        FF (a, b, c, d, Block[ 8],  7, $698098d8);
        FF (d, a, b, c, Block[ 9], 12, $8b44f7af);
        FF (c, d, a, b, Block[10], 17, $ffff5bb1);
        FF (b, c, d, a, Block[11], 22, $895cd7be);
        FF (a, b, c, d, Block[12],  7, $6b901122);
        FF (d, a, b, c, Block[13], 12, $fd987193);
        FF (c, d, a, b, Block[14], 17, $a679438e);
        FF (b, c, d, a, Block[15], 22, $49b40821);
        GG (a, b, c, d, Block[ 1],  5, $f61e2562);
        GG (d, a, b, c, Block[ 6],  9, $c040b340);
        GG (c, d, a, b, Block[11], 14, $265e5a51);
        GG (b, c, d, a, Block[ 0], 20, $e9b6c7aa);
        GG (a, b, c, d, Block[ 5],  5, $d62f105d);
        GG (d, a, b, c, Block[10],  9,  $2441453);
        GG (c, d, a, b, Block[15], 14, $d8a1e681);
        GG (b, c, d, a, Block[ 4], 20, $e7d3fbc8);
        GG (a, b, c, d, Block[ 9],  5, $21e1cde6);
        GG (d, a, b, c, Block[14],  9, $c33707d6);
        GG (c, d, a, b, Block[ 3], 14, $f4d50d87);
        GG (b, c, d, a, Block[ 8], 20, $455a14ed);
        GG (a, b, c, d, Block[13],  5, $a9e3e905);
        GG (d, a, b, c, Block[ 2],  9, $fcefa3f8);
        GG (c, d, a, b, Block[ 7], 14, $676f02d9);
        GG (b, c, d, a, Block[12], 20, $8d2a4c8a);
        HH (a, b, c, d, Block[ 5],  4, $fffa3942);
        HH (d, a, b, c, Block[ 8], 11, $8771f681);
        HH (c, d, a, b, Block[11], 16, $6d9d6122);
        HH (b, c, d, a, Block[14], 23, $fde5380c);
        HH (a, b, c, d, Block[ 1],  4, $a4beea44);
        HH (d, a, b, c, Block[ 4], 11, $4bdecfa9);
        HH (c, d, a, b, Block[ 7], 16, $f6bb4b60);
        HH (b, c, d, a, Block[10], 23, $bebfbc70);
        HH (a, b, c, d, Block[13],  4, $289b7ec6);
        HH (d, a, b, c, Block[ 0], 11, $eaa127fa);
        HH (c, d, a, b, Block[ 3], 16, $d4ef3085);
        HH (b, c, d, a, Block[ 6], 23,  $4881d05);
        HH (a, b, c, d, Block[ 9],  4, $d9d4d039);
        HH (d, a, b, c, Block[12], 11, $e6db99e5);
        HH (c, d, a, b, Block[15], 16, $1fa27cf8);
        HH (b, c, d, a, Block[ 2], 23, $c4ac5665);
        II (a, b, c, d, Block[ 0],  6, $f4292244);
        II (d, a, b, c, Block[ 7], 10, $432aff97);
        II (c, d, a, b, Block[14], 15, $ab9423a7);
        II (b, c, d, a, Block[ 5], 21, $fc93a039);
        II (a, b, c, d, Block[12],  6, $655b59c3);
        II (d, a, b, c, Block[ 3], 10, $8f0ccc92);
        II (c, d, a, b, Block[10], 15, $ffeff47d);
        II (b, c, d, a, Block[ 1], 21, $85845dd1);
        II (a, b, c, d, Block[ 8],  6, $6fa87e4f);
        II (d, a, b, c, Block[15], 10, $fe2ce6e0);
        II (c, d, a, b, Block[ 6], 15, $a3014314);
        II (b, c, d, a, Block[13], 21, $4e0811a1);
        II (a, b, c, d, Block[ 4],  6, $f7537e82);
        II (d, a, b, c, Block[11], 10, $bd3af235);
        II (c, d, a, b, Block[ 2], 15, $2ad7d2bb);
        II (b, c, d, a, Block[ 9], 21, $eb86d391);
        inc(State[0], a);
        inc(State[1], b);
        inc(State[2], c);
        inc(State[3], d);
end;



//////////////////////////////////////////////////////////////////////////////////////////////////////
constructor TMD5.Create();
begin
     Self.Init();
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
procedure TMD5.Init();
begin
     //CRC32-Struktur initialisieren
     with Self.MyCheck do
          begin
                State[0] := $67452301;
                State[1] := $efcdab89;
                State[2] := $98badcfe;
                State[3] := $10325476;
                Count[0] := 0;
                Count[1] := 0;
                ZeroMemory(@Buffer, SizeOf(TMD5Buffer));
          end;

     ZeroMemory(@PADDING,SizeOf(TMD5Buffer));
     Self.Padding[0]:=$80;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Einen String bestimmen und der Prüfsumme zufügen
procedure TMD5.add(sInput : Longstring);
begin
     //Den kompletten String checken
     Self.Calc(Addr(sInput[1]),Length(sInput));

end;

procedure TMD5.add(u8Input : Byte);
begin
     //Nur ein Byte
     Self.Calc(Addr(u8Input),1);
end;

procedure TMD5.add(pbuffer : Pointer;Size:unsigned32);
begin
     Self.Calc(pBuffer,Size);
end;


//////////////////////////////////////////////////////////////////////////////////////////////////////
//Checksumme bestimmen
procedure TMD5.Calc (p: pointer; u32Size: unsigned32);
var
   pb      : ^Byte;
   Index   : unsigned32;
   PartLen : unsigned32;
   I       : unsigned32;
begin
     //Rollieren
     with Self.MyCheck do
          begin
               Index := (Count[0] shr 3) and $3f;
               inc(Count[0], u32Size shl 3);
               if Count[0] < (u32Size shl 3) then
                  begin
                       inc(Count[1]);
                  end;
               inc(Count[1], u32Size shr 29);
          end;

     PartLen := 64 - Index;
     pb:=p;

     //Und den eigentlichen Check durchführen
     if u32Size >= PartLen then
        begin
             CopyMemory(@Self.MyCheck.Buffer[Index], pb, PartLen);
             Transform (@Self.MyCheck.Buffer, Self.MyCheck.State);
             I := PartLen;

             //Größe anpassen
             inc(pb,PartLen);

             while (I + 63 < u32Size) do
                   begin
                        Transform(pb, Self.MyCheck.State);
                        inc(pb,64);
                        inc(I, 64);
                   end;
             Index := 0;
        end
     else
         begin
              I := 0;
              pb:= p;
         end;
     CopyMemory(@Self.MyCheck.Buffer[Index], pb,u32Size - I);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Endberechnung
function TMD5.Finalize():TMD5Struct;
var
        Bits  : TMD5CBits;
        Index : unsigned32;
        PadLen: unsigned32;
begin
        Decode   (@Self.MyCheck.Count, @Bits, 2);
        Index := (Self.MyCheck.Count[0] shr 3) and $3f;
        if (Index < 56) then
           begin
                PadLen := 56 - Index
           end
        else
           begin
                PadLen := 120 - Index;
           end;

        Self.Calc(@PADDING, PadLen);
        Self.Calc(@Bits, 8);
        Decode(@Self.MyCheck.State, @Self.MyCheck.aChecksum, 4);
//        ZeroMemory(@MD5Struct, SizeOf(MD5Context));

        //Und den String erzeugen
        Self.MyCheck.sChecksum:='';
        for Index:=0 to 15 do
            begin
                 Self.MyCheck.sChecksum:=Self.MyCheck.sChecksum + IntToHex(Self.MyCheck.aChecksum[Index],2);
            end;
        result:=Self.MyCheck;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Prüfsumme aus einem String berechnene
function TMD5.FromString(sInput:LongString):TMD5Struct;
begin
     Self.Init();

     Self.Add(sInput);
     //Ergebnis bestimmen
     Result:=Self.Finalize();
end;

//////////////////////////////////////////////////////////////////////////////////////////////////////
//Prüfsumme aus einer Datei berechnene
function TMD5.FromFile (sInput:LongString) : TMD5Struct;
var
   Reader  : TBufferedReader;
   pBuff   : Pointer;
   u32Size : unsigned32;
begin
     //Initialisieren
     Self.Init();

     //Variablen initialisieren
     //eigentlich nicht nötig, aber der Compiler warnt dann nicht
     pBuff   :=nil;
     u32Size :=0;

     //Reader-Objet erzeugen
     Reader:=TBufferedReader.Create();
     Reader.BufferSize:=BUFFER_SIZE;

     //Datei öffnen
     if (Reader.Open(sInput)) then
        begin
             //Filepointer an den Anfang setzen
             Reader.Reset;

             //Solange den Puffer abarbeiten, bis nichts mehr komm
             while reader.ReadBuffer(pBuff,u32Size) do
                   begin
                        Self.Calc(pBuff,u32Size);
                   end;
        end;


     //Reader entladen
     Reader.Free;

     //Ergebnis bestimmen
     Result:=Self.Finalize();
end;


end.

