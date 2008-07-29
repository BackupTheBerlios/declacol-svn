unit class_lzw;
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
/// Klasse um Speicherbereiche mit LZW zu komprimieren
/// Alle anderen Klassen sollten hierauf zugreifen.
/// Die öffentlichen Eigenschaften/Methoden sind für alle Kompressionsklassen gleich
/// Werden aber nicht für jede Klasse benutzt
///
/// Um die Kompression zu initialisieren wird sie einmal mit dem Pointer nil und
/// Size 0 aufgerufen. Gleiches gilt für die Dekompression
/// Output enthält einen Pointer auf den Beginn der Ausgabe. 
///
/// Der Speicher auf den Output verweist liegt intern in einem Memorystream.
/// Eine weiterer Aufruf der (Un)Pack-Funktion überschreibt alte Daten 
/// Free zerstört die Memorystreams. Damit werden die Outputpointer ungültig
///
////////////////////////////////////////////////////////////////////////////////

interface

uses unit_compiler,unit_typedefs,classes,windows,sysutils,class_rle,class_btree;

//Aufbau des Wörtebuches
type TDicEntry   = array of byte;
//Unseren eigenen Type für die Datenbreite benutzen
type TWide       = unsigned16;


//Ein paar Flags
const //Vorgaben der Wörterbuchgröße
      LZW_STORE   = 512;
      LZW_FAST    = 1024;
      LZW_NORMAL  = 16384;
      LZW_HIGH    = 32768;
      LZW_EXTREME = HIGH(TWide);

      //Min/Max Wörterbuchgröße
      LZW_MIN_DIC= High(unsigned8);
      LZW_MAX_DIC= High(TWide);


//Packen erhält eine eigene Klasse
type TLZWPack = class(TObject)
     private
           //Wörterbücher
           Dictionary : TBTree;
           u32maxdic  : unsigned32;

           //Zum Puffern benutzen wir einfach Memorystreams
           Buffer     : TMemoryStream;
     protected
           function _init ():Boolean;
           function _pack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;


           //Datenfunktionen
           function WideToSmall(data : TWide):byte;
           function SmallToWide(data : byte):TWide;
           function WideOut(data : TWide):boolean;

           //Wörterbuchfunktionen
           procedure HandleOverflow   ();
           function  InitDictionary  ():Boolean;
     public
           constructor Create();
           constructor Free();

           function pack   (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
           property maxdic : unsigned32 read u32maxdic write u32maxdic;
end;

//Entpacken erhält eine eigene Klasse
type TLZWUnPack = class(TObject)
     private
           //Wörterbücher
           Dictionary : TBTree;
           u32maxdic  : unsigned32;


           //Zum Puffern benutzen wir einfach Memorystreams
           Buffer     : TMemoryStream;
     protected
           function _init ():Boolean;
           function _unpack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;


           //Datenfunktionen
           function WideToSmall(data : TWide):byte;
           function SmallToWide(data : byte):TWide;
           function WideOut(data : TWide):boolean;
           function WideIn (var data : pointer):TWide;

           //Wörterbuchfunktionen
           procedure HandleOverflow   ();
           function InitDictionary  ():Boolean;
     public
           constructor Create();
           constructor Free();

           function unpack   (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
           property maxdic : unsigned32 read u32maxdic write u32maxdic;
end;


type TLZW = class(TObject)
     private
            Packer    : TLZWPack;
            UnPacker  : TLZWUnPack;
            RLE       : TRLE;
            sID       : longstring;
            u32mode   : unsigned32;
            u32buffer : unsigned32;
     protected
            function  _test(input : pointer; insize : unsigned32):Boolean;
            function  getmode():unsigned32;
            procedure setmode(value : unsigned32);
     public
           Constructor Create();
           Destructor  Free();

           function pack   (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
           function unpack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
           function test   (size  : unsigned32):boolean;
                      
           property id         : longstring read sID       write sID;
           //Steuert die Kompressionstiefe
           property mode       : unsigned32 read getmode   write setmode;
           //Wird hier nicht benutzt
           property buffersize : unsigned32 read u32buffer write u32buffer;
end;


implementation


////////////////////////////////////////////////////////////////////////////////
/// Die Wrapper-Klasse
////////////////////////////////////////////////////////////////////////////////
Constructor TLZW.Create();
begin
     Self.mode:=LZW_NORMAL;

     Self.Packer  :=TLZWPack.Create();
     Self.UnPacker:=TLZWUnPack.Create();
     Self.RLE:=TRLE.Create();
end;

////////////////////////////////////////////////////////////////////////////////
Destructor  TLZW.Free();
begin
     Self.Packer.Free();
     Self.UnPacker.Free();
     Self.RLE.Free();
end;

////////////////////////////////////////////////////////////////////////////////
function  TLZW.getmode():unsigned32;
begin
     result:=Self.u32Mode;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TLZW.setmode(value : unsigned32);
begin
     if (value >= LZW_MIN_DIC) AND (value <= LZW_MAX_DIC) then
        begin
             Self.u32mode:=value;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TLZW.pack   (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
begin
     //Wörterbuchgröße einstellen
     Self.Packer.maxdic:=Self.mode;

     if (Self.RLE.Pack(input,insize,input,insize)=TRUE) then
        begin
             result:=Self.Packer.Pack(input,insize,output,outsize);
        end
     else
        begin
             result:=FALSE;
        end;

     //RLE können wir hier freigeben
     Self.RLE.Pack(nil,0,input,insize);
end;

////////////////////////////////////////////////////////////////////////////////
function TLZW.unpack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
begin
     //Wörterbuchgröße einstellen
     Self.UnPacker.maxdic:=Self.mode;

     if ( Self.UnPacker.UnPack(input,insize,input,insize) = TRUE ) then
        begin
             result:=Self.RLE.Unpack(input,insize,output,outsize);
        end
     else
        begin
             result:=FALSE;
        end;

     //LZW können wir hier freigeben
     Self.UnPacker.UnPack(nil,0,input,insize);
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Interne Testfunktion
function TLZW.test    (size : unsigned32):Boolean;
var
   u32Index : unsigned32;
   aTemp    : Array of Byte;
begin
     result:=TRUE;

     //Interner Test des Wörterbuches
//     result:=result AND Self.Packer.Dictionary.test(8192);
//     result:=result AND Self.UnPacker.Dictionary.test(8129);

     //Test der Packermodule
     SetLength(aTemp,Size);

     //Speicher mit Zufallsdaten füllen
     u32Index := 0;
     While (u32Index < unsigned32(Length(aTemp))) do
           begin
                aTemp[u32Index]:=Byte(random(High(Byte)));
                inc(u32Index);
           end;

     //Testen
     Result:=Result AND Self._test(Addr(aTemp[0]),Length(aTemp));

     //Speicher mit Daten füllen
     u32Index := 0;
     While (u32Index < unsigned32(Length(aTemp))) do
           begin
                aTemp[u32Index]:=0;
                inc(u32Index);
           end;

     //Testen
     Result:=Result AND Self._test(Addr(aTemp[0]),Length(aTemp));


     //Speicher mit Daten füllen
     u32Index := 0;
     While (u32Index < unsigned32(Length(aTemp))) do
           begin
                aTemp[u32Index]:=255;
                inc(u32Index);
           end;

     //Testen
     Result:=Result AND Self._test(Addr(aTemp[0]),Length(aTemp));

     //Speicher freigeben
     SetLength(aTemp,0);
end;

//interne Testfunktion um Streams zu vergleichen
function TLZW._test(input : pointer; insize : unsigned32):Boolean;
var
   pPack    : ^Byte;
   pUnpack  : ^Byte;
   pData    : ^Byte;
   u32Size  : unsigned32;
   u32Index : unsigned32;
begin
     result:=TRUE;

     pData:=input;

    //Init
     Self.Pack(nil,0,Pointer(pPack),u32Size);

     //Komprimieren
     if (Self.Pack(input,insize,pointer(pPack),u32Size) = TRUE) then
        begin
             //Dekomprimieren
             Self.Unpack(nil,0,pointer(pUnpack),u32Size);
             if ( Self.Unpack(pointer(pPack),u32Size,pointer(pUnpack),u32Size) = TRUE) then
                begin
                     //Daten vergleichen
                     if (u32Size = insize) then
                        begin
                             u32Index:=0;
                             while (u32Index < u32Size) do
                                   begin
                                        //Bei Fehler abbrechen
                                        if (pData^ <> pUnpack^) then
                                           begin
                                                result:=FALSE;
                                                u32Index:=u32Size - 1;
                                           end;

                                        inc(pUnpack);
                                        inc(pData);
                                        inc(u32Index);
                                   end;

                        end
                     else
                        begin
                             result:=FALSE;
                        end; 
                end;
        end;

    //Speicher freigeben
     Self.Pack(nil,0,Pointer(pPack),u32Size);
     Self.Unpack(nil,0,Pointer(pPack),u32Size);
end;


////////////////////////////////////////////////////////////////////////////////
/// Die Packer-Klasse
////////////////////////////////////////////////////////////////////////////////
constructor TLZWPack.Create();
begin
     Self.Buffer:=TMemoryStream.Create();
     Self.Dictionary:=TBTree.Create();
     Self.Dictionary.readonly:=FALSE;
     Self._Init();
end;

////////////////////////////////////////////////////////////////////////////////
constructor TLZWPack.Free();
begin
     Self.Buffer.Free();
     Self.Dictionary.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Initialisieren des Packens
function TLZWPack._Init():Boolean;
begin
     Self.InitDictionary();
     Self.Buffer.Clear();
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Packen
function TLZWPack.Pack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
begin
     //Initialisierung machen
     if (Input = nil) then
        begin
             result:=Self._Init();
        end
     else
        begin
             //Komprimieren
             if (insize > 0) then
                begin
                     Self.Buffer.SetSize(insize);
                     result:=Self._Pack(input,insize,output,outsize);
                end
             else
                begin
                     result:=FALSE;
                end; 
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TLZWPack._Pack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
var
   pData   : ^Byte;
   pEnd    : ^Byte;
   cData   : Char;
   pToken  : pLeaf;
   //Zum einfachen Handling nehmen wir Strings, da diese binarysafe sind
   sData   : longstring;
begin
     //Um mit Zeigern zu arbeiten müssen wir unter Delphi leider etwas tricksen
     pData := Input;
     pEnd  := Input;

     inc(pEnd,InSize);

     //Ausgabestrom vordimmen
     Self.Buffer.SetSize(Insize shl 1);

     //Erstes Zeichen wird plain gelesen
     sData:=chr(pData^);
     inc(pData);

     while ( unsigned64(pData) < unsigned64(pEnd) ) do
           begin
           cData:=Chr(pData^);
           inc(pData);

           //Token suchen und gleichzeitig zufügen
           if ( Self.Dictionary.add( sData + cData, pToken ) = FALSE ) then
              begin
                   //Ist diese Sequenz im Wörterbuch ?
                   //Ja, dann brauchen wir sie nicht zu speichern
                   //sondern verlängern die Kompressionssequenz um eins
                   sData:=sData + cData;
              end
           else
              begin
                   //Nein, dann müssen wir in den Ausgangsstrom die Repräsentation
                   //des Tokens schreiben um diese Sequenz abzuschließen
                   pToken:=Self.Dictionary.find(sData);
                   Self.WideOut(pToken^.ID);

                   sData:=cData;
              end;

           //Überlauf im Wörterbuch behandeln
           Self.HandleOverflow();
     end;

     //Den letzen Code müssen wir leider gesondert verarbeiten
     pToken:=Self.Dictionary.find(sData);
     Self.WideOut(pToken^.ID);

     //Den Buffer behalten wir in der Klassen und geben
     //nur den Pointer raus, damit erspare ich dem Aufrufer
     //das Streamhandling
     Self.Buffer.SetSize(Self.Buffer.Position + 1);
     Output:=Self.Buffer.Memory;
     OutSize:=unsigned32(Self.Buffer.Position);

     //Kompression sollte immer gutgehen
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// Wörterbuchfunktionen
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//Aus einem WideToken ein Byte machen
function TLZWPack.WideToSmall(data : TWide):byte;
begin
     result:=data and $ff;
end;

////////////////////////////////////////////////////////////////////////////////
//Aus einem Byte ein Widetoken machen
function TLZWPack.SmallToWide(data : byte):TWide;
begin
     result:=0;
     result:=result or data;
end;

////////////////////////////////////////////////////////////////////////////////
//Token in den Datenstrom schieben
function TLZWPack.WideOut(data : TWide):boolean;
begin
     //Valide ?
     if (data < Self.Dictionary.size) then
        begin
             result:=Self.Buffer.Write(data,SizeOf(data)) = SizeOf(data);
        end
     else
        begin
             result:=FALSE;
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Ist das Wörterbuch voll wird ein Fehler gemeldet.
//Als Reaktion darauf könnte man das Wörterbuch kpl. leeren
//oder die Breite von TWide erhöhen
//ToDo
procedure TLZWPack.HandleOverflow ();
begin
     if (Self.Dictionary.Size >= Self.maxdic) then
        begin
             //ToDO
             Self.InitDictionary();
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TLZWPack.InitDictionary  ():Boolean;
var
   u32Index : unsigned32;
   pTemp    : pLeaf;
begin
     Self.Dictionary.clear();

     for u32Index := 0 to High(unsigned8) do
         begin
              Self.dictionary.add(chr(u32Index),pTemp);
        end;
     //ToDo : Fehlerfälle ?
     result:=TRUE;
end;



////////////////////////////////////////////////////////////////////////////////
/// Die Entpacker-Klasse
////////////////////////////////////////////////////////////////////////////////
constructor TLZWUnPack.Create();
begin
     Buffer:=TMemoryStream.Create();
     Self.Dictionary:=TBTree.Create();
     Self.Dictionary.readonly:=FALSE;
     Self._Init();
end;

////////////////////////////////////////////////////////////////////////////////
constructor TLZWUnPack.Free();
begin
     Self.Dictionary.Free();
     Self.Buffer.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Initialisieren des Packens
function TLZWUnPack._Init():Boolean;
begin
     Self.Buffer.Clear();
     Self.InitDictionary();
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Packen
function TLZWUnPack.UnPack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
begin
     //Initialisierung machen
     if (Input = nil) then
        begin
             result:=Self._Init();
        end
     else
        begin
             //Komprimieren
             if (insize > 0) then
                begin
                     result:=Self._UnPack(input,insize,output,outsize);
                end
             else
                begin
                     result:=FALSE;
                end; 
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TLZWUnPack._UnPack (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
var
   pData   : ^Byte;
   pEnd    : ^Byte;
   wOld    : TWide;
   wNew    : TWide;
   sData   : longstring;
   cData   : Char;
   u32limit: unsigned32;
   pTemp   : pLeaf;  
begin
     //Um mit Zeigern zu arbeiten müssen wir unter Delphi leider etwas tricksen
     pData := Input;
     pEnd  := Input;
     inc(pEnd,InSize);

     Self.Buffer.SetSize(insize shl 2);
     u32Limit:=Self.Buffer.Size - 10; 

     //Erstes Zeichen plain ausgeben
     //WideIn schiebt den Pointer von selbst weiter
     wOld:=Self.WideIn(pointer(pData));
     Self.WideOut(wOld);

     //Erstes Zeichen muß im Wörterbuch liegen
     //daher können wir einfach runtertrimmen
     cData:=Chr(Self.WideToSmall(wOld));

     while ( unsigned64(pData) < unsigned64(pEnd) ) do
           begin
           wNew:=Self.WideIn(pointer(pData));

           //Ist der Eintrag im Dictionär
           pTemp:=Self.Dictionary.find(wNew);
           if ( pTemp <> nil ) then
              begin
                   //Yo!
                   sData:=string(pTemp^.Data);
              end
           else
              begin
                   //Nope!
                   pTemp:=Self.Dictionary.find(wOld);
                   sData:=string( pTemp^.Data );

                   sData:=sData +  cData;
              end;

           //String einfach in den Stream werfen
           Self.Buffer.Write( sData[1] , Length(sData) );

           //Tabelle aufbauen
           cData:=sData[1];

           Self.Dictionary.add( string( Self.Dictionary.find( wOld )^.Data) + cData,pTemp);
           Self.HandleOverFlow();

           //Outbuffer nachtrimmen, da Einzelwrites sehr langsam sind
           if ( unsigned32(Self.Buffer.Position) > u32Limit ) then
              begin
                   Self.Buffer.SetSize(Self.Buffer.Size shl 1);
                   u32Limit:=Self.Buffer.Size - 10;
              end;

           wOld:=wNew;
           end;

     //Den Buffer behalten wir in der Klassen und geben
     //nur den Pointer raus, damit erspare ich dem Aufrufer
     //das Streamhandling
     Output:=Self.Buffer.Memory;
     OutSize:=unsigned32(Self.Buffer.Position);
     
     //Kompression sollte immer gutgehen
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// Wörterbuchfunktionen
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//Aus einem WideToken ein Byte machen
function TLZWUnPack.WideToSmall(data : TWide):byte;
begin
     result:=data and $ff;
end;

////////////////////////////////////////////////////////////////////////////////
//Aus einem Byte ein Widetoken machen
function TLZWUnPack.SmallToWide(data : byte):TWide;
begin
     result:=0;
     result:=result or data;
end;

////////////////////////////////////////////////////////////////////////////////
//Das String aus einem Token in den Datenstrom schieben
function TLZWUnPack.WideOut(data : TWide):boolean;
var
   u32Size : unsigned32;
   pTemp   : pLeaf;
begin
     //Einfach das Bytearray in den Stream kopieren
     //Positionierung im Stream selbst ist egal, da der Vorschub
     //von der Streamklasse verwaltet wird
     pTemp:=Self.Dictionary.find(Data);
     if (pTemp <> nil ) then
        begin
             u32Size:=Self.Buffer.Write( pTemp^.Data[0] , Length( pTemp^.Data ) );
             result:=u32Size = unsigned32( Length( pTemp^.Data ) );
        end
     else
        begin
             result:=FALSE;
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Wide aus dem Datenstrom lesen
//Adaptiv auf die Breite von TWide reagieren.
//Im Moment in ByteSchritten, optimal wäre Bitweise
function TLZWUnpack.WideIn (var data : pointer):TWide;
begin
     CopyMemory(Addr(result),data,SizeOf(TWide));
     inc(pByte(data),SizeOf(TWide));
end;


////////////////////////////////////////////////////////////////////////////////
//Ist das Wörterbuch voll wird ein Fehler gemeldet.
//Als Reaktion darauf könnte man das Wörterbuch kpl. leeren
//oder die Breite von TWide erhöhen
procedure TLZWUnPack.HandleOverflow();
begin
     if (Self.Dictionary.Size >= self.maxdic) then
        begin
             //ToDO
             Self.InitDictionary();
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Das Wörterbuch mit ASCII initialisieren
function TLZWUnPack.InitDictionary  ():Boolean;
var
   u32Index : unsigned32;
   pTemp    : pLeaf;
begin
     Self.Dictionary.clear();

     for u32Index := 0 to High(unsigned8) do
         begin
              Self.dictionary.add( chr(u32Index),pTemp);
        end;
     //ToDo : Fehlerfälle ?
     result:=TRUE;
end;

end.
