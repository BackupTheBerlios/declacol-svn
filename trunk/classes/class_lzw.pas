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

uses unit_compiler,unit_typedefs,classes,windows,sysutils,class_rle;


//Aufbau des Wörtebuches
type TDicEntry   = array of byte;
type TDictionary = array of TDicEntry;
//Unseren eigenen Type für die Datenbreite benutzen
type TWide       = unsigned16;



//Packen erhält eine eigene Klasse
type TLZWPack = class(TObject)
     private
           //Wörterbücher
           Dictionary : TDictionary;
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
           function FindInDictionary(entry : longstring; var index : TWide):Boolean;
           function AddToDictionary (entry : longstring; var index : TWide):Boolean;
           function InitDictionary  ():Boolean;
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
           Dictionary : TDictionary;
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
           function FindInDictionary(entry : TWide):Boolean;
           function AddToDictionary (entry : longstring):Boolean;
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

//Ein paar Flags
const LZW_PACK   = 1;
      LZW_UNPACK = 2;

      //Vorgaben der Wörterbuchgröße
      LZW_STORE   = 1024;
      LZW_NORMAL  = 4096;
      LZW_EXTREME = 16384;

      //Min/Max Wörterbuchgröße
      LZW_MIN_DIC= High(unsigned8); 
      LZW_MAX_DIC= High(TWide); 

////////////////////////////////////////////////////////////////////////////////
/// Die Wrapper-Klasse
////////////////////////////////////////////////////////////////////////////////
Constructor TLZW.Create();
begin
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
     if (value > LZW_MIN_DIC) AND (value < LZW_MAX_DIC) then
        begin
             Self.u32mode:=value;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TLZW.pack   (input : Pointer; insize : unsigned32; var output : pointer; var outsize : unsigned32):boolean;
begin
     //Wörterbuchgröße einstellen
     Self.Packer.maxdic:=Self.BufferSize;

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
     Self.UnPacker.maxdic:=Self.BufferSize;

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

     //Buffer setzen
     Self._Init();
end;

////////////////////////////////////////////////////////////////////////////////
constructor TLZWPack.Free();
begin
     Buffer.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Initialisieren des Packens
function TLZWPack._Init():Boolean;
begin
     Buffer.Clear();

     Self.InitDictionary();

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
   wToken  : TWide;
   //Zum einfachen Handling nehmen wir Strings, da diese binarysafe sind
   sData   : longstring;
begin
     //Um mit Zeigern zu arbeiten müssen wir unter Delphi leider etwas tricksen
     pData := Input;
     pEnd  := Input;
     inc(pEnd,InSize);

     //Erstes Zeichen wird plain gelesen
     sData:=chr(pData^);
     wToken:=Self.SmallToWide( pData^ );
     inc(pData);

     while ( unsigned64(pData) < unsigned64(pEnd) ) do
           begin
           cData:=Chr(pData^);
           inc(pData);

           //Ist diese Sequenz im Wörterbuch ?
           if ( Self.FindInDictionary(sData + cData,wToken) = TRUE) then
              begin
                   //Ja, dann brauchen wir sie nicht zu speichern
                   //sondern verlängern die Kompressionssequenz um eins
                   sData:=sData + cData;
              end
           else
              begin
                   //Nein, dann müssen wir in den Ausgangsstrom die repräsentation
                   //des Tokens schreiben um diese Sequenz abzuschließen
                   Self.FindInDictionary(sData,wToken);
                   Self.WideOut(wToken);

                   //Neue Sequenz auf das nächste Token legen.
                   //evtl wird hier ein Dic-Full gemeldet.
                   //ToDo : Verhalten bei vollem Wörterbuch
                   //       Bei u32 eher kein Problem verschlechter aber die
                   //       Kompressionsrate
                   Self.AddToDictionary(sData + cData,wToken);

                   sData:=cData;
              end;
     end;

     //Den letzen Code müssen wir leider gesondert verarbeiten
     Self.FindInDictionary(sData,wToken);
     Self.WideOut(wToken);

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
     if (data < unsigned32(Length(Self.Dictionary))) then
        begin
             result:=Self.Buffer.Write(data,SizeOf(data)) = SizeOf(data);
        end
     else
        begin
             result:=FALSE;
        end; 
end;

////////////////////////////////////////////////////////////////////////////////
//Checken, ob ein String im Wörterbuch schon vorhanden ist
function TLZWPack.FindInDictionary(entry : longstring; var index : TWide):Boolean;
var
   u32Index : unsigned32;
begin
     result:=FALSE;

     //Eigentlich sollte diese Schleife schöner sein,
     //aber bei 30.000 Aufrufen die Sekunde stört jedes IF
     for u32Index:=0 to unsigned32(length(Self.Dictionary)-1) do
         begin
              if (entry[1] = Chr(Self.Dictionary[u32Index][0])) then
                 begin
                      if ( entry = string(Self.Dictionary[u32Index]) ) then
                         begin
                              result:=TRUE;
                              index:=u32index;
                              exit;
                           end;
                 end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag zum Wörterbuch zufügen.
//Ist das Wörterbuch voll wird ein Fehler gemeldet.
//Als Reaktion darauf könnte man das Wörterbuch kpl. leeren
//oder die Breite von TWide erhöhen
function TLZWPack.AddToDictionary (entry : longstring; var index : TWide):Boolean;
var
   u32Size  : unsigned32;
begin
     u32Size := Length(entry);
     index:= Length(Self.Dictionary);

     if (u32Size > 0) AND (index < Self.maxdic) then
        begin
             //Wörterbuch erweitern
             SetLength(Self.Dictionary,index + 1);

             //Eintrag einkopieren (Leider LowLevel aber sonst sehr langsam)
             SetLength( Self.Dictionary[index], u32Size );
             CopyMemory(Addr(Self.Dictionary[index][0]),Addr(entry[1]), u32Size );

             Result:=TRUE;
        end
     else
        begin
             //ToDO
             Self.InitDictionary();
             Result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TLZWPack.InitDictionary  ():Boolean;
var
   u32Index : unsigned32;
begin
     //Aller ASCII-Einträge sind nicht dynamisch und können  daher immer im
     //Wörterbuch bleiben
     if (Length(Self.Dictionary) > High(unsigned8) + 1) then
        begin
             for u32Index := High(unsigned8) + 1 to unsigned32(Length(Self.Dictionary) - 1) do
                 begin
                      SetLength(Self.Dictionary[u32Index],0);
                 end;
             SetLength(Self.Dictionary,High(unsigned8) + 1);
        end
     else
        begin
             //ASCII füllen
             SetLength(Self.Dictionary,High(unsigned8) + 1);
             for u32Index := 0 to unsigned32(Length(Self.Dictionary) - 1) do
                 begin
                      SetLength(Self.Dictionary[u32Index],1);
                      Self.Dictionary[u32Index][0]:=u32Index AND $ff;
                 end;
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

     Self._Init();
end;

////////////////////////////////////////////////////////////////////////////////
constructor TLZWUnPack.Free();
begin
     Buffer.Free();
end;

////////////////////////////////////////////////////////////////////////////////
//Initialisieren des Packens
function TLZWUnPack._Init():Boolean;
begin
     Buffer.Clear();

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

           if ( Self.FindInDictionary(wNew) = TRUE) then
              begin
                   sData:=string(Self.Dictionary[wNew]);
              end
           else
              begin
                   sData:=string(Self.Dictionary[wOld]);

                   sData:=sData +  cData;
              end;

           //String einfach in den Stream werfen
           Self.Buffer.Write( sData[1] , Length(sData) );

           //Tabelle aufbauen
           cData:=sData[1];

           Self.AddToDictionary( string( Self.Dictionary[wOld]) + cData );

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
begin
     //Valide ?
     if (data < unsigned32(Length(Self.Dictionary))) then
        begin
             //Einfach das Bytearray in den Stream kopieren
             //Positionierung im Stream selbst ist egal, da der Vorschub
             //von der Streamklasse verwaltet wird
             u32Size:=Self.Buffer.Write( Self.Dictionary[data][0] , Length( Self.Dictionary[data] ) );
             result:=u32Size = unsigned32(Length( Self.Dictionary[data] ) );
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
//Checken, ob ein Token im Wörterbuch schon vorhanden ist
function TLZWUnPack.FindInDictionary(entry : TWide):Boolean;
begin
     result:=entry < TWide(Length(Self.Dictionary));
end;


////////////////////////////////////////////////////////////////////////////////
//Einen Eintrag zum Wörterbuch zufügen.
//Ist das Wörterbuch voll wird ein Fehler gemeldet.
//Als Reaktion darauf könnte man das Wörterbuch kpl. leeren
//oder die Breite von TWide erhöhen
function TLZWUnPack.AddToDictionary (entry : longstring):Boolean;
var
   u32Size  : unsigned32;
   u32Index : unsigned32;
begin
     u32Size := Length(entry);
     u32index:= Length(Self.Dictionary);

     if (u32Size > 0) AND (u32index < self.maxdic) then
        begin
             //Wörterbuch erweitern
             SetLength(Self.Dictionary,u32index + 1);

             //Eintrag einkopieren (Leider LowLevel aber sonst sehr langsam)
             SetLength( Self.Dictionary[u32index], u32Size );
             CopyMemory(Addr(Self.Dictionary[u32index][0]),Addr(entry[1]), u32Size );
             Result:=TRUE;
        end
     else
        begin
             //ToDO
             Self.InitDictionary();
             result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Das Wörterbuch mit ASCII initialisieren
function TLZWUnPack.InitDictionary  ():Boolean;
var
   u32Index : unsigned32;
begin
     //Aller ASCII-Einträge sind nicht dynamisch und können  daher immer im
     //Wörterbuch bleiben
     if (Length(Self.Dictionary) > High(unsigned8) + 1) then
        begin
             for u32Index := High(unsigned8) + 1 to unsigned32(Length(Self.Dictionary) - 1) do
                 begin
                      SetLength(Self.Dictionary[u32Index],0);
                 end;
             SetLength(Self.Dictionary,High(unsigned8) + 1);
        end
     else
        begin
             //ASCII füllen
             SetLength(Self.Dictionary,High(unsigned8) + 1);
             for u32Index := 0 to unsigned32(Length(Self.Dictionary) - 1) do
                 begin
                      SetLength(Self.Dictionary[u32Index],1);
                      Self.Dictionary[u32Index][0]:=u32Index AND $ff;
                 end;
        end;
     //ToDo : Fehlerfälle ?
     result:=TRUE;
end;

end.
