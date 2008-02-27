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
unit Unit_Bits;
////////////////////////////////////////////////////////////////////////////////
///
/// Diverse Bitfunktionen
/// (c) 2005 Borg@Sven-of-Nine.de
///
////////////////////////////////////////////////////////////////////////////////
// Da die Funktionen alle mit Overload arbeiten sollte im zweifelsfall ein Type-
// casting angewandt werden, da es sonst zu unvorhergesehenen Ausgaben kommen kann
// z.B.
// Bit_GetLo($0000ffff) der Übergebene Wert kann als Word angesehen werden,
// obwohl ein Lonword gemeint ist. Bei der Interpretation als Word ist das
// Ergebnis = $ff bei der Interpretation als Longword ergibt sich $ffff
// Bit_GetLo(Longword($0000ffff)) umgeht dieses Problem

interface
uses Unit_TypeDefs;  //Typendefinitionen importieren

////////////////////////////////////////////////////////////////////////////////
//Bits rollen
function BIT_ror(bValue  : Byte;        bBitsToMove:Byte):Byte;       overload;
function BIT_rol(bValue  : Byte;        bBitsToMove:Byte):Byte;       overload;

function BIT_ror(u16Value : Unsigned16; bBitsToMove:Byte):Unsigned16; overload;
function BIT_rol(u16Value : Unsigned16; bBitsToMove:Byte):Unsigned16; overload;

function BIT_ror(u32Value : Unsigned32; bBitsToMove:Byte):Unsigned32; overload;
function BIT_rol(u32Value : Unsigned32; bBitsToMove:Byte):Unsigned32; overload;

////////////////////////////////////////////////////////////////////////////////
//Lo und Hi Wert lesen oder setzen
function BIT_GetHi(u16Value : unsigned16):Byte;                            overload;
function BIT_SetHi(u16Value : unsigned16; bHiByte:Byte):unsigned16;        overload;

function BIT_GetLo(u16Value : unsigned16):Byte;                            overload;
function BIT_SetLo(u16Value : unsigned16; bLoByte:Byte):unsigned16;        overload;

function BIT_GetHi(u32Value : unsigned32):unsigned16;                      overload;
function BIT_SetHi(u32Value : unsigned32; u16HiWord:unsigned16):unsigned32;overload;

function BIT_GetLo(u32Value : unsigned32):unsigned16;                      overload;
function BIT_SetLo(u32Value : unsigned32; u16LoWord:unsigned16):unsigned32;overload;


////////////////////////////////////////////////////////////////////////////////
//Einzelne Bits lesen oder setzen
function BIT_GetBit(u32Value:unsigned32;bBitPosition:Byte):Bit;

function BIT_SetBit(u32Value:unsigned32;bBitPosition:Byte;bState:Bit):unsigned32;

////////////////////////////////////////////////////////////////////////////////
//Bißchen Rechnen
//Eine Zahl mod 2^bShifter
function BIT_ModX  (u32Value:LongWord;bShifter:Byte):LongWord;
//Direkte Berechnung einiger MODs
function BIT_Mod8  (u32Value:unsigned32):Byte;
function BIT_Mod16 (u32Value:unsigned32):Byte;
function BIT_Mod32 (u32Value:unsigned32):Byte;
function BIT_Mod64 (u32Value:unsigned32):Byte;
function BIT_Mod128(u32Value:unsigned32):Byte;
function BIT_Mod256(u32Value:unsigned32):Byte;

implementation
////////////////////////////////////////////////////////////////////////////////
//Funktionen zum Bitrolling
////////////////////////////////////////////////////////////////////////////////
// Byte rechts rollen
function BIT_ror(bValue  : Byte;     bBitsToMove:Byte):Byte;     overload;
begin
     //So oft durchlaufen wie nötig
     while (bBitsToMove>0) do
           begin
                //Niedrigstes Bit eine 1 ?
                if ( (bValue and 1)=1) then
                   begin
                        //Ja, dann shifte und oben wieder eine eins ankleben
                        bValue:=bValue shr 1;
                        bValue:=bValue or  $80;
                   end
                else
                   begin
                        //Nein, dann nur shiften
                        bValue:=bValue shr 1;
                   end;
                dec(bBitsToMove);
           end;
     result:=bValue;
end;

////////////////////////////////////////////////////////////////////////////////
// Byte links rollen
function BIT_rol(bValue  : Byte;     bBitsToMove:Byte):Byte;     overload;
begin
     //So oft durchlaufen wie nötig
     while (bBitsToMove>0) do
           begin
                //Höchstes Bit eine 1 ?
                if ( (bValue and $80)=$80) then
                   begin
                        //Ja, dann shifte und unten wieder eine eins ankleben
                        bValue:=bValue shl 1;
                        bValue:=bValue or  1;
                   end
                else
                   begin
                        //Nein, dann nur shiften
                        bValue:=bValue shl 1;
                   end;
                dec(bBitsToMove);
           end;
     result:=bValue;
end;

////////////////////////////////////////////////////////////////////////////////
function BIT_ror(u16Value  : unsigned16;     bBitsToMove:Byte):unsigned16;     overload;
begin
     //So oft durchlaufen wie nötig
     while (bBitsToMove>0) do
           begin
                //Niedrigstes Bit eine 1 ?
                if ( (u16Value and 1)=1) then
                   begin
                        //Ja, dann shifte und oben wieder eine eins ankleben
                        u16Value:=u16Value shr 1;
                        u16Value :=u16Value or  $80;
                   end
                else
                   begin
                        //Nein, dann nur shiften
                        u16Value:=u16Value shr 1;
                   end;
                dec(bBitsToMove);
           end;
     result:=u16Value;
end;

////////////////////////////////////////////////////////////////////////////////
function BIT_rol(u16Value  : unsigned16;     bBitsToMove:Byte):unsigned16;     overload;
begin
     //So oft durchlaufen wie nötig
     while (bBitsToMove>0) do
           begin
                //Höchstes Bit eine 1 ?
                if ( (u16Value and $8000)=$8000) then
                   begin
                        //Ja, dann shifte und unten wieder eine eins ankleben
                        u16Value:=u16Value shl 1;
                        u16Value:=u16Value or  1;
                   end
                else
                   begin
                        //Nein, dann nur shiften
                        u16Value:=u16Value shl 1;
                   end;
                dec(bBitsToMove);
           end;
     result:=u16Value;
end;

////////////////////////////////////////////////////////////////////////////////
function BIT_ror(u32Value:unsigned32;bBitsToMove:Byte):unsigned32;
begin
     //So oft durchlaufen wie nötig
     while (bBitsToMove>0) do
           begin
                //Niedrigstes Bit eine 1 ?
                if ( (u32Value and 1)=1) then
                   begin
                        //Ja, dann shifte und oben wieder eine eins ankleben
                        u32Value:=u32Value shr 1;
                        u32Value:=u32Value or  $80000000;
                   end
                else
                   begin
                        //Nein, dann nur shiften
                        u32Value:=u32Value shr 1;
                   end;
                dec(bBitsToMove);
           end;
     result:=u32Value;
end;

////////////////////////////////////////////////////////////////////////////////
//RollLeft
function BIT_rol(u32Value:unsigned32;bBitsToMove:Byte):unsigned32;
begin
     //So oft durchlaufen wie nötig
     while (bBitsToMove>0) do
           begin
                //Höchstes Bit eine 1 ?
                if ( (u32Value and $80000000)=$80000000) then
                   begin
                        //Ja, dann shifte und unten wieder eine eins ankleben
                        u32Value:=u32Value shl 1;
                        u32Value:=u32Value or  1;
                   end
                else
                   begin
                        //Nein, dann nur shiften
                        u32Value:=u32Value shl 1;
                   end;
                dec(bBitsToMove);
           end;
     result:=u32Value;
end;

////////////////////////////////////////////////////////////////////////////////
// Funktionen zum schreiben oder lesen von Word oder Byte
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Das Hi und Lo Word eines Longword lesen und setzen
function BIT_GetHi(u32Value:unsigned32):unsigned16; overload;
begin
     result:=( u32Value shr 16 ) and $ffff;
end;

function BIT_GetLo(u32Value:unsigned32):unsigned16; overload;
begin
     result:= u32Value and $ffff;
end;

////////////////////////////////////////////////////////////////////////////////
//Das Hi und Lo Byte eines Wortes lesen
function BIT_GetHi(u16Value:unsigned16):byte; overload;
begin
     result:=( u16Value shr 8 ) and $ff;
end;

function BIT_GetLo(u16Value:unsigned16):Byte; overload;
begin
     result:= u16Value and $ff;
end;


////////////////////////////////////////////////////////////////////////////////
//Das Hi und Lo Word eines Longword schreiben
function BIT_SetHi(u32Value : unsigned32;u16HiWord:unsigned16):unsigned32; overload;
begin
     //HiWord löschen
     result:=u32Value and $0000ffff;
     //Und das neue einkopieren
     result:=result or (u16HiWord shl 16);
end;

function BIT_SetLo(u32Value : unsigned32;u16LoWord:unsigned16):unsigned32; overload;
begin
     //HiWord löschen
     result:=u32Value and $ffff0000;
     //Und das neue einkopieren
     result:=result or u16LoWord;
end;

////////////////////////////////////////////////////////////////////////////////
//Das Hi und Lo Byte eines Wortes schreiben
function BIT_SetHi(u16Value : unsigned16;bHiByte:Byte):unsigned16; overload;
begin
     //HiByte löschen
     result:=u16Value and $00ff;
     //Und das neue einkopieren
     result:=result or (bHiByte shl 8);
end;


function BIT_SetLo(u16Value : unsigned16;bLoByte:Byte):unsigned16; overload;
begin
     //LoByte löschen
     result:=u16Value and $ff00;
     //Und das neue einkopieren
     result:=result or bLoByte;
end;


////////////////////////////////////////////////////////////////////////////////
// Funktionen zum schreiben oder lesen von einzelnen Bits
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Einzelne Bits lesen
function BIT_GetBit(u32Value:unsigned32;bBitPosition:Byte):Bit;
begin
     //Einfach rechts Shiften und prüfen ob das
     //gefragte Bit gesetzt ist
     result:=(u32Value shr bBitPosition) and 1;
end;

//Einzelne Bits setzen
function BIT_SetBit(u32Value:unsigned32;bBitPosition:Byte;bState:Bit):unsigned32;
var
   u32Set : unsigned32;
begin
     //Eingabe filtern
     bState:=bState and 1;

     //gewünschtes Bit invertiert positionieren
     u32Set :=1 shl bBitPosition;

     //Das gesuchte Bit aus der Eingabe löschen
     Result:=u32Value and not u32Set;

     //Gewünschtes Bit evtl. setzen
     result:=result or (bState shl bBitPosition);
end;

////////////////////////////////////////////////////////////////////////////////
// Funktionen zum schnellen Berechnen Binärer Mods
////////////////////////////////////////////////////////////////////////////////
//Egentliche Berechnung
function BIT_ModX(u32Value:unsigned32;bShifter:Byte):unsigned32;
var
   u32Calc : unsigned32;
begin
     //Durch 2^bShifter Teilen
     u32Calc:= (u32Value shr bShifter);

     //Mal 2^bShifter nehmen
     u32Calc:=u32Calc shl bShifter;

     //Differenz ist lwValue MOD 2^bShifter
     result:=u32Value - u32Calc;
end;


////////////////////////////////////////////////////////////////////////////////
function BIT_Mod8  (u32Value:unsigned32):Byte;
begin
     result:=Byte(BIT_ModX(u32Value,3));
end;

////////////////////////////////////////////////////////////////////////////////
function BIT_Mod16 (u32Value:unsigned32):Byte;
begin
     result:=Byte(BIT_ModX(u32Value,4));
end;

////////////////////////////////////////////////////////////////////////////////
function BIT_Mod32 (u32Value:unsigned32):Byte;
begin
     result:=Byte(BIT_ModX(u32Value,5));
end;

////////////////////////////////////////////////////////////////////////////////
function BIT_Mod64 (u32Value:unsigned32):Byte;
begin
     result:=Byte(BIT_ModX(u32Value,6));
end;

////////////////////////////////////////////////////////////////////////////////
function BIT_Mod128(u32Value:unsigned32):Byte;
begin
     result:=Byte(BIT_ModX(u32Value,7));
end;

////////////////////////////////////////////////////////////////////////////////
function BIT_Mod256(u32Value:unsigned32):Byte;
begin
     result:=Byte(BIT_ModX(u32Value,8));
end;


end.


