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
unit Unit_TypeDefs;
////////////////////////////////////////////////////////////////////////////////
///
/// Einige zusätzliche Typendefinitionen
///
////////////////////////////////////////////////////////////////////////////////
interface

////////////////////////////////////////////////////////////////////////////////
//Basis Unsigned Typen
//Ein Bit
{$IFNDEF bit}
type Bit =               0..1;
{$ENDIF}

{$IFNDEF unsigned1}
type unsigned1 =         Bit;
{$ENDIF}

//Ein Byte
{$IFNDEF byte}
type byte =              0..$ff;
{$ENDIF}

{$IFNDEF unsigned8}
type unsigned8 =         Byte;
{$ENDIF}

//Ein Word
{$IFNDEF word}
type word =              0..$ffff;
{$ENDIF}

{$IFNDEF unsigned16}
type unsigned16 =        Word;
{$ENDIF}

//Ein LangWord
{$IFNDEF longword}
type longword =          0..$ffffffff;
{$ENDIF}

{$IFNDEF unsigned32}
type unsigned32 =        longword;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//Basis Signed Typen
{$IFNDEF signed8}
type signed8 =           -128..127;
{$ENDIF}

{$IFNDEF signed16}
type signed16 =          -32768..32767;
{$ENDIF}

{$IFNDEF signed32}
type signed32 =          -2147483647..2147483647;
{$ENDIF}

{$IFNDEF unsigned64}
type unsigned64 =        0..9223372036854775807;
{$ENDIF}

{$IFNDEF signed64}
type signed64 =          Int64;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//Basis Float Typen
{$IFNDEF longfloat}
type longfloat =           double;
{$ENDIF}

{$IFNDEF shortfloat}
type shortfloat =          single;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// String-Typen
{$IFNDEF longstring}
type longstring =           Ansistring;
{$ENDIF}

{$IFNDEF shortstring}
type shortstring =          String[255];
{$ENDIF}

{$IFNDEF tinystring}
type tinystring =           String[64];
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// RGB-Typen
{$IFNDEF trgbquad}
//Ein RGB-Pixel im Scanlinemodus

type TRGBquad = record
     rgbblue     : byte;
     rgbgreen    : byte;
     rgbred      : byte;
     rgbreserved : byte;
end;
{$ENDIF}

{$IFNDEF trgb}
//Eine RGB-Zahl
type trgb = longword;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// Hilfsdefinitionen für Bitmaps
{$IFNDEF TScanLine}
//Definition einer Scanline (PixelFormat 32Bit)
type TScanLine = array[0..High(Signed32) div SizeOf(TRGBQUAD) - 1] of TRGBQUAD;
{$ENDIF}

//Zeiger auf eine Scanline
{$IFNDEF PScanLine}
type PScanLine = ^TScanLine;
{$ENDIF}

//evtl. Point definieren
{$IFNDEF tpoint}
type TPoint =record
     x : Signed32;
     y : Signed32;
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// Handle Typen
{$IFNDEF thnd}
type thnd = unsigned32;
{$ENDIF}

{$IFNDEF tfilehandle}
type tfilehandle = unsigned32;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//Methodentypen
type TMethodPointer = packed record
       pMethod : Pointer;
       pObject : TObject;
end;

////////////////////////////////////////////////////////////////////////////////
// DateTime Typen
{$IFNDEF timestamp}
type timestamp = longword;
{$ENDIF}

{$IFNDEF daterecord}
type daterecord = record
     hour  : unsigned32;
     minute: unsigned32;
     sec   : unsigned32;
     msec  : unsigned32;
     day   : unsigned32;
     month : unsigned32;
     year  : unsigned32;
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
/// Tabellentypen
{$IFNDEF tstringtable}
type TStringTable = array of array of longstring;
{$ENDIF}

{$IFNDEF tvarianttable}
type TVariantTable = array of array of Variant;
{$ENDIF}


implementation


end.
