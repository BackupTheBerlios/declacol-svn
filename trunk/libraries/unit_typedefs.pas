{$IFDEF FPC}
        //Einstellungen für den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
unit Unit_TypeDefs;
////////////////////////////////////////////////////////////////////////////////
///
/// Einige zusätzliche Typendefinitionen
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
////////////////////////////////////////////////////////////////////////////////
interface

//Breiten der einzelnen Typen
const
  WIDTH_OF_UNSIGNED1  = 1;
  WIDTH_OF_UNSIGNED8  = 8;
  WIDTH_OF_UNSIGNED16 = 16;
  WIDTH_OF_UNSIGNED32 = 32;
  WIDTH_OF_UNSIGNED64 = 64;

  WIDTH_OF_BIT        = WIDTH_OF_UNSIGNED1;
  WIDTH_OF_BYTE       = WIDTH_OF_UNSIGNED8;
  WIDTH_OF_WORD       = WIDTH_OF_UNSIGNED16;
  WIDTH_OF_LONGWORD   = WIDTH_OF_UNSIGNED32;
  WIDTH_OF_QUADWORD   = WIDTH_OF_UNSIGNED64;

  //Masken für die einzelnen Type
  MASK_UNSIGNED1     = $01;
  MASK_UNSIGNED8     = $ff;
  MASK_UNSIGNED16    = $ffff;
  MASK_UNSIGNED32    = $ffffffff;
  MASK_UNSIGNED64    = $ffffffffffffffff;

  MASK_BIT           = MASK_UNSIGNED1;
  MASK_BYTE          = MASK_UNSIGNED8;
  MASK_WORD          = MASK_UNSIGNED16;
  MASK_LONGWORD      = MASK_UNSIGNED32;
  MASK_QUADWORD      = MASK_UNSIGNED64;

//Sonderwerte
  //Teiler für KByte
  KA                 = 1024;
  WIDTH_OF_KA        = 10;

  ID_NONE            = high(Cardinal);

////////////////////////////////////////////////////////////////////////////////
//Basis Unsigned Typen
//Ein Bit
{$IFNDEF bit}
{$IFNDEF unsigned1}
type unsigned1 =         0..1;
{$ENDIF}


type Bit =               unsigned1;
{$ENDIF}

//Ein Byte
{$IFNDEF unsigned8}
type unsigned8 =         0..$ff;
{$ENDIF}

{$IFNDEF byte}
type byte =              unsigned8;
{$ENDIF}


//Ein Word
{$IFNDEF unsigned16}
type unsigned16 =        0..$ffff;
{$ENDIF}

{$IFNDEF word}
type word =              unsigned16; 
{$ENDIF}


//Ein LangWord
{$IFNDEF unsigned32}
type unsigned32 =        0..$ffffffff;
{$ENDIF}

{$IFNDEF longword}
type longword =          unsigned32;
{$ENDIF}


{$IFNDEF unsigned64}
//Keine echte unsignde64 (da Delphi5 damit nicht umgehen kann)
//aber zumindes ist die anzahl der Bits fast passend
type unsigned64 =        0..$7fffffffffffffff;
{$ENDIF}

{$IFNDEF longword}
type quadword =          unsigned64;
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

{$IFNDEF thandle}
type thandle = unsigned32;
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


////////////////////////////////////////////////////////////////////////////////
/// Netzwerktypen
{$IFNDEF tip}
type TIP = record
     A   : unsigned8;
     B   : unsigned8;
     C   : unsigned8;
     D   : unsigned8;
end;
{$ENDIF}

implementation


end.
