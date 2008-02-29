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
/////////////////////////////////////////////////////////////////////////////////
///
/// Grafikbibliothek
///
/// v 0.3
///
///
///
///
///
///
///
///
///
///
///
/////////////////////////////////////////////////////////////////////////////////
unit Unit_Grafix;

interface
uses Windows,Graphics,Forms,Unit_ScreenFunctions;

type PBitmap=^TBitmap;
     //Kleines Arry zum schnelleren Zugriff auf Bitmaps
     TLine = array[0..MaxInt div SizeOf(TRGBQUAD) - 1] of TRGBQUAD;
     PLine = ^TLine;

type
  LogPal = record
  lpal : TLogPalette;
  dummy:Array[0..255] of TPaletteEntry;
  end;

/////////////////////////////////////////////////////////////////////////////////
// Grafikfunktionen
/////////////////////////////////////////////////////////////////////////////////


//Ein Bitmap auf NewWidth und NewHeight skalieren (einfache Pixelanpassung)
procedure Bitmap_Resize(BM:TBitmap;NewWidth,NewHeight:Cardinal);

//Ein Bitmap auf NewWidth und NewHeight skalieren (Standard Resampling)
Function  Bitmap_Resample(Bitmap:TBitmap;NewWidth,NewHeight:Integer):Boolean;

//EIn Bitmap in ein anderes kopieren
Function Bitmap_Copy(Source:TBitmap;Target:TBitmap):Boolean;

//Bild in Grautöne wandeln
function Bitmap_GrayScale(Source:TBitmap):Boolean;

//Bild in 1Bit wandeln
function Bitmap_BinaryScale(Source:TBitmap;Level:Byte=128):Boolean;

//Einem Bild die Firefunktion verpassen
function Bitmap_Fire(Source:TBitmap):Boolean;

//Ein Bild weichzeichnen
function Bitmap_Blur(Source:TBitmap):Boolean;

//Ein Bild invertieren
function Bitmap_Invert(Source:TBitmap):Boolean;

//Hellighkeit anheben
procedure Bitmap_AddLight(Source:TBitmap;Value:Byte);

//mit Rauschen Füllen
Function Bitmap_Noise(Source:TBitmap;RLevel,GLevel,BLevel:Byte):Boolean;

//Mit einer Farbe füllen
Function Bitmap_Fill(Source:TBitmap;clColor:TColor):Boolean; overload;
Function Bitmap_Fill(Source:TBitmap;rgbColor:TRGBQuad):Boolean; overload;

//Bitmap mit einem Gradienten füllen
procedure Bitmap_Fill(Source:TBitmap;StartColor,EndColor:TColor;Mode:Byte=1); overload

//Einen Screenshot eines Windows erzeugen (0 = Screen)
function Bitmap_ScreenShot(Window:HWND; bm : TBitMap):Boolean;

//Zwei Bitmaps mischen, wobei Bitmap1 ins Bitmap2 anstatt der Farbe Transcolor eingeblendet wird
procedure Bitmap_Mix(SourceBitmap1,SourceBitmap2:TBitmap;TargetBitmap:TBitmap;TransColor:TColor);

//Bitmap spiegeln
procedure Bitmap_Mirror(Source:TBitmap);

//Mirror Flippen
procedure Bitmap_Flip(Source:TBitmap);

procedure Bitmap_TurnLeft(Source:TBitmap);
procedure Bitmap_TurnRight(Source:TBitmap);

//Pixel setzen und lesen
procedure Bitmap_SetPixel(Source:TBitmap;iXPos,iYPos:Integer;clColor:TColor); overload;
procedure Bitmap_SetPixel(Source:TBitmap;iXPos,iYPos:Integer;rgbColor:TRGBQuad); overload;

function Bitmap_GetPixel(Source:TBitmap;iXPos,iYPos:Integer;var clColor:TColor):Boolean;   overload;
function Bitmap_GetPixel(Source:TBitmap;iXPos,iYPos:Integer;var rgbColor:TRGBQuad):Boolean; overload;


//Gibt ein Rect zurück, welches alle Punkte mit der Farbe Color einrahmnt
function Bitmap_TraceColor(Bitmap:TBitmap;ColorToTrace:TRGBQuad):TRect;

//Scanline-Funktionen
procedure Bitmap_CopyScanLine(Source,Target:PLine;Size:Integer);
procedure Bitmap_FillScanLine(Source:PLine;Color:TRGBQuad;Size:Integer);
procedure Bitmap_FillScanLineRandom(Source:PLine;rLevel,gLevel,bLevel:Byte;Size:Integer);

/////////////////////////////////////////////////////////////////////////////////
// Diverse Funktionen zum vereinfachten Umgang mit Farben
/////////////////////////////////////////////////////////////////////////////////
procedure Bitmap_ReduceColorDeepth(var BM:TBitmap;Bits:byte); //Noch in Arbeit

//Liegt eine Farbe um den LEVEL von der anderen entfernt ?
function Color_IsNear(Color1, Color2: TRGBQUAD; Level: Byte): Boolean;
//Subfunktion
function _IsNear(Zahl1, Zahl2: Byte; Level: Byte): Boolean;

//Sind zwei Farben identisch ?
function Color_IsEqual(Color1, Color2: TRGBQUAD): Boolean;
//Subfunktion
function _IsEqual(Zahl1, Zahl2: Byte): Boolean;

//TCOL in RGBQuad konvertieren
function Color_ColToRGB(color: TColor): TRGBQUAD;

//RGBQuad To TCol konvertieren
function Color_RGBToCol(color: TRGBQuad): TColor;

//Gib den Grauwert einer Farbe zurück
function Color_GrayValue(Farbe: TColor): TRGBQUAD;  overload
function Color_GrayValue(Farbe: TRGBQuad): TRGBQUAD;  overload

//Eine Farbe aufhellen
procedure Color_Lighten(var rgbColor:TRGBQuad;iValue:Integer=1); overload
procedure Color_Lighten(var clColor :TColor  ;iValue:Integer=1); overload
procedure Color_Darken (var rgbColor:TRGBQuad;iValue:Integer=1); overload
procedure Color_Darken (var clColor :TColor  ;iValue:Integer=1); overload

//Einen Farbverlauf bestimmen Min und Max geben die Grenzen an, Pos di aktuelle Position
//und Mincol / Maxcol die Grenzen des Farbverlaufes
//Bsp.  Min=0,Max=100,Pos=10,MinCol=clBlack,MaxCol=clWhite  Result=10% Grau
function Color_CreateDiff(Min,Max,Pos:Cardinal;MinCol,MaxCol:TColor):TColor;



implementation

/////////////////////////////////////////////////////////////////////////////////
//Globale Hilfsfunktionen
/////////////////////////////////////////////////////////////////////////////////
procedure Bitmap_CopyScanLine(Source,Target:PLine;Size:Integer);
var
   x : Integer;
begin
     for x:=0 to Size-1 do
         begin
              Target[x]:=Source[x];
         end;
end;

procedure Bitmap_FillScanLine(Source:PLine;Color:TRGBQuad;Size:Integer);
var
   x : Integer;
begin
     for x:=0 to Size-1 do
         begin
              Source[x]:=Color;
         end;
end;

procedure Bitmap_FillScanLineRandom(Source:PLine;rLevel,gLevel,bLevel:Byte;Size:Integer);
var
   x : Integer;
begin
     for x:=0 to Size-1 do
         begin
              //Mit Zufallswerten füllen
              Source[x].rgbRed:=Random(RLevel);
              Source[x].rgbGreen:=Random(GLevel);
              Source[x].rgbBlue:=Random(BLevel);
         end;
end;

/////////////////////////////////////////////////////////////////////////////////
//  Grafikfunktionen
/////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////
// Ein Bitmap in ein anderes kopieren
Function Bitmap_Copy(Source:TBitmap;Target:TBitmap):Boolean;
begin
     Result:=TRUE;
     Target.Assign(Source);
end;


/////////////////////////////////////////////////////////////////////////////////
// Eine Bitmap skalieren (Auf Pixelebene)
procedure Bitmap_Resize(BM:TBitmap;NewWidth,NewHeight:Cardinal);
var
   cOldWidth  : Cardinal;
   cOldHeight : Cardinal;
   bTemp      : TBitmap;
begin
     cOldHeight:=BM.Height;
     cOldWidth :=BM.Width;

     //Temporäres Bitmap erzeugen
     bTemp:=TBitmap.Create;
     bTemp.Assign(BM);
     //Neue Größe setzen
     bTemp.Width :=NewWidth;
     bTemp.Height:=NewHeight;

     //Direkt auf das WindowsAPI zugreifen,
     //da diese Funktionen sehr schnell sind
     StretchBlt(bTemp.Canvas.Handle,0,0,NewWidth,NewHeight,BM.Canvas.Handle,0,0,cOldWidth,cOldHeight,SRCCopy);

     //Und Bitmap übergeben
     BM.Width :=NewWidth;
     BM.Height:=NewHeight;

     BM.Assign(bTemp);
     bTemp.Free;
end;

/////////////////////////////////////////////////////////////////////////////////
//Eine Bitmap Skaliern (Resampling)
/////////////////////////////////////////////////////////////////////////////////
//FÜR einen Block im Bild die mittlere Farbe bestimmen
function  ResampleSubBitmap(Bitmap:TBitmap;XPos,YPos,width,height:integer):TRGBQuad;
var
   r,g,b:Cardinal;
   Line:PLine;
   x,y,z:Integer;
begin
     //Anzahl der Pixel
     z:=(Width*Height);
     //Farbwerte nullen
     r:=0;
     g:=0;
     b:=0;

     //Grenzüberschreitungen abfangen
     if (YPos+Height)>=Bitmap.Height then Height:=(Bitmap.Height-YPos)-1;
     if (XPos+Width)>=Bitmap.Width then Width:=(Bitmap.Width-XPos)-1;

     //Für jedes Pixel die Werte lesen und aufaddieren
     for y:=YPos to YPos+Height do
         begin
              //Mit Scanlines arbeiten, da diese viel schneller als Pixel sind
              Line:=Bitmap.ScanLine[y];
              for x:=XPos to XPos+Width do
                  begin
                       //Farbwerte einfach aufaddieren
                       r:=r+Line[x].rgbRed;
                       g:=g+Line[x].rgbGreen;
                       b:=b+Line[x].rgbBlue;
                       inc(z);
                  end;
         end;

     if (z=0) then z:=1;
     //Mittelwert bestimmen und kleine Helligkeitskorrektur
     r:=Round( (r/z)*1.4 ); if (r>255) then r:=255;
     g:=Round( (g/z)*1.4 ); if (g>255) then g:=255;
     b:=Round( (b/z)*1.4 ); if (b>255) then b:=255;

     //Ausgabequad setzen
     Result.rgbRed  := r;
     Result.rgbGreen:= g;
     Result.rgbBlue := b;
end;
//Eigentliche Funktion
Function  Bitmap_Resample(Bitmap:TBitmap;NewWidth,NewHeight:Integer):Boolean;
var
   Temp:TBitmap;
   Line:PLine;
   x,y:Integer;
   Blockheight,Blockwidth:Cardinal;
   BlockPosX,BlockPosY:Single;
   BlockDiffX,BlockDiffY:Single;
   XPos,YPos : Single;
   DiffX,Diffy :Single;
begin
     Result:=TRUE;

     //Arbeitsbitmap erzeugen
     Temp:=TBitmap.Create;

     //Alles muß 32 Bit sein
     Bitmap.PixelFormat:=pf32Bit;
     Temp.PixelFormat:=pf32Bit;

     //Neue Höhe unseres Bitmap
     Temp.Height:=NewHeight;
     Temp.Width:=NewWidth;

     //Altes Bild in Blöcke zerlegen, deren jeweiliger Mittelwert die Farbe eines neuen Pixels bildet
     //Blockschrittweite pro neues Pixel
     BlockDiffY :=( Bitmap.Height / NewHeight );
     BlockDiffX  :=( Bitmap.Width  / NewWidth  );
     //Größe eines Blockes
     BlockHeight :=Trunc ( BlockDiffY );
     BlockWidth  :=Trunc ( BlockDiffY );

     //Schrittweite der Pixel im neuen Bild
     DiffX       :=1;
     DiffY       :=1;

     //Alle initialisieren
     BlockPosY:=0;
     YPos:=0;
     //Jede Spalte
     for y:=0 to NewHeight-1 do
         begin
              //Vorne anfangen
              BlockPosX:=0;
              XPos:=0;
              //Jede Zeile
              Line:=Temp.ScanLine[Trunc(YPos)];
              for x:=0 to NewWidth-1 do
                  begin
                       //Aus einem angegebenen Block des alten Bitmaps den Mittelwert der Farbe bestimmen
                       Line[Trunc(XPos)]:=ResampleSubBitmap(Bitmap,Round(BlockPosX),Round(BlockPosY),Blockwidth,BlockHeight);

                       //Einen Block/Pixel weiter
                       BlockPosX:=BlockPosX+BlockDiffX;
                       XPos:=XPos+DiffX;
                  end;
              //Einen Block/Pixel weiter
              BlockPosY:=BlockPosY+BlockDiffY;
              YPos:=YPos+DiffY;
         end;
     //Alte Bitmap mit der neuen überschreiben
     Bitmap.Assign(Temp);

     //Hilfsbitmap freigeben
     Temp.Free;
end;


/////////////////////////////////////////////////////////////////////////////////
//Ein Bitmap in Schwarz/Weiß umwandlen
Function  Bitmap_GrayScale(Source:TBitmap):Boolean;
var
   Line:PLine;
   x,y:Integer;
begin
     Result:=TRUE;
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;

     //Alle Zeilen
     for y:=0 to Source.Height-1 do
         begin
              Line:=Source.ScanLine[y];
              //Alle Punkte
              for x:=0 to Source.Width-1 do
                  begin
                       //Pixel einfach durch deren Grauwert ersetzen
                       Line[x]:=Color_GrayValue(Line[x]);
                  end;
         end;
//     Source.PixelFormat:=pf32Bit;
end;

/////////////////////////////////////////////////////////////////////////////////
//Bild in 1Bit wandeln
function Bitmap_BinaryScale(Source:TBitmap;Level:Byte=128):Boolean;
var
   Line : PLine;
   x,y  : Integer;
   Pal  : TMaxLogPalette;
begin
     Result:=TRUE;

     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     Source.IgnorePalette:=TRUE;
     for y:=0 to Source.Height-1 do
         begin
              Line:=Source.ScanLine[y];
              for x:=0 to Source.Width-1 do
                  begin
                       //Pixel einfach durch deren Grauwert ersetzen
                       if (Color_GrayValue(Line[x]).rgbRed<=level) then Line[x]:=Color_ColToRGB(clBlack) else Line[x]:=Color_ColToRGB(clWhite);
                  end;
         end;

     //Palette erzeugen
     Pal.palVersion:=$300;
     Pal.palNumEntries:=2;

     //Und auf Schwarz und Weiß mappen
     With Pal.palPalEntry[0] do
          begin
               peRed   := GetRValue(clBlack);
               peGreen := GetGValue(clBlack);
               peBlue  := GetBValue(clBlack);
          end;
     With Pal.palPalEntry[1] do
          begin
               peRed   := GetRValue(clWhite);
               peGreen := GetGValue(clWhite);
               peBlue  := GetBValue(clWhite);
          end;

     Source.Palette:=CreatePalette(pLogPalette(@Pal)^);
     Source.PixelFormat:=pf1Bit;
end;

/////////////////////////////////////////////////////////////////////////////////
// Den Fire Effekt erzeugen
// Dabei wird von unten nach oben weichgezeichnet
// Dadurch scheint das Bild von unten nach oben auseinander zu laufen
Function Bitmap_Fire(Source:TBitmap):Boolean;
var
   Blur :PLine;
   Line1:PLine;
   x,y  :Integer;
   r,g,b:Integer;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     Result:=TRUE;
     //Jede Zeile
     for y:=1 to Source.Height-1 do
         begin
              //Zu lesende Zeile
              Line1:=Source.ScanLine[y];
              //Ergebnis des Weichzeichnens
              Blur :=Source.ScanLine[y-1];
              for x:=1 to Source.Width-1 do
                  begin
                       //Pixel im Umkreis um den aktuellen Wert lesen
                       r:=Line1[x-1].rgbRed+Line1[x].rgbRed+Line1[x+1].rgbRed+Line1[x].rgbRed;
                       r:=r shr 2;
                       g:=Line1[x-1].rgbGreen+Line1[x].rgbGreen+Line1[x+1].rgbGreen+Line1[x].rgbGreen;
                       g:=g shr 2;
                       b:=Line1[x-1].rgbBlue+Line1[x].rgbBlue+Line1[x+1].rgbBlue+Line1[x].rgbBlue;
                       b:=b shr 2;
                       //Aus den einzelnen Farben den Mittelwert bilden und
                       //diesen eine Zeile höher eintragen
                       Blur[x].rgbRed:=r;
                       Blur[x].rgbGreen:=g;
                       Blur[x].rgbBlue:=b;
                  end;
         end;
end;


/////////////////////////////////////////////////////////////////////////////////
//Eine Bitmap weichzeichnen
Function Bitmap_Blur(Source:TBitmap):Boolean;
var
   Blur :PLine;
   Line1:PLine;
   x,y  :Integer;
   r,g,b:Integer;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;

     Result:=TRUE;
     //Alle Zeilen
     for y:=1 to Source.Height-1 do
         begin
              //Alle Pixel
              Line1:=Source.ScanLine[y];
              Blur :=Source.ScanLine[y];
              for x:=1 to Source.Width-1 do
                  begin
                       //Mittelwerte der benachbarten Pixel erzeugen
                       r:=Line1[x-1].rgbRed+Line1[x].rgbRed+Line1[x+1].rgbRed+Line1[x].rgbRed;
                       r:=r shr 2;
                       g:=Line1[x-1].rgbGreen+Line1[x].rgbGreen+Line1[x+1].rgbGreen+Line1[x].rgbGreen;
                       g:=g shr 2;
                       b:=Line1[x-1].rgbBlue+Line1[x].rgbBlue+Line1[x+1].rgbBlue+Line1[x].rgbBlue;
                       b:=b shr 2;
                       //Und setzen
                       Blur[x].rgbRed:=r;
                       Blur[x].rgbGreen:=g;
                       Blur[x].rgbBlue:=b;
                  end;
         end;
end;
/////////////////////////////////////////////////////////////////////////////////
// Ein Bitmap invertieren
function Bitmap_Invert(Source:TBitmap):Boolean;
var
   Line1:PLine;
   x,y  :Integer;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;

     Result:=TRUE;
     //Alle Zeilen
     for y:=0 to Source.Height-1 do
         begin
              //Alle Spalten
              Line1:=Source.ScanLine[y];
              for x:=0 to Source.Width-1 do
                  begin
                       //Einfach logisches not für RGB machen
                       Line1[x].rgbRed:=not Line1[x].rgbRed;
                       Line1[x].rgbBlue:=not Line1[x].rgbBlue;
                       Line1[x].rgbGreen:=not Line1[x].rgbGreen;
                  end;
         end;
end;

/////////////////////////////////////////////////////////////////////////////////
//Alles um den Anteil Value heller machen
procedure Bitmap_AddLight(Source:TBitmap;Value:Byte);
var
   Line1 : PLine;
   x,y   : Integer;
   c     : TRGBQuad;
   v     : Byte;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;

     //Differenz ist der Helligkeitsunterschied
     v:=255-Value;
     //Alle Zeilen
     for y:=0 to Source.Height-1 do
         begin
              //Scanline lesen
              Line1:=Source.ScanLine[y];
              //Alle Pixel durcharbeiten
              for x:=0 to Source.Width-1 do
                  begin
                       //Pixel holen
                       c:=Line1[x];

                       //Rot zu dunkel ?
                       if (c.rgbRed > v)   then c.RGBRed:=255    else inc(c.RGBRed,Value);
                       //Grün zu dunkel
                       if (c.rgbGreen > v) then c.RGBGreen:=255  else inc(c.RGBGreen,Value);
                       //Blau zu dunkel ?
                       if (c.rgbBlue > v)  then c.RGBBlue:=255   else inc(c.RGBBlue,Value);

                       //Und neu setzen
                       Line1[x]:=c;
                  end;
         end;
end;

/////////////////////////////////////////////////////////////////////////////////
// Ein Bitmap verrauschen
// mit den RGB-Leveln lässt sich der Farbanteil des Rauschens bestimmen
Function Bitmap_Noise(Source:TBitmap;RLevel,GLevel,BLevel:Byte):Boolean;
var
   Line:PLine;
   x,y  :Integer;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     Result:=TRUE;
     //Zufall initialisieren
     Randomize();
     //Allle Zeilen
     for y:=0 to Source.Height-1 do
         begin
              //Scanline lesen
              Line:=Source.ScanLine[y];
              //Und mit Zufallswerten füllen
              Bitmap_FillScanlineRandom(Line,rLevel,gLevel,bLevel,Source.Width);
         end;
end;

/////////////////////////////////////////////////////////////////////////////////
//Ein Bitmap füllen
Function Bitmap_Fill(Source:TBitmap;clColor:TColor):Boolean; overload;
begin
     result:=Bitmap_Fill(Source,Color_ColToRGB(clColor));

end;

Function Bitmap_Fill(Source:TBitmap;rgbColor:TRGBQuad):Boolean; overload;
var
   Line :PLine;
   y    :Integer;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     Result:=TRUE;

     //Allle Zeilen
     for y:=0 to Source.Height-1 do
         begin
              //Scanline lesen
              Line:=Source.ScanLine[y];
              //Und füllen
              Bitmap_FillScanLine(Line,rgbColor,Source.Width);
         end;
end;
/////////////////////////////////////////////////////////////////////////////////
//Eine Bitmap mit einem Farbgradienten füllen
procedure Bitmap_Fill(Source:TBitmap;StartColor,EndColor:TColor;Mode:Byte=1); overload;
var
   bmp :TBitmap;
   c   : TRGBQuad;
   x,y : Integer;
   Line: PLine;
begin
     bmp:=TBitmap.Create;
     bmp.Assign(Source);
     bmp.PixelFormat:=pf32Bit;

     for y:=0 to bmp.Height-1 do
         begin
              //Vertikale füllung
              if (mode<>1) then
                 begin
                      c:=Color_ColToRGB(Color_CreateDiff(0,bmp.Height,y,StartColor,EndColor));
                 end;
              Line:=bmp.ScanLine[y];
              for x:=0 to bmp.Width-1 do
                  begin
                       if (mode=1) then
                          begin
                               c:=Color_ColToRGB(Color_CreateDiff(0,bmp.Width,x,StartColor,EndColor));
                          end;
                       //In das Zielbitmap schreiben
                       Line[x]:=c;
                  end;
         end;

     Source.Assign(bmp);
     bmp.Free;
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Screenshot erzeugen
////////////////////////////////////////////////////////////////////////////////////////////////////
function  Bitmap_ScreenShot(Window:HWND; bm : TBitMap):Boolean;
var
  dc   : HDC;
  Box  :TRect;
begin
     Result:=FALSE;

     //Device-Context öffnen
     dc := GetDc(GetDesktopWindow);
     //Fehler, dann abflug
     if (dc = 0) then Exit;

     //Ausschnitt festlegen
     if (Window<>0) then
        begin
             if (GetWindowRect(Window,Box)=FALSE) then Window:=0;
        end;
     //Wenn wir keine Fensterbox finden nehmen wir den ganzen Bildschirm
     if (Window=0) then
         begin
              Box.Left:=0;
              Box.Top:=0;
              Box.Right:=Screen.Width;
              Box.Bottom:=Screen.Height;
         end;

     //Größe des Bitmaps festlegen
     Bm.Width:=Box.Right-Box.Left;
     Bm.Height:=Box.Bottom-Box.Top;
     Bm.PixelFormat:=pf32Bit;

     //Her mit dem Bildschirm
     Result:=BitBlt(bm.Canvas.Handle,0,0,bm.Width,bm.Height,Dc,Box.Left,Box.Top,SRCCOPY);

     //Alles wieder freigeben
     ReleaseDc(0, dc);
end;


/////////////////////////////////////////////////////////////////////////////////
// Analysefunktionen
/////////////////////////////////////////////////////////////////////////////////
//Einen Kasten um einen Farbwert ziehen.
//Der Kasten hüllt alle Pixel gleicher farbe ein
function Bitmap_TraceColor(Bitmap:TBitmap;ColorToTrace:TRGBQuad):TRect;
var
   x, y : Integer;
   SLine : PLine;
begin
     if (Bitmap.Empty) then Exit;

     //Alles muß 32 Bit sein
     Bitmap.PixelFormat:=pf32Bit;

     With Bitmap do
          begin
               //Wir fangen links oben an
               Result.Top:=Height;
               Result.Bottom:=0;
               Result.Left:=Width;
               Result.Right:=0;
               //Zeilen durchackern
               for y:=0 to Height-1 do
                   begin
                        //Scanline holen
                        SLine:=ScanLine[y];
                        //Pixel durcharbeiten
                        for x:=0 to Width-1 do
                            begin
                                 //Wir benutzen nicht die Is Equal-Funktion, da diese Version
                                 //für unsere Zwecke schneller ist
                                 if SLine[x].rgbBlue=ColorToTrace.rgbBlue then
                                 if SLine[x].rgbRed=ColorToTrace.rgbRed then
                                 if SLine[x].rgbGreen=ColorToTrace.rgbGreen then
                                    begin
                                         //Positionen merken
                                         if (y <= Result.Top) and (y > 0 ) then Result.Top:=y;
                                         if (y >= Result.Bottom) and (y < Height ) then Result.Bottom:=y+1;

                                         if (x <= Result.Left) and ( x > 0 ) then Result.Left:=x;
                                         if (x >= Result.Right) and (x < Width) then Result.Right:=x+1;
                                    end;
                            end;
                   end;
          end;
end;



/////////////////////////////////////////////////////////////////////////////////
//Aus zwei Bitmaps eine machen, wobei Bitmap1überall dort in Bitmap2
//gezeichnet wird, wor die Farber TransColor entspricht.
//das ergebnis wird in Target kopiert
/////////////////////////////////////////////////////////////////////////////////
procedure Bitmap_Mix(SourceBitmap1,SourceBitmap2:TBitmap;TargetBitmap:TBitmap;TransColor:TColor);
var
   //Traceline-Puffer
   S1Line:PLine;
   S2Line:PLine;
   T1Line:PLine;

   //Koordinaten
   s1x,s1y:Integer;
   s2x,s2y:Integer;
   t1x,t1y:Integer;

   //Farben
   s2c : TRGBQuad;
   t1c : TRGBQuad;

begin
//     src1.PixelFormat:=pf32Bit;
//     src2.PixelFormat:=pf32Bit;
//     trg1.PixelFormat:=pf32Bit;

     //Startkoordinaten initialisieren
     s1y:=0;
     s2y:=0;

     t1c:=Color_ColToRGB(TransColor);

     //Komplettes Ziel abarbeiten
     for t1y:=0 to TargetBitmap.Height-1 do
         begin
              //Wieder links beginnen
              s1x:=0;
              s2x:=0;

              //Ziel
              T1Line:=TargetBitmap.ScanLine[t1y];
              //Quelle 1
              S1Line:=SourceBitmap1.ScanLine[s1y];
              //Quelle 2
              S2Line:=SourceBitmap2.ScanLine[s2y];

              for t1x:=0 to TargetBitmap.Width-1 do
                  begin
                       //Farben holen und durch anderes Bitmap ersetzen mischen
                       s2c:=S2Line[s2x];
                       if (s2c.rgbRed=t1c.rgbRed) then
                          if (s2c.rgbGreen=t1c.rgbGreen) then
                             if (s2c.rgbBlue=t1c.rgbBlue) then
                                begin
                                     s2c:=S1Line[s1x];
                                end;
                       //In das Zielbitmap schreiben
                       T1Line[t1x]:=s2c;

                       //Koordinaten der Quellen weiterzählen
                       inc(s1x);
                       inc(s2x);
                       //Bei Überschreitung wieder vorne anfangen
                       if (s1x > SourceBitmap1.width-1) then s1x:=0;
                       if (s2x > SourceBitmap2.width-1) then s2x:=0;
                  end;
              //Koordinaten der Quellen weiterzählen
              inc(s1y);
              inc(s2y);
              //Bei Überschreitung wieder vorne anfangen
              if (s1y > SourceBitmap1.height-1) then s1y:=0;
              if (s2y > SourceBitmap2.height-1) then s2y:=0;
         end;
end;




/////////////////////////////////////////////////////////////////////////////////
//Mirror Flippen
procedure Bitmap_Flip(Source:TBitmap);
var
   y     : Integer;
   yMax  : Integer;
   BMP   : TBitmap;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     BMP:=TBitmap.Create;
     BMP.Assign(Source);
     yMax:=Source.Height-1;
     //Einfach die Scanlines von oben nach unten kopieren
     for y:=0 to Source.Height-1 do
         begin
              Bitmap_CopyScanLine(Source.Scanline[y],BMP.ScanLine[yMax-y],Source.Width);
         end;
     Source.Assign(BMP);
     BMP.Free;
end;

/////////////////////////////////////////////////////////////////////////////////
//Eine Bitmap spiegeln
procedure Bitmap_Mirror(Source:TBitmap);
var
   y     : Integer;
   x     : Integer;
   SLine : PLine;
   TLine : PLine;
   xMax  : Integer;
   BMP   : TBitmap;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     BMP:=TBitmap.Create;
     BMP.Assign(Source);
     xMax:=Source.Width-1;
     for y:=0 to Source.Height-1 do
         begin
              //Alle Pixel tauschen
              SLine:=Source.ScanLine[y];
              TLine:=BMP   .ScanLine[y];
              for x:=0 to Source.Width-1 do
                  begin
                       TLine[xMax-x]:=SLine[x];
                  end;
         end;
     Source.Assign(BMP);
     BMP.Free;
end;


/////////////////////////////////////////////////////////////////////////////////
//Einbitmap um 90° nach links drehen
procedure Bitmap_TurnLeft(Source:TBitmap);
var
   y     : Integer;
   x     : Integer;
   SLine : PLine;
   TLine : PLine;
   xMax  : Integer;
   BMP   : TBitmap;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     BMP:=TBitmap.Create;
     BMP.Assign(Source);
     //Format kippen
     BMP.Width:=Source.Height;
     BMP.Height:=Source.Width;

     xMax:=Source.Width-1;
     for y:=0 to Source.Height-1 do
        begin
              //Einfach die Position der Pixel transformieren
              SLine:=Source.ScanLine[y];
              for x:=0 to Source.Width-1 do
                  begin
                       TLine:= BMP.ScanLine[xMax-x];
                       TLine[y]:=SLine[x];
                  end;
         end;
     Source.Assign(BMP);
     BMP.Free;
end;

/////////////////////////////////////////////////////////////////////////////////
// Eine Bitmap nach rechts drehen
procedure Bitmap_TurnRight(Source:TBitmap);
var
   y     : Integer;
   x     : Integer;
   SLine : PLine;
   TLine : PLine;
   yMax  : Integer;
   BMP   : TBitmap;
begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     BMP:=TBitmap.Create;
     BMP.Assign(Source);
     //Format kippen
     BMP.Width:=Source.Height;
     BMP.Height:=Source.Width;

     yMax:=Source.Height-1;
     for y:=0 to Source.Height-1 do
         begin
              SLine:=Source.ScanLine[y];
              for x:=0 to Source.Width-1 do
                  begin
                       TLine:= BMP.ScanLine[x];
                       TLine[yMax-y]:=SLine[x];
                  end;
         end;
     Source.Assign(BMP);
     BMP.Free;
end;


/////////////////////////////////////////////////////////////////////////////////
//Ein Pixel setzen
procedure Bitmap_SetPixel(Source:TBitmap;iXPos,iYPos:Integer;clColor:TColor); overload;
begin
     //Einfach die RGB-Routine aufrufen
     Bitmap_SetPixel(Source,iXPos,iYPos,Color_ColToRGB(clColor));
end;
/////////////////////////////////////////////////////////////////////////////////
//Ein Pixel setzen
procedure Bitmap_SetPixel(Source:TBitmap;iXPos,iYPos:Integer;rgbColor:TRGBQuad); overload;
var
   SLine : PLine;
begin

     //Überläufe abfangen
     if (iYPos < 0) then Exit;
     if (iXPos < 0) then Exit;
     if (iYPos >= Source.Height) then Exit;
     if (iXPos >= Source.Width)  then Exit;

     //Scanline holen
     SLine:=Source.ScanLine[iYPos];

     //Pixel setzen
     SLine[iXPos]:=rgbColor;
end;

/////////////////////////////////////////////////////////////////////////////////
//Ein Pixel lesen
function Bitmap_GetPixel(Source:TBitmap;iXPos,iYPos:Integer;var clColor:TColor):Boolean;  overload;
var
   rgbCol : TRGBQuad;
begin
     //Einfach die RGB-Routine aufrufen
     result:=Bitmap_GetPixel(Source,iXPos,iYPos,rgbCol);

     //Und das Ergebnis konvertieren
     clColor:=Color_RGBToCol(rgbCol);
end;
/////////////////////////////////////////////////////////////////////////////////
//Ein Pixel setzen
function Bitmap_GetPixel(Source:TBitmap;iXPos,iYPos:Integer;var rgbColor:TRGBQuad):Boolean;   overload;
var
   SLine : PLine;
begin
     result:=FALSE;
     //Überläufe abfangen
     if (iYPos < 0) then Exit;
     if (iXPos < 0) then Exit;
     if (iYPos >= Source.Height) then Exit;
     if (iXPos >= Source.Width)  then Exit;
     result:=TRUE;

     //Scanline holen
     SLine:=Source.ScanLine[iYPos];

     //Pixel lesen
     rgbColor:=SLine[iXPos];
end;


/////////////////////////////////////////////////////////////////////////////////
//  Farbfunktionen
/////////////////////////////////////////////////////////////////////////////////
procedure Bitmap_ReduceColorDeepth(var BM:TBitmap;Bits:byte);
begin
//ToDo
//     BM.PixelFormat:=pf8Bit;
end;
/////////////////////////////////////////////////////////////////////////////////
//Konvertierfunktionen
//Color in RGB konvertieren
/////////////////////////////////////////////////////////////////////////////////
function Color_ColToRGB(color: TColor): TRGBQUAD;
begin
  Result.rgbRed := (Color shr 0) and $FF;
  Result.rgbGreen := (Color shr 8) and $FF;
  Result.rgbBlue := (Color shr 16) and $FF;
end;

/////////////////////////////////////////////////////////////////////////////////
//Und andersrum
function Color_RGBToCol(color: TRGBQuad): TColor;
begin
     Result:=Color.rgbBlue;
     Result:=Result shl 8;
     Result:=Result or Color.rgbGreen;
     Result:=Result shl 8;
     Result:=Result or Color.rgbRed;
end;

/////////////////////////////////////////////////////////////////////////////////
//Sind beide Farben gleich ?
/////////////////////////////////////////////////////////////////////////////////
//Subfunktion zum Zahlenvergleich
function _IsEqual(Zahl1, Zahl2: Byte): Boolean;
begin
     //Sind die Zahlen gleich ?
  if Zahl1 = Zahl2 then Result := TRUE else Result := FALSE;
end;

//Einmal fuer RGB-Quad
function Color_IsEqual(Color1, Color2: TRGBQUAD): Boolean; overload;
begin
  Result := _IsEqual(Color1.rgbBlue, Color2.rgbBlue) and
    _IsEqual(Color1.rgbGreen, Color2.rgbGreen) and
    _IsEqual(Color1.rgbRed, Color2.rgbRed);
end;

/////////////////////////////////////////////////////////////////////////////////
//Sind sich zwei farben ähnlich ?
/////////////////////////////////////////////////////////////////////////////////
//Subfunktion zum Zahlenvergleich
function _IsNear(Zahl1, Zahl2: Byte; Level: Byte): Boolean;
begin
  if (abs(Zahl2 - Zahl1) < Level) then Result := TRUE else Result := FALSE;
end;

//Einmal fuer RGB-Quad
function Color_IsNear(Color1, Color2: TRGBQUAD; Level: Byte): Boolean; overload;
begin
  Result := _IsNear(Color1.rgbBlue, Color2.rgbBlue, Level) and
    _IsNear(Color1.rgbGreen, Color2.rgbGreen, Level) and
    _IsNear(Color1.rgbRed, Color2.rgbRed, Level);
end;

////////////////////////////////////////////////////////////////////////////////////////////////////
//Gib den Grauwert einer Farbe zurück
function Color_GrayValue(Farbe: TColor): TRGBQUAD;  overload
var
   r,g,b:Integer;
begin
     //Rot-Wert
     r := Farbe and $000000FF;
     //Grünwert
     g := (Farbe and $0000FF00) shr 8;
     //Blauwert
     b := (Farbe and $00FF0000) shr 16;

     //Grauwert
     //Die Faktoren sind empirisch bestimmt
     //bei einem einfachen Mittelwert erscheint das
     //Bild dem Auge als unausgewogen 
     g:=HiByte(r*77+g*151+b*28);
     //RGBQuad zurückgeben
     Result.rgbBlue:=g;
     Result.rgbGreen:=g;
     Result.rgbRed:=g;
end;

/////////////////////////////////////////////////////////////////////////////////
//Den Grauwert eines Quads bestimmen
function Color_GrayValue(Farbe: TRGBQUAD): TRGBQUAD;  overload
var
   g:Integer;
begin
     g:=HiByte (Farbe.rgbRed*77+Farbe.rgbGreen*151+Farbe.rgbBlue*28);
     Result.rgbBlue:=g;
     Result.rgbGreen:=g;
     Result.rgbRed:=g;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
// Eine Farbe aufhellen
procedure Color_Lighten(var rgbColor:TRGBQuad;iValue:Integer=1); overload
begin
     with rgbColor do
          begin
               if ( (rgbRed   + iValue) <= 255) then inc(rgbRed  ,iValue);
               if ( (rgbGreen + iValue) <= 255) then inc(rgbGreen,iValue);
               if ( (rgbBlue  + iValue) <= 255) then inc(rgbBLue ,iValue);
          end;

end;

procedure Color_Lighten(var clColor :TColor  ;iValue:Integer=1); overload
var
   rgbCol : TRGBQuad;
begin
     //Einfach die RGB-Funktion benutzen
     rgbCol:=Color_ColToRGB(clColor);
     Color_Lighten(rgbCol,iValue);
     clColor:=Color_RGBToCol(rgbCol);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
// Eine Farbe abdunkeln
procedure Color_Darken(var rgbColor:TRGBQuad;iValue:Integer=1); overload
begin
     with rgbColor do
          begin
               if ( (rgbRed   - iValue) >= 0) then dec(rgbRed  ,iValue);
               if ( (rgbGreen - iValue) >= 0) then dec(rgbGreen,iValue);
               if ( (rgbBlue  - iValue) >= 0) then dec(rgbBLue ,iValue);
          end;
end;

procedure Color_Darken(var clColor :TColor  ;iValue:Integer=1); overload
var
   rgbCol : TRGBQuad;
begin
     //Einfach die RGB-Funktion benutzen
     rgbCol:=Color_ColToRGB(clColor);
     Color_Lighten(rgbCol,iValue);
     clColor:=Color_RGBToCol(rgbCol);
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Farbverläufe bestimmen
////////////////////////////////////////////////////////////////////////////////////////////////
function Color_CreateDiff(Min,Max,Pos:Cardinal;MinCol,MaxCol:TColor):TColor;
var
   i_max,i_pos:Integer;
   c_Min,c_Max:LongInt;
   i_MinR,i_MinG,i_MinB:Integer;
   i_MaxR,i_MaxG,i_MaxB:Integer;
   i_PosR,i_PosG,i_PosB:Integer;
begin
     //Einfach Fälle ausklammern
     if (Pos >  Max) then
        begin
             Result:=MaxCol;
             Exit;
        end;
     if (Pos <= Min) then
        begin
             Result:=MinCol;
             Exit;
        end;

     //Alles in RGB wandeln
     c_Min:=ColorToRGB(MinCol);
     c_Max:=ColorToRGB(MaxCol);

     //Auf Null setzen
     i_max:=Max-Min;
     i_Pos:=Pos-Min;
     if (i_Pos=0) then i_Pos:=1;
     if (i_Max=0) then i_Max:=1;

     //Werte auf 255 Normieren
     i_Pos:=Round( (i_Pos / i_Max) * 255);

     //In die Farbkanäle aufspalten
     i_MinB:=(c_Min and $00ff0000) shr 16;
     i_MinG:=(c_Min and $0000ff00) shr 8;
     i_MinR:=(c_Min and $000000ff);

     //In die Farbkanäle aufspalten
     i_MaxB:=(c_Max and $00ff0000) shr 16;
     i_MaxG:=(c_Max and $0000ff00) shr 8;
     i_MaxR:=(c_Max and $000000ff);

     //Nun die Differenzen bestimmen und die Farben abstufen (evtl. Vorzeichen wenden
     //Einmal blau
     i_PosB:=(i_MaxB-i_MinB)*i_Pos shr 8;
     i_PosB:=(i_MinB+i_PosB) shl 16;

     //Einmal Grün
     i_PosG:=(i_MaxG-i_MinG)*i_Pos shr 8;
     i_PosG:=(i_MinG+i_PosG) shl 8;

     //Einmal Rot
     i_PosR:=(i_MaxR-i_MinR)*i_Pos shr 8;
     i_PosR:=(i_MinR+i_PosR) shl 0;

     //Jetzt setzen wir die Farben zusammen
     Result:=i_PosB + i_PosG + i_PosR;
     Result:=Result and $00ffffff;
end;


end.
