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
/// Einfaches Bumpmapping.
/// Ist vom Algorithmus sehr einfach, erzeugt aber ansehnliche Efffekte
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
/// Function
///
/// onActivate
/// InitBumpmapping (Bitmap to Bump)
///
/// Animation
/// DoBumpmapping(TargetBitmap,xPosOfLight,yPosOfLight);
///
/// onExit
/// FlushBumpmapping
///
///
///
///
///
/////////////////////////////////////////////////////////////////////////////////
unit Unit_Bumpmapping;

interface
uses Windows,Graphics;

///Bumpmapping
procedure Bump_Init(SourceBitMap:TBitmap;r:Single=3;g:Single=3.6;b:Single=4);
procedure Bump_Flush();
procedure Bump_Do(Target:TBitmap;XLight,YLight:Integer);
procedure Bump_SetSource(SourceBitMap:TBitmap);
procedure Bump_SetColor(r,g,b:single);

///Blobs

/////////////////////////////////////////////////////////////////////////////////
implementation
//Noch ein paar nützliche Types definieren
type PBitmap=^TBitmap;
     //Kleines Arry zum schnelleren Zugriff auf Bitmaps
     TLine = array[0..MaxInt div SizeOf(TRGBQUAD) - 1] of TRGBQUAD;
     PLine = ^TLine;

//Einige interne Variablen
var
  ColorArray  : Array of TRGBQuad;                //Array für die Farbtabelle beim Bumpmapping
  SourceArray : Array of Byte;                    //Quell Muster
  TargetBMP   : TBitmap;                          //ZielBitmap
  Black       : TRGBQuad;                         //Schwart
  White       : TRGBQuad;                         //Weiß

/////////////////////////////////////////////////////////////////////////////////
///Die Quelle für das Bumpmapping erzeugen
///aus einem Bitmap wird ein Schwarzweißarray erzeugt
procedure Bump_SetSource(SourceBitMap:TBitmap);
var
   iX,iY    : Integer;
   z        : Integer;
   sLine    : PLine;
   iDot     : Integer;
begin
     //QuellArray erzeugen
     SourceBitmap.PixelFormat:=pf32Bit;
     SetLength(SourceArray,SourceBitMap.Height*SourceBitMap.Width);

     for iY:=0 to SourceBitMap.Height-1 do
         begin
              //Scanline holen
              sLine:=SourceBitMap.ScanLine[iY];

              //Und durchwursten
              for iX:=0 to SourceBitMap.Width-1 do
                  begin
                       //Koordinaten errechnene
                       z:=iY * SourceBitMap.Width + iX;

                       //Grauwert bestimmen
                       idot:=       sLine[iX].rgbRed;
                       idot:=idot + sLine[iX].rgbGreen;
                       idot:=idot + sLine[iX].rgbBlue;
                       iDot:=(iDot div 3);
                       //Und eintragen
                       SourceArray[z]:=iDot;
                  end;
         end;

end;

/////////////////////////////////////////////////////////////////////////////////
//Farbtabelle erzeugen
procedure Bump_SetColor(r,g,b:single);
var
   iIndex : Integer;
   c      : Byte;
begin
     if (r > 4) then r:=4;
     if (r < 0) then r:=0;
     if (g > 4) then g:=4;
     if (g < 0) then g:=0;
     if (b > 4) then b:=4;
     if (b < 0) then b:=0;

     //Länge setzen
     SetLength(ColorArray,255);
     //Und erstmalschwarz machen
     FillMemory(ColorArray,255*SizeOf(TRGBQuad),0);

     //Schoener Blauverlauf
     for iIndex:=0 to 127 do
         begin
              c:=63 - iIndex div 2;

              //Hier kann die Farber eingestellt werden 0.0-4.0
              ColorArray[iIndex].rgbRed   :=round (c * r);
              ColorArray[iIndex].rgbGreen :=round (c * g);
              ColorArray[iIndex].rgbBlue  :=round (c * b);
         end;

     //Schwarz und Weiß definieren
     Black.rgbRed:=0;
     Black.rgbBlue:=0;
     Black.rgbGreen:=0;
     White.rgbRed:=255;
     White.rgbBlue:=255;
     White.rgbGreen:=255;
end;

/////////////////////////////////////////////////////////////////////////////////
//Eigentliches Bumpmapping ausführen
procedure Bump_Do(Target:TBitmap;XLight,YLight:Integer);
var
   iX,iY : Integer;
   sLine: PLine;
   iR1,iT1 : integer;
   iR,iT   : Integer;
   z       : Integer;
begin
     //Alle Zeile (bis auf oben und unten)
     for iY:=1 to TargetBMP.Height-2 do
         begin
              //Scanline holen
              sLine:=TargetBMP.ScanLine[iY];

              //Startposition im Quell-Array
              z:=iY * TargetBMP.Width;

              //Vorberechnung zur Beleuchtung
              iT1:=(iY-YLight);

              //Und alle Pixel durchwursten
              for iX:=1 to TargetBMP.Width-2 do
                  begin
                       //Position im Array aktualisieren
                       inc(z);

                       //Steigung in unserem Punkt bestimmen
                       iT:=iT1           - (SourceArray[z+TargetBMP.Width] - SourceArray[z-TargetBMP.Width]);
                       iR:=(iX - XLight) - (SourceArray[z+1]               - SourceArray[z-1]);

                       //Absolut machen
                       if (iR<0) then iR:=-iR;
                       if (iT<0) then iT:=-iT;

                       //Wie sieht die Steigung aus ?
                        iR1:=iR+iT;
                       if (iR1 < 129) then
                          begin
                               //Hohe steigung, Farbe holen
                               sLine[iX]:=ColorArray[iR1];
                          end
                       else
                          begin
                               //Ansonsten schwarz
                               sLine[iX]:=Black;
                          end;
                  end;
         end;
     //Ergebnis übergeben
     Target.Assign(TargetBMP);
end;

/////////////////////////////////////////////////////////////////////////////////
///Bumpmapping initialisieren
procedure Bump_Init(SourceBitMap:TBitmap;r:Single=3;g:Single=3.6;b:Single=4);
begin
     //Zielbitmap erzeugen
     TargetBMP:=TBitmap.Create;
     with TargetBMP do
          begin
               Height:=SourceBitMap.Height;
               Width:=SourceBitMap.Width;
               PixelFormat:=pf32Bit;
          end;

     //Farbtabellen initialisieren
     Bump_SetColor(r,g,b);

     //Und aus dem Quellbitmap ein Array machen
     Bump_SetSource(SourceBitmap);
end;

/////////////////////////////////////////////////////////////////////////////////
//Bumpmapping beenden
procedure Bump_Flush();
begin
     //Speicher freimachen
     TargetBMP.Free;
     SetLength(ColorArray,0);
end;



end.
