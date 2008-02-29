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
/// Einfach blob-Bibliothek
///
/// komplett in Delphi, daher gibt es noch viel Platz für Optimierungen
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
///
///
///
///
///
///
///
///
////////////////////////////////////////////////////////////////////////////////
unit Unit_Blobs;

interface
uses Windows,Graphics;
//Initialisierung mit einer Bitmap, die als Hintergrund dient und gleichzeitig die
// größe vorgibt
procedure Blob_Init(BMP:TBitmap);

///Die Blobzeichenfläche löschen
procedure Blob_Clear();

//Einen Blob einzeichnen
procedure Blob_Paint(XPos,YPos:Integer);

//Die komplette Grafik ausgeben
procedure Blob_Get(BMP:TBitmap);

//Farbe des Blobs als rgb Teiler setzen
procedure Blob_SetColor(r,g,b:integer);


//Speicher freimachen
procedure Blob_Flush();

implementation

////////////////////////////////////////////////////////////////////////////////
//Noch ein paar nützliche Types definieren
type PBitmap=^TBitmap;
     //Kleines Arry zum schnelleren Zugriff auf Bitmaps
     TLine = array[0..MaxInt div SizeOf(TRGBQUAD) - 1] of TRGBQUAD;
     PLine = ^TLine;
////////////////////////////////////////////////////////////////////////////////
/// Interne variablen
////////////////////////////////////////////////////////////////////////////////
var
  //Farbtabelle
  aColMap : Array[0..512] of TRGBQuad;
  //Unsere Blobs
  aBlob   : Array[0..256]of Array[0..256]of byte;
  //Pufferbitmap;
  aBMP    : Array of Array of Integer;
  //Puffer für den Hintergrund
  aTMP    : Array of Array of Integer;

////////////////////////////////////////////////////////////////////////////////
/// Den BMPbuffer löschen und das gespeicherte Logo einzeichnen
////////////////////////////////////////////////////////////////////////////////
procedure Blob_Clear();
var
   xs,ys : integer;
begin
     for ys:=0 to Length(aBMP)-1 do
         begin
              for xs:=0 to Length(aBMP[ys])-1 do
                  begin
                       aBMP[ys][xs]:=aTMP[ys][xs];
                  end;
         end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Alle notwendigen Tabellen initialisieren
////////////////////////////////////////////////////////////////////////////////
procedure Blob_Init(BMP:TBitmap);
var
   iTemp : Integer;
   iTemp1: Integer;
   iTemp2: Integer;
   iTempd: Integer;
   sTemp1: Single;
   sTemp2: Single;
   Line  : PLine;
   r,g,b : Integer;
begin
     //Farbtabelle initialisieren
     iTempd:=(Length(aColMap)-1);
     //Alles Weiß
     for iTemp:=0 to Length(aColMap)-1 do
         begin
              aColMap[iTemp].rgbBlue  :=255;
              aColMap[iTemp].rgbGreen :=255;
              aColMap[iTemp].rgbRed   :=255;
         end;

     //Wir fangen einfach mit blau an
     sTemp1:=0;
     for iTemp:=0 to iTempd do
         begin
              r:=Trunc(sTemp1 / 3);
              b:=Trunc(sTemp1);
              g:=Trunc(sTemp1 / 1.5);
              aColMap[iTemp].rgbBlue  :=b;
              aColMap[iTemp].rgbGreen :=g;
              aColMap[iTemp].rgbRed   :=r;

              sTemp1:=sTemp1 + (255 / iTempd);
         end;

     //Pufferbitmap initialisieren
     SetLength(aBMP,BMP.Height);
     for iTemp:=0 to BMP.Height-1 do
         begin
              SetLength(aBMP[iTemp],BMP.Width);
              for iTemp1:=0 to BMP.Width-1 do
                  begin
                       aBMP[iTemp][iTemp1]:=random(Length(aColMap));
                  end;
         end;

     //Übergebenes Bitmap in den Logopuffer legen
     BMP.PixelFormat:=pf32Bit;
     SetLength(aTMP,BMP.Height);
     for iTemp:=0 to BMP.Height-1 do
         begin
              Line:=BMP.ScanLine[iTemp];
              SetLength(aTMP[iTemp],BMP.Width);
              for iTemp1:=0 to BMP.Width-1 do
                  begin
                       //Grauwert bestimmen
                       iTemp2:=         Line[iTemp1].rgbRed;
                       iTemp2:=iTemp2 + Line[iTemp1].rgbGreen;
                       iTemp2:=iTemp2 + Line[iTemp1].rgbBlue;
                       iTemp2:=(iTemp2 div 3);
                       //Und speichern
                       aTMP[iTemp][iTemp1]:=iTemp2;
                  end;
         end;


     //Blob erzeugen
     for iTemp:=0 to Length(aBlob)-1 do
         begin
              for iTemp1:=0 to Length(aBlob[iTemp])-1 do
                  begin
                       //Abstand zum Mittelpunkt bestimmen
                       sTemp1:=iTemp  - (Length(aBlob) shr 1);
                       sTemp2:=iTemp1 - (Length(aBlob) shr 1);
                       sTemp1:=(Length(aBlob) shr 1) - Sqrt(sTemp1*sTemp1+sTemp2*sTemp2);

                       //Auf Byte umskalieren
                       //sTemp1:=sTemp1 / ( (Length(aBlob)-1) shr 1);

                       iTemp2:=Trunc(sTemp1*2);
                       if (iTemp2>255) then iTemp2:=255;
                       if (iTemp2>=0) then
                          begin
                               aBlob[iTemp][iTemp1]:=iTemp2;
                          end
                       else
                          begin
                               aBlob[iTemp][iTemp1]:=0;
                          end;
                  end;
         end;

end;

////////////////////////////////////////////////////////////////////////////////
/// Einen Blob in unserem Puffer platzieren
////////////////////////////////////////////////////////////////////////////////
procedure Blob_Paint(XPos,YPos:Integer);
var
   xd,yd : Integer;
   xs,ys : Integer;
   xp,yp : Integer;
   xt,yt : Integer;
begin
     //Größen holen
     yd:=Length(aBMP)-1;
     xd:=Length(aBMP[yd])-1;

     //Auf die Mitte des Blobs positionieren
     XPos:=XPos - (Length(aBlob) shr 1);
     YPos:=YPos - (Length(aBlob) shr 1);


     //Fehler abfangen
     //XPos auuserhalb des Puffers, dann ignorieren
     if (XPos > xd) then Exit;
     if (YPos > yd) then Exit;

     //Unsere Position läuft am linken rand raus ?
     if (XPos < 0) then
        begin
             //Und Bloboffset bestimmtn
             xp:=-XPos;
             //Startposition auf null setzen
             XPos:=0;
        end
     else
        begin
             xp:=0;
        end;


     //Unsere Position läuft am oberen rand raus ?
     if (YPos < 0) then
        begin
             //Und Bloboffset bestimmtn
             Yp:=-YPos;
             //Startposition auf null setzen
             YPos:=0;
        end
     else
        begin
             yp:=0;
        end;

     //Und los gehts
     ys:=yp;
     yt:=YPos;
     while (yt <= yd) and (ys < length(aBlob)) do
           begin
                xt:=XPos;
                xs:=xp;
                while (xt <= xd) and (xs < Length(aBlob[ys])) do
                      begin
                           //Blob aufaddieren
                           inc(aBMP[yt][xt],aBlob[ys][xs]);

                           //Und begrenzen
                           if (aBMP[yt][xt]>=Length(aColMap)) then aBMP[yt][xt]:=Length(aColMap)-1;

                           inc(xt);
                           inc(xs);
                      end;
                inc(yt);
                inc(ys);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Unseren Puffer mittels der Farbtabelle auf ein Bitmap projezieren
////////////////////////////////////////////////////////////////////////////////
procedure Blob_Get(BMP:TBitmap);
var
   xd,yd   : Integer;
   xs,ys   : Integer;
   Line    : PLine;
begin
     yd:=Length(aBMP)-1;
     xd:=Length(aBMP[0])-1;

     BMP.PixelFormat:=pf32Bit;
     for ys:=0 to yd do
         begin
              Line:=BMP.ScanLine[ys];
              for xs:=0 to xd do
                  begin
                       //Und den Farbtabellenwert übertragen


                       Line[xs]:=aColMap[aBMP[ys][xs]];
                  end;
         end;

end;

procedure Blob_SetColor(r,g,b:integer);

var
   itemp  : integer;
   dr     : integer;
   dg     : integer;
   db     : integer;
begin
     //Startrichtung bestimmen
     if (r > 254) then dr:=-1 else dr:=1;
     if (g > 254) then dg:=-1 else dg:=1;
     if (b > 254) then db:=-1 else db:=1;

     for iTemp:=0 to (Length(aColMap)-1) do
         begin
              inc(r,dr);
              inc(g,dg);
              inc(b,db);

              if (r > 254) or (r < 1) then dr:=-dr;
              if (g > 254) or (g < 1) then dg:=-dg;
              if (b > 254) or (b < 1) then db:=-db;


              aColMap[iTemp].rgbRed  :=r;
              aColMap[iTemp].rgbGreen:=g;
              aColMap[iTemp].rgbBlue :=b;

         end;
end;



////////////////////////////////////////////////////////////////////////////////
/// Speicherfreimachen
////////////////////////////////////////////////////////////////////////////////
procedure Blob_Flush();
begin
     SetLength(aBMP,0);
     SetLength(aTMP,0);
end;

end.
