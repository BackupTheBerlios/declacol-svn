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

unit Unit_Histogramm;

interface
uses Windows,Graphics,SysUtils;


procedure Histo_Init(XGridSize,YGridSize:Cardinal;cBack:TColor=clBlack;cGrid:TColor=clGreen;cBar:TColor=clGray);
procedure Histo_Paint(Canvas:TCanvas);
procedure Histo_SetData(Data:array of Integer);
procedure Histo_AddData(Data:         Integer);

implementation

var
   cHisto_Back : TColor = clBlack;
   cHisto_Grid : TColor = clGreen;
   cHisto_Bar  : TColor = clGray;

   cGridX      : Cardinal = 10;
   cGridY      : Cardinal = 10;

   aData  : Array of Integer;
   aHisto : Array of TPoint;

   iHistoMax : Integer;

////////////////////////////////////////////////////////////////////////////////
///Graphic leeren
////////////////////////////////////////////////////////////////////////////////
function ClearCanvas(Canvas:TCanvas):Boolean;
begin
     with Canvas do
          begin
               Brush.Style:=bsSolid;
               Brush.Color:=cHisto_Back;
               FillRect(ClipRect);
          end;
     Result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Gitter zeichnen
////////////////////////////////////////////////////////////////////////////////
function PaintGrid(Canvas:TCanvas):Boolean;
var
   ix,iy  : integer;
   iIndex : integer;
   iw,ih  : integer;
   iwd,ihd: integer;
begin
     with Canvas do
          begin
               //Größe bestimmen
               iw:=(ClipRect.Right-ClipRect.Left)-1;
               ih:=(ClipRect.Bottom-ClipRect.Top)-1;

               //Abstände zwischen den Linien
               iwd:=Round(iw/ (cGridX));
               ihd:=Round(ih/ (cGridY));

               //Startpunkte setzen
               ix:=iw;
               iy:=ih;

               //Farbe richtig setzen
               Pen.Color:=cHisto_Grid;
               Pen.Style:=psSolid;

               //Alle Linien von oben nach unten
               if (cGridX>0) then
               for iIndex:=0 to cGridX-1 do
                     begin
                          MoveTo(ix,0);
                          LineTo(ix,ih);
                          dec(ix,iwd);
                     end;

               //Alle Linien von Links nach rechts
               if (cGridY>0) then
               for iIndex:=0 to cGridY do
                     begin
                          MoveTo(0,iy);
                          LineTo(iw,iy);
                          dec(iy,ihd);
                     end;
          end;
     Result:=TRUE;
end;


//Einen Eintrag im Histogramm finden
function FindHistoEntry(Value:Integer):Integer;
var
   iTemp : Integer;
begin
     Result:=-1;
     iTemp:=0;
     while (iTemp < Length(aHisto)) do
           begin
                if (aHisto[iTemp].y=Value) then
                   begin
                        Result:=iTemp;
                        iTemp:=Length(aHisto);
                   end;
                inc(iTemp);
           end;
end;


procedure SortHisto();
var
   iTemp1 : Integer;
   iTemp2 : Integer;
   pTemp  : TPoint;
begin
     iTemp1:=0;
     while (iTemp1 < Length(aHisto)) do
           begin
                iTemp2:=1;
                while (iTemp2 < Length(aHisto)) do
                      begin
                           if (aHisto[iTemp2-1].y > aHisto[iTemp2].y) then
                              begin
                                   pTemp:=aHisto[iTemp2-1];
                                   aHisto[iTemp2-1]:=aHisto[iTemp2];
                                   aHisto[iTemp2]:=pTemp;
                              end;
                           inc(iTemp2);
                      end;
                inc(iTemp1);
           end;
end;

procedure CalcHisto();
var
   iTemp  : Integer;
   iPos   : Integer;
begin
     SetLength(aHisto,0);
     iTemp:=0;
     while (iTemp < Length(aData)) do
           begin
                //Gibt es unseren Eintrag schon ?
                iPos:=FindHistoEntry(aData[iTemp]);

                //Nein, dann zufügen
                if (iPos<0) then
                   begin
                        iPos:=Length(aHisto);
                        SetLength(aHisto,iPos+1);
                        aHisto[iPos].y:=aData[iTemp];
                        aHisto[iPos].x:=1;
                   end
                else
                   begin
                        //Ansonsten Anzahl erhöhen
                        inc(aHisto[iPos].x);
                   end;

                //Maximum gleich mitbestimmen
                if (aHisto[iPos].x>iHistoMax) then
                   begin
                        iHistoMax:=aHisto[iPos].x;
                   end;


                inc(iTemp);
           end;
end;


Procedure PaintHisto(Canvas:TCanvas);
var
   iTemp : Integer;
   xPos  : Single;
   yPos  : Single;
   yDiff : Single;
   xDiff : Single;
   iW    : Integer;
   iH    : Integer;
   iSize : Integer;
   rTemp : TRect;
begin
     //Keine Daten, dann direkt abbrechen
     if (Length(aHisto)<1) then Exit;

     iW:=(Canvas.ClipRect.Right - Canvas.ClipRect.Left)-1;
     iH:=(Canvas.ClipRect.Bottom - Canvas.ClipRect.Top)-1;

     //Schrittweiten berechnen
     xDiff:=iW / Length(aHisto);
     if (iHistoMax>0) then yDiff:=(iH * 0.9) / iHistoMax else yDiff:=1;


     //Breite eines Balken bestimmen
     iSize:=Round(xDiff);
     if (iSize <1) then iSize:=1;

     iTemp:=0;
     xPos:=0;
     while (iTemp < Length(aHisto)) do
           begin
                yPos:=aHisto[iTemp].x * yDiff;

                rTemp.Left  :=Trunc(xPos);
                rTemp.Right :=rTemp.Left + iSize;

                rTemp.Top   :=ih - Trunc(yPos);
                rTemp.Bottom:=iH;

                //Am Maximum den Wert anzeigen
                if (aHisto[iTemp].x=iHistoMax) then
                   begin
                        Canvas.Brush.Style:=bsClear;
                        Canvas.Font.Color:=cHisto_Grid;
                        Canvas.TextOut(rTemp.Left,rTemp.Top-15,IntToStr(aHisto[iTemp].y)+' ['+IntToStr(iHistoMax)+']');
                   end;

                Canvas.Brush.Color:=cHisto_Bar;
                Canvas.Brush.Style:=bsSolid;
                Canvas.Pen.Color:=clNone;
                Canvas.FillRect(rTemp);


                xPos:=xPos + xDiff;
                inc(iTemp);
           end;
end;


procedure Histo_Init(XGridSize,YGridSize:Cardinal;cBack:TColor=clBlack;cGrid:TColor=clGreen;cBar:TColor=clGray);
begin
     cHisto_Back:=cBack;
     cHisto_Grid:=cGrid;
     cHisto_Bar :=cBar;

     cGridX:=XGridSize;
     cGridY:=YGridSize;

     SetLength(aData,0);
     SetLength(aHisto,0);
     iHistoMax:=0;
end;

procedure Histo_Paint(Canvas:TCanvas);
begin
     ClearCanvas(Canvas);
     PaintGrid  (Canvas);

     //Und Histogramm berechnen (Wenn wir nicht im DataAdd-Modus sind)
     if (Length(aData)=0) and (Length(aHisto)>0) then
        begin
             //Und Zeichnen
             SortHisto();
             PaintHisto(Canvas);
        end
     else
        begin
             //Und Zeichnen
             SortHisto();
             CalcHisto();
             PaintHisto(Canvas);
        end;
end;

procedure Histo_SetData(Data:array of Integer);
var
   iTemp  : Integer;
   iIndex : Integer;
begin
     //Die Daten in den lokalen Kontext kopieren
     iTemp:=0;
     SetLength(aData,0);

     while (iTemp < Length(Data)) do
           begin
                iIndex:=Length(aData);
                SetLength(aData,iIndex+1);
                aData[iIndex]:=Data[iTemp];
                inc(iTemp);
           end;
end;

procedure Histo_AddData(Data:Integer);
var
   iPos  : Integer;
begin
     //Gibt es unseren Eintrag schon ?
     iPos:=FindHistoEntry(Data);

     //Nein, dann zufügen
     if (iPos<0) then
        begin
             iPos:=Length(aHisto);
             SetLength(aHisto,iPos+1);
             aHisto[iPos].y:=Data;
             aHisto[iPos].x:=1;
        end
     else
        begin
             //Ansonsten Anzahl erhöhen
             inc(aHisto[iPos].x);
        end;

     //Maximum gleich mitbestimmen
     if (aHisto[iPos].x>iHistoMax) then
        begin
             iHistoMax:=aHisto[iPos].x;
        end;

    // messagebox(0,PCHar(IntToStr(Data)),PChar(IntToStr(Length(aHisto))),MB_OK);
end;

end.
