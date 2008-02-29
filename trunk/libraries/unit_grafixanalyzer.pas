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
/// Unit zur Grafikanalyse (Kantendetektion etc)
///
/// v 0.6.1
///
/////////////////////////////////////////////////////////////////////////////////
unit Unit_GrafixAnalyzer;

interface
uses Windows, Graphics, Math,Unit_Grafix;

var
   //Array welches die gefunden Objekte als Recs enthält
  GA_Objects: array of TRect;

/////////////////////////////////////////////////////////////////////////////////
// Hauptfunktion
/////////////////////////////////////////////////////////////////////////////////
//Objekte in einer Grafik umzeichnen
procedure TraceObjects(Bild: TBitmap; Size: Byte);

//Capture eines Bildbereiches machen
procedure CaptureArea(Bild: TBitMap; Area: TRect);

/////////////////////////////////////////////////////////////////////////////////
// Subfunktionen die einzelne Teile der Objekterkennung übernehmen
/////////////////////////////////////////////////////////////////////////////////

//In einem 8x8-Pixel Block die störung messen
function GetNoise(Bild: TBitMap; XPos, YPos: Integer; Size: Byte): Integer;

//Einen Block der Kantenlänge Size mit einer Farbe füllen
procedure FillBlock(Bild: TBitMap; XPos, YPos: Integer; Size: Byte; Col: TRGBQUAD);

//Ein Areal umranden
procedure PaintFrame(Bild: TBitMap; Area: TRect; Col: TRGBQUAD);

//Die Grenzen eine mit Farbe gefüllten Blockes bestimmen und zurückgeben
function GetArea(Bild: TBitMap; XPos, YPos: Integer; Col: TRGBQUAD): TRect;

//Prüft wieviele Ecken von AREA1 in AREA2 liegen
function IsInArea(Area1, Area2: TRect): Integer;

//AREA1 hat eine mindesten eine gemeinsame Kante mit AREA2
function HasEdge(Area1, Area2: TRect): Boolean;

/////////////////////////////////////////////////////////////////////////////////
// Grafikanalyse-Funktionen
/////////////////////////////////////////////////////////////////////////////////
Procedure  EdgeDetection(Source,Target:TBitmap;Level:Byte);

/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////

implementation
type
   //Ein kleines Array, welches beim schnelleren Zugriff auf Scanlines hilft
  TLine = array[0..MaxInt div SizeOf(TRGBQUAD) - 1] of TRGBQUAD;
  PLine = ^TLine;

/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
//
//  Die Funktionen
//
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
//Objecte umranden

procedure TraceObjects(Bild: TBitmap; Size: Byte);
var
  x, y, z        : Integer; //Variablen für die Koordinaten-Schleifen
  maxx, maxy     : Integer; //Hilfsvariablen für die Stückelung der Grafik
  Level          : Integer; //Automatisch bestimmter Pegel zur Objektbestimmung
  Line           : PLine; //HilfArray zum schnellen Zugriff auf Scanlines
  Col, TempCol   : TRGBQUAD; //Farbvariablen zur Objektbestimmung
  B, F           : TRGBQUAD; //Background und Foreground Farben
  Area           : TRect; //Bereich eines gefundenen Objektes
  Fields         : array of TRect; //Array aller gefundenen Objekte
  Rbild          : TRect; //Rec der Ursprungsgrafik
  found          : Boolean; //Flag zur Objektelimierung
begin
     //Bei einer Stückelung in SIZE Bereiche die maximalen Grenzen bestimmen
     maxx := ((Bild.Width - 1) div Size) * Size;
     maxy := ((Bild.Height - 1) div Size) * Size;

     //Rauschpegel der Objekterkennung (empirisch bestimmt)
     Level := Size * 3;


     //Objekt-Array vorsorglich löschen
     if (Length(GA_Objects)>0) then GA_Objects:=nil;

     //Farbformat anpassen
     Bild.PixelFormat := pf32BIT;

     //Einfach differenzierbare Vorder und Hintergrundfarbe wählen
     F := ColToRGB(clWhite);
     B := ColToRGB(clBlack);

     //Erst alle Objekte aus dem Hintergrund herauskristallisieren
     //Im Prinzip werden hierdurch vertikale Kanten verstärkt.
     for y := 0 to Bild.Height - 1 do
     begin
          //Eine komplette Zeile lesen
          Line := Bild.ScanLine[y];

          //Jedes Pixel analysieren
          for x := 0 to Bild.Width - 1 do
              begin
                   //Farbe holen
                   Col := Line[x];

                   //Sind zwei nebeneinanderliegende Pixel unterschiedlich (20%)
                   if (not IsNear(Col, TempCol, 80)) then
                      begin

                           //Ja, dann Vordergrundfarbe setzen
                           Line[x] := F;
                      end
                   else
                      begin
                           //Ansonsten Hintergrundfarbe setzen
                           Line[x] := B;
                      end;
                   //Farbe dieses durchlaufes für das nächste Pixel speichern
                   TempCol := Col;
              end;
          end;

     //Und nun anhand des Rauschens die Objekte bestimmen
     //Hat ein Feld mit der Kantenlänge Size mehr Pixelübergänge als Level, haben wir ein
     //Objekt gefunden. Dann wird der Bereich mit einem roten Kasten der Kantenlänge Size
     //gefüllt.
     //Vordergrundfarbe setzen
  F := ColToRGB(clRed);

     //Im Abstand von Size alle Blöcke absuchen
  y := 0;
  while (y < maxy) do
  begin
    x := 0;
    while (x < maxx) do
    begin
                           //Anzahl der Pixelübergänge zählen
      if (GetNoise(Bild, x, y, Size) > Level) then
      begin
                                   //Genug Übergänge
                                   //Block mit der Vordergrundfarbe füllen
        FillBlock(Bild, x, y, Size, F)
      end
      else
      begin
                                   //Zu wenig Übergänge
                                   //Block mit der Hintergrundfarbe füllen
        FillBlock(Bild, x, y, Size, B);
      end;
                           //Nächsten Block anvisieren
      x := x + Size;
    end;
                //Nächsten Block anvisieren
    y := y + Size;
  end;

     //Jetzt sind alle Objekte mit roten Klötzen überdeckt.
     //Daher brauch ich nur noch die Klötze zu ummalen und ich habe meine Objekte
     //Neue Farbe setzen
  B := ColToRGB(clGreen);

  y := 0;
  z := 0;

     //Im Abstand von Size alle Blöcke absuchen
  while (y < maxy) do
  begin
                //Neue komplette Zeile holen
    Line := Bild.ScanLine[y];
    x := 0;
    while (x < maxx) do
    begin
                           //Haben wir einen Blockanfang ?
                           //Pixelfarbe = Vordergrund ?
      if (IsEqual(Line[x], F)) then
      begin
                                   //Dann einen Rahmen drumziehen und den Bereich speichern
        Area := GetArea(Bild, x, y, F);
        PaintFrame(Bild, Area, B);

                                   //Neues Areal speichern
        SetLength(Fields, z + 1);
        Fields[z] := Area;
        inc(z);

                                   //Und hinter dem Objekt weitermachen
        x := Area.Right;
      end;
                           //Nächsten Block anvisieren
      x := x + Size;
    end;
                //Nächsten Block anvisieren
    y := y + Size;
  end;

     //Jetzt eliminieren wir noch alle Felder, die innerhalb andere liegen
     //Bereich der das Bild definiert als Rec speichern (+1Pixel)
  RBild.Left := -1;
  RBild.Top := -1;
  RBild.Right := Bild.Width;
  RBild.Bottom := Bild.Height;

  z := 0;

     //Alle gefundenen Areale durchgehen
  for x := 0 to Length(Fields) - 1 do
  begin
              //Flag auf False
    found := FALSE;

              //Bereich holen
    Area := Fields[x];

              //Gibt es ein Areal in das AREA reinpasst ?
    for y := 0 to Length(Fields) - 1 do
    begin
                       //Ja, dann gefunden Flag setzen
      if (IsInArea(Area, Fields[y]) > 2) then
      begin
        found := TRUE;
      end;
    end;

              //Liegt das Objekt auch sicher im gewünschten Bereich ?
    if (IsInArea(Area, RBild) <> 4) then
    begin
                      //found:=TRUE;
    end;

              //Mit dem Bereich alles OK, dann in Ausgabearray speichern
    if (found = FALSE) then
    begin
      SetLength(GA_Objects, z + 1);
      GA_Objects[z] := Area;
      inc(z);
      PaintFrame(Bild, Area, ColToRGB(clYellow));
    end;
  end;
     //Unnötiges Array freigeben
  Fields := nil;
end;


/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////

procedure CaptureArea(Bild: TBitMap; Area: TRect);
var
  H_Screen: THandle;
  H_Desktop:THandle;
begin

     Bild.FreeImage;

     Bild.Width := abs(Area.Right - Area.Left);
     Bild.Height := abs(Area.Bottom - Area.Top);
//     Bild.PixelFormat := pf32Bit;
     //Bildschirmhandle holen
     H_Desktop:=GetDeskTopWindow;
  H_Screen := GetDC(H_Desktop);
  if (H_Screen<>0) then
     begin

          //Unseren Ausschnitt rausholen
          BitBlt(Bild.Canvas.Handle, 0, 0, Bild.Width, Bild.Height, H_Screen, Area.Left, Area.Top, SRCCOPY);

          //Bildschirm wieder freigeben
          ReleaseDC(H_Desktop, H_Screen);
     end;
end;



/////////////////////////////////////////////////////////////////////////////////
// Subfunktionen zur Verarbeitung
/////////////////////////////////////////////////////////////////////////////////
//Rauschen in einem Quadrat der Kantenlänge Size bestimmen

function GetNoise(Bild: TBitMap; XPos, YPos: Integer; Size: Byte): Integer;
var
  x, y: Cardinal; //Variablen für Positionszähler
  Line: PLine; //Hilfsarray zum schnellen Zugriff auf Scanlines
  TempCol, Col: TRGBQUAD; //Farbvariablen zur Objekterkennung
begin
  Result := 0;
     //Ab der gewünschten Position nur ein Quardrat der Kantenlänge Size absuchen
  for y := YPos to (YPos + Size) do
  begin
              //Eine komplette Zeile holen
    Line := Bild.ScanLine[Y];
    for x := XPos to (XPos + Size) do
    begin
                       //Farbe des aktuellen Pixels holen
      Col := Line[x];
                       //Ist es genauso wie das davor ?
      if (not IsEqual(Col, TempCol))
        then
      begin
                                   //Nein, dann haben wir einen Übergang
                                   //Alse ergebnis erhöhen
        Inc(Result);
      end;
                       //Aktuelle Farbe für das nächste Pixel puffern
      TempCol := Col;
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
//Einen Block der Kantenlänge Size füllen
/////////////////////////////////////////////////////////////////////////////////

procedure FillBlock(Bild: TBitMap; XPos, YPos: Integer; Size: Byte; Col: TRGBQUAD);
var
  x, y: Cardinal; //Variablen für Positionszähler
  Line: PLine; //Hilfsarray zum schnellen Zugriff auf Scanlines
begin
     //Quadrat der Kantenlänge Size abfahren
  for y := YPos to (YPos + Size) do
  begin
              //ganze Zeile holen
    Line := Bild.ScanLine[Y];
              //Und alle gewünschten Pixel mit der Farbe COL füllen
    for x := XPos to (XPos + Size) do
    begin
      Line[x] := Col;
    end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
//Einen Rahmen um den Bereich AREA ziehen

procedure PaintFrame(Bild: TBitMap; Area: TRect; Col: TRGBQUAD);
var
  x, y: Cardinal; //Variablen für Positionszähler
  Line1, Line2: PLine; //Hilfsarray zum schnellen Zugriff auf Scanlines
begin
     //Linker und rechter Rand
  for y := Area.Top to Area.Bottom do
  begin
    Line1 := Bild.ScanLine[Y];
    Line1[Area.Left] := Col;
    Line1[Area.Right] := Col;
  end;

     //Oberer und unterer Rand
  Line1 := Bild.ScanLine[Area.Top];
  Line2 := Bild.ScanLine[Area.Bottom];
  for x := Area.Left to Area.Right do
  begin
    Line1[x] := Col;
    Line2[x] := Col;
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
//Einen Bereich mit der Farbe COL umranden und seine Koordinaten zurückgeben
/////////////////////////////////////////////////////////////////////////////////

function GetArea(Bild: TBitMap; XPos, YPos: Integer; Col: TRGBQUAD): TRect;
var
  Line: PLine; //Hilfsarray zum schnellen Zugriff auf Scanlines
begin
     //Linke obere Ecke
  Result.Left := XPos;
  Result.Top := YPos;

     //Einfach den gleichfarbigen Pixel solange nach rechts folgen, bis keine mehr
     //da sind. Damit haben wir den rechten Rand gefunden.
  Line := Bild.Scanline[YPos];
  while (XPos < Bild.Width) and (IsEqual(Line[XPos], Col)) do
  begin
    inc(XPos);
  end;
     //Erstes falsches Pixel abziehen
  dec(XPos);

     //Nun den gleichfarbigen Pixeln nach unten folgen bis am linken und am rechten Rand
     //kein Pixel mehr die Vordergrundfarbe enthält.
     //Damit ist der untere Rand gefunden.
  repeat
    begin
      Line := Bild.Scanline[YPos];
      inc(YPos);
    end;
  until (YPos >= Bild.Height) or
    (
    (IsEqual(Line[XPos], Col) = FALSE) and
    (IsEqual(Line[Result.Left], Col) = FALSE)
    );

     //Erstes Falsches Pixel ignorieren
  dec(YPos);

     //Voila der rechte untere Rand.
  Result.Right := XPos;
  Result.Bottom := YPos;
end;

/////////////////////////////////////////////////////////////////////////////////
//Wieviel Ecken von Area1 liegen in Area2
/////////////////////////////////////////////////////////////////////////////////

function IsInArea(Area1, Area2: TRect): Integer;
begin
  Result := 0;
     //Einfach alle Kanten miteinander vergleichen
  if (Area1.Left > Area2.Left) and
    (Area1.Left < Area2.Right) then inc(Result);

  if (Area1.Right < Area2.Right) and
    (Area1.Right > Area2.Left) then inc(Result);

  if (Area1.Top > Area2.Top) and
    (Area1.Top < Area2.Bottom) then inc(Result);

  if (Area1.Bottom < Area2.Bottom) and
    (Area1.Bottom > Area2.Top) then inc(Result);
end;

//AREA1 hat eine mindesten eine gemeinsame Kante mit AREA2 ?

function HasEdge(Area1, Area2: TRect): Boolean;
begin
     //Einfach alle Ecken miteinander vergleichen
  if (Area1.Left = Area2.Left) or
    (Area1.Right = Area2.Right) or
    (Area1.Top = Area2.Top) or
    (Area1.Bottom = Area2.Bottom) then
  begin
    Result := TRUE
  end
  else
  begin
    Result := FALSE;
  end;
end;


/////////////////////////////////////////////////////////////////////////////////
//Kantenverstärkung
/////////////////////////////////////////////////////////////////////////////////
Procedure  EdgeDetection(Source,Target:TBitmap;Level:Byte);
var
   SLine,TLine:PLine;
   x,y:Integer;
   t,b,w:TRGBQUAD;

begin
     //Alles muß 32 Bit sein
     Source.PixelFormat:=pf32Bit;
     Target.PixelFormat:=pf32Bit;

     //Neue Höhe unseres Bitmap
     Target.Height:=Source.Height;
     Target.Width:=Source.Width;

     t:=ColToRGB(clBlack);
     w:=ColToRGB(clWhite);
     b:=t;

     
     for y:=0 to Source.Height-1 do
         begin
              //Jede Zeile
              SLine:=Source.ScanLine[y];
              TLine:=Target.ScanLine[y];

              for x:=0 to Source.Width do
                  begin
                       if (IsNear(t,SLine[x],Level) ) then TLine[x]:=b else TLine[x]:=w;
                       t:=SLine[x];
                  end;
         end;
end;


end.
