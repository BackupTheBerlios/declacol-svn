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

///////////////////////////////////////////////////////////////////////////////////////
///
/// Einfache Klasse zur Verwaltung eines neuralen Netzwerkes mit
/// BackPropagation
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
/// v 07012006
///
///
///////////////////////////////////////////////////////////////////////////////////////

unit class_NeuralNet;

interface

uses Graphics,      // Wegen Bitmapausgabe...
     Classes,       // Wegen FileStream
     Unit_TypeDefs,
     class_random; 
type
  TNeuralNetwork = class(TObject)
  private
    { Private-Deklarationen }

    //Verhalten eines Nodiums
    //Drei verschiedene Funktionen zur Auswahl
    function NodeFunction_Lin  (input:longfloat):longfloat;        //Index 0
    function NodeFunction_Exp  (input:longfloat):longfloat;        //Index 1
    function NodeFunction_Tan  (input:longfloat):longfloat;        //Index 2

    //Funktione zur zufälligen Initialisierung der Internodien
    procedure InitInternodes   ();

    //Berechnung eines Nodiums in der Eingabeschicht
    procedure CalcInput        (u32Index:unsigned32);
    //Berechnung eines Nodiums in der ersten versteckten Schicht
    procedure CalcNode1        (u32Index:unsigned32);
    //Berechnung eines Nodiums in der zweiten versteckten Schicht
    procedure CalcNode2        (u32Index:unsigned32);

    //Netzwerk auslesen
    procedure SetInput         (Index:unsigned32;Value:longFloat);
    function  GetInput         (Index:unsigned32):longFloat;
    function  GetOutput        (Index:unsigned32):longFloat;

    procedure SetOutputWish    (Index:unsigned32;Value:longFloat);
    function  GetOutputWish    (Index:unsigned32):longFloat;
    function  GetTeachError    ():longFloat;

    //Lernrate
    procedure SetTeach         (Value:longFloat);
    function  GetTeach         ():longFloat;

    //Neuronenfunktion
    procedure SetFunc          (Value:signed32);
    function  GetFunc          ():signed32;

    //Grafikfunktionen
    function fround(input:longfloat;u32Size:unsigned32):longfloat;
    function CreateColorDiff(u32Min,u32Max,u32Pos:unsigned32;MinCol,MaxCol:TColor):TColor;

  public
    constructor Create (u32InputNodes,u32HiddenNodes1,u32HiddenNodes2,u32OutputNodes:unsigned32);
    destructor  Destroy(); override;
    { Public-Deklarationen }
    //Netzwerk neu Initialisieren
    procedure ResetNetWork     ();  

    //Komplettes Netzwerk berechnen
    procedure CalcNetwork      ();

    //Ausgabefehler bestimmen
    //Dazu muß aber vorher das Array aOutputWish richtig gefüllt sein
    procedure CalcError        ();

    function Save              (filename:string):boolean;
    function Load              (filename:string):boolean;

    //Mittels Backpropagation aus aOutput und aOutputWish das Netzwerk
    //trainieren
    procedure TeachNetwork     ();

    //Das Netzwerk auf ein Bitmap zeichnen
    procedure PaintNetwork     (Canvas:TCanvas);

    //Eingabeschicht veröffentlichen
    property Input[Index : unsigned32] : longFloat read GetInput write SetInput;

    //Ausgabeschicht veröffentlichen
    property Output[Index : unsigned32] : longFloat read GetOutput;
    function GetMaxOutput     ():longFloat;
    function GetMinOutput     ():longFloat;
    function GetMaxOutputIndex():signed32;
    function GetMinOutputIndex():signed32;


    //Trainingsvorgaben veröffentlichen
    property OutputWish[Index : unsigned32] : longFloat read GetOutputWish write SetOutputWish;

    //Fehler veröffentlichen
    property TeachError : longFloat read GetTeachError;

    //Lernfaktor
    property TeachRate  : longFloat read GetTeach write SetTeach;

    //Neuronenfunktion
    property NeuroFunction : signed32 read GetFunc write SetFunc;

  end;

const
     NEUROFUNC_LINEAR = 1;
     NEUROFUNC_EXP    = 2;
     NEUROFUNC_SIGMOID= 3;

implementation
uses SysUtils,                         // Wegen FloatToStr
     Math;                             // Wegen Tanh
////////////////////////////////////////////////////////////////////////////////
/// Unsere Internen Typen
type TNeuroFunction = function(input:longfloat):longfloat;

////////////////////////////////////////////////////////////////////////////////
/// Unsere Internen Variablen
var
   //Eingabeschicht
   aInput      : array of longfloat;
   aInputError : array of longfloat;
   aInputNode  : array of array of longfloat;

   //Erste Verarbeitungsschicht
   aCalc1      : array of longfloat;
   aCalc1Error : array of longfloat;
   aCalc1Node  : array of array of longfloat;

   //Zweite Verarbeitungsschicht
   aCalc2      : array of longfloat;
   aCalc2Error : array of longfloat;
   aCalc2Node  : array of array of longfloat;

   //Ausgabeschicht
   aOutput     : array of longfloat;
   aOutputError: array of longfloat;

   //Erwartete Ausgabeschicht
   aOutputWish : array of longfloat;

   //Mittlerer Fehler zwischen erwarteter Ausgaber und wirklicher Ausgabe
   fMeanError  : longFloat = 0;
   bMeanError  : Boolean= FALSE;

   //Anzahl der Eingabeknoten
   u32Input : unsigned32;

   //Anzahl der versteckten Knoten
   u32Calc1 : unsigned32;
   u32Calc2 : unsigned32;

   //Anzhal der Ausgangsknoten
   u32Output: unsigned32;

   //Fehlerkorrektur pro Durchlauf
   fTeach : longFloat   = 0.03;

   //Gewählte Neuronenfunktione
   iNodeFunction  : signed32  = 1; //Standard ist Sigmoid
   pNodeFunction  : TNeuroFunction;


////////////////////////////////////////////////////////////////////////////////
///
/// Private Funktionen des Netzwerkes
///
///
////////////////////////////////////////////////////////////////////////////////
//Hilfsfunktionen für die Bitmapausgabe

////////////////////////////////////////////////////////////////////////////////////////////////
//Erzeugt einen Farbton zwischen MinCol und MaxCol je nach Pos zwischen Min und Max
////////////////////////////////////////////////////////////////////////////////////////////////
function TNeuralNetwork.CreateColorDiff(u32Min,u32Max,u32Pos:unsigned32;MinCol,MaxCol:TColor):TColor;
var
   u32Index : unsigned32;
   rgbMin   : TRGB;
   rgbMax   : TRGB;
   rgbResult: TRGB;
   bMaxR    : Byte;
   bMaxG    : Byte;
   bMaxB    : Byte;
   bMinR    : Byte;
   bMinG    : Byte;
   bMinB    : Byte;
   bResultR : Byte;
   bResultG : Byte;
   bResultB : Byte;
begin
     //Fehler abfangen
     if (u32Min > u32Max) then
        begin
             //Werte einfach tauschen
             u32Index:=u32Max;
             u32Max  :=u32Min;
             u32Min  :=u32Index;
        end;

     //Einfach Fälle ausklammern
     if (u32Pos >= u32Max) then
        begin
             Result:=MaxCol;
             Exit;
        end;
     if (u32Pos <= u32Min) then
        begin
             Result:=MinCol;
             Exit;
        end;

     //Farben in RGB wandeln ($00rrggbb)
     rgbMin:=TRGB( ColorToRGB(MinCol) );
     rgbMax:=TRGB( ColorToRGB(MaxCol) );

     //In die Farbkanäle aufspalten
     bMinR:=( (rgbMin and $00ff0000) shr 16 ) and $ff;
     bMinG:=( (rgbMin and $0000ff00) shr 8  ) and $ff;
     bMinB:=(  rgbMin and $000000ff);
     bMaxR:=( (rgbMax and $00ff0000) shr 16 ) and $ff;
     bMaxG:=( (rgbMax and $0000ff00) shr 8  ) and $ff;
     bMaxB:=(  rgbMax and $000000ff);

     //Position auf 0-255 normieren
     u32Index:=trunc( ( (u32Pos  - u32Min) / (u32Max - u32Min) ) * 255);

     //Farbkanäle für Ergebnis bestimmen


     //Und wieder Color daraus machen
     Result:=bResultB;
     Result:=Result shl 8;
     Result:=Result or bResultG;
     Result:=Result shl 8;
     Result:=Result or bResultR;
end;

/////////////////////////////////////////////////////////////////////////////////
//Rundet auf iSize stellen hinter dem Komma
function TNeuralNetWork.fround(input:longfloat;u32Size:unsigned32):longfloat;
begin
     if (u32Size=0) then u32Size:=1;
     u32Size:=u32Size * 10;

     result:=round(input * u32Size) / u32Size;
end;

////////////////////////////////////////////////////////////////////////////////
/// Netzwerkfunktionen
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// Die Beschreibungsfunktionen eines Nodiums


//mittels einer Rechteckfunktion
function TNeuralNetwork.NodeFunction_Lin(input:longfloat):longfloat;
begin
     if (input > 0.8) then result:=1 else result:=0;
end;

//mittels eine Exponentialfunktion
function TNeuralNetwork.NodeFunction_Exp(input:longfloat):longfloat;
const
     //Ausgleichskonstante
     sFac  : longfloat = 1;
begin
     //Ist input größe 50 , dann ist exp(50) Ergebnis quasi unendlich,
     //daher sparen wir uns die Berechnung
     if (input > 80) then
        begin
             result:=0;
             exit;
        end;

     //Ist input kleiner -50 ist das Ergebniss quasi 1
     if (input < -80) then
        begin
             result:=1;
             exit;
        end;
     //Schön über eine Exp-Funktion ausgleichen
     result:=1 / (1 + exp(-sFac * input));
end;

//mittels einer Tangensfunktion
function TNeuralNetwork.NodeFunction_Tan(input:longfloat):longfloat;
begin
     //Oben und Unten ohne rechnen zurückgeben
     if (input > 20) then
        begin
             result:=1;
             exit;
        end;
     if (input < 20) then
        begin
             result:=-1;
             exit;
        end;

     result:=tanh(Input);
end;

////////////////////////////////////////////////////////////////////////////////
// Alle Nodien mit null und die Internodien mit Zufallswerten initialisieren
procedure TNeuralNetwork.InitInternodes();
var
   u32Index1 : unsigned32;
   u32Index2 : unsigned32;
   random    : TRandom;
begin
     random:=TRandom.Create(0);
     //Eingang
     for u32Index1:=0 to u32Input - 1 do
         begin
              //Eingangknoten
              aInput[u32Index1]:=0;

              //Verbindungsgewichte
              for u32Index2:=0 to u32Calc1 - 1 do
                  begin
                       aInputNode[u32Index1,u32Index2]:=Random.GetFloat - 0.5;
                  end;
         end;

     //Perceptron 1
     for u32Index1:=0 to u32Calc1 - 1 do
         begin
              //Eingangknoten
              aCalc1[u32Index1]:=0;

              //Verbindungsgewichte
              for u32Index2:=0 to u32Calc2 - 1 do
                  begin
                       aCalc1Node[u32Index1,u32Index2]:=Random.GetFloat() - 0.5;
                  end;
         end;

     //Perceptron  2

     for u32Index1:=0 to u32Calc2 - 1 do
         begin
              //Eingangknoten
              aCalc2[u32Index1]:=0;

              //Verbindungsgewichte
              for u32Index2:=0 to u32Output - 1 do
                  begin
                       aCalc2Node[u32Index1,u32Index2]:=Random.GetFloat() - 0.5;
                  end;
         end;

     //Ausgabe
     for u32Index1:=0 to u32Output - 1 do
         begin
              //Eingangknoten
              aOutput[u32Index1]:=0;
         end;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Eingabe-Nodium berechnen
procedure TNeuralNetwork.CalcInput(u32Index : unsigned32);
var
   u32Pos  : unsigned32;
   fTemp : longfloat;
begin
     fTemp:=0;

     //Alle eingänge eines Nodiums in der Perceptronschicht durchrechnen
     for u32Pos := 0 to u32Input-1 do
         begin
              //Level aufaddieren (nur für ein Eingabenodium
              fTemp:=fTemp + aInput[u32Pos] * aInputNode [u32Pos][u32Index];
         end;

     //Einpegeln
     fTemp:=pNodeFunction(fTemp);

     //Auf Perceptron umkopieren
     aCalc1[u32Index]:=fTemp;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Perceptron-Nodium berechnen
procedure  TNeuralNetwork.CalcNode1(u32Index : unsigned32);
var
   u32Pos  : unsigned32;
   fTemp   : longFloat;
begin
     fTemp:=0;

     //Alle eingänge eines Nodiums in der Perceptronschicht durchrechnen
     for u32Pos := 0 to u32Calc1-1 do
         begin
              //Level aufaddieren (nur für ein Eingabenodium
              fTemp := fTemp + aCalc1[u32Pos] * aCalc1Node [u32Pos][u32Index];
         end;
     //Einpegeln
     fTemp:=pNodeFunction(fTemp);

     //Auf Perceptron umkopieren
     aCalc2[u32Index]:=fTemp;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein Perceptron-Nodium berechnen
procedure  TNeuralNetwork.CalcNode2(u32Index : unsigned32);
var
   u32Pos  : unsigned32;
   fTemp   : longFloat;
begin
     fTemp:=0;

     //Alle eingänge eines Nodiums in der Perceptronschicht durchrechnen
     for u32Pos := 0 to u32Calc2-1 do
         begin
              //Level aufaddieren (nur für ein Eingabenodium
              fTemp := fTemp + aCalc2[u32Pos] * aCalc2Node [u32Pos][u32Index];
         end;
     //Einpegeln
     fTemp:=pNodeFunction(fTemp);

     //Auf Perceptron umkopieren
     aOutput[u32Index]:=fTemp;
end;


////////////////////////////////////////////////////////////////////////////////
///
/// Öffentliche Funktionen des Netzwerkes
///
///
////////////////////////////////////////////////////////////////////////////////
/// Der Konstruktor erzeugt die dynamischen Arrays für das Netzwerk.
/// damit kann  nie der Fall vorkommen, daß die Arrays nicht initialisiert sind
constructor TNeuralNetwork.Create (u32InputNodes,u32HiddenNodes1,u32HiddenNodes2,u32OutputNodes:unsigned32);
var
   u32Index : unsigned32;
begin
     //Fehler abfangen
     if (u32Input        < 1) then u32Input        := 1;
     if (u32HiddenNodes1 < 1) then u32HiddenNodes1 := 1;
     if (u32HiddenNodes2 < 1) then u32HiddenNodes2 := 1;
     if (u32Output       < 1) then u32Output       := 1;

     //Benutzte Neuronenfunktion setzen
     SetFunc(iNodeFunction);

     //Interne Werte setzen
     u32Input :=u32InputNodes;
     u32Calc1 :=u32HiddenNodes1;
     u32Calc2 :=u32HiddenNodes2;
     u32Output:=u32OutputNodes;
     //Und Arrays setzen
     try
        //Eingabe
        SetLength(aInput         ,u32Input);
        SetLength(aInputError    ,u32Input);
        SetLength(aInputNode     ,u32Input);
        for u32Index:=0 to u32Input-1 do SetLength(aInputNode[u32Index],u32Calc1);

        //Hidden1
        SetLength(aCalc1         ,u32Calc1);
        SetLength(aCalc1Error    ,u32Calc1);
        SetLength(aCalc1Node     ,u32Calc1);
        for u32Index:=0 to u32Calc1-1 do SetLength(aCalc1Node[u32Index],u32Calc2);

        //Hidden2
        SetLength(aCalc2         ,u32Calc2);
        SetLength(aCalc2Error    ,u32Calc2);
        SetLength(aCalc2Node     ,u32Calc2);
        for u32Index:=0 to u32Calc2-1 do SetLength(aCalc2Node[u32Index],u32Output);

        //Ausgabe
        SetLength(aOutput        ,u32Output);
        SetLength(aOutputError   ,u32Output);

        //Fehler
        SetLength(aOutputWish    ,u32Output);

        //Und Netzwerk vorinitialisieren
        InitInternodes();
     except
        //Fehler, dann Abflug
        //Destroy;
     end;
end;


////////////////////////////////////////////////////////////////////////////////
/// Der Destruktor gibt einfach alle Arrays wieder frei
destructor  TNeuralNetwork.Destroy();
var
   iIndex : signed32;
begin
     //Alle Arrays freigeben
     //Wird zwar normalerweise automatisch erledigt, aber es ist einfach guter
     //Stil
        //Eingabe
        for iIndex:=0 to length(aInputNode)-1 do SetLength(aInputNode[iIndex],0);
        SetLength(aInput         ,0);
        SetLength(aInputError    ,0);
        SetLength(aInputNode     ,0);
        //Hidden1
        for iIndex:=0 to length(aCalc1Node)-1 do SetLength(aCalc1Node[iIndex],0);
        SetLength(aCalc1         ,0);
        SetLength(aCalc1Error    ,0);
        SetLength(aCalc1Node     ,0);
        //Hidden2
        for iIndex:=0 to length(aCalc2Node)-1 do SetLength(aCalc2Node[iIndex],0);
        SetLength(aCalc1         ,0);
        SetLength(aCalc1Error    ,0);
        SetLength(aCalc1Node     ,0);
        //Ausgabe
        SetLength(aOutput        ,0);
        SetLength(aOutputError   ,0);

        //Fehler
        SetLength(aOutputWish    ,0);
end;

////////////////////////////////////////////////////////////////////////////////
/// Properties
////////////////////////////////////////////////////////////////////////////////

/// Den Wert eines Eingabenodiums mit dem Index "INDEX" auf Value setzen
procedure TNeuralNetwork.SetInput         (Index:unsigned32;Value:longFloat);
begin
     //Fehler abfangen
     if (Index < u32Input) then
        begin
             aInput[Index]:=longFloat(Value);
        end
end;

// Den aktuellen Wert des Eingabenodiums mit dem Index "INDEX" auslesen
function  TNeuralNetwork.GetInput         (Index:unsigned32):longFloat;
begin
     //Fehler abfangen
     if (Index < u32Input) then
        begin
             result:=aInput[Index];
        end
     else
        begin
             result:=-255
        end;
end;

//Den Zustand des Ausgabenodiums mit dem Index "INDEX" auslesen
function  TNeuralNetwork.GetOutput         (Index:unsigned32):longFloat;
begin
     //Fehler abfangen
     if (Index < u32Output) then
        begin
             result:=aOutput[Index];
        end
     else
        begin
             result:=-255
        end;
end;

//Den höchsten Ausgabewert zurückliefern
function  TNeuralNetwork.GetMaxOutput ():longFloat;
var
   u32Index : signed32;
begin
     //Alle ausgaben durchgehen
     result:=-$ffff;
     for u32Index:=0 to u32Output-1 do
         begin
              if (aOutput[u32Index]>result) then
                 begin
                      result:=aOutput[u32Index];
                 end;
         end;
end;

//Den kleinsten Ausgabewert zurückliefern
function  TNeuralNetwork.GetMinOutput ():longFloat;
var
   u32Index : unsigned32;
begin
     //Alle ausgaben durchgehen
     result:=$ffff;
     for u32Index:=0 to u32Output-1 do
         begin
              if (aOutput[u32Index]<result) then
                 begin
                      result:=aOutput[u32Index];
                 end;
         end;
end;

//Den Index des höchsten Ausgabewert zurückliefern
function  TNeuralNetwork.GetMaxOutputIndex ():signed32;
var
   u32Index : unsigned32;
   fTemp  : longFloat;
begin
     //Alle ausgaben durchgehen
     fTemp :=-$ffff;
     result:=-1;
     for u32Index:=0 to u32Output-1 do
         begin
              //Und die Position merken
              if (aOutput[u32Index]>fTemp) then
                 begin
                      result:=u32Index;
                      fTemp:=aOutput[u32Index];
                 end;
         end;
end;

//Den Index des niedrigsten Ausgabewert zurückliefern
function  TNeuralNetwork.GetMinOutputIndex ():signed32;
var
   u32Index : unsigned32;
   fTemp  : longFloat;
begin
     //Alle ausgaben durchgehen
     fTemp :=$ffff;
     result:=-1;
     for u32Index:=0 to u32Output-1 do
         begin
              if (aOutput[u32Index]<fTemp) then
                 begin
                      result:=u32Index;
                      fTemp:=aOutput[u32Index];
                 end;
         end;
end;

//Den erwünschten Ausgabezustand für Ausgabenodium "INDEX" setzen
procedure TNeuralNetwork.SetOutputWish  (Index:unsigned32;Value:longFloat);
begin
     //Fehler abfangen
     if (Index < u32Output) then
        begin
             aOutputWish[Index]:=Value;
        end;
end;

//Den Zustand des Ausgabenodiums mit dem Index "INDEX" auslesen
function  TNeuralNetwork.GetOutputWish     (Index:unsigned32):longFloat;
begin
     //Fehler abfangen
     if (Index < u32Output) then
        begin
             result:=aOutputWish[Index];
        end
     else
        begin
             result:=-255
        end;
end;

//Den mittleren Fehler nach einer Netzwerk UND Fehlerberechnung auslesen
function  TNeuralNetwork.GetTeachError         ():longFloat;
begin
     if (bMeanError) then result:=fMeanError else result:=-255;
end;

//Die Lernrate setzen (0..1)
procedure TNeuralNetwork.SetTeach         (Value:longFloat);
begin
     fTeach:=Value;
end;

//Die aktuelle Lernrate auslesen
function  TNeuralNetwork.GetTeach         ():longFloat;
begin
     Result:=fTeach;
end;


//Neuronenfunktion
procedure TNeuralNetwork.SetFunc          (Value:signed32);
begin
     //Begrenzen auf Maximal 16 Indezees
     if (Value<0 ) then Value:= 0;
     if (Value>16) then Value:=16;

     iNodeFunction:=Value;

     case (Value) of
          //Linear
           NEUROFUNC_LINEAR : pNodeFunction := @TNeuralNetwork.NodeFunction_Lin;
           NEUROFUNC_EXP    : pNodeFunction := @TNeuralNetwork.NodeFunction_Exp;
           NEUROFUNC_SIGMOID: pNodeFunction := @TNeuralNetwork.NodeFunction_Tan;
     else
         pNodeFunction := @TNeuralNetwork.NodeFunction_Exp;
     end;
end;

function  TNeuralNetwork.GetFunc          ():signed32;
begin
     Result:=iNodeFunction;
end;


//Das kpl. Netzwerk neu initialiseren
procedure TNeuralNetWork.ResetNetWork     ();
begin
     InitInterNodes();
end;


////////////////////////////////////////////////////////////////////////////////
// kpl. Berechnung des Netzwerkes durchführen
procedure TNeuralNetwork.CalcNetwork();
var
   u32Index : unsigned32;
begin
     //Eingabe auf erste Schicht berechnen
     for u32Index:=0 to u32Calc1-1 do
         begin
              CalcInput(u32Index);
         end;

     //Erste Schicht auf zweite Schicht berechnen
     for u32Index:=0 to u32Calc2-1 do
         begin
              CalcNode1(u32Index);
         end;

     //Zweite Schicht auf Ausgabe berechnen
     for u32Index:=0 to u32Output-1 do
         begin
              CalcNode2(u32Index);
         end;

     //Netzwerk wurde neu berechnet, damit sind alle alten Fehlerwerte
     //falsch.
     bMeanError:=FALSE;
end;


////////////////////////////////////////////////////////////////////////////////
/// Fehler der einzelnen internodien bestimmen
procedure TNeuralNetwork.CalcError();
var
   u32Index1 : unsigned32;
   u32Index2 : unsigned32;
   fError    : longFloat;
begin
     //Ausgangsfehler berechnen
     fMeanError :=0;
     fError     :=0;
     for u32Index1:=0 to u32Output -1 do
         begin
              //Der Fehler jedes Ausgabeneurons ist die Differenz zwischen Wunsch und Wirklichkeit 
              aOutputError[u32Index1]:=aOutputWish[u32Index1] - aOutput[u32Index1];
              //Hier bestimmen wir den mittleren Fehler einfach mit
              fMeanError:=fMeanError+abs(aOutputError[u32Index1]);
         end;
     //Mittlerer Fehler
     fMeanError:=fMeanError / u32Output;

     //Calc2 Fehler berechnen
     for u32Index1:=0 to u32Calc2 -1 do
         begin
              //Fehler des Nodes bestimmen
              for u32Index2:=0 to u32Output -1 do
                  begin
                       fError:=aOutputError[u32Index2] * aCalc2Node[u32Index1][u32Index2];
                  end;
              aCalc2Error[u32Index1]:=aCalc2[u32Index1] * (1 - aCalc2[u32Index1]) * fError
         end;

     //Calc1 Fehler berechnen
     for u32Index1:=0 to u32Calc1 -1 do
         begin
              //Fehler des Nodes bestimmen
              for u32Index2:=0 to u32Calc2 -1 do
                  begin
                       fError:=aCalc2Error[u32Index2] * aCalc1Node[u32Index1][u32Index2];
                  end;
              aCalc1Error[u32Index1]:=aCalc1[u32Index1] * (1 - aCalc1[u32Index1]) * fError
         end;

     //Input Fehler berechnen
     for u32Index1:=0 to u32Input -1 do
         begin
              //Fehler des Nodes bestimmen
              for u32Index2:=0 to u32Calc1 -1 do
                  begin
                       fError:=aCalc1Error[u32Index2] * aInputNode[u32Index1][u32Index2];
                  end;
              aInputError[u32Index1]:=aInput[u32Index1] * (1 - aInput[u32Index1]) * fError
         end;

     //OK, alle Fehler sind berechnet
     bMeanError:=TRUE;
end;


////////////////////////////////////////////////////////////////////////////////
//Aus den berechneten Fehlern der einzel Internodien deren Gewichte neu bestimmen
procedure TNeuralNetwork.TeachNetwork();
var
   u32Index1      : unsigned32;
   u32Index2      : unsigned32;
begin
     //Netzwerk nur trainieren,
     //wenn auch vorher die Fehler bestimmt wurden
     if (not bMeanError) then CalcError;

     //Gewichte anpassen
     /// Calc2 => Output
     for u32Index1:=0 to u32Calc2-1 do
         begin
              for u32Index2:=0 to u32Output-1 do
                  begin
                       //Das neue Gewicht ist das alte Gewicht plus Abweichung * Lernfaktor
                       aCalc2Node[u32Index1][u32Index2]:=aCalc2Node[u32Index1][u32Index2] + fTeach * aCalc2[u32Index1] * aOutputError[u32Index2];
                  end;
         end;

     // Calc1 => Calc2
     for u32Index1:=0 to u32Calc1-1 do
         begin
              for u32Index2:=0 to u32Calc2-1 do
                  begin
                       //Das neue Gewicht ist das alte Gewicht plus Abweichung * Lernfaktor
                       aCalc1Node[u32Index1][u32Index2]:=aCalc1Node[u32Index1][u32Index2] + fTeach * aCalc1[u32Index1] * aCalc2Error[u32Index2];
                  end;
         end;

     // Input => Calc1
     for u32Index1:=0 to u32Input-1 do
         begin
              for u32Index2:=0 to u32Calc1-1 do
                  begin
                       //Das neue Gewicht ist das alte Gewicht plus Abweichung * Lernfaktor
                       aInputNode[u32Index1][u32Index2]:=aInputNode[u32Index1][u32Index2] + fTeach * aInput[u32Index1] * aCalc1Error[u32Index2];
                  end;
         end;

end;


////////////////////////////////////////////////////////////////////////////////
/// Das Netzwerk in ein TCanvas zeichnen
procedure TNeuralNetwork.PaintNetwork(Canvas:TCanvas);
var
   u32Width  : unsigned32;
   u32Height : unsigned32;

   u32Index1 : unsigned32;
   fYPos1    : longFloat;
   fDiff1    : longFloat;
   s32Xpos1  : signed32;
   s32YPos1  : signed32;

   u32Index2 : signed32;
   fYPos2    : longFloat;
   fDiff2    : longFloat;
   s32Xpos2  : signed32;
   s32YPos2  : signed32;

begin
     //Fehler abfangen
     if (Canvas=nil) then Exit;

     //Breite und Höhe bestimmen
     u32Width :=Canvas.ClipRect.Right -Canvas.ClipRect.Left;
     u32Height:=Canvas.ClipRect.Bottom-Canvas.ClipRect.Top;

     //Fläche leeren
     Canvas.FillRect(Canvas.ClipRect);
     ////////////////////////////////////////////
     /// Internodien zeichnen
     ////////////////////////////////////////////
     //Eingang
     s32XPos1 :=round(u32Width * 0.05);
     fDiff1 :=(u32Height * 0.94) / u32Input;
     fYPos1 :=fDiff1 / 2;

     //Erste Schicht
     s32XPos2 :=round(u32Width * 0.35);
     fDiff2 :=(u32Height * 0.94) / u32Calc1;

     for u32Index1:=0 to u32Input - 1 do
         begin
              s32YPos1:=Trunc(fYpos1);

              //Pointer auf erste Schicht wieder setzen
              fYPos2 :=fDiff2 / 2;
              for u32Index2:=0 to u32Calc1 - 1 do
                  begin
                       s32YPos2:=Trunc(fYpos2);

                       //Stiftfarbe bestimmen
                       Canvas.Pen.Color:=CreateColorDiff(0,255, abs(round(aInputNode[u32Index1,u32Index2]* 128)) + 128,clGreen,clRed);

                       Canvas.MoveTo(s32XPos1,s32YPos1);
                       Canvas.LineTo(s32XPos2,s32YPos2);

                       //Position weiterführen
                       fYPos2:=fYPos2+fDiff2;
                  end;

              //Text ausgeben
              Canvas.TextOut(s32XPos1,s32YPos1,FloatToStr(fround(aInput[u32Index1],2)));

              fYPos1:=fYPos1+fDiff1;
         end;

     //Erste Schicht
     s32XPos1 :=round(u32Width * 0.35);
     fDiff1 :=(u32Height * 0.94) / u32Calc1;
     fYPos1 :=fDiff1 / 2;

     //Zweite Schicht
     s32XPos2 :=round(u32Width * 0.60);
     fDiff2 :=(u32Height * 0.94) / u32Calc2;

     for u32Index1:=0 to u32Calc1 - 1 do
         begin
              s32YPos1:=Trunc(fYpos1);

              //Pointer auf erste Schicht wieder setzen
              fYPos2 :=fDiff2 / 2;
              for u32Index2:=0 to u32Calc2 - 1 do
                  begin
                       s32YPos2:=Trunc(fYpos2);

                       //Stiftfarbe bestimmen
                       Canvas.Pen.Color:=CreateColorDiff(0,255, round(aCalc1Node[u32Index1,u32Index2]* 128) + 128,clGreen,clRed);

                       Canvas.MoveTo(s32XPos1,s32YPos1);
                       Canvas.LineTo(s32XPos2,s32YPos2);

                       fYPos2:=fYPos2+fDiff2;
                  end;

              //Text ausgeben
              Canvas.TextOut(s32XPos1,s32YPos1,FloatToStr(fround(aCalc1[u32Index1],2)));

              fYPos1:=fYPos1+fDiff1;
         end;

     //Zweite Schicht
     s32XPos1 :=round(u32Width * 0.60);
     fDiff1 :=(u32Height * 0.94) / u32Calc2;
     fYPos1 :=fDiff1 / 2;

     //Ausgabe
     s32XPos2 :=round(u32Width * 0.80);
     fDiff2 :=(u32Height * 0.94) / u32Output;

     for u32Index1:=0 to u32Calc2 - 1 do
         begin
              s32YPos1:=Trunc(fYpos1);

              //Pointer auf erste Schicht wieder setzen
              fYPos2 :=fDiff2 / 2;
              for u32Index2:=0 to u32Output - 1 do
                  begin
                       s32YPos2:=Trunc(fYpos2);

                       //Stiftfarbe bestimmen
                       Canvas.Pen.Color:=CreateColorDiff(0,255, round(aCalc2Node[u32Index1,u32Index2]* 128) + 128,clGreen,clRed);

                       Canvas.MoveTo(s32XPos1,s32YPos1);
                       Canvas.LineTo(s32XPos2,s32YPos2);

                       fYPos2:=fYPos2+fDiff2;
                  end;

              //Text ausgeben
              Canvas.TextOut(s32XPos1,s32YPos1,FloatToStr(fround(aCalc2[u32Index1],2)));

              fYPos1:=fYPos1+fDiff1;
         end;

     //Ausgabe Text
     s32XPos1 :=round(u32Width * 0.80);
     fDiff1   :=(u32Height * 0.94) / u32Output;
     fYPos1   :=fDiff1 / 2;

     for u32Index1:=0 to u32Output - 1 do
         begin
              s32YPos1:=Trunc(fYpos1);
              //Text ausgeben
              Canvas.TextOut(s32XPos1,s32YPos1,FloatToStr(fround(aOutput[u32Index1],2)));
              Canvas.TextOut(s32XPos1+50,s32YPos1,FloatToStr(fround(aOutputWish[u32Index1],2)));
              fYPos1:=fYPos1+fDiff1;

         end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Die Netzwerkdaten speichern und laden
function TNeuralNetwork.Save(filename:string):boolean;
var
   hFile     : TFileHandle;
   u32Index1 : unsigned32;
   u32Index2 : unsigned32;
   Stream    : THandleStream;
   Writer    : TWriter;
begin
     result:=FALSE;

     hFile:=FileCreate(filename);
     if (hFile>0) then
        begin
             //Stream erzeugen
             Stream:=THandleStream.Create(hFile);

             //Writer erzeugen
             Writer :=TWriter.Create(Stream,8192);
             try

                //Signatur schreiben  (Name+Anzahl der einzelnen Nodes)
                Writer.WriteSignature;
                Writer.WriteIdent('NeuroNet'+Format('%d|%d|%d|%d',[u32Input,u32Calc1,u32Calc2,u32Output]));

                //Und die Arrays hinterher
                for u32Index1:=0 to u32Input-1 do
                    begin
                         for u32Index2:=0 to u32Calc1-1 do
                             begin
                                  Writer.WriteFloat(aInputNode[u32Index1][u32Index2]);
                             end;
                    end;

                //Und die Arrays hinterher
                for u32Index1:=0 to u32Calc1-1 do
                    begin
                         for u32Index2:=0 to u32Calc2-1 do
                             begin
                                  Writer.WriteFloat(aCalc1Node[u32Index1][u32Index2]);
                             end;
                    end;

                //Und die Arrays hinterher
                for u32Index1:=0 to u32Calc2-1 do
                    begin
                         for u32Index2:=0 to u32Output-1 do
                             begin
                                  Writer.WriteFloat(aCalc2Node[u32Index1][u32Index2]);
                             end;
                    end;
                result:=TRUE;
             finally
                    Writer.Free;
                    Stream.Free;
                    FileClose(hFile);
             end;
        end;
end;

function TNeuralNetwork.Load(filename:string):boolean;
var
   hFile     : TFileHandle;
   u32Index1 : unsigned32;
   u32Index2 : unsigned32;
   Stream    : THandleStream;
   Reader    : TReader;
begin
     result:=FALSE;

     hFile:=FileOpen(filename,fmOpenRead);
     if (hFile>0) then
        begin
             //Stream erzeugen
             Stream:=THandleStream.Create(hFile);

             //Writer erzeugen
             Reader :=TReader.Create(Stream,8192);

             try

                //Signatur lesen
                Reader.ReadSignature;
                //Und prüfen, ob das Netzwerk passt (Name + Anzahl der Nodes);
                if (Reader.ReadIdent = 'NeuroNet'+Format('%d|%d|%d|%d',[u32Input,u32Calc1,u32Calc2,u32Output])) then
                   begin
                        //Signatur richtig,
                        //dann daten lesen
                        for u32Index1:=0 to u32Input-1 do
                            begin
                                 for u32Index2:=0 to u32Calc1-1 do
                                     begin
                                          aInputNode[u32Index1][u32Index2]:=Reader.ReadFloat;
                                     end;
                            end;
                        //dann daten lesen
                        for u32Index1:=0 to u32Calc1-1 do
                            begin
                                 for u32Index2:=0 to u32Calc2-1 do
                                     begin
                                          aCalc1Node[u32Index1][u32Index2]:=Reader.ReadFloat;
                                     end;
                            end;
                        //dann daten lesen
                        for u32Index1:=0 to u32Calc2-1 do
                            begin
                                 for u32Index2:=0 to u32Output-1 do
                                     begin
                                          aCalc2Node[u32Index1][u32Index2]:=Reader.ReadFloat;
                                     end;
                            end;
                        result:=TRUE;
                   end;
                finally;
                        Reader.Free;
                        Stream.Free;
                        FileClose(hFile);
                end;
        end;
end;
end.
