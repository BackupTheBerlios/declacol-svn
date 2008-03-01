###############################################
# Neuronales Netz
# (c) 2005 Borg@Sven-of-Nine.de
###############################################
###############################################
# Danke an :
#
# Ingenieur-Büro Tschaggelar für die Hinweise
# WikiPediea für die Grundlagen 
# Dan Patterson für die Literatur
#
###############################################
###############################################

Die Klasse  NeuralNetwork erlaubt es, sehr einfach ein
Neuronales Netz zu trainieren und einzusetzen.

Hier eine kurze Einleitung :

Es gibt nur wenige Funktionen, die notwendig sind, um
das Netzwerk zu benutzen. Alle für den Benutzer unwichtigen
Vorgänge sind als privat deklariert. Wer möchte kann natürlich
den Quelltext der Unit entsprechend abändern.


Um ein Netzwerk zu benutzen sind zuerst einige Vorab-Überlegungen
notwendig. Primär folgende :

1. Wie erfolgt die Eingabe der Daten in das Netzwerk ?
2. Wieviele Ausgänge braucht das Netzwerk, um meine Bedürfnisse zu
   erfüllen ?
3. Wieviele groß sollten die versteckten Schichten sein ?


Zur Vereinfachung des Vorgehens, seien diese Überlegungen anhand des
beiliegenden Beispiels zur Mustererkennung durchgeführt.

1. Da unsere Bitmaps, die unterschieden werden sollen aus 8x8 Pixeln bestehen,
bietet sich ein Eingangsfeld von 64 Neuronen an, welches einfach mit Nullen
und Einsen gefüllt wird, je nachde, ob ein Pixel Weiß oder Schwarz ist.

2. Wir wollen nur neun Muster trainieren, ergo brauchen wir nur neuen Ausgänge.
Der "richtige" Ausgang soll bei erfolgter Erkennung eins sein, alle anderen
null. In der Praxis wird ein Ausgang nur sehr selten den Wert 1.000 annehmen.
Daher wird einfach der Ausgang mit dem höchsten Wert als der "richtige"
definiert.

3. Es scheint vorteilhafter zu sein, wenn die erste versteckte Schicht gleich groß
oder größer als die Eingangsschicht ist. Die zweite Schicht sollte dann etwas über
dem Mittelwert zwischen erster Schicht und Ausgang liegen. In der Praxis hilft hier
leider nur ein wenig probieren.


Damit wären die drei wichtigsten Fragen schon geklärt. Die Implementation ist denkbar
einfach :


Training 

while (Fehler > 0.00001) do
   Bekanntes Muster eingeben
   Erwünschtes Ausgabesmuster eingeben
   Netz berechnen
   Fehler berechnen
   Netz Trainieren

Benutzen

   Bekanntes Muster eingeben
   Netz berechnen
   Ausgabe holen    


In Delphi kann dies dann wie folgt aussehen
Das Netzwerk soll darauf trainiert werden, zu prüfen, ob eine
8Bit-Zahl gerade oder ungerade ist.

var
  Neuro : TNeuralNetwork

procedure Init
begin
  //Netzwerk mit 8 Eingängen, 8 und 6 versteckten und 2 Ausgaben erzeugen
  Neuro:=TNeuralNetwork.Create(8,8,6,2);
end;


//Das Netz darauf trainieren, ob eine Zahl gerade oder ungerade ist
procedure Train
var
   iLoop : integer;
   iInput: integer;
   iDigit: integer;
   iBit  : integer;
begin
     //10000 Mal trainieren
     for iLoop:=0 to 10000 do
         begin
              //Zufallszahl holen
              iDigit:=Random(255);

             //Deren Bitmuster auf die Eingänge legen
             iBit:=1;
             for iInput:=0 to 7 do
                 begin
                      //Ist ein Bit gesetzt ?
                      if (iDigit and iBit >0) then
                         begin
                              //Ja
                              Neuro.Input[iInput]:=1;
                         end
                      else
                         begin
                              //Nein
                              Neuro.Input[iInput]:=1;
                         end;
                      //Nächstes Bit
                      iBit:=iBit shl 1;
                 end;

             //Erwünschte Ausgabe festlegen
             if (odd(iDigit)) then
                begin
                     //Bei einer ungeraden Zahl soll 
                     //Ausgabeneuron null 1 sein
                     Neuro.OutputWish[0]:=1;
                     Neuro.OutputWish[1]:=0;
                end	
             else
                begin
                     //Bei einer geraden Zahl soll 
                     //Ausgabeneuron eins 1 sein
                     Neuro.OutputWish[0]:=0;
                     Neuro.OutputWish[1]:=1;
                end;
             //Netzwerk berechnen
             Neuro.CalcNetwork;

             //Fehler berechnen
             Neuro.CalcError;
 
             //Und anhand des eben berechneten Fehlers trainieren
             Neuro.TeachNetwork;
         end;
end;



//Mit dem Netzwerk rechnen
//Ist iDigit ungerade wird TRUE gemeldet, ansonsten FALSE
function IsOdd(iDigit:integer):Boolean;
var
   iInput: integer;
   iDigit: integer;
   iBit  : integer;
begin
     //Bitmuster auf die Eingänge legen
     iBit:=1;
     for iInput:=0 to 7 do
         begin
              //Ist ein Bit gesetzt ?
              if (iDigit and iBit >0) then
                 begin
                      //Ja
                      Neuro.Input[iInput]:=1;
                 end
              else
                 begin
                     //Nein
                     Neuro.Input[iInput]:=1;
                 end;
              //Nächstes Bit
              iBit:=iBit shl 1;
         end;
     //Netzwerk berechnen
     Neuro.CalcNetwork;

     //Ist Ausgang null der größere, dann ist unsere Zahl ungerade
     if (Neuro.Output[0]>Neuro.Output[1]) then
        begin
             Result:=TRUE;
        end
     else
        begin
             Result:=FALSE;
        end;
end;



Damit wäre die Grundlegende Funktion der Klasse schon erklärt.


Hier eine Übersicht aller öffentlichen Funktionen und Eigenschaften

FUNKTIONEN
##########

Procedure ResetNetWork()
 Setzt das Netzwerk auf einen zufälligen Zustand. Damit kann eine neue Lernphase
 beginnen. Die Anzahl der Neuronen ändert sich nicht. Lediglich die "Nervenbahnen"
 werden mit Zfallswerten gefüllt.

Procedure CalcNetwork()
 Berechnet das kpl. Netzwerk mit den aktuellen Werten die Ausgänge können
 über die Eigenschaft Output[] geholt werden

procedure CalcError()
 Berechnet die Fehlerwerte zwischen der gewünschten und der realen Ausgabe des
 Netzwerkes. Diese Funktion ist nur währen der Lernphase notwendig.

procedure TeachNetwork()
 Pass die Werte der "Nervenbahnen" anhand des durch CalcError berechneten Fehlers
 an. Die Stärke der Anpassung wird durch den Wert TeachRate gesteuert. Als Sinnvoll
 haben sich Werte zwischen 0.001 und 0.1 gezeigt. Bei höheren Werte beginnt das Netz
 zu schwingen.

procedure PaintNetwork(Canvas:TCanvas)
 Zeichnet den aktuellen Zustand des Netzwerkes auf eine Canvas. Die Zustände der
 Neuronen werden durch Zahlen dargestellt, die der Bahnen durch Farbabstufungen.
 Grün bedeutet wenig, Rot bedeutet viel.

function Save(filename:string):boolean
 Speichert die Daten des Netzwerkes ab

function Load(filename:string):boolean
 Lädt die Daten des Netzwerkes

Eigenschaften
#############

Input[Index]:Single  (read/write)
 Die Eingabe Werte in das Netzwerk. Die Zählung beginnt bei null. Nicht zugelassene
 Indizees werden ignoriert.

Output[Index]:Single; (read)
 Ausgabe-Werte des Netzwerkes.  Die Zählung beginnt bei null. Nicht zugelassene
 Indizees werden ignoriert.

OutputWish[Index]:Single; (write)
 Gewünschte Ausgabe-Werte des Netzwerkes. Anhand der Differenz zwischen diesen Werten
 und den Output[] Werten erfolgt die Prägung des Netzwerkes.
 Die Zählung beginnt bei null. Nicht zugelassene Indizees werden ignoriert.

TeachError:Single (read)
 Der beim Letzten Lernvorgang aufgetretene mittlere Fehler

TeachRate:single (read/write)
 Lernrate des Netzwerkes. Sollte nicht über 0.1 hinausgehen. Default = 0.03

NeuroFunction(read/write)
 Intergerzahl, welche die zur Simulation der Neuronen benutzten Funktion umschaltet

  0 = Rechteckeckfunktion
      Erregungswerte über 0.6 werden als 1 interpretiert, ansonsten null.
      Sehr kurze Lernphasen, aber wenig Fehlertoleranz.

  1 = Exp-Funktion
      Die Erregung wird auf eine Exponentialfunktion abgebildet
      Sehr lange Lernphasen, hohe Fehlertoleranz.

  2 = Tangenshyperbolicus
      Ähnelt der Exp. Funktion, bindet aber zusätzlich negative Wert mit ein.
      Dadurch wird auch negative Erregung verarbeitet.
      Extrem lange Lenrphasen





