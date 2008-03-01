###############################################
# Neuronales Netz
# (c) 2005 Borg@Sven-of-Nine.de
###############################################
###############################################
# Danke an :
#
# Ingenieur-B�ro Tschaggelar f�r die Hinweise
# WikiPediea f�r die Grundlagen 
# Dan Patterson f�r die Literatur
#
###############################################
###############################################

Die Klasse  NeuralNetwork erlaubt es, sehr einfach ein
Neuronales Netz zu trainieren und einzusetzen.

Hier eine kurze Einleitung :

Es gibt nur wenige Funktionen, die notwendig sind, um
das Netzwerk zu benutzen. Alle f�r den Benutzer unwichtigen
Vorg�nge sind als privat deklariert. Wer m�chte kann nat�rlich
den Quelltext der Unit entsprechend ab�ndern.


Um ein Netzwerk zu benutzen sind zuerst einige Vorab-�berlegungen
notwendig. Prim�r folgende :

1. Wie erfolgt die Eingabe der Daten in das Netzwerk ?
2. Wieviele Ausg�nge braucht das Netzwerk, um meine Bed�rfnisse zu
   erf�llen ?
3. Wieviele gro� sollten die versteckten Schichten sein ?


Zur Vereinfachung des Vorgehens, seien diese �berlegungen anhand des
beiliegenden Beispiels zur Mustererkennung durchgef�hrt.

1. Da unsere Bitmaps, die unterschieden werden sollen aus 8x8 Pixeln bestehen,
bietet sich ein Eingangsfeld von 64 Neuronen an, welches einfach mit Nullen
und Einsen gef�llt wird, je nachde, ob ein Pixel Wei� oder Schwarz ist.

2. Wir wollen nur neun Muster trainieren, ergo brauchen wir nur neuen Ausg�nge.
Der "richtige" Ausgang soll bei erfolgter Erkennung eins sein, alle anderen
null. In der Praxis wird ein Ausgang nur sehr selten den Wert 1.000 annehmen.
Daher wird einfach der Ausgang mit dem h�chsten Wert als der "richtige"
definiert.

3. Es scheint vorteilhafter zu sein, wenn die erste versteckte Schicht gleich gro�
oder gr��er als die Eingangsschicht ist. Die zweite Schicht sollte dann etwas �ber
dem Mittelwert zwischen erster Schicht und Ausgang liegen. In der Praxis hilft hier
leider nur ein wenig probieren.


Damit w�ren die drei wichtigsten Fragen schon gekl�rt. Die Implementation ist denkbar
einfach :


Training 

while (Fehler > 0.00001) do
   Bekanntes Muster eingeben
   Erw�nschtes Ausgabesmuster eingeben
   Netz berechnen
   Fehler berechnen
   Netz Trainieren

Benutzen

   Bekanntes Muster eingeben
   Netz berechnen
   Ausgabe holen    


In Delphi kann dies dann wie folgt aussehen
Das Netzwerk soll darauf trainiert werden, zu pr�fen, ob eine
8Bit-Zahl gerade oder ungerade ist.

var
  Neuro : TNeuralNetwork

procedure Init
begin
  //Netzwerk mit 8 Eing�ngen, 8 und 6 versteckten und 2 Ausgaben erzeugen
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

             //Deren Bitmuster auf die Eing�nge legen
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
                      //N�chstes Bit
                      iBit:=iBit shl 1;
                 end;

             //Erw�nschte Ausgabe festlegen
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
     //Bitmuster auf die Eing�nge legen
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
              //N�chstes Bit
              iBit:=iBit shl 1;
         end;
     //Netzwerk berechnen
     Neuro.CalcNetwork;

     //Ist Ausgang null der gr��ere, dann ist unsere Zahl ungerade
     if (Neuro.Output[0]>Neuro.Output[1]) then
        begin
             Result:=TRUE;
        end
     else
        begin
             Result:=FALSE;
        end;
end;



Damit w�re die Grundlegende Funktion der Klasse schon erkl�rt.


Hier eine �bersicht aller �ffentlichen Funktionen und Eigenschaften

FUNKTIONEN
##########

Procedure ResetNetWork()
 Setzt das Netzwerk auf einen zuf�lligen Zustand. Damit kann eine neue Lernphase
 beginnen. Die Anzahl der Neuronen �ndert sich nicht. Lediglich die "Nervenbahnen"
 werden mit Zfallswerten gef�llt.

Procedure CalcNetwork()
 Berechnet das kpl. Netzwerk mit den aktuellen Werten die Ausg�nge k�nnen
 �ber die Eigenschaft Output[] geholt werden

procedure CalcError()
 Berechnet die Fehlerwerte zwischen der gew�nschten und der realen Ausgabe des
 Netzwerkes. Diese Funktion ist nur w�hren der Lernphase notwendig.

procedure TeachNetwork()
 Pass die Werte der "Nervenbahnen" anhand des durch CalcError berechneten Fehlers
 an. Die St�rke der Anpassung wird durch den Wert TeachRate gesteuert. Als Sinnvoll
 haben sich Werte zwischen 0.001 und 0.1 gezeigt. Bei h�heren Werte beginnt das Netz
 zu schwingen.

procedure PaintNetwork(Canvas:TCanvas)
 Zeichnet den aktuellen Zustand des Netzwerkes auf eine Canvas. Die Zust�nde der
 Neuronen werden durch Zahlen dargestellt, die der Bahnen durch Farbabstufungen.
 Gr�n bedeutet wenig, Rot bedeutet viel.

function Save(filename:string):boolean
 Speichert die Daten des Netzwerkes ab

function Load(filename:string):boolean
 L�dt die Daten des Netzwerkes

Eigenschaften
#############

Input[Index]:Single  (read/write)
 Die Eingabe Werte in das Netzwerk. Die Z�hlung beginnt bei null. Nicht zugelassene
 Indizees werden ignoriert.

Output[Index]:Single; (read)
 Ausgabe-Werte des Netzwerkes.  Die Z�hlung beginnt bei null. Nicht zugelassene
 Indizees werden ignoriert.

OutputWish[Index]:Single; (write)
 Gew�nschte Ausgabe-Werte des Netzwerkes. Anhand der Differenz zwischen diesen Werten
 und den Output[] Werten erfolgt die Pr�gung des Netzwerkes.
 Die Z�hlung beginnt bei null. Nicht zugelassene Indizees werden ignoriert.

TeachError:Single (read)
 Der beim Letzten Lernvorgang aufgetretene mittlere Fehler

TeachRate:single (read/write)
 Lernrate des Netzwerkes. Sollte nicht �ber 0.1 hinausgehen. Default = 0.03

NeuroFunction(read/write)
 Intergerzahl, welche die zur Simulation der Neuronen benutzten Funktion umschaltet

  0 = Rechteckeckfunktion
      Erregungswerte �ber 0.6 werden als 1 interpretiert, ansonsten null.
      Sehr kurze Lernphasen, aber wenig Fehlertoleranz.

  1 = Exp-Funktion
      Die Erregung wird auf eine Exponentialfunktion abgebildet
      Sehr lange Lernphasen, hohe Fehlertoleranz.

  2 = Tangenshyperbolicus
      �hnelt der Exp. Funktion, bindet aber zus�tzlich negative Wert mit ein.
      Dadurch wird auch negative Erregung verarbeitet.
      Extrem lange Lenrphasen





