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

unit class_random;
////////////////////////////////////////////////////////////////////////////////
///
/// Diverse Zufallsgeneratoren
/// (c) 2005 Borg@Sven-of-Nine.de
///
/// Vor Benutzung sollte mit RND_RandomSeed der Zufallsgenerator initialisiert
/// werden. Dies kann entweder direkt mit einer Integerzahl <> 0 geschehen oder
/// mit einem String, der zu einem Hash konvertiert wird und damit den RNG
/// "seeded". Während der Initialisierung wird der Generator einmal von selbst
/// initialisiert.
///
////////////////////////////////////////////////////////////////////////////////
/// Man sollte sich grundsätzlich für einen Generator entscheiden,
/// da sich mit einem Wechsel des Generators natürlich auch die Zufalls-
/// sequenzen ändern. Dies bedeutet, daß die Funktionen bei gleichem Samen
/// unterschiedliche Kennwörter liefern
///
////////////////////////////////////////////////////////////////////////////////
///Hier kann der verwendete Zufallsgenerator gewählt werden                 Speed %
//{$DEFINE USE_LC_PRNG}  //Zum Verständnis eines PseudoZufallsGenerators   100%
//{$DEFINE USE_LC+_PRNG} //Zum Verständnis eines PseudoZufallsGenerators   50%
//{$DEFINE USE_DELPHI_PRNG} //Nicht Empfohlen, da evtl. versionabhängig    110%
{$DEFINE USE_ECUYER_PRNG} //Empfohlen                                    105%
//{$DEFINE USE_MLC_PRNG} //Gut aber Langsam                                40%
////////////////////////////////////////////////////////////////////////////////

interface
//Typendefinitionen importieren
uses Unit_TypeDefs;

type TRandom = class (TObject)
        protected
                 u32Seed : unsigned32;
                 sName   : Longstring;
        private
               //Samen für Unsigned Generatoren
               u32Seed1 : unsigned32;
               u32Seed2 : unsigned32;
               u32Seed3 : unsigned32;

               //Samen für Signed Generatoren
               s32Seed1 : Signed32;
               s32Seed2 : Signed32;
               s32Seed3 : Signed32;

               //Den Standardsamen erzeugen
               function GetSeed():unsigned32;
        public
                constructor Create(u32Seed:unsigned32=0);

                // Den Zufallsgenerator initialisieren
                procedure Seed (u32Seed : unsigned32); overload;
                procedure Seed (sSeed   : LongString); overload;

                //Zufallszahl holen 0 <= Result < lwMax
                function  Get (u32Max : Unsigned32) : Unsigned32;

                //Konkrete Wertebereiche holen
                function  GetBit                ():Bit;
                function  GetByte               ():unsigned8;
                function  GetWord               ():unsigned16;
                function  GetLongWord           ():unsigned32;
                function  GetFloat              ():longfloat;

                property  Name : LongString read sName;
        end;

implementation

uses Unit_Bits,           //Wegen ROR;
     Class_Checksum       //Wegen StringToAdler32
     ;

////////////////////////////////////////////////////////////////////////////////
/// Konstruktor
constructor TRandom.Create(u32Seed:unsigned32=0);
begin
     Self.Seed(u32Seed);
end;


////////////////////////////////////////////////////////////////////////////////
//Ein paar Zentrale Funktionen
//Den "Standardsamen" liefern, wenn kein Wert übergeben wurde
function TRandom.GetSeed():unsigned32;
begin
     //Da nur Windows ein Tickcount hat benutzen wir hier die StdRandomfunktion
     //result:=GetTickCount();
     result:=unsigned32(random($ffffffff));
end;

////////////////////////////////////////////////////////////////////////////////
///
/// Zufallszahlen-Generatoren
///
/// Kapselung in einzelne Funktionen, um ihn einfach gegen andere RNGs tauschen
/// zu können
////////////////////////////////////////////////////////////////////////////////
{$IFDEF USE_LC_PRNG}
////////////////////////////////////////////////////////////////////////////////
// Linear Kongruent (LC-PRNG)
// Fällt im ChiQuadrat Test gnadenlos durch
// Aber recht gut in MonteCarlo und Korellation
// Gute Entropie
// Kurze Periode
// Kein Guter aber extrem leicht zu portieren
// Durch die vielen Multiplikationen eher langsam

function TRandom.Get(u32Max:unsigned32):unsigned32;
begin

//Hier ist ein Überlauf gewollt
{$IFOPT Q+}
        {$DEFINE OVERFLOW_ON}
        {$OVERFLOWCHECKS OFF}
{$ENDIF}
     //Nächste Zahl berechnen
     Self.u32Seed1 := (Self.u32Seed1 * 214013) + 2531011;
{$IFDEF OVERFLOW_ON}
        {$OVERFLOWCHECKS ON}
        {$UNDEF OVERFLOW_ON}
{$ENDIF}

     //Unsere Zahl in die Schranken weisen
     if (u32Max < High(longword)) then
        begin
             result:=Self.u32Seed1 mod u32Max;
        end
     else
        begin
             result:=self.u32Seed1;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Den RNG initialisieren
procedure TRandom.Seed(u32Seed : unsigned32);
begin
     //Name setzen
     self.sName := 'LC-PRNG';

     //Seed = 0 übergeben ?
     if (u32Seed=0) then
        begin
             //Dann die Systemzeit nehmen
             Self.u32Seed1:=Self.GetSeed();
        end
     else
        begin
             //Ansonsten normal Seeden und Salzen
             Self.u32Seed1:=u32Seed;
        end;
end;
{$ENDIF}

{$IFDEF USE_LC+_PRNG}
////////////////////////////////////////////////////////////////////////////////
// Linear Kongruent (LC-PRNG)
// Fällt im ChiQuadrat Test gnadenlos durch
// Aber recht gut in MonteCarlo und Korrellation
// Gute Entropie
// Kurze Periode
// Kein Guter aber extrem leicht zu portieren
// Durch die vielen Multiplikationen eher langsam
// die oberen 16Bit ergeben die eigentlich guten Zufallszahlen
// da wir den kpl. 32 Bit Bereich abdecken wollen, werden zwei
// RNGs geführt und deren signifikanten Bits genutzt

function TRandom.Get(u32Max:unsigned32):unsigned32;
begin
//Hier ist ein Überlauf gewollt
{$IFOPT Q+}
        {$DEFINE OVERFLOW_ON}
        {$OVERFLOWCHECKS OFF}
{$ENDIF}
     //Nächste Zahl berechnen
     Self.u32Seed1 := Self.u32Seed1 * 214013 + 2531011;

     //Nächste Zahl berechnen
     Self.u32Seed2 := Self.u32Seed2 * 214013 + 2531011;
//Hier ist ein Überlauf gewollt
{$IFDEF OVERFLOW_ON}
        {$OVERFLOWCHECKS ON}
        {$UNDEF OVERFLOW_ON}
{$ENDIF}

     //Von jeder Zahl nur die oberen 16 Bits benutzen
     result:=Self.u32Seed1 and $ffff0000;
     result:=result or ( (Self.u32Seed2 shr 16) and $0000ffff);

     //Unsere Zahl in die Schranken weisen
     if (u32Max < High(unsigned32)) then
        begin
             result:=result mod u32Max;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Den RNG initialisieren
procedure TRandom.Seed(u32Seed:unsigned32);
begin
     //Name setzen
     self.sName := 'LCPLUS-PRNG';

     //Seed = 0 übergeben ?
     if (u32Seed=0) then
        begin
             //Dann die Systemzeit nehmen
             Self.u32Seed1:=Self.GetSeed();
        end
     else
        begin
             //Ansonsten normal Seeden und Salzen
             Self.u32Seed1:=u32Seed;
        end;

     //Seed zwei einfach ein wenig anders initialisieren
     Self.u32Seed2:=BIT_ror(unsigned32(Self.u32Seed1),4);
end;
{$ENDIF}

{$IFDEF USE_MLC_PRNG}
//Weiter Implementierung eine Linear Kongruenten Generators
////////////////////////////////////////////////////////////////////////////////
// MultiLinear Kongruent (MLC-PRNG)
// Gutes ChiQuadrat
// Gutes MonteCarlo
// Gute Korrelation
// Gute Entropie
// Periode = 2,3E18
// Leicht zu portieren
// Durch Änderung der Ergebnisrechnung lassen sich auch
// negative Zahlen erzeugen
// Durch die vielen Multiplikationen/Divisionen eher langsam
// Funktionier quasi identisch wie LC nur das zwei parallele
// Generatoren mit Unterschiedlichen Koeffizienten genutzt
// werden.
function TRandom.Get(u32Max:unsigned32):unsigned32;
var
   k1     : Signed32;
   k2     : Signed32;
begin
//Bereichsprüfung deaktivieren
{$IFOPT R+}
        {$DEFINE RANGE_ON}
        {$R-}
{$ENDIF}

//Hier ist ein Überlauf gewollt
{$IFOPT Q+}
        {$DEFINE OVERFLOW_ON}
        {$Q-}
{$ENDIF}

     //Generator 1
     k1:=Self.s32Seed1 div 53668;
     Self.s32Seed1:= 40014 * (Self.s32Seed1 - k1 * 53668) - k1 * 12211;
     if (Self.s32Seed1 < 0) then inc(Self.s32Seed1,2147483563);
                                             ;
     //Generator 2
     k2:=self.s32Seed2 div 52774;
     self.s32Seed2:= 40692 * (self.s32Seed2 - k2 * 52774) - k2 * 3791;
     if (self.s32Seed2 < 0) then inc(self.s32Seed2,2147483399);

     //Und nun kombinieren wir die Signifikanten Bits
     k1:=BIT_ror(unsigned32(self.s32Seed1),8);
     k2:=BIT_rol(unsigned32(self.s32Seed2),8);

     result:=BIT_GetHi(unsigned32(k1)) shl 16;
     result:=result or BIT_GetHi(unsigned32(k2));

{$IFDEF OVERFLOW_ON}
        {$UNDEF OVERFLOW_ON}
        {$OVERFLOWCHECKS ON}
{$ENDIF}
{$IFDEF RANGE_ON}
        {$UNDEF RANGE_ON}
        {$RANGECHECKS ON}
{$ENDIF}

     //Und passen evtl den Bereich an
     if (u32Max<High(unsigned32)) then
        begin
             result:=result mod u32Max;
        end;

end;

////////////////////////////////////////////////////////////////////////////////
//Den RNG initialisieren
procedure TRandom.Seed(u32Seed : unsigned32);
begin
     //Name setzen
     sName := 'MLC-PRNG';

     if (u32Seed=0) then
        begin
             Self.s32Seed1:=signed32(Self.GetSeed());
        end
     else
        begin
             Self.s32Seed1:=signed32(u32Seed);
        end;
     //Seed zweiten rollen
     Self.s32Seed2:=Signed32( BIT_ror ( Longword ( Self.s32Seed1 ) , 4 ) );
end;
{$ENDIF}

{$IFDEF USE_DELPHI_PRNG}
/// Das hier wäre die Anwendung des in Delphi integrierten Generators
////////////////////////////////////////////////////////////////////////////////
//Eine Zufallszahl 0 <= Rnd < Max erzeugen
function TRandom.Get(u32Max:unsigned32):unsigned32;
begin
     //Da der Delphi-Generator keine Unsigned32 produziert müssen
     //wir ein wenig bastelen

{IFOPT RANGECHECKS}
{$RANGECHECKS OFF}
{ENDIF}
     //Ein Wort
     result:=Word ( Random ( High ( Word ) ) ) shl 16;
     //Noch ein Wort
     result:=result or Word ( Random ( High ( Word ) ) );
end;

////////////////////////////////////////////////////////////////////////////////
//Den RNG initialisieren
procedure TRandom.Seed(u32Seed : unsigned32);
begin
     //Name setzen
     Self.sName := 'GENERIC-PRNG';

     if (u32Seed=0) then
        begin
             RandSeed:=Self.GetSeed();
        end
     else
        begin
             RandSeed:=u32Seed;
        end;
end;
{$ENDIF}

{$IFDEF USE_ECUYER_PRNG}
////////////////////////////////////////////////////////////////////////////////
/// Zufallsgenerator nach Pierre L'Ecuyer
/// Etwas langsam und benötigt ZWINGEND 32Bit Zahlen
/// Gute Entropie
/// Gutes MonteCarlo
/// Gutes ChiQuadrat
/// Gute Korrelation
///
//  MUSS initialisiert werden
///
/// nach Pierre L'Ecuyer (1995)
/// "Maximally equidistributed combined Tausworthe generators"
/// http://www.iro.umontreal.ca/~lecuyer/
////////////////////////////////////////////////////////////////////////////////
//Eine Zufallszahl 0 <= Rnd < Max erzeugen
function TRandom.Get(u32Max:longword):unsigned32;
var
   u32Shift : unsigned32;
begin
     //Die Bitoperationen durchführen
     u32Shift      := ((Self.u32Seed1 shl 13) xor Self.u32Seed1) shr 19;
     Self.u32Seed1 := ((Self.u32Seed1 and 4294967294) shl 12) xor u32Shift;

     u32Shift      := ((Self.u32Seed2 shl  2) xor Self.u32Seed2) shr 25;
     Self.u32Seed2 := ((Self.u32Seed2 and 4294967288) shl 12) xor u32Shift;

     u32Shift      := ((Self.u32Seed3 shl  3) xor Self.u32Seed3) shr 11;
     Self.u32Seed3 := ((Self.u32Seed3 and 4294967280) shl 12) xor u32Shift;

     //Und ergebnis bestimmen
     result:=Self.u32Seed1 xor Self.u32Seed2 xor Self.u32Seed3;

     //Auf unser gewünschtes Ergebnis skalieren
     if (u32Max < High(unsigned32)) then
        begin
             result:=result mod (u32Max);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Den RNG initialisieren
procedure TRandom.Seed(u32Seed: unsigned32);
begin
     //Name setzen
     Self.sName := 'ECUYER95-PRNG';

     //Bei Seed gleich null den Timestamp nehmen
     if (u32Seed=0) then
        begin
             Self.u32Seed1:=Self.GetSeed();
        end
     else
        begin
             //Ansonsten den übergebenen Wert
             //Nicht mit XOR da der Generator mit Seeds=0 nicht
             //richtig startet
             Self.u32Seed1:=u32Seed;
        end;

     //Die anderen Seeds um 3 und 7 Bits rollen
     Self.u32Seed1:=Self.u32Seed1 shl 1;
     Self.u32Seed2:=Self.u32Seed1 shl 3;
     Self.u32Seed3:=Self.u32Seed1 shl 9;
end;
{$ENDIF}


////////////////////////////////////////////////////////////////////////////////
//Aus einem String einen Hash machen und damit den RNG seeden
procedure TRandom.Seed (sSeed:LongString);
var
   adler32 : TAdler32;
begin
     adler32:=TAdler32.Create();

     //Mit Hashwert initialisieren
     Self.Seed(unsigned32(adler32.fromstring(sSeed).u32checksum));

     adler32.free();
end;


////////////////////////////////////////////////////////////////////////////////
//Konkrete Zufallswerte holen
function  TRandom.GetFloat              ():longfloat;
begin
     //Die Generatoren (sollten) LongWord (longword) liefern
     result:=Self.Get(High(unsigned32)) / High(Unsigned32) ;
end;

function  TRandom.GetLongWord           ():unsigned32;
begin
     //Die Generatoren (sollten) LongWord (longword) liefern
     result:=Self.Get(High(unsigned32));
end;

function  TRandom.GetWord               ():unsigned16;
begin
     //Ein Word aus dem Strom extrahieren
     //Mit Mod 2^16.
     //Meine BitMod-Funktion benutzt nur Shifting und Minus
     //und braucht keine Sonderfallbehandlung
     result:= unsigned16 ( BIT_ModX(Self.GetLongword(),16 ) );
end;

function  TRandom.GetByte               ():unsigned8;
begin
     //Ein Byte aus dem Strom extrahieren
     //Meine BitMod-Funktion benutzt nur Shifting und Minus
     //und braucht keine Sonderfallbehandlung
     result:= BIT_Mod256( Self.GetLongWord() );
end;

function  TRandom.GetBit                ():Bit;
begin
     //Das höchstwertige Bit aus dem Strom extrahieren
     result:= BIT_GetBit( Self.GetLongWord (),16 );
end;


////////////////////////////////////////////////////////////////////////////////
/// Initialisierung
////////////////////////////////////////////////////////////////////////////////
initialization
end.
