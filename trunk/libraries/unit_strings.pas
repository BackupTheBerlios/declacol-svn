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
unit Unit_Strings;
////////////////////////////////////////////////////////////////////////////////
///
/// Hilfsunit für leichtere Stringverarbeitung
///
/// (c) 2005 Borg@Sven-of-Nine.de
////////////////////////////////////////////////////////////////////////////////
///History
/// 20.04.06 Funktion Replace geänder, so daß a durch ba ersetzt werden kann
/// 06.04.06 Funktion String_ReplaceUmlaute eingefügt
/// 17.02.06 Schweren Fehler in Funktion String_RightPos behoben
///
////////////////////////////////////////////////////////////////////////////////
interface

uses
    Unit_Typedefs,
    Classes
    ;
////////////////////////////////////////////////////////////////////////////////
/// Einfügen / Ausschneiden / Ersetzen
//Linken Teil eines Strings holen
function String_Left (sInput:LongString;u32Size:Unsigned32)          : Longstring;
//Mittleren Teil eines String holen
function String_Mid  (sInput:LongString;u32Start,u32Size:Unsigned32) : Longstring;
//Rechten Teil eines String holen
function String_Right(sInput:LongString;u32Size:Unsigned32)          : Longstring;

//Stringteile ersetzen
//Ohne Groß/Klein
function String_Replace(sHaystack,sNeedle,sReplace:LongString)       : Longstring;

//Einen String anfügen, wenn er noch nicht vorhanden ist
function String_Append(sHaystack,sAppend:LongString)                 : Longstring;

//Einen String entfernen, wenn er vorhanden ist
function String_Remove(sHaystack,sTail:LongString)                   : Longstring;

//Einen Teil aus einem String rausschneiden
function String_Delete(sHaystack:LongString;u32Start,u32Length:unsigned32):Longstring;

////////////////////////////////////////////////////////////////////////////////
/// Suchen
//Binary-Safe Stringposition ohne Offset
function String_Pos(sHayStack,sNeedle:Longstring;u32Offset:unsigned32=0; CaseIgnore:Boolean=TRUE):unsigned32;

//Von Links suchen
function String_LeftPos (sHaystack,sNeedle:Longstring;u32Offset:Unsigned32=0;CaseIgnore:Boolean=TRUE):Unsigned32;
//Von rechts suchen
function String_RightPos(sHaystack,sNeedle:Longstring;u32Offset:Unsigned32=0;CaseIgnore:Boolean=TRUE):Unsigned32;

////////////////////////////////////////////////////////////////////////////////
// Längen ect.
//Länge eines Strings
function String_Length(sHaystack:Longstring):unsigned32;

//Anzahl der Zeichenfolgen in einem String
function String_Count (sHaystack:Longstring;sNeedle:LongString):unsigned32;

////////////////////////////////////////////////////////////////////////////////
/// Typprüfungen
function String_IsAlpha     (cInput:Char):Boolean;
function String_IsNumber    (cInput:Char):Boolean;
function String_IsAlphaNum  (cInput:Char):Boolean;
function String_IsBase64    (cInput:Char):Boolean;
function String_IsVocal     (cInput:Char):Boolean;
function String_IsConsonant (cInput:Char):Boolean;

////////////////////////////////////////////////////////////////////////////////
//Hilfsfunktionen zur leichteren Konvertierung
function String_StringToBoolean(sInput:LongString):Boolean;
function String_BooleanToString(bInput:Boolean)   :LongString;


//Strings zerlegen und wieder zusammensetzen
function String_Explode(sInput,sLimiter:Longstring; slResult: TStringlist; bAutoTrim : Boolean) : integer;
function String_Implode(slInput:TStringList;sLimiter:Longstring):Longstring;


////////////////////////////////////////////////////////////////////////////////
//Textanalyse
function String_ReplaceUmlaute(sInput:Longstring):Longstring;
//Bestimmt den Levenshtein-Abstand zwischen zwei Strings
//und gibt deren Übereinstimmung in Prozent zurück
function String_LevenShtein(sInput1,sInput2:Longstring):Unsigned32;


//Dateifunktionen
//Einen String in eine Datei schreiben
function String_Write(sFilename:Longstring;Value:Longstring):Boolean;
function String_Read(sFilename:Longstring):Longstring;

//Hilfsfunktionen
//Ermöglichen, ein CaseOf mit Strings zu machen
function StringToCaseSelect (Selector : string;CaseList: array of string): Integer;


////////////////////////////////////////////////////////////////////////////////
/// Filterfunktionen
/// Gibt nur Zeichen Zurück, auf die die gesetzten Filter zutreffen
/// mehrere Filter können gemischt werden
const
     //Grundfilter
     sftALPHA    =   1;          //Nur Buchstaben
     sftNUMBER   =   2;          //Nur Zahlen
     sftBASE64   =   4;          //Alle im erweiterten Base64-Standard
     sftMATH     =   8;          //Nur Mathematische Zeichen
     sftSIGN     =  16;          //Nur Satzzeichen
     sftSPACE    =  32;          //Nur Spaces
     sftBREAK    =  64;          //Nur Linebreaks
     sftURL      = 128;          //Alle in URLs zugelassenen Zeichen
     sftFLOAT    = 256;          //Fließkommazahlen
     sftPathes   = 512;          //Nur Pfadzeichen

     //Und vordefinierte
     sftALPHANUM = sftALPHA    + sftNUMBER;
     sftSECURE   = sftURL      + sftSIGN   + sftSPACE;
     sftSTANDARD = sftURL      + sftSIGN   + sftMATH   + sftSPACE  + sftBREAK;
     sftPATH     = sftALPHANUM + sftPATHES;

function String_Filter (sInput:Longstring;StringFilterType:Unsigned32=sftSTANDARD):Longstring;


////////////////////////////////////////////////////////////////////////////////
/// Funktionen für Dateipfade und Namen
//Pfadstrings lesefreundlich kürzen
function String_ShortenPath(sFilepath:Longstring;u32Size:Unsigned32):Longstring;



implementation
uses
    Sysutils,
    Unit_Stringsets,   //Sets für Leichtere Typeprüfung und Filterung
    Unit_Multiplatform //Wegen der Slashes
    ;
////////////////////////////////////////////////////////////////////////////////
/// Einfügen / Ausschneiden / Ersetzen
function String_Left (sInput:LongString;u32Size:Unsigned32)          :Longstring;
begin
     //Einfach mit Copy rausschneiden
     result:=Copy(sInput,1,u32Size);
end;

////////////////////////////////////////////////////////////////////////////////
function String_Mid  (sInput:LongString;u32Start,u32Size:Unsigned32)          :Longstring;
begin
     //Einfach mit Copy rausschneiden
     result:=Copy(sInput,u32Start,u32Size);
end;

////////////////////////////////////////////////////////////////////////////////
function String_Right(sInput:LongString;u32Size:Unsigned32) :Longstring;
var
   u32Length : unsigned32;
begin
     //Länge holen
     u32Length:=unsigned32(Length(sInput));

     //String zu kurz dann kpl. übergeben
     if (u32Length <= u32Size) then
        begin
             result:=sInput;
             exit;
        end;
     
     //Ansonsten einfach mit Copy rausschneiden
     result:=Copy(sInput,u32Length-u32Size+1,u32Size);
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
// Ersetzt alle Vorkommen von Needle durch Replace
function String_Replace (sHaystack, sNeedle, sReplace : Longstring) : Longstring;
var
   u32Pos : unsigned32;
   u32Len : unsigned32;
   u32Rep : unsigned32;
begin
     //Erstes Vorkommen suchen
     u32Pos      := String_Pos(sHaystack,sNeedle);
     u32Len      := String_Length(sNeedle);
     u32Rep      := String_Length(sReplace);

     //Und ersetzen, bis nichts mehr gefunden wird
     while (u32Pos > 0) do
           begin
                //Alten Teil rausschneiden
                Delete(sHaystack, u32Pos, u32Len);
                //Neuen Teil einfügen
                Insert(sReplace, sHaystack, u32Pos);

                //Nächstes Vorkommen suchen
                u32Pos      := String_LeftPos(sHaystack,sNeedle,u32Pos+u32Rep-1,FALSE);
           end;
     //Fertig
     Result := sHaystack;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
//Einen String anfügen, wenn er noch nicht vorhanden ist
function String_Append(sHaystack,sAppend:LongString)                 : Longstring;
begin
     result:=sHaystack;
     //Fehler abfangen
     if (sAppend='') then
        begin
             exit;
        end;
     if (sHaystack='') then
        begin
             result:=sAppend;
             exit;
        end;

     //Und Ende vergleichen
     if (String_Right(sHaystack,unsigned32(Length(sAppend)))<>sAppend) then
        begin
             //Und String anhängen, wenn es sich unterscheidet
             result:=result +  sAppend;
        end;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
//Einen String entfernen, wenn er vorhanden ist
function String_Remove(sHaystack,sTail:LongString)                 : Longstring;
var
   u32Tail  : unsigned32;
   u32Stack : unsigned32;
begin
     result:=sHaystack;
     //Fehler abfangen
     if (sTail='') then
        begin
             exit;
        end;
     if (sHaystack='') then
        begin
             exit;
        end;

     //Größe des Schwanzes holen
     u32Tail :=unsigned32 ( Length(sTail) );
     //Größes des Haufens holen
     u32Stack:=unsigned32 (Length(sHaystack));

     //Und Ende vergleichen
     if (String_Right(sHaystack,u32Tail)=sTail) then
        begin
             //Und String abschneiden, wenn er identisch ist
             delete (result,u32Stack - u32Tail,u32Tail);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Einen Teil aus einem String rausschneiden
function String_Delete(sHaystack:LongString;u32Start,u32Length:unsigned32):Longstring;
begin
     result:=sHaystack;
     Delete(result,u32Start,u32Length);
end;

////////////////////////////////////////////////////////////////////////////////
//Binary-Safe Stringposition
function String_Pos(sHayStack,sNeedle:Longstring;u32Offset:unsigned32=0; CaseIgnore:Boolean=TRUE):unsigned32;
var
   u32Index : unsigned32;
   u32Scan  : unsigned32;
   u32Stack : unsigned32;
   u32Needle: unsigned32;
begin
     result:=0;

     //Die größen der Teile holen
     u32Stack  := String_Length(sHaystack);
     u32Needle := String_Length(sNeedle);

     //Fehler abfangen
     if (u32Stack=0) or (u32Needle=0) then
        begin
             exit;
        end;

     //Alles auf Kleinschrift
     if (CaseIgnore) then
        begin
             sHaystack := LowerCase(sHaystack);
             sNeedle   := LowerCase(sNeedle);
        end;

     //Und eine einfache Suche machen
     u32Index:=u32Offset + 1;
     while (u32Index <= u32Stack) do
           begin
                //Zeichenvergleich
                u32Scan:=1;
                while (u32Scan  <= u32Needle) and
                      (u32Index <= u32Stack) do
                      begin
                           //Wenn die Zeichen nicht identisch sind,
                           //die schleife abbrechen
                           if (sHaystack[u32Index]<>sNeedle[u32Scan]) then
                              begin
                                   break;
                              end;

                           //Ansonsten nächstes Zeichen addressieren 
                           inc(u32Scan);
                           inc(u32Index);
                      end;
                //Ist u32Scan größer als u32Needle,
                //ist der kpl. Vergleich durchgelaufen und
                //wir haben unseren String gefunden
                if (u32Scan>u32Needle) then
                   begin
                        //Position merken
                        result:=(u32Index - u32Scan) +1 ;
                        //Und Schleifen beenden
                        break;
                   end;
                inc(u32Index);
           end;


end;

////////////////////////////////////////////////////////////////////////////////
/// Suchen
//Von Links suchen
function String_LeftPos (sHaystack,sNeedle:Longstring;u32Offset:Unsigned32=0;CaseIgnore:Boolean=TRUE):Unsigned32;
begin
     //Einfach über Pos bestimmen
     result:=String_Pos(sHaystack,sNeedle,u32Offset,CaseIgnore);
end;


//////////////////////////////////////////////////////////////////////////////////////////////////
//Von rechts suchen
function String_RightPos(sHaystack,sNeedle:Longstring;u32Offset:Unsigned32=0;CaseIgnore:Boolean=TRUE):Unsigned32;
var
   u32Pos      : unsigned32;
   u32Cmp      : unsigned32;
   u32Needle   : unsigned32;
   u32Haystack : unsigned32;
   bFound      : Boolean;
begin
     //Fehler annehmen
     result:=0;

     //Längen holen
     u32Needle  :=Length(sNeedle);
     u32Haystack:=Length(sHaystack);

     //Wenn einer der Strings null ist, dann abbrechen
     if (u32Needle  =0) then Exit;
     if (u32Haystack=0) then Exit;
     
     //Wenn die Nadel größer als der Heuhaufen ist abbrechen
     if (u32Needle > u32Haystack) then Exit;
     
     //Wenn das Offset einen Überlauf erzeugt abbrechen
     if (u32Offset > u32Haystack) then Exit;

     //Offset aus Needle rausrechnen
     if (u32Offset > u32Needle) then
        begin
             u32Offset:=u32Offset-u32Needle;
        end
     else
        begin
             u32Offset:=0;
        end;

     //Rechts starten
     u32Pos:=u32Haystack - (u32Needle-1) - u32Offset;

     //Und bis ganz nach links gehen
     while (u32Pos > 0) do
           begin
                //Als gefunden markieren
                bFound:=TRUE;

                //Erstes Zeichen gefunden ?
                if (sHaystack[u32Pos] = sNeedle[1]) then
                   begin
                        //Den Rest vergleichen
                        u32Cmp:=1;
                        while (u32Cmp < u32Needle) do
                              begin
                                   //Bei unterschiedlichen Zeichen abbrechen
                                   if (sHaystack[u32Pos+u32Cmp] <> sNeedle[u32Cmp+1]) then
                                      begin
                                           bFound:=FALSE;
                                           break;
                                      end;
                                   //Nächstes Zeichen vergleichen
                                   inc(u32Cmp);
                              end;
                   end
                else
                   begin
                        //Zeichen nicht gefunden, abbrechen
                        bFound:=FALSE;
                   end;

                //Gefunden ?
                if (bFound) then
                   begin
                        //Ergebnis merken und abbrechen
                        result:=u32Pos;
                        break;
                   end;
                //Nächstes Zeichen prüfen
                dec (u32Pos);
           end;
end;


//////////////////////////////////////////////////////////////////////////////////////////////////
//Längen etc.
//////////////////////////////////////////////////////////////////////////////////////////////////
//Länge eines Strings
function String_Length(sHaystack:Longstring):unsigned32;
begin
     result:=unsigned32(Length(sHaystack));
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
//Anzahl der Zeichenfolgen in einem String
function String_Count (sHaystack:Longstring;sNeedle:LongString):unsigned32;
var
   u32Pos : unsigned32;
begin
     u32Pos:=0;
     result:=0;
     repeat
           //Einfach mit Pos und Offset durchsuchen
           u32Pos:=String_LeftPos(sHaystack,sNeedle,u32Pos+1);
           //Und Mitzählen
           if (u32Pos>0) then
              begin
                   inc(result);
              end;
     until (u32Pos=0);
end;



//////////////////////////////////////////////////////////////////////////////////////////////////
//Typprüfungen
function String_IsAlpha   (cInput:Char):Boolean;
begin
     result:=cInput in ALPHASET;
end;

function String_IsNumber  (cInput:Char):Boolean;
begin
     result:=cInput in NUMBERSET;
end;

function String_IsAlphaNum(cInput:Char):Boolean;
begin
     result:=cInput in ALPHANUMSET;
end;

function String_IsBase64  (cInput:Char):Boolean;
begin
     result:=cInput in BASE64SET;
end;

function String_IsVocal     (cInput:Char):Boolean;
begin
     result:=cInput in VOCALSET;
end;

function String_IsConsonant (cInput:Char):Boolean;
begin
     result:=(cInput in ALPHASET) and not (cInput in VOCALSET);
end;

////////////////////////////////////////////////////////////////////////////////
//Typkonversion
//////////////////////////////////////////////////////////////////////////////////////////
//Einen Stringausdruck in Bool wandlen
//////////////////////////////////////////////////////////////////////////////////////////
function String_StringToBoolean(sInput:String):Boolean;
begin
     // Ein Set lohnt sicht wegen drei Möglichkeiten nicht
     result:=FALSE;
     if (string_pos(sInput,'true')>0) then result:=TRUE;
     if (string_pos(sInput,'on')>0)   then result:=TRUE;
     if (string_pos(sInput,'1')>0)    then result:=TRUE;
end;

//////////////////////////////////////////////////////////////////////////////////////////
//Einen Bool in String wandeln
//////////////////////////////////////////////////////////////////////////////////////////
function String_BooleanToString(bInput:Boolean):String;
begin
     if (bInput) then Result:='true' else Result:='false';
end;

//////////////////////////////////////////////////////////////////////////////////////////
//Explode
//////////////////////////////////////////////////////////////////////////////////////////
function String_Explode(sInput,sLimiter:Longstring; slResult: TStringlist; bAutoTrim:Boolean) : integer;
var
   u32Start : unsigned32;
   u32Stop  : unsigned32;
   u32Limit : unsigned32;
   u32Size  : unsigned32;
   sTemp    : Longstring;
begin
     result := 0;

     //Fehler abfangen
     u32Limit:=String_Length(sLimiter);
     if (u32Limit=0) then
        begin
             exit;
        end;


     //Den Limiter auf jeden Fall einmal ankleben
     sInput:=sInput+sLimiter;

     //Und nun einfach durchgehen, bis nix mehr kommt
     u32Size:=String_Length(sInput);
     u32Start:=1;

     while (u32Start < u32Size ) do
           begin
                //Neue Suche mit erhöhtem Offset
                u32Stop:=String_Pos(sInput,sLimiter,u32Start,FALSE) + u32Limit-1;

                //Ergebnis holen
                sTemp:= String_Mid(sInput,u32Start,u32Stop - u32Start);

                //Soll das Ergebnis getrimmt werden ?
                if (bAutoTrim) then
                   begin
                        sTemp:=Trim(sTemp);
                   end;

                //Ergebnis ablegen
                if (sTemp<>'') then
                   begin
                        slResult.Add(sTemp);
                   end;

                //Altes Ende ist der neue Start
                u32Start:=u32Stop;
           end;
     //Anzahl der Einträge zurückliefern
     result:=slResult.Count;
end;

//////////////////////////////////////////////////////////////////////////////////////////
/// Implode
//////////////////////////////////////////////////////////////////////////////////////////
function String_Implode(slInput:TStringList; sLimiter:Longstring):Longstring;
var
   u32Index : unsigned32;
   u32Size  : unsigned32;
begin
     result:='';

     //Einfach alle Strings nehmen und zusammensetzen
     u32Index:=0;
     u32Size:=unsigned32(slInput.Count);

     //Alle Strings in der Liste durchgehen
     while (u32Index < u32Size) do
           begin
                result:=result+slInput[u32Index];

                inc(u32Index);

                //Und den Limiter zwischen die Strings
                 if (u32Index < u32Size) then
                    begin
                         result:=result+sLimiter;
                    end;
           end;
end;



////////////////////////////////////////////////////////////////////////////////////////////////
// Einen StringCase ermöglichen
////////////////////////////////////////////////////////////////////////////////////////////////
function StringToCaseSelect (Selector : string;CaseList: array of string): Integer;
var cnt: integer;
begin
   Result:=-1;
   for cnt:=0 to Length(CaseList)-1 do
begin
     if CompareText(Selector, CaseList[cnt]) = 0 then
     begin
       Result:=cnt;
       Break;
     end;
   end;
end;


//////////////////////////////////////////////////////////////////////////////////////////
//Textanalyse
//////////////////////////////////////////////////////////////////////////////////////////
//Alle Umlaute in einem String ersetzen
function String_ReplaceUmlaute(sInput:Longstring):Longstring;
var
   u32Count : unsigned32;
begin
     result:='';

     //Einfach alle Zeichen durchgehen und mit Switch ersetzen
     u32Count:=1;
     while (u32Count <= String_Length(sInput)) do
           begin
                case (sInput[u32Count]) of
                     'ä' : result:=result+'ae';
                     'ö' : result:=result+'oe';
                     'ü' : result:=result+'ue';
                     'Ä' : result:=result+'AE';
                     'Ö' : result:=result+'OE';
                     'Ü' : result:=result+'UE';
                     'ß' : result:=result+'ss';
                else
                    result:=result+sInput[u32Count];
                end;

                inc(u32Count);
           end;
end;

//Zwei Strings vergleichen und in Prozent deren Übereinstimmung
//zurückgeben
function String_LevenShtein(sInput1,sInput2:Longstring):Unsigned32;
var
   aMatrix  : array of array of unsigned32;
   u32Row   : unsigned32;
   u32Col   : unsigned32;
   u32X     : unsigned32;
   u32Y     : unsigned32;
   u32Input1: unsigned32;
   u32Input2: unsigned32;
   u32Cost  : unsigned32;

//Liefert den kleinsten der Drei Werte zurück
function Minimum(u32A,u32B,u32C:unsigned32):unsigned32;
begin
     if (u32A<u32B) then
        //A < B
        begin
             //A < C
             if (u32A<u32C) then
                begin
                     // A < B < C
                     result:=u32A;
                end
             else
                begin
                     // C < A < B
                     result:=u32C;
                end;
        end
     else
        //B < A
        begin
             //B < C
             if (u32B>u32C) then
                begin
                     result:=u32C;
                end
             else
                begin
                     result:=u32B;
                end;
        end;

end;


begin
     //Fehler annehmen
     result := 0;

     //Fehler abfangen
     if (string_length(sInput1) = 0) or
        (string_length(sInput2) = 0) then
        begin
             exit;
        end;

     //Umlaute ersetzen
     sInput1:=String_ReplaceUmlaute(sInput1);
     sInput2:=String_ReplaceUmlaute(sInput2);

     //Alles auf ALPHANUM
     sInput1:=String_Filter(sInput1,sftALPHANUM);
     sInput2:=String_Filter(sInput2,sftALPHANUM);

     //Und noch trimmen
     sInput1:=Trim(sInput1);
     sInput2:=Trim(sInput2);

     //Längen holen und merken
     u32X:=String_Length(sInput1);
     u32Y:=String_Length(sInput2);


     //Berechnung der LevenShtein-Distanz
     ////////////////////////////////////

     //Arrays initialisieren
     SetLength(aMatrix,u32X+1);
     for u32Col:=0 to u32X do
         begin
              SetLength(aMatrix[u32Col],u32Y+1);
         end;

     //Oberste Zeile mit Wertigkeit füllen
     for u32Col:=0 to u32X do
         begin
              aMatrix[u32Col][0]:=u32Col;
         end;

     //Linke Spalte mit Wertigkeit füllen
     for u32Row:=0 to u32Y do
         begin
              aMatrix[0][u32Row]:=u32Row;
         end;

     //Nun Vergleichen wir Zeichen für Zeichen
     //und speichern den "Änderungsaufwand" in der Matrix
     for u32Input1:=1 to u32X do
         begin
              for u32Input2:=1 to u32Y do
                  begin
                       //Zeichen gleich ?
                       if (sInput1[u32Input1]=sInput2[u32Input2]) then
                          begin
                               //Ja, dann kein Aufwand
                               u32Cost:=0;
                          end
                       else
                          begin
                               //Aufwand
                               u32Cost:=1;
                          end;

                       //Und nun den Aufwand merken
                       aMatrix[u32Input1,u32Input2]:=Minimum ( aMatrix[u32Input1-1,u32Input2  ] + 1,       //Einfügen
                                                               aMatrix[u32Input1  ,u32Input2-1] + 1,       //Entfernen
                                                               aMatrix[u32Input1-1,u32Input2-1] + u32Cost  //Ersetzen
                                                             );
                  end;
         end;
     //Ergebnis holen
     result:=aMatrix[u32X,u32Y];

     //Und auf Prozent normieren
     if (u32X>u32Y) then
        begin
             result:=100 - round(result div u32X * 100);
        end
     else
        begin
             result:=100 - round(result div u32Y * 100);
        end;

end;


////////////////////////////////////////////////////////////////////////////////
/// Filterfunktionen
///     sftALPHA    : unsigned32 = 1;
///     sftNUMBER   : unsigned32 = 2;
///     sftBASE64   : unsigned32 = 4;
///     sftMATH     : unsigned32 = 8;
///     sftSIGN     : unsigned32 = 16
///     sftSPACE    : unsigned32 = 32;
///     sftBREAK    : unsigned32 = 64;
///     sftURL      : unsigned32 = 128;
/// Gibt nur Zeichen Zurück, auf die die gesetzten Filter zutreffen
/// mehrere Filter können gemischt werden
function String_Filter (sInput:Longstring;StringFilterType:Unsigned32=sftSTANDARD):Longstring;
var
   u32Index : Unsigned32;
   bFound   : Boolean;
begin
     result:='';
     //Den ganzen String durchgehen
     u32Index:=1;
     while (u32Index <= unsigned32(Length (sInput)) ) do
           begin
                bFound:=FALSE;
                //Filter durchgehen
                if ( (StringFilterType and sftALPHA ) = sftALPHA ) then bFound := bFound or (sInput[u32Index] in ALPHASET  );
                if ( (StringFilterType and sftNUMBER) = sftNUMBER) then bFound := bFound or (sInput[u32Index] in NUMBERSET );
                if ( (StringFilterType and sftBASE64) = sftBASE64) then bFound := bFound or (sInput[u32Index] in BASE64SET );
                if ( (StringFilterType and sftMATH  ) = sftMATH  ) then bFound := bFound or (sInput[u32Index] in MATHSET   );
                if ( (StringFilterType and sftSIGN  ) = sftSIGN  ) then bFound := bFound or (sInput[u32Index] in SIGNSET   );
                if ( (StringFilterType and sftSPACE ) = sftSPACE ) then bFound := bFound or (sInput[u32Index] in SPACESET  );
                if ( (StringFilterType and sftBREAK ) = sftBREAK ) then bFound := bFound or (sInput[u32Index] in BREAKSET  );
                if ( (StringFilterType and sftURL   ) = sftURL   ) then bFound := bFound or (sInput[u32Index] in URLSET    );
                if ( (StringFilterType and sftFLOAT ) = sftFLOAT ) then bFound := bFound or (sInput[u32Index] in FLOATSET  );
                if ( (StringFilterType and sftPATHES) = sftPATHES) then bFound := bFound or (sInput[u32Index] in PATHSET   );

                //Trifft ein Filter zu ?
                if ( bFound ) then
                   begin
                        //Dann merken
                        result:=result+sInput[u32Index];
                   end;
                //Nächstes Zeichen
                inc(u32Index);
           end;
end;


//////////////////////////////////////////////////////////////////////////////////////////
//Pfadstrings lesefreundlich kürzen
function String_ShortenPath(sFilepath:Longstring;u32Size:Unsigned32):Longstring;
const
   cLimiter = '...';
var
   u32Left  : unsigned32;
   u32Right : unsigned32;
begin
     //Limiter von der Größe abziehen
     dec(u32Size,Length(cLimiter));

     //Ganzen String zurückgeben
     result:=sFilePath;

     //String schon kurz genug ?
     if ( unsigned32 (Length(sFilepath)) <= u32Size) then
        begin
             exit;
        end;

     //Ersten Slash von rechts suchen
     u32Right:=String_RightPos(sFilepath,MultiPlatform_GetSlash());

     //Keinen Slash gefunden, dann ganzen String zurückgeben
     if (u32Right=0) then
        begin
             exit;
        end;

     //Slash mitreinnehmen
     dec (u32Right);

     //Restgröße des linken Teils bestimmen
     u32Right :=( unsigned32(Length(sFilepath)) - u32Right );
     u32Left  :=u32Size - u32Right;
     //Linke Seite auskopieren
     result:=String_Left(sFilepath,u32Left);
     
     //Limiter dazwischen
     result:=result+cLimiter;

     //Rechte Seite ankleben
     result:=result+String_Right(sFilepath,u32Right);
end;


//////////////////////////////////////////////////////////////////////////////////////////
//Einen String in eine Dtaei schreiben
function String_Write(sFilename:Longstring;Value:Longstring):Boolean;
var
   sOut : Textfile;
begin
     result:=FALSE;
     try
        Assignfile(sOut,sFilename);
        Rewrite(sOut);
        Write(sOut,Value);
        Result:=TRUE;
     finally
        CloseFile(sOut);
     end;
end;


function String_Read(sFilename:Longstring):Longstring;
var
   fh      : TFileHandle;
   aBuff   : array [0..8191] of Char;
   iBuff   : integer;
   u32Read : unsigned32;
   u32Index: unsigned32;
   sTemp   : Longstring;
begin
     result:='';

     if (not fileexists(sFilename)) then
        begin
             exit;
        end;


     fh:=FileOpen(sFilename,fmOPENREAD);
     if (fh>0) then
        begin
             repeat
                   iBuff:=Length(aBuff);
                   u32Read:=FileRead(fh,aBuff[0],iBuff);
                   if (u32Read>0) then
                      begin
                           //Hart umkopieren, um Binarysafe zu sein
                           sTemp:=stringofchar(#00,u32Read);
                           for u32Index:=0 to u32Read do
                               begin
                                    sTemp[u32Index+1]:=aBuff[u32Index];
                               end;
                           result:=result+sTemp;
                      end;
             until (u32Read=0);
             fileclose(fh);
        end;
end;


end.

