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

//////////////////////////////////////////////////////////////////////////////////////////////////
//  v0.1.5.5
//  Unit mit nützlichen Stringfunktionen die Delphi nicht bietet
//  (c) 2003  Borg@Sven-of-Nine.de
//
// is_alpha auf SETs umgestellt
//
//
//
//
//
//
//
//
//
//////////////////////////////////////////////////////////////////////////////////////////////////
unit Unit_StringFunctions;

interface
uses     SysUtils, Classes, Windows,Variants;


Const
     LANG_DE = 1;
     LANG_ENG= 2;

     LOWERCHARS=1;
     UPPERCHARS=2;
     NUMBER    =4;
     ALL_CHARS =LOWERCHARS+UPPERCHARS+NUMBER;

     ALPHASET  =[
                'A','B','C','D','E','F','G','H','I','J',
                'K','L','M','N','O','P','Q','R','S','T',
                'U','V','W','X','Y','Z',
                'a','b','c','d','e','f','g','h','i','j',
                'k','l','m','n','o','p','q','r','s','t',
                'u','v','w','x','y','z'
                ];
     NUMBERSET =[
                '1','2','3','4','5','6','7','8','9','0'
                ];
     ALPHANUMSET =[
                   'A','B','C','D','E','F','G','H','I','J',
                   'K','L','M','N','O','P','Q','R','S','T',
                   'U','V','W','X','Y','Z',
                   'a','b','c','d','e','f','g','h','i','j',
                   'k','l','m','n','o','p','q','r','s','t',
                   'u','v','w','x','y','z',
                   '1','2','3','4','5','6','7','8','9','0'
                   ];

//Filename Funktionen

//Fügt einen Backslash an, wenn keiner besteht
function AddBackSlash(input:string):string;

//Entfernt einen evtl. vorhandenen Backslash
function RemoveBackSlash(input:string):string;

//Verkürzt einen Pfad nach dem Muster c:\abc\...\def.exe
function ResizeFilePath(input:string;Limit:Integer):String;

//Entfernt eine Dateiextension aus einem Dateinamen
function RemoveFileExtension(Input:String):String;

//Extrahiert nur die Extension aus einem Dateinamen
function ExtractFileExtension(Input:String):String;

//String-Funktionen
//Overload der Suchfunktion
function PosEx(SubStr:String;S:String;iStart:Integer):integer;

//Ersetzten mit Groß/Kleinschreibung
function SearchAndReplace (sSrc, sLookFor, sReplaceWith : string) : string;
//Ersetzten ohne Groß/Kleinschreibung
function ISearchAndReplace (sSrc, sLookFor, sReplaceWith : string) : string;

//Durchsuchen mit Groß/Kleinschreibung
function FindCaseString (Haystack:TStringList;Needle:String):LongInt; overload;
function FindCaseString (Haystack:TStrings;Needle:String):LongInt; overload;
function FindCaseString (Haystack:String;Needle:String):LongInt; overload;

//Durchsuchen ohne Groß/Kleinschreibung
function FindNoCaseString (Haystack:TStringList;Needle:String):LongInt; overload;
function FindNoCaseString (Haystack:TStrings;Needle:String):LongInt; overload;
function FindNoCaseString (Haystack:String;Needle:String):LongInt; overload;

//Vergleich eine mit * abgekürzten String, ob eine übereinstimmung
//mit dem normalen String besteht
function CompareWildcards(WildcardedString,NormalString:String):Boolean;

//Einen String mit dem Text Value Count-mal füllen
function FillString (Value:String;Count:Integer):String;
//Einen String mit einer zufälligen Zeichenfolge der Länge Count füllen
function FillStringRandom(Count:Integer;Mode:Integer=ALL_CHARS;Secure:Boolean=TRUE):String;

//Remove Zero from String
function UnzeroString(Input:String):String;


//Einträge aus einer Stringliste entfernen
procedure RemoveStrings(var Input:TStringList;Index,Count:LongInt); overload;
procedure RemoveStrings(var Input:TStrings;Index,Count:LongInt); overload;

//Einträge aus einer STringliste extrahieren
function ExtractStrings(Input:TStringList;Index,Count:LongInt):TStringList;

//Einträge mit dem Stringinhalt Filter ausfiltern
Procedure FilterStrings(var Input:TStringList;Filter:String);

//Alle Strings einer Stringliste trimmen
Procedure TrimStrings(var Input:TStringList);overload;

//Das Vorkommen von Needle in Haystack zählen
function CountSubStr(sNeedle,sHaystack:String):integer;

//Konvertierfunktionen
function Str2Bool(param:string):Boolean;
function Bool2Str(param:Boolean):String;
function Str2Hex(param:string):string;
function Hex2Str(param:string):string;
function VarToBin(input:Variant): String; overload
function VarToBin(input:Int64): String; overload

//Einen Datei direkt in einen String einlesen
function File2Str(Pfad:String):String;
//Eine Datei direkt in einen Strin schreiben
function Str2File(Pfad,Output:String):Boolean;

//Testfunktionen
//Ist das Zeichen Alpha ?
function is_alpha(input:char):boolean;
//Ist das Zeichen numerisch
function is_numeric(input:char):boolean;
//Ist das Zeichen alphanumerisch
function is_alphanum(input:char):boolean;


//Sonderfunktionen
//Anzahl von Needel in Haystack zählen
function CountChars(Haystack:String;Needle:Char):Cardinal;

//Jeden Wortanfang großschreiben
function Capitalize(Input:String):String;

//CVS-Funktionen
//Aus einem String alle Teile, welche durch Separator getrennt sind aufspalten
function explode(Input:String;Separator:String):TStringList;

//Aus einer Stringliste einen String bilden, bei welchecm
//Die einzelnen Werte durch Separator getrennt sind
function implode(Input:TStringList;Separator:String):String;

//Duplicate in einer Stringliste löschen
procedure CleanupDuplicates(var StrgList:TStringList);


//Monatsnamen in eine Zahl zwischen 1 und 12 wandeln
function MonthToInteger(Input:String):Integer;
//Zahl zwischen 1 und 12  in Monatsnamen wandeln
function IntegerToMonth(monat:integer;Language:Integer=LANG_ENG;shortname:boolean=TRUE):String;

//Monatsnamen in eine Zahl zwischen 1 und 12 wandeln
function DayToInteger(Input:String):Integer;
//Zahl zwischen 1 und 12  in Monatsnamen wandeln
function IntegerToDay(tag:integer;Language:Integer=LANG_ENG;shortname:boolean=TRUE):String;


//Kodierfunktionen
function EncodeString(Input:String;XCode:Byte=0):String;
function DecodeString(Input:String;XCode:Byte=0):String;

//Hifsfunktion, um String in Case zu benutzen
function StringToCaseSelect (Selector : string;CaseList: array of string): Integer;


///Strings zerschneiden
function ChopFromString(input:string;limiter:string):string;
function ChopToString(input:string;limiter:string):string;

/// Histogramm einer Stringliste machen
Function StrToHisto(var slInput:TStringList;var histo:array of cardinal):boolean;


implementation

Var
     Month_Ger_Long : Array[1..12]of String=('januar','februar','märz','april','mai','juni','juli','august','september','oktober','november','dezember');
     Month_Ger_Short: Array[1..12]of String=('jan','feb','mar','apr','mai','jun','jul','aug','sep','okt','nov','dez');
     Day_Ger_Long   : Array[1..7] of String=('sonntag','montag','dienstag','mittwoch','donnerstag','freitag','samstag');
     Day_Ger_Short  : Array[1..7] of String=('so','mo','di','mi','do','fr','sa');

     Month_Eng_Long : Array[1..12]of String=('january','february','march','april','may','june','july','august','september','october','november','december');
     Month_Eng_Short: Array[1..12]of String=('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec');
     Day_Eng_Long   : Array[1..7] of String=('sunday','monday','tuesday','wednesday','thursday','friday','saturday');
     Day_Eng_Short  : Array[1..7] of String=('so','mo','di','mi','do','fr','sa');

     //Zugelassene Zeichen
     _LowChars : Array[0..25]of Char=( 'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z');
     _HiChars  : Array[0..25]of Char=( 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z');
     _Numbers  : Array[0..9] of Char=( '1','2','3','4','5','6','7','8','9','0');


//////////////////////////////////////////////////////////////////////////////////////////////////
//Fügt einem Pfad einen Backslash zu, wenn noch keiner vorhanden ist
function AddBackSlash(input:string):string;
begin
     Result:='';
     IF (input='') then exit;
     if (Input[Length(Input)]<>'\') then Result:=Input+'\' else Result:=Input;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
//Entfernt aus einem Pfad ein evtl. vorhandenen Backslash
function RemoveBackSlash(input:string):string;
begin
     Result:='';
     IF (input='') then exit;
     if (Input[Length(Input)]<>'\') then Result:=Input else Result:=Copy(Input,1,Length(Input)-1);
end;


//////////////////////////////////////////////////////////////////////////////////////////////////
//Verkürzt eine Dateipfad auf maximal Limit Zeichen
function ResizeFilePath(input:string;Limit:Integer):String;
var
   left,right,temp:string;
begin
     if (Length(Input)>Limit) then
        begin
             //Die Linken zwei Teile rausholen
             Temp:=Input;
             Left:=Copy(Temp,1,Pos('\',Temp));
             Delete(Temp,1,Pos('\',Temp));
             Left:=Left+Copy(Temp,0,Pos('\',Temp));

             //Die rechten zwei Teile rausholen
             Temp:=RemoveBackSlash(Input);
             Right:=ExtractFileName(Temp);
             Delete(Temp,Length(Temp)-Length(Right),1024);
             Right:=ExtractFileName(Temp)+'\'+Right;

             //Und zusammen setzen
             Result:=Left+'...\'+Right;
        end
     else
        begin
             Result:=Input;
        end;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
//Entfern das Suffix aus einem Dateinamen
function RemoveFileExtension(Input:String):String;
begin
     Result:=Copy(Input,0,Length(Input)-Length(ExtractFileExt(Input)));
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
//Extrahiert das Suffix aus einem Dateinamen
function ExtractFileExtension(Input:String):String;
begin
     Result:=ExtractFileExt(Input);
end;

////////////////////////////////////////////////////////////////////////////////
//Overload der Suchfunktion
//Mit der Möglichkeit einen Startindex bei der Suche anzugeben 
function PosEx(SubStr:String;S:String;iStart:Integer):Integer;
begin
     //Start ausserhalb des Sinnvollen Bereiches ?
     if (iStart<2) or (iStart >= Length(S)) then
        begin
             //Dann ignorieren
             result:=Pos(SubStr,S);
             iStart:=0;
        end
     else
        begin
             //Nur im durch Start definierten Substring suchen
             //Copy fängt einen Überlauf ab, daher können wir einfach die alte Länge angeben
             result:=Pos(SubStr,Copy(S,iStart,Length(S)));
        end;

     //Was gefunden ?
     //Dann ggf. Ergebnis anpassen
     if (result > 0) and (iStart > 1)then
        begin
             result:=result+iStart-1;
        end;
end;


//////////////////////////////////////////////////////////////////////////////////////////////////
// Ersetzt alle Vorkommen von Lookfor durch ReplaceWith
function SearchAndReplace (sSrc, sLookFor, sReplaceWith : string) : string;
var
  nPos, nLenLookFor : integer;
begin
  nPos        := Pos(sLookFor, sSrc);
  nLenLookFor := Length(sLookFor);

  while (nPos > 0) do begin
    Delete(sSrc, nPos, nLenLookFor);
    Insert(sReplaceWith, sSrc, nPos);
    nPos := Pos(sLookFor, sSrc);
  end;
  Result := sSrc;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
// Ersetzt alle Vorkommen von Lookfor durch ReplaceWith
// unabhängig von Groß- und Kleinschreibung
function ISearchAndReplace (sSrc, sLookFor, sReplaceWith : string) : string;
var
  nPos, nLenLookFor : integer;
begin
  nPos        := Pos(LowerCase(sLookFor),LowerCase(sSrc));
  nLenLookFor := Length(sLookFor);

  while (nPos > 0) do begin
    Delete(sSrc, nPos, nLenLookFor);
    Insert(sReplaceWith, sSrc, nPos);
    nPos := Pos(sLookFor, sSrc);
  end;
  Result := sSrc;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// Stringsuchfunktionen als Überladene Funktionen für Strings und Stringlisten
//////////////////////////////////////////////////////////////////////////////////////////
//Einmal mit Groß/Kleinschreibung
function FindCaseString (Haystack:TStringList;Needle:String):LongInt;
var
   z,cnt : LongInt;
begin
     Result:=-1;
     cnt:=Haystack.Count;

     //Wildcard ?
     if (Pos('*',Needle)<>0) then
        begin
             Needle:=Copy(Needle,1,Pos('*',Needle)-1);
        end;
     z:=0;
     while (z<cnt) do
           begin
                if (Pos(Needle,Haystack.Strings[z])>0) then
                   begin
                        Result:=z;
                        z:=cnt;
                   end;
                inc(z);
           end;
end;

function FindCaseString (Haystack:TStrings;Needle:String):LongInt;
var
   z,cnt : LongInt;
begin
     Result:=-1;
     cnt:=Haystack.Count;

     //Wildcard ?
     if (Pos('*',Needle)<>0) then
        begin
             Needle:=Copy(Needle,1,Pos('*',Needle)-1);
        end;

     z:=0;
     while (z<cnt) do
           begin
                if (Pos(Needle,Haystack.Strings[z])>0) then
                   begin
                        Result:=z;
                        z:=cnt;
                   end;
                inc(z);
           end;
end;

function FindCaseString (Haystack:String;Needle:String):LongInt;
var
   z : LongInt;
   t : String;
begin
     Result:=-1;

     if Length(Needle)> Length(Haystack) then Exit;

     z:=1;
     while (z <= Length(Needle)) do
           begin
                //Ist das erste Zeichen identisch ?
                if (HayStack[z]=Needle[1])then
                       begin
                            //Ja, dann komplett vergleichen
                            t := Copy(HayStack,z,Length(Needle));
                            if (Needle=t) then
                               begin
                                    Result:=z;
                                    z:=Length(Haystack);
                               end;
                       end;
                inc(z);
           end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
//Einmal ohne Groß/Kleinschreibung
function FindNoCaseString (Haystack:TStringList;Needle:String):LongInt;
var
   z,cnt : LongInt;
begin
     Result:=-1;
     cnt:=Haystack.Count;

     //Wildcard ?
     if (Pos('*',Needle)<>0) then
        begin
             Needle:=Copy(Needle,1,Pos('*',Needle)-1);
        end;

     Needle:=lowercase(Needle);

     z:=0;
     while (z<cnt) do
           begin
                if (Pos(Needle,lowercase(Haystack.Strings[z]))>0) then
                   begin
                        Result:=z;
                        z:=cnt;
                   end;
                inc(z);
           end;
end;

function FindNoCaseString (Haystack:TStrings;Needle:String):LongInt;
var
   z,cnt : LongInt;
begin
     Result:=-1;
     cnt:=Haystack.Count;


     //Wildcard ?
     if (Pos('*',Needle)<>0) then
        begin
             Needle:=Copy(Needle,1,Pos('*',Needle)-1);
        end;

     Needle:=lowercase(Needle);

     z:=0;
     while (z<cnt) do
           begin
                if (Pos(Needle,lowercase(Haystack.Strings[z]))>0) then
                   begin
                        Result:=z;
                        z:=cnt;
                   end;
                inc(z);
           end;
end;

function FindNoCaseString (Haystack:String;Needle:String):LongInt;
begin
     Needle:=lowercase(Needle);
     Haystack:=LowerCase(HayStack);
     if (Pos(Haystack,Needle)>0) then Result:=Pos(Haystack,Needle) else Result:=-1;
end;



////////////////////////////////////////////////////////////////////////////////////////////////
//Einen mit * abgekürtzen String checken
function CompareWildcards(WildcardedString,NormalString:String):Boolean;
var
   z     : Integer;
   l1,l2 : Integer;
   Temp  : String;
begin
     Result :=FALSE;
     z      :=255;
     l1:=Length(WildCardedString);
     l2:=Length(NormalString);

     //Fehler abfangen
     if l1<1 then Exit;
     if l2<1 then Exit;

     //Strings anpassen
     Temp:=Copy(NormalString,1,l1-1)+'*';

     //Und vergleichen
     if (Temp[1]=WildCardedString[1]) then
        if (Temp=WildCardedString) then z:=0;

     //Kein Wildcard angegeben, dann normal vergleichen
     if WildCardedString[l1]<>'*' then z:=StrComp(PChar(WildCardedString),PChar(NormalString));

     //Kleiner Umweg zum Bool-Wert wegen StrComp
     if z=0 then Result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
// Einen String in seinen Boolean-Inhalt wandeln
function Str2Bool(param:string):Boolean;
var
   s:String;
begin
     s:=trim(lowercase(param));
     if (s='on') or (s='true') then Result:=TRUE else Result:=FALSE;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
// Einen Boolinhalt in eine String wandeln
function Bool2Str(param:Boolean):String;
begin
     if (Param) then Result:='TRUE' else Result:='FALSE';
end;

////////////////////////////////////////////////////////////////////////////////////////////////
// Einen String in seine Hexcodes wandeln
function Str2Hex(param:string):string;
var
   z : LongInt;
begin
     z:=1;
     Result:='';
     while (z <= Length(Param)) do
           begin
                Result:=Result+IntToHex(Ord(Param[z]),2);
                inc(z);
           end;
end;
////////////////////////////////////////////////////////////////////////////////////////////////
// HexCodes wieder in einen String wandeln
function Hex2Str(param:string):string;
var
   z : LongInt;
begin
     z:=1;
     Result:='';
     while (z <= Length(param)-1) do
           begin
                Result:=Result+Chr( StrToIntDef('$'+param[z]+param[z+1],32));
                inc(z,2);
           end;
end;
////////////////////////////////////////////////////////////////////////////////////////////////
//Erzeugt einen String in welchem Count mal Value vorkommt
function FillString (Value:String;Count:Integer):String;
var
   z:Integer;
begin
     Result:='';
     for z:=1 to Count do Result:=Result+Value;

end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Erzeugt einen String in welchem Count mal  vorkommt
function FillStringRandom(Count:Integer;Mode:Integer=ALL_CHARS;Secure:Boolean=TRUE):String;
var
   z : Integer;
   c : Integer;
   a : Array of Char;

begin
     //Fehleingaben abfangen
     if Mode < LOWERCHARS then Exit;

     //Zufällige Zeit warten, um keine Laufzeitanalyse zu verhindern
     if (Secure) then Sleep(Random(10));

     //Unser Sucharray zusammensetzen
     if (Mode AND LOWERCHARS=LOWERCHARS) then
        begin
             c:=Length(a);
             SetLength(a,c+Length(_LowChars));
             for z:=0 to Length(_LowChars)-1 do
                 begin
                      a[c+z]:=_LowChars[z];
                 end;
        end;

     if (Mode AND UPPERCHARS=UPPERCHARS) then
        begin
             c:=Length(a);
             SetLength(a,c+Length(_HiChars));
             for z:=0 to Length(_HiChars)-1 do
                 begin
                      a[c+z]:=_HiChars[z];
                 end;
        end;

     if (Mode AND NUMBER=NUMBER) then
        begin
             c:=Length(a);
             SetLength(a,c+Length(_Numbers));
             for z:=0 to Length(_Numbers)-1 do
                 begin
                      a[c+z]:=_Numbers[z];
                 end;
        end;

     //Und den Ergebnisstring füllen
     Result:='';
     for z:=1 to Count do
         begin
              Result:=Result+a[Random(Length(a)-1)];
         end;

     //Speicher freigeben
     SetLength(a,0);
end;
////////////////////////////////////////////////////////////////////////////////////////////////
//Löscht aus einem String anhängende Chr(0);
function UnzeroString (Input:String):String;
var
   z:Integer;
begin
     z:=1;
     while (Input[z]<>Chr(0)) and (z <= Length(Input)) do inc(z);
     Result:=Copy(Input,1,z-1);
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Löscht Strings aus einer Stringliste
Procedure RemoveStrings(var Input:TStringList;Index,Count:LongInt);
var
   z : LongInt;
   m : LongInt;
begin
     m:=Input.Count-Count;
     z:=Index;

     //Fehler abfangen
     if (Index > Input.Count-1) then Exit;
     if (Index < 0) then exit;
     if (m < 0) then Exit;

     while (Input.Count>(z+m)) do
           begin
                Input.Delete(z);
           end;
end;
////////////////////////////////////////////////////////////////////////////////////////////////
//Löscht Strings aus einer Stringliste
Procedure RemoveStrings(var Input:TStrings;Index,Count:LongInt);
var
   z : LongInt;
   m : LongInt;
begin
     m:=Input.Count-Count;
     z:=Index;

     //Fehler abfangen
     if (Index > Input.Count-1) then Exit;
     if (Index < 0) then exit;
     if (m < 0) then Exit;

     while (Input.Count>(z+m)) do
           begin
                Input.Delete(z);
           end;
end;

Procedure TrimStrings(var Input:TStringList);overload;
var
   z : LongInt;
begin
     z:=Input.Count-1;
     while (z >= 0) do
           begin
                if ( Trim(Input[z])='') then Input.Delete(z);
                dec(z);
           end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Zählt das Vorkommen von Needle in Haystack
function CountChars(Haystack:String;Needle:Char):Cardinal;
var
   z  : integer;
begin
     Result:=0;
     for z:=0 to length(Haystack) do
         begin
              if (Haystack[z]=Needle) then inc(Result);
         end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function ExtractStrings(Input:TStringList;Index,Count:Integer):TStringList;
begin
     Result:=TStringList.Create;
     Result.Clear;

     if (Index > Input.Count-1) then exit;
     if (Index < 0) then Exit;

     while (Index < Input.Count) and (Index <= Count) do
           begin
                Result.Add(Input[Index]);
                inc(Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Filter Eintrage mit dem Stringbeginn filter aus und löscht diese
Procedure FilterStrings(var Input:TStringList;Filter:String);
var
   z : LongInt;
begin
     z:=Input.Count-1;
     while (z >= 0) do
           begin
                if ( Copy(Input[z],0,Length(Filter))=Filter) then Input.Delete(z);
                dec(z);
           end;
end;
////////////////////////////////////////////////////////////////////////////////////////////////
//Löscht doppelte Einträge aus der Stringliste
procedure CleanupDuplicates(var StrgList:TStringList);
var
   z1,z2   : LongInt;
   s       : String;
begin
     if (StrgList.Count<1) then Exit;

     z1:=0;
     while (z1 < StrgList.Count) do
         begin
              //String holen
              s:=StrgList[z1];

              //Nun nach einem weiteren Vorkommen suchen
              z2:=z1+1;
              while (z2 < StrgList.Count) do
                    begin
                         //Gefunden, dann löschen
                         if ( StrgList[z2]=s ) then
                            begin
                                 StrgList.Delete(z2);
                            end;
                         Inc(z2);
                    end;
              Inc(z1);
         end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
function File2Str(Pfad:String):String;
var
   fp    : THandle;
   buff  : array[0..255] of Char;
   z,c   : Integer;
begin
     Result:='';
     if (FileExists(Pfad) ) then
        begin
             fp := FileOpen(Pfad,fmOpenRead);
             if (fp <> 1) then
                begin
                     repeat
                           c:=FileRead(fp,buff,SizeOf(buff));
                           z:=0;
                           while (z < c) do
                                 begin
                                      Result:=Result+buff[z];
                                      inc(z);
                                 end;
                     until (c=0);
                     FileClose(fp);
                end;

        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function Str2File(Pfad,Output:String):Boolean;
var
   z     : Integer;
   fp    : Integer;
   buffer: array of char;
begin
     Result:=FALSE;
     if (Length(Output)=0) then Exit;

     //Array anpassen
     SetLength(buffer,Length(Output));
     //Daten einkopieren
     for z:=1 to Length(Output) do
         begin
              buffer[z-1]:=Output[z];
         end;

     //Und schreiben
     fp := FileCreate(Pfad);
     if (fp > 0) then
        begin
             //Ganz an den Anfang gehen
             FileSeek(fp,0,0);
             //Und raus damit
             if (FileWrite(fp,Buffer[0],Length(Buffer))=Length(Buffer)) then Result:=TRUE;
             FileClose(fp);
        end;

     //Puffer freigeben
     Buffer:=nil;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
// CVS - Funktionen
////////////////////////////////////////////////////////////////////////////////////////////////
function explode(Input:String;Separator:String):TStringList;
var
   z : Cardinal;
begin
     Result:=TStringList.Create;
     Result.Clear;
     if (Length(Input)=0) then Exit;

     if (Input[Length(Input)]=';') then Input[Length(Input)]:=' ';

     while (Length(Input)>0) and (Pos(Separator,Input)>0) do
           begin
                z:=Pos(Separator,Input);
                if (z>0) then
                   begin
                        Result.Add(Trim(Copy(Input,1,z-1)));
                        Delete(Input,1,z);
                   end;
           end;
     Result.Add(Trim(Input));
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function implode(Input:TStringList;Separator:String):String;
var
   z : Integer;
begin
     z:=0;
     Result:='';
     while (z < Input.Count) do
           begin
                Result:=Result+Trim(Input.Strings[z]);
                if (z<>Input.Count-1) then Result:=Result+Separator;
                Inc(z);
           end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
function is_alpha(input:char):boolean;
begin
     if (input in ALPHASET) then result:=TRUE else result:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function is_numeric(input:char):boolean;
begin
     if (input in NUMBERSET) then result:=TRUE else result:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function is_alphanum(input:char):boolean;
begin
     if (input in ALPHANUMSET) then result:=TRUE else result:=FALSE;
end;



////////////////////////////////////////////////////////////////////////////////////////////////
//Den jeweils ersten Buchstaben in einem Wort großschreiben
function Capitalize(Input:String):String;
var
   z : Integer;
begin
     input:=LowerCase(Input);
     Result:=UpperCase(Copy(Input,1,1));
     z:=2;
     while z <= Length(Input) do
         begin
              if (Input[z]=' ') then
                 begin
                      Result:=Result+' '+UpperCase(Copy(Input,z+1,1));
                      inc(z,2);
                 end
              else
                 begin
                      Result:=Result+Input[z];
                      inc(z);
                 end;
         end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function MonthToInteger(Input:String):Integer;
var
   z : Integer;
begin
     input:=Trim(LowerCase(Input));
     Result:=0;

     for z:=1 to 12 do
         begin
              if (Input=Month_Ger_Long[z]) then Result:=z;
              if (Input=Month_Ger_Short[z]) then Result:=z;

              if (Input=Month_Eng_Long[z]) then Result:=z;
              if (Input=Month_Eng_Short[z]) then Result:=z;
         end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function IntegerToMonth(monat:integer;Language:Integer=LANG_ENG;shortname:boolean=TRUE):String;
begin
     Result:='';
     if (Language=LANG_DE) then
        begin
             if (Shortname=TRUE) then
                begin
                     Result:=Month_Ger_Short[monat];
                end
             else
                begin
                     Result:=Month_Ger_Long[monat];
                end;
        end;
     if (Language=LANG_ENG) then
        begin
             if (Shortname=TRUE) then
                begin
                     Result:=Month_Eng_Short[monat];
                end
             else
                begin
                     Result:=Month_Eng_Long[monat];
                end;
        end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
//Einen Tagesnamen in seine Integer wandeln
function DayToInteger(Input:String):Integer;
var
   z : Integer;
begin
     input:=Trim(LowerCase(Input));
     Result:=0;

     for z:=1 to 7 do
         begin
              if (Input=Day_Ger_Long[z]) then Result:=z;
              if (Input=Day_Ger_Short[z]) then Result:=z;

              if (Input=Day_Eng_Long[z]) then Result:=z;
              if (Input=Day_Eng_Short[z]) then Result:=z;
         end;
end;

//Zahl zwischen 1 und 7 in Tagessnamen wandeln
////////////////////////////////////////////////////////////////////////////////////////////////
function IntegerToDay(tag:integer;Language:Integer=LANG_ENG;shortname:boolean=TRUE):String;
begin
     Result:='';
     if (tag>7) then  tag:=tag mod 7;
     if (Language=LANG_DE) then
        begin
             if (Shortname=TRUE) then
                begin
                     Result:=Day_Ger_Short[tag];
                end
             else
                begin
                     Result:=Day_Ger_Long[tag];
                end;
        end;
     if (Language=LANG_ENG) then
        begin
             if (Shortname=TRUE) then
                begin
                     Result:=Day_Eng_Short[tag];
                end
             else
                begin
                     Result:=Day_Eng_Long[tag];
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Einen String verschlüsseln und entschlüsseln
function EncodeString(Input:String;Xcode:Byte=0):String;
var
   a        : Cardinal;
   x1,x2,x3 : Byte;
   code     : Char;
   s        : String;
begin
     Result:='';
     if (Length(Input)=0) then Exit;

     //Zufallswerte erzeugen
     x1:=0; while (x1=0) do x1:=Random(255);

     Sleep(Random(10));

     x2:=x1; while (x2=x1) do x2:=Random(255);
     x3:=x1+x2+XCode;

     for a:=Length(Input) downto 0 do
         begin
              //Zeichen holen
              Code:=Input[a];

              //Kodieren
              Code:=Chr ( Ord(Code) xor x3 );
              inc(x3,x2);
              //In Zeichen wandeln
              s:=Str2Hex(Code);
              Result:=Result+s;
         end;
     Result:=Str2Hex(Chr(x1))+Result+Str2Hex(Chr(x2));
end;

function DecodeString(Input:String;XCode:Byte=0):String;
var
   a        : LongWord;
   x1,x2,x3 : Byte;
   code     : Char;
   s        : String;
begin
     Result:='';
     if (Length(Input)=0) then Exit;

     //Auf die richtige Länge schneiden (zweierpotenz)
     Input:=Copy(Input,1,Length(Input)-Length(Input) mod 2);

     //Die Start und End Codes holen
     //Vorne
     s:=Hex2Str(Copy(Input,1,2));
     x1:=Ord(Char(s[1]) );
     //Hinten
     s:=Hex2Str(Copy(Input,Length(Input)-1,2));
     x2:=Ord(Char(s[1]) );
     x3:=x1+x2+XCode;

     //Alles abschneiden
     Input:=Copy(Input,3,Length(Input)-4);

     //Und komplett dekodieren
     a:=1;
     while (a <= Cardinal(Length(Input))) do
         begin
              //Zeichen holen
              s:=Hex2Str(Copy(Input,a,2));
              if (s<>'') then
                 begin
                      Code := s[1];

                      //Deodieren
                      Code:=Chr ( Ord(Code) xor x3 );

                      //Und merken
                      Result:=Result+Code;

                      //Bereichsüberprüfung bei Überlauf abfangen
                      //inc(x3,x2);
                      asm
                         mov al,x3;
                         add al,x2;
                         mov x3,al;
                      end;

                      inc(a,2);
                 end;
         end;

     s:=Result;
     //Wenn wir ein ergebnis haben, dann String spiegeln
     if (s<>'') then
        begin
             Result:='';
             a:=Length(s)-1;
             while (a > 0) do
                   begin
                        Result:=Result+s[a];
                        dec(a);
                   end;
        end;
end;



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


////////////////////////////////////////////////////////////////////////////////////////////////
///Gibt eine IntZahl als Byte aus
function VarToBin(input:Variant): String; overload
var
   iTemp : Integer;
   iSize : Integer;
   iC    : Byte;
begin
     iSize:=-1;
     case (VarType(input)) of
          varByte     : iSize:=7;
          varSmallInt : iSize:=15;
          varInteger  : iSize:=31;
     end;

     Result:='';
     iC:=0;
     for iTemp:=iSize downto 0 do
         begin
              if (iC > 7) then
                 begin
                      Result:=Result+' ';
                      iC:=0;
                 end;
              inc(iC);
              if ( (Input shr iTemp) and 1 = 1) then Result:=Result+'1' else Result:=Result+'0';
         end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
///Gibt eine IntZahl als Byte aus
function VarToBin(input:Int64): String; overload
var
   iTemp : Integer;
   iSize : Integer;
   iC    : Byte;
begin
     iSize:=63;
     Result:='';
     iC:=0;
     for iTemp:=iSize downto 0 do
         begin
              if (iC > 7) then
                 begin
                      Result:=Result+' ';
                      iC:=0;
                 end;
              inc(iC);
              if ( (Input shr iTemp) and 1 = 1) then Result:=Result+'1' else Result:=Result+'0';
         end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
/// Einen String zerschneiden (ab dem Limiter)
function ChopFromString(input:string;limiter:string):string;
var
   iPos : integer;
begin
     iPos:=pos(limiter,input);
     if (iPos>0) then
        begin
             result:=copy(input,iPos+1,length(input)-ipos);
        end
     else
        begin
             result:='';
        end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
/// Einen String zerschneiden (bis zum Limiter)
function ChopToString(input:string;limiter:string):string;
var
   iPos : integer;
begin
     iPos:=pos(limiter,input);
     if (iPos>0) then
        begin
             result:=copy(input,1,iPos);
        end
     else
        begin
             result:=input;
        end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
/// Aus einer Stringliste ein Histogramm machen
Function StrToHisto(var slInput:TStringList;var histo:array of cardinal):boolean;
var
   iIndex : integer;
   iPos   : integer;
begin
     Result:=FALSE;

     //Array initialisieren
     for iIndex:=0 to 255 do histo[iIndex]:=0;
     if (slInput.Count < 1 ) then Exit;

     //Und Histogramm erzeugen
     for iIndex:=0 to slInput.Count -1 do
           begin
                iPos:=0;
                while (iPos < length(slInput[iIndex])) do
                      begin
                           inc(histo[Ord(slInput[iIndex][iPos])]);
                           inc(iPos);
                      end;
           end;
end;

//Zählt das vorkommen von sNeedle in Haystack
function CountSubStr(sNeedle,sHaystack:String):integer;
var
   pPos      : pChar;
begin
     result:=0;
     //Wir machens mit Zeigern, da das schneller ist
     pPos  :=PChar(sHayStack);
     repeat
           //StrPos gibt uns den Zeiger auf das erste Vorkommen von Needle zurück
           pPos:=StrPos(pPos,PChar(sNeedle));
           //Was gefunden ?
           if (pPos<>nil) then
              begin
                   //Dann Zeiger erhöhen, um das erste gefundene Zeichen im nächsten
                   //Lauf nicht wieder zu finden
                   pPos:=pPos+1;
                   //Und den Zähler erhöhen
                   inc(result);
              end;
     //Bis nichts mehr gefunden wird
     until (pPos=nil);
end;




end.





