unit unit_tokenizer;
{$IFDEF FPC}
        //Einstellungen für den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
////////////////////////////////////////////////////////////////////////////////
///
/// Tokenizer (c) 2006 Borg@Sven-of-Nine.de
///
/// Zerlegt einen String in seine Bestandteile
////////////////////////////////////////////////////////////////////////////////
///ToDo
/// multiple Wildcards verarbeiten
///
///
///
///
///
///
////////////////////////////////////////////////////////////////////////////////
/// Beispiel : Simpler HTML-Formatierer
/// uses
///   Unit_TypeDefs,
///   Unit_Tokenizer
///   ;
///
/// const
///      TOKEN_OPEN  =  1;
///      TOKEN_CLOSE =  2;
///
/// var
///    //Die Tokenizerklasse
///    Tokenizer : TTokenizer;
///    //Ein Token
///    MyToken   : TToken;
///
///    //Der Einzugzähler
///    u32Space  : unsigned32;
///
/// begin
///      //Tokenizer erzeugen
///      Tokenizer:=TTokenizer.Create;
///
///      //Ein paar Token eintragen
///      Tokenizer.AddToken('TableStart',TOKEN_OPEN ,'<table>');
///      Tokenizer.AddToken('TableEnd'  ,TOKEN_CLOSE,'</table>');
///
///      Tokenizer.AddToken('RowStart'  ,TOKEN_OPEN ,'<tr>');
///      Tokenizer.AddToken('RowEnd'    ,TOKEN_CLOSE,'</tr>');
///
///      Tokenizer.AddToken('DataStart' ,TOKEN_OPEN ,'<td>');
///      Tokenizer.AddToken('DataEnd'   ,TOKEN_CLOSE,'</td>');
///
///      //Einen String laden
///      Tokenizer.Input:='<table><tr><td>test11</td><td>test12</td></tr><tr><td>test21</td><td>test22</td></tr></table>';
///
///      //Auch Daten ausserhalb von Token zulassen
///      Tokenizer.IgnoreNonToken:=FALSE;
///
///      u32Space:=0;
///      //Und hier wird der Sourcecode formatiert
///      while (Tokenizer.GetNextToken(MyToken)) do
///            begin
///                 case (MyToken.ID) of
///                      TOKEN_CLOSE : if (u32Space>3) then dec(u32Space,4);
///                 end;
///
///                 //Ausgeben
///                 writeln(stringofchar(' ',u32Space) + MyToken.sValue);
///
///                 case (MyToken.ID) of
///                      TOKEN_OPEN : inc(u32Space,4);
///                 end;
///
///            end;
///
///      readln;
/// end.
///
/// Die Ausgabe sieht dann wie folgt aus :
///<table>
///    <tr>
///        <td>
///            test11
///        </td>
///        <td>
///            test12
///        </td>
///    </tr>
///    <tr>
///        <td>
///            test21
///        </td>
///        <td>
///            test22
///        </td>
///    </tr>
///</table>
////////////////////////////////////////////////////////////////////////////////
interface

uses
    unit_typedefs
    ,unit_objectbuffer     //Für die Tokenverwaltung
    ,unit_bufferedreader   //Für LoadInputFromFile
    ,unit_strings          //Für String Left etc.
    ;


//Vordefinierte Tokentype
const
     TOKEN_ERROR = -255; //Konnte nichts finden
     TOKEN_NONE  = -1;   //Ergebnis ist kein Token (sondern String zwischen zwei Token)
     TOKEN_OK    =  0;   //Token OK, aber keine ID vom Benutzer vergeben

//Tokentype
type PToken   = ^TToken;
     TToken   = record
     Name     : Longstring;   //Name des Tokens
     ID       : signed32;     //Vom Benutzer frei vergebbare ID (Im posisitiven Bereich)
     sToken   : Longstring;   //Eigentliches Token, nach dem gesucht werden soll
     sValue   : Longstring;   //Das eigentliche dem Token zugerordnete Ergebnis
     u32Value : unsigned32;   //Ein Wert, der vom User frei vergeben werden kann
     bWild    : Boolean;      //Enthält das Token Wildcards (wird vom System gesetzt)
     sWild    : LongString;   //Das Wildcardzeichen (wir mitgeschleift, um auch externe Funktionen aufrufen zu können)
     u32Wild  : unsigned32;   //Anzahl der Wildcards im Token
     sLeft    : LongString;   //Linke Seite des Tokens vor einem Wildcard
     u32Left  : unsigned32;
     sRight   : LongString;   //Rechte Seite des Tokens nach einem Wildcard
     u32right : unsigned32;
     u32Pos   : unsigned32;   //Position im Eingabstring
     u32Size  : unsigned32;   //Länge des TokenStrings
end;

type TTokenizer = class(TObject)
     protected
              //Interner Puffer für die Token
              TokenBuffer : TObjectBuffer;
              
              //String der Tokenisiert werden soll
              sInputString: Longstring;

              //Interner Arbeitspuffer
              sWorkString : LongString;

              //Wildcard
              sWildcard   : Longstring;

              //Verarbeitungsflags
              bIgnoreNonToken : Boolean;
              bCaseSensitive  : Boolean;

              //Einige Hilfsvariablen
              //Die Länge des kürzesten Token im Speicher
              u32MinTokenSize : unsigned32;
              //Die Länge des größten Token im Speicher
              u32MaxTokenSize : unsigned32;
              //Array mit allen ersten Buchstaben der Token
              aTokenChars     : array of Char;



     private
            //Die Anzahl der geladenen Token zurückgeben
            function GetCount ():unsigned32;

            //Den Eingabestring setzen
            procedure SetInput(value : Longstring);
            //Neues Wildcard setzen
            procedure SetWildcard(value : Longstring);

            //Das Anfangszeichen eines Tokens in das Zeichenarray bringen
            function _AddTokenStart(NewToken:TToken):Boolean;

            //Prüfen, ob das Zeichen der Anfang eines Tokens ist
            function _IsTokenStart(MyChar:Char):Boolean;

            //Das nächste verfügbar Token zurückgeben
            function _GetNextToken():TToken;

            //Wildcardsuche durchführen
            function WildcardMatch(sHaystack:LongString; pNeedle:pToken):Boolean;
     public
            //Konstruktor, Destruktor
            constructor Create();
            destructor  Free();

            //Externe Funktionen für den ObjectBuffer
            function DisposeToken (pTokenToFree     : Pointer) : Boolean;
            function CompareToken(pToken1,pToken2 : Pointer) : Boolean;
            function FindTokenValue(pToken1 : Pointer;pCompare : Pointer) : Boolean;


            //Token-Funktionen
            //Ein Token mit Standardwerten initialisieren
            procedure InitToken(var Token : TToken);
            //Interne Werte eines Tokens bestimmen
            procedure CalculateToken(var Token : TToken);

            //Ein Token in die Liste zufügen
            function AddToken        (Token : TToken):Boolean; overload;
            function AddToken        (sTokenName : LongString; s32ID : Signed32; sToken : LongString):Boolean; overload;

            //Die Tokenliste löschen
            procedure ClearToken        ();
            
            //Auswertefunktionen
            procedure  Reset();
            function   GetNextToken(var Token : TToken):Boolean;
            
            
            //Eingabefunktionen
            function   LoadInputFromFile(Filename:Longstring):Boolean;

            //Properties
            property Input            : Longstring read sInputString     write SetInput;
            property Wildcard         : Longstring read sWildcard        write SetWildcard;
            property IgnoreNonToken   : Boolean    read bIgnoreNonToken  write bIgnoreNonToken default FALSE;
            property CaseSensitive    : Boolean    read bCaseSensitive   write bCaseSensitive;
            property Count            : unsigned32 read GetCount;
end;

implementation
uses
    Sysutils; //Wegen LowerCase

////////////////////////////////////////////////////////////////////////////////
//Interne Hilfsfunktionen
////////////////////////////////////////////////////////////////////////////////
//Ein Token freigeben
function TTokenizer.DisposeToken (pTokenToFree     : Pointer) : Boolean;
begin
     Dispose(PToken(pTokenToFree));
     result:=TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
//Zwei Token vergleichen
function TTokenizer.CompareToken(pToken1,pToken2 : Pointer) : Boolean;
begin
     //Einfach deren Namen vergleichen
     result:=pToken(pToken1)^.Name > pToken(pToken2)^.Name;
end;


////////////////////////////////////////////////////////////////////////////////
//Eine Wildcardsuche durchführen
function TTokenizer.WildcardMatch(sHaystack:LongString; pNeedle:pToken):Boolean;
var
   u32LeftPos  : unsigned32;
   u32RightPos : unsigned32;
   u32Index    : unsigned32;
   bFound      : boolean;
begin
     //Fehler annehmen
     result:=FALSE;

     //Den Anfang des String mit dem Haystack vergleichen
     bFound   := TRUE;
     u32Index := 1;
     while (
             (u32Index <= String_Length(sHayStack)) and
             (u32index <= pNeedle^.u32Left)
           ) do
           begin
                //Jedes der Zeichen vergleichen
                if (sHayStack[u32Index]<>pNeedle^.sLeft[u32Index]) then
                   begin
                        bFound:=FALSE;
                        break;
                   end;
                //Zähler erhöhen
                inc(u32Index);
           end;

     //Ist der linke Teil des Tokens der Beginn des Strings ?
     if ( bFound = TRUE ) then
        begin
             //linken Start setzen
             u32LeftPos :=u32Index;

             //Dann den rechten Teil suchen (Mit Offset nach dem Beginn)
             u32RightPos:=String_LeftPos(sHaystack,pNeedle^.sRight , u32LeftPos);

             //Rechten Teil gefunden ?
             if (u32RightPos >= u32LeftPos) then
                begin
                     //So, damit ist die Hauptarbeit getan.
                     //Nun suchen wir nach multiplen Wildcards
                     if (pNeedle^.u32Wild > 1) then
                        begin
                             //Mehrere Wildcards gesondert behandlen
                        end
                     else
                        begin
                             //Ansonsten sind wir fertig
                             //Das Ergebnis speichern
                             pNeedle^.sValue:=String_Left(sHayStack,u32RightPos);

                             //Erfolg melden
                             result:=TRUE;
                        end;
                end;

        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Den Namen eines Token vergleichen
function TTokenizer.FindTokenValue(pToken1 : Pointer;pCompare : Pointer) : Boolean;
var
   pWorkToken : pToken;
begin
     //Hier wird die kpl. Suche abgehandelt
     //Haben wir keine Wildcardsuche,
     //vergleichen wir einfach den entsprechenden Teil des Strings

     //Eigenen Zeiger machen, damit wir nicht immer casten müssen
     pWorkToken:=pToken(pToken1);

     //Hat das Token ein Wildcard ?
     if (pWorkToken^.bWild) then
        begin
             //Ja
             //Dann Wildcardmatch machen
             result:=WildCardMatch(String(pCompare),pWorkToken);
        end
     else
        begin
             //Nein
             //Dann einfach den Tokeninhalt vergleichen
             result:=pWorkToken^.sToken = copy(String(pCompare),1,pWorkToken^.u32Size);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Konstruktor, Destruktor
constructor TTokenizer.Create();
begin
     //Flags setzen
     Self.IgnoreNonToken:=FALSE;
     
     //Tokenbuffer initialisieren
     TokenBuffer:=TObjectBuffer.Create;
     //Nicht mehr als 64K Token zulassen (sollte eigentlicg reichen)
     TokenBuffer.Maximum:=65535;

     //Und die Externen Funktionszeiger übergeben
     TokenBuffer.DisposeFunction:=DisposeToken;
     TokenBuffer.CompareFunction:=CompareToken;
     TokenBuffer.FindFunction   :=FindTokenValue;

     //Standards setzen
     Self.Wildcard    :='$-#-$';
     Self.Input:='';

     //Und den Buffer zur Sicherheit nochmal löschen
     Self.ClearToken;
end;

////////////////////////////////////////////////////////////////////////////////
destructor TTokenizer.Free();
begin
     //Den Tokenbuffer freigeben
     TokenBuffer.Free();
end;


////////////////////////////////////////////////////////////////////////////////
/// Properties-Funktionen
////////////////////////////////////////////////////////////////////////////////
//Die Anzahl der geladenen Token zurükgeben
function TTokenizer.GetCount ():unsigned32;
begin
     //Einfach den Tokenbuffer abfragen
     result:=TokenBuffer.Size;
end;

////////////////////////////////////////////////////////////////////////////////
//Neuen Eingabestring setzen
procedure TTokenizer.SetInput(value : Longstring);
begin
     //Einmal auf den Eingabestring
     sInputString := Value;

     //Einmal auf den Arbeitsstring
     Self.Reset();
end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//Neues Wildcard setzen
//Wir ein Wildcard veränder, werden alle Tokenteile neu bestimmt
procedure TTokenizer.SetWildcard(value : Longstring);
var
   u32Index : unsigned32;
begin
     //Einmal auf den Eingabestring
     sWildcard := Value;

     //Und alle Token neu bestimmen
     u32Index:=0;
     while (u32Index < TokenBuffer.Size) do
           begin
                Self.CalculateToken(pToken(TokenBuffer.Get(u32Index))^);
                inc(u32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Tokenfunktionen
////////////////////////////////////////////////////////////////////////////////
//Ein Token initialisieren
procedure TTokenizer.InitToken(var Token : TToken);
begin
     with Token do
          begin
               Name     := '';
               ID       := TOKEN_NONE;
               sToken   := '';
               bWild    := FALSE;
          end;
     //Und den Rest ausrechnen
     Self.CalculateToken(Token);
end;

////////////////////////////////////////////////////////////////////////////////
//interne Tokenwerte bestimmen
procedure TTokenizer.CalculateToken(var Token : TToken);
var
   u32LeftPos  : unsigned32;
   u32RightPos : unsigned32;
begin
     with Token do
          begin
               //Wildcard im Token mitspeichern
               sWild  := sWildcard;

               //Position im String
               u32Pos := 0;

               //Länge des Tokens
               u32Size:= String_Length(sToken);

               //Wildcards einmal von links und einmal von rechts suchen
               u32LeftPos :=String_LeftPos (sToken,sWild)-1;
               u32RightPos:=String_RightPos(sToken,sWild);

               //Anzahl der Wildcards im String
               u32Wild:=String_Count(sToken,sWild);

               //Wildcardflag setzen
               bWild:=(u32Wild > 0);

               //Und die Teilstrings extrahieren
               //Wenn wir im Wildcardmodus laufen
               if (bWild) then
                  begin
                       sLeft   := String_Left  (sToken,u32LeftPos);
                       u32Left := String_Length(sLeft);
                       sRight  := String_Right (sToken,(String_Length(sToken)-u32RightPos+1) - String_Length(sWild) );
                       u32Right:= String_Length(sRight);
                  end
               else
                  begin
                       //Ansonsten alle Felder leer lassen
                       sLeft   := '';
                       u32Left :=0;
                       sRight  := '';
                       u32Right:=0;
                  end;


          end;
end;



////////////////////////////////////////////////////////////////////////////////
//Ein Token in die Liste zufügen
function TTokenizer.AddToken (Token : TToken):Boolean;
var
   NewToken : PToken;
begin
     //Fehler annehmen
     result:=FALSE;

     //Fehler abfangen
     if (Token.sToken='') then
        begin
             exit;
        end;

     //Ein neues Token erzeugen
     new (NewToken);
     
     //Den Zeiger speichern
     if (TokenBuffer.Add(NewToken)) then
        begin
             //Das Token kommt hier vordefiniert an.
             //Wir bestimmen nur noch ein paar innere Werte
             Self.CalculateToken(Token);

             //Daten übertragen
             NewToken^:=Token;

             //Kleinste Tokengröße evtl. anpassen
             if (Token.u32Size < u32MinTokenSize) then
                begin
                     u32MinTokenSize:=Token.u32Size;
                end;

             //Größte Tokengröße anpassen
             if (Token.u32Size > u32MaxTokenSize) then
                begin
                     u32MaxTokenSize:=Token.u32Size;
                end;

             //Und zur Beschleunigung der Suche den ersten Buchstaben des Tokens in
             //einem Array speichern
             //Die verhinderung von Dubletten etc. macht die Unterfunktion
             //damit müssen wir uns hier nicht belasten
             Self._AddTokenStart(Token);

             //Alle OK
             result:=TRUE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Hier die Einfache Version, ohne erst ein Token zu machen
function TTokenizer.AddToken        (sTokenName : LongString; s32ID : Signed32; sToken : LongString):Boolean;
var
   MyToken : TToken;
begin
     //Und die Werte einschreiben
     MyToken.Name   := sTokenName;
     MyToken.ID     := s32ID;
     MyToken.sToken := sToken;
     
     //Und die eigentliche Funktion aufrufen
     result:=Self.AddToken(MyToken);
end;

////////////////////////////////////////////////////////////////////////////////
//Die Tokenliste löschen
procedure TTokenizer.ClearToken        ();
begin
     //Kleinste Tokengröße auf Maximum setzen
     u32MinTokenSize:=High(unsigned32);

     //Größte Tokengröße auf Minimum setzen
     u32MaxTokenSize:=0;
     
     //Die Tokenanfänge löschen
     SetLength(aTokenChars,0);

     //Und den Puffer entladen
     //Dispose übernimmt die eingehängt Funktion im Objectpuffer
     TokenBuffer.Clear;
end;

////////////////////////////////////////////////////////////////////////////////
//Auswertefunktionen
////////////////////////////////////////////////////////////////////////////////
//Den Tokenizerzeiger wieder an den Anfang setzen
procedure  TTokenizer.Reset();
begin
     //Dazu kopieren wir einfach den Eingabestring auf den Arbeitsstring
     if (bCaseSensitive) then
        begin
             sWorkString:=sInputString;
        end
     else
        begin
             sWorkString:=LowerCase(sInputString);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Prüfen, ob das Zeichen der Beginn eines Tokens ist
//(Interne Funktion)
function TTokenizer._AddTokenStart(NewToken:TToken):Boolean;
var
   u32Index : unsigned32;
begin
     //Fehler annehmen
     result:=FALSE;

     //Fehler im Namen abfangen
     if (NewToken.sToken='') then
        begin
             Exit;
        end;

     //Prüfen, ob es diesen Anfang schon gibt
     if (not _IsTokenStart(NewToken.sToken[1])) then
        begin
             //Nein, dann zufügen
             //Länge des Arrays holen
             u32Index:=Length(aTokenChars);

             //Um eins vergrößern
             SetLength(aTokenChars,u32Index+1);

             //Und neuen Wert speichern
             aTokenChars[u32Index]:=NewToken.sToken[1];

             //Fertig
             result:=TRUE;
        end;

end;

////////////////////////////////////////////////////////////////////////////////
//Prüfen, ob das Zeichen der Beginn eines Tokens ist
//(Interne Funktion)
function TTokenizer._IsTokenStart(MyChar:Char):Boolean;
var
   u32Index : unsigned32;
begin
     //Fehler annehmen
     result:=FALSE;

     //Alle Einträge im Array durchgehen
     u32Index:=0;
     while (u32Index < unsigned32(Length(aTokenChars))) do
           begin
                //Char identisch ?
                if (MyChar=aTokenChars[u32Index]) then
                   begin
                        //Ja, dann Schleife hier abbrechen
                        result:=TRUE;
                        break;
                   end;

                //Nächstes Zeichen ansprechen
                inc(u32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Das nächste Token finden
//(Interne Funktion)
function TTokenizer._GetNextToken():TToken;
var
   pTokenCandidate : pToken;
   u32Start        : unsigned32;
   u32Length       : unsigned32;
begin
     //Ein Token zu finden ist ganz leicht
     //Wir gehen den String durch, bis wir auf ein Zeichen stoßen,
     //das im Puffer der Anfangsbuchstaben steht.
     //Dann schneiden wir die kleinste Tokengröße aus und vergleichen diese
     //mit den Token im Puffer.
     //Wenn wir nichts finden erweitern wir den Ausschnitt so lange, bis die
     //maximal Tokengröße erreicht ist.
     //haben wir auch dann nichts gefunden, gibt es kein passendes Token
     //Wildcards etc. werden in der eigentlichen Suchfunktion behandelt und
     //können hier ignoriert werden
     
     u32Start  :=1;
     u32Length :=String_Length(sWorkString);

     //Hier einfach was ins Token schreiben, um Kompilerwarnungen zu vermeiden
     Result.ID:=TOKEN_ERROR;
     //Rückgabe mit einem kpl. initialisierten Token annehmen
     //Init setzt den Tokenstatus auf TOKEN_NONE
     InitToken(Result);

     //Das erste nützliche Zeichen suchen
     while (u32Start <= u32Length) do
           begin
                //Anfangszeichen vergleichen
                if (_IsTokenStart(sWorkString[u32Start])) then
                   begin
                        //So, das erste Zeichen des Tokens stimmt
                        //Nun rufen wir die eigentliche Suchfunktion auf.
                        //Der Wildcardvergleich passiert dort, darum
                        //müssen wir immer den ganzen Reststring schicken.
                        pTokenCandidate:=TokenBuffer.Find(pString( Copy ( sWorkString, u32Start, String_Length(sWorkString) ) ) );

                        //Schleife beenden, wenn ein Ergebnis vorliegt
                        if (pTokenCandidate<>nil) then
                           begin
                                //Ergebnis merken
                                result:=pTokenCandidate^;

                                //Position speichern
                                result.u32Pos:=u32Start;
                                
                                //Und bei einem Nichtwildcard das Ergebnis=Token setzen
                                if (not Result.bWild) then
                                   begin
                                        Result.sValue:=Result.sToken;
                                   end;

                                //Und Schleifenabbruch
                                break;
                           end;
                   end;

                //Die Startposition erhöhen
                inc(u32Start);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
function   TTokenizer.GetNextToken(var Token:TToken):Boolean;
var
   u32Pos : unsigned32;
begin
     //Nix zu tun ?
     if (sWorkString = '') then
        begin
             result:=FALSE;
             exit;
        end;

     //Standardtoken holen
     Token:=_GetNextToken();

     //Auf einen Fehler prüfen
     result:= (Token.ID<>TOKEN_NONE);

     //Wenn wir keine Fehler haben prüfen wir die weitere Verarbeitung
     if (result<>FALSE) then
        begin
             //Soll auch der Bereich zwischen den Token ausgegeben werden ?
             //Und wir sind nicht am Anfang der Eingabe ?
             if (not Self.IgnoreNonToken) and
                (Token.u32Pos <> 1)       then
                begin
                     //Position merken
                     u32Pos:=Token.u32Pos-1;

                     //Dann das Result-Token neu initialisieren
                     InitToken(Token);

                     //Und die Daten einfügen
                     Token.Name    := 'NoneToken';
                     Token.sToken  := Copy(sWorkString,1,u32Pos);
                     Token.sValue  := Token.sToken;
                     Token.u32Pos  := 1;
                     Token.u32Size := u32Pos;

                     //Und nun den Arbeitsstring um den "verbrauchten" Bereich
                     //kürzen
                     Delete(sWorkString,1,u32Pos);
                end
             else
                begin
                     //Nur um den "verbrauchten" Bereich kürzen
                     Delete(sWorkString,1,(Token.u32Pos-1)+String_Length(Token.sValue));
                end;
       end
     else
       begin
            //Nichts gefunden, aber NonToken aktiviert ?
            if (not Self.IgnoreNonToken) then
               begin
                    //Dann einfach den ganzen Rest ausgeben
                     InitToken(Token);

                     //Und die Daten einfügen
                     Token.Name    :='NoneToken';
                     Token.sToken  :=sWorkString;
                     Token.u32Pos  :=1;
                     Token.u32Size :=String_Length(sWorkString);
                     
                     //Fertig
                     sWorkString:='';
                     
                     //Erfolg melden
                     Result:=TRUE;
               end;
       end;
end;


////////////////////////////////////////////////////////////////////////////////
//Den Eingabestring aus einer Datei laden
function TTokenizer.LoadInputFromFile(Filename:Longstring):Boolean;
var
   BuffReader : TBufferedReader;
   sInput     : Longstring;
begin
     //Klasse erzeugen
     BuffReader:=TBufferedReader.Create();

     //Eingabe leeren
     sWorkString:='';

     //Datei öffnen
     if (BuffReader.Open(Filename)) then
        begin
             //Einfach alles einlesen
             sInput:=Chr(BuffReader.ReadFirstByte());
             while (not BuffReader.EOF) do
                   begin
                        sInput:=sInput+Chr(BuffReader.ReadNextByte());
                   end;
        end;

     //Klasse wieder freigeben
     BuffReader.Free;
     
     //Ergebnis setzen
     Self.Input:=sInput;
     result:=(sInput <> '');
end;


end.

