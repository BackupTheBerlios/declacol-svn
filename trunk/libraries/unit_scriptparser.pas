unit unit_scriptparser;
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
/// Einfacher Scriptparser, um kleine Skripte zu verarbeiten
///
/// er versteht folgende Befehle :
///
/// LOOP (n)  ... POOL          //Schleife n mal durchlaufen
/// IF   a==b ... ELSE ... FI   //Abfragen [==/>/</!=]
/// CLEAR                       //Ausgabe löschen
/// ECHO (n)                    //Daten ausgeben
/// SEED (n)                    //Den Zufallsgenerator mit n seeden
/// SET  %var% Value            //Belegt die Variable var mit Value [(n)/[test]/%true%/%false%]
/// INC %var%                   //Erhöht den Wert von Var um eins
/// DEC %var%                   //Verringert den Wert von Var um eins
/// Reset                       //Sendet ein Reset-Signal an die Aufrufende Klasse
/// ;BlaBla                     //Kommentar
///
/// Schnittstellenfunktionen
/// SENDVALUE (n)               //Den Wert n an die OnValue-Funktion senden
/// SENDTIME  (n)               //Den Wert n an die OnTime-Funktion senden
///
/// Type sind
/// [Text]                      //Ein String
/// (123)                       //Eine Integerzahl
/// %KONST%                     //Vordefinierte Konstanten
///
/// Konstanten sind
/// %STATE%                     //Bool-Rückmeldung des letzten OnValue-Aufrufes
/// %VALUE%                     //Letzte erfolgreich übertragene Value-Wert
/// %DATE%                      //Das aktuelle Datum
/// %TIME%                      //Die aktuelle Zeit
/// %TRUE%                      //true
/// %FALSE%                     //false
/// %RND100%                    //Eine Zufallszahl im Bereich 0<=n<100
/// %RND1000%                   //Eine Zufallszahl im Bereich 0<=n<1000
/// %RND10000%                  //Eine Zufallszahl im Bereich 0<=n<10000
/// %LOOP%                      //Nummer der akutellen Schleife
///
/// Über die Arrays IntVariables, StrVariables unf BoolVariables können eigene
/// Konstanten eingeblendet werden. Der Name wird dabe ohne die Prozentzeichen
/// übergeben.
////////////////////////////////////////////////////////////////////////////////
/// Um eine Schnittstelle nach aussen zu haben, kann die Übergeordnete Klasse
/// Funktionen einhängen :
///
/// OnProcess   = function (bActive:Boolean;StatusPackage:TProcessPackage):Boolean
/// Wird bei jedem Befehl aufgerufen. Ist die Antwort false bricht die
/// Verarbeitung ab.
///
/// OnSendValue = function(signed32):boolean;
/// Wird bei jedem SENDVALUE aufgerufen. Die Rückgabe wird in der Konstante
/// %STATE% abgelegt 
///
/// OnSendtime  = function(signed32):boolean;
/// Wird bei jedem SENDTIME aufgerufen.
///
/// OnGetTime   = function():tdatetime;
/// Wird bei Nutzung der Konstante %DATE% und %TIME% aufgerufen
/// Dadurch kann das Skript in einer "falschen" Zeit ablaufen
///
////////////////////////////////////////////////////////////////////////////////
/// Beispiel-Skript
///
/// seed (123)
/// loop (5)
///      echo [anfang äussere schleife]
///      loop (3)
///           echo [anfang innere schleife]
///                if %RND100% > (50)
///                   echo [zufall größer 50]
///                else
///                   echo [zufall kleiner gleich 50]
///                fi
///           echo [ende innere schleife]
///      pool
///      echo [ende äussere schleife]
/// pool
///
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,
     unit_scriptconstants,
     unit_objectbuffer,
     unit_tokenizer,
     unit_strings,
     unit_random,
     classes;

//Typ eines Verarbeitungspaketes
type TProcessPackage = record
     Index  : unsigned32;      //Index des Aktuellen Befehles
     ID     : unsigned32;      //ID des Befehles
     Name   : Longstring;      //Text des Befehles
     Output : Longstring;      //Ausgabe eines echo
end;


//Externe Funktionen
//Wird nach jedem Befehl aufgerufen
//Gibt die Funktion FALSE zurück, wird die Verarbeitung abgebrochen
type TPollFunction  = function (bActive:Boolean;StatusPackage:TProcessPackage):Boolean of Object;

//Übergibt den Wert des Befehles SetValue()
//Die Antwort entspricht der Antwort des Kontrollmodules
type TSendValueFunction = function (s32Value:signed32):Boolean of Object;

//Übergibt den Wert des Befehles SetTime()
type TSendTimeFunction = function (s32Value:signed32):Boolean of Object;

//Abfrage für Datum und Zeit
type TGetTimeFunction  = function ():TDateTime of Object;

//Reset der äusseren Zustände
type TResetFunction    = function ():Boolean of Object;

Type TScriptParser = class (TObject)
     protected
              //Status der letzten Buchung
              bState       : Boolean;
              u32LastValue : unsigned32;

              //Status der Verarbeitung
              bExit        : Boolean;
              bBusy        : Boolean;

              //Eingabestring
              sInput       : LongString;

              //Zeiger auf Externe Funktionen
              PollFunction      : TPollFunction;
              SendValueFunction : TSendValueFunction;
              SendTimeFunction  : TSendTimeFunction;
              GetTimeFunction   : TGetTimeFunction;
              ResetFunction     : TResetFunction;

              //Verarbeitung vorbereiten
              procedure SetInput();

              //Den Inhalt einer Variablen in einen String konvertieren
              function ConvertVarsToString(pWorkToken:pToken):LongString;
              //Den Inhalt einer Variablen in eine Integer konvertieren
              function ConvertVarsToInteger(pWorkToken:pToken):signed32;

              //Einen Befehl verarbeiten
              function ProcessCommand(var u32Index:unsigned32):Boolean;

              //Den nächsten Befehl mit der ID u32ID finden
              function FindCommand   (var u32Index:unsigned32; u32ID:unsigned32):Boolean;
              //Den Korrespondieren CloseBefehl finden
              function FindClose (var u32Index:unsigned32; u32StartID,u32StopID:unsigned32):Boolean;

              //Unterfunktionen für die einzelnen Befehle
              function ProcessECHO  (var u32Index:unsigned32; var sOutput:Longstring):Boolean;
              function ProcessLOOP  (var u32Index:unsigned32; var sOutput:Longstring):Boolean;
              function ProcessIF    (var u32Index:unsigned32; var sOutput:LongString):Boolean;
              function ProcessELSE  (var u32Index:unsigned32; var sOutput:LongString):Boolean;
              function ProcessSEED  (var u32Index:unsigned32; var sOutput:LongString):Boolean;
              function ProcessCALC  (var u32Index:unsigned32; var sOutput:LongString; value:signed32):Boolean;
              function ProcessSET   (var u32Index:unsigned32; var sOutput:LongString):Boolean;
              function ProcessValue (var u32Index:unsigned32; var soutput:LongString):Boolean;
              function ProcessTime  (var u32Index:unsigned32; var soutput:LongString):Boolean;

              //Dummyfunktionen
              function __ParserPoll(bActive:Boolean;StatusPackage:TProcessPackage):Boolean;
              function __ParserValue(s32Value:signed32):Boolean;
              function __ParserTime(s32Value:signed32):Boolean;
              function __GetTime   ():TDateTime;
              function __Reset     ():Boolean;

              //Kleine Hilfsfunktion um die Tokengruppe zu finden
              function GetGroupID     (u32ID:unsigned32):unsigned32;
              function GetGroupString (u32ID:unsigned32):longstring;

              //Den Wert einer Variable beziehen
              function ReadVariable(Key : Longstring; DefaultVal : Variant): Variant;
              function FindVariable(Key : Longstring): unsigned32;

              //Properties
              function    GetInt (Key : LongString): signed32;
              procedure   SetInt (Key : LongString;  Value: signed32);
              function    GetStr (Key : LongString): longstring;
              procedure   SetStr (Key : LongString;  Value: longstring);
              function    GetBool(Key : LongString): boolean;
              procedure   SetBool(Key : LongString;  Value: boolean);
     private

     public
              //Der Puffer mit allen Tokens
              TokenBuffer : TObjectBuffer;

              //Ausgabepuffer
              slOutput     : TStringList;
              slDebug      : TStringlist;

              //Konstruktoren etc.
              constructor Create ();
              destructor  Free   ();
              procedure   Clear  ();

              //Verarbeitung starten
              procedure   Run    ();

              //Properties
              property Input : longstring read sInput write sInput;
              property OnProcess   : TPollFunction  read PollFunction  write PollFunction;
              property OnSendValue : TSendValueFunction read SendValueFunction write SendValueFunction;
              property OnSendTime  : TSendTimeFunction  read SendTimeFunction  write SendTimeFunction;
              property OnGetTime   : TGetTimeFunction   read GetTimeFunction   write GetTimeFunction;
              property OnReset     : TResetFunction     read ResetFunction     write ResetFunction;

              //Array mit dem script eingeblendeten Variablen
              //Name die mit internen Konstanten kollidieren werden ignoriert
              property IntVariables  [Key : Longstring]:signed32   read GetInt   write SetInt;
              property StrVariables  [Key : Longstring]:Longstring read GetStr   write SetStr;
              property BoolVariables [Key : Longstring]:Boolean    read GetBool  write SetBool;
end;



implementation
uses
    SysUtils //Wegen FormatDate...
    ,unit_assocarray
    ;
var
   //Unser Tokenizer
   Tokenizer : TTokenizer;


   //Variablenpuffer
   aIntVariables  : TAssocIntegerArray;
   aBoolVariables : TAssocIntegerArray;
   aStrVariables  : TAssocStringArray;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Dummyfunktionen um nicht uninitialisiert arbeiten zu müssen
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
function TScriptParser.__ParserPoll(bActive:Boolean;StatusPackage:TProcessPackage):Boolean;
begin
     result:=TRUE;
end;

function TScriptParser.__ParserValue(s32Value:signed32):Boolean;
begin
     result:=TRUE;
end;

function TScriptParser.__ParserTime(s32Value:signed32):Boolean;
begin
     result:=TRUE;
end;

function TScriptParser.__GetTime   ():TDateTime;
begin
     result:=now();
end;

function TScriptParser.__Reset   ():Boolean;
begin
     result:=TRUE;
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Konstruktor und Destruktor
///
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
constructor TScriptParser.Create ();
var
   u32Index : unsigned32;
   u32Count : unsigned32;
begin

     //Die externen Funktion mit Dummies laden
     Self.OnProcess   :=__ParserPoll;
     Self.OnSendValue :=__ParserValue;
     Self.OnSendTime  :=__ParserTime;
     Self.OnGetTime   :=__GetTime;
     Self.OnReset     :=__Reset;


     //Ausgabepuffer initialisieren
     Self.slOutput := TStringList.Create;
     Self.slDebug  := TStringList.Create;

     //Das Tokenizerobject erzeugen
     Tokenizer:=TTokenizer.Create;

     //Den Puffer für die Token initialisieren
     TokenBuffer:=TObjectBuffer.Create;

     //Grundeinstellungen machen
     TokenBuffer.Clear;

     //Wildcard setzen
     Tokenizer.Wildcard:=SCR_WILD;
     Tokenizer.IgnoreNonToken:=TRUE;
     Tokenizer.CaseSensitive:=FALSE;

     //Die Token laden
     for u32Index:=0 to Length(aIDList)-1 do
         begin
              //Für jeden Eintrag alle Alias einlesen
              for u32Count:=0 to Length(aCommandList[u32Index])-1 do
                  begin
                       //Und als Token einschreiben
                       if (aCommandList[u32Index][u32Count]<>'') then
                          begin
                               Tokenizer.AddToken( aCommandList[u32Index][0], //Name ist das erste Alias
                                                   aIDList[u32Index],         //ID
                                                   aCommandList[u32Index][u32Count]); //Alias
                          end;
                  end;
         end;

     //Variablenspeicher initialisieren
     aIntVariables  := TAssocIntegerArray.Create;
     aStrVariables  := TAssocStringArray.Create;
     aBoolVariables := TAssocIntegerArray.Create;

     //Und ein paar Grundvariablen ablegen
     Self.BoolVariables['true'] :=TRUE;
     Self.BoolVariables['false']:=FALSE;
     Self.BoolVariables['state']:=FALSE;
end;

////////////////////////////////////////////////////////////////////////////////
destructor  TScriptParser.Free   ();
begin
     //Unseren Puffer leeren
     Self.Clear;

     //Alle Objekte Freigeben
     Tokenizer.Free;
     TokenBuffer.Free;

end;

////////////////////////////////////////////////////////////////////////////////
procedure   TScriptParser.Clear  ();
begin
     //Alle Token im Tokenpuffer entfernen
     while (Tokenbuffer.Size>0) do
           begin
                //Token entladen
                Dispose(pToken ( TokenBuffer.Get(0) ) );

                //Und Speicher freigeben
                TokenBuffer.Delete(0);
           end;
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Properties
///
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Den internen Variablenspeicher abfragen
function    TScriptParser.GetInt(Key : LongString): signed32;
begin
     result:=aIntVariables[Key];
end;

//Den internen Variablenspeicher belegen
procedure   TScriptParser.SetInt(Key : LongString;  Value: signed32);
begin
     aIntVariables[Key]:=Value;
end;

function    TScriptParser.GetStr (Key : LongString): longstring;
begin
     result:=aStrVariables[Key];
end;

procedure   TScriptParser.SetStr (Key : LongString;  Value: longstring);
begin
     aStrVariables[Key]:=Value;
end;

function    TScriptParser.GetBool(Key : LongString): boolean;
begin
     result:=Boolean(aIntVariables[Key]);
end;

procedure   TScriptParser.SetBool(Key : LongString;  Value: boolean);
begin
     aIntVariables[Key]:=signed32(Value);
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Hilfsfunktionen
///
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Eine Variable in einen String wandeln
function TScriptParser.ConvertVarsToString(pWorkToken:pToken):LongString;
var
   InternalTime : TDateTime;
begin
     //Interne Variablen konvertieren
     case (pWorkToken^.ID) of
          //Vordefinierte Variablen
          ID_VAR_TIME     : begin
                                 //Interne Zeit holen
                                 InternalTime:=GetTimeFunction();
                                 result:=FormatDateTime('hh:mm:ss,zzz',InternalTime);
                            end;
          ID_VAR_DATE     : begin
                                 //Interne Zeit holen
                                 InternalTime:=GetTimeFunction();
                                 result:=FormatDateTime('yy.mm.dd',InternalTime);
                            end;
          ID_VAR_RND100   : result:=IntToStr(RND_GetRandom(100));
          ID_VAR_RND1000  : result:=IntToStr(RND_GetRandom(1000));
          ID_VAR_RND10000 : result:=IntToStr(RND_GetRandom(10000));

          //Variable aus dem Puffer
          ID_VAR_CONST    : begin
                                 //Wir suchen nach einer Variablen
                                 case (Self.FindVariable(pWorkToken^.sValue)) of
                                      //Und konvertieren sie passend
                                      varInteger : result:=IntToStr ( Self.IntVariables[pWorkToken^.sValue] );
                                      varString  : result:=           Self.StrVariables[pWorkToken^.sValue];
                                      varBoolean : result:=String_BooleanToString(Self.BoolVariables[pWorkToken^.sValue]);
                                 else
                                     //Fehlermeldung ausgeben
                                     slDebug.Add(Format('not a var %s',[pWorkToken^.sValue]));
                                 end;
                            end;

          //Alle anderen Variablen werden direkt konvertiert
          ID_VAR_STRING : result:=         pWorkToken^.sValue;
          ID_VAR_INTEGER: result:=IntToStr(pWorkToken^.u32Value);
          ID_VAR_BOOL   : result:=String_BooleanToString(String_StringToBoolean(pWorkToken^.sValue));
     else
         result:='';
     end;

     //Debugausgabe
     slDebug.Add(Format('converting %s to %s',[pWorkToken^.sValue,result]));
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Variable in eine Integer wandeln
function TScriptParser.ConvertVarsToInteger(pWorkToken:pToken):signed32;
begin
     result:=0;
     //Interne Variablen konvertieren
     case (pWorkToken^.ID) of
          //Vordefinierte Variablen
//          ID_VAR_TIME     : ;
//          ID_VAR_DATE     : ;
          ID_VAR_RND10    : result:=RND_GetRandom(10);
          ID_VAR_RND100   : result:=RND_GetRandom(100);
          ID_VAR_RND1000  : result:=RND_GetRandom(1000);
          ID_VAR_RND10000 : result:=RND_GetRandom(10000);

          //Variable aus dem Puffer
          ID_VAR_CONST    : begin
                                 //Wir suchen nach einer Variablen
                                 case (Self.FindVariable(pWorkToken^.sValue)) of
                                      //Und konvertieren sie passend
                                      varInteger : result:=             Self.IntVariables[pWorkToken^.sValue];
                                      varString  : result:=StrToIntDef( Self.StrVariables[pWorkToken^.sValue],0);
                                      varBoolean : result:=Signed32   ( Self.BoolVariables[pWorkToken^.sValue]);
                                 else
                                     //Fehlermeldung ausgeben
                                     slDebug.Add(Format('not a var %s',[pWorkToken^.sValue]));
                                 end;
                            end;

          //Alle anderen Variablen werden direkt konvertiert
          ID_VAR_STRING : result:=StrToIntDef(pWorkToken^.sValue,0);
          ID_VAR_INTEGER: result:=pWorkToken^.u32Value;
          ID_VAR_BOOL   : result:=signed32(pWorkToken^.sValue);
     else
         result:=0;
     end;

     //Debugausgabe
     slDebug.Add(Format('converting %s to %d',[pWorkToken^.sValue,result]));
end;


////////////////////////////////////////////////////////////////////////////////
//Den nächsten Befehl mit der ID u32ID finden
function TScriptParser.FindCommand   (var u32Index:unsigned32; u32ID:unsigned32):Boolean;
begin
     result:=FALSE;
     while (u32Index < TokenBuffer.Size) do
           begin
                //ID gefunden ?
                if ( unsigned32( pToken (TokenBuffer.Get(u32Index))^.ID ) = u32ID) then
                   begin
                        //Was gefunden, dann abbrechen
                        result:=TRUE;
                        break;
                   end;
                //Nächste Token holen
                inc(u32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Den zugehörigen Close-Befehl suchen z.B. loop ... pool
function TScriptParser.FindClose (var u32Index:unsigned32; u32StartID,u32StopID:unsigned32):Boolean;
var
   u32Level   : unsigned32;
   pWorkToken : pToken;
begin
     result:=FALSE;

     //Wir fangen mit Level eins an.
     //Jeder OpenTag erhöht den Level.
     //Jeder CloseTag verringert den Level
     //Finden wir ein Close bei Level null, sind wir fertig
     u32Level:=1;
     while (u32Index < TokenBuffer.Size) do
           begin
                //Token ziehen
                pWorkToken:=pToken(TokenBuffer.Get(u32Index));
                //Starttag ?
                if ( unsigned32(pWorkToken^.ID) = u32StartID ) then
                   begin
                        inc(u32Level);
                   end;

                //EndTag
                if ( unsigned32(pWorkToken^.ID) = u32StopID ) then
                   begin
                        dec(u32Level);
                        //Niedrigster Level erreicht ?
                        if (u32Level=0) then
                           begin
                                //Dann hier abbrechen
                                result:=TRUE;
                                break;
                           end;
                   end;
                //Nächster Befehl
                inc(u32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Kleine Hilfsfunktion um die Tokengruppe zu finden
function TScriptParser.GetGroupID     (u32ID:unsigned32):unsigned32;
begin
     //GruppenID rausholen
     result:=u32ID and $f00000;
end;

////////////////////////////////////////////////////////////////////////////////
function TScriptParser.GetGroupString (u32ID:unsigned32):longstring;
begin
     case (Self.GetGroupID(u32ID)) of
          ID_COMMAND   : result:='command';
          ID_COMPARE   : result:='compare';
          ID_FUNCTION  : result:='function';
          ID_VAR       : result:='variable';
          ID_TYPE      : result:='type';
          ID_TEXT      : result:='text';
     else
          result:='unknown';
     end;
end;

////////////////////////////////////////////////////////////////////////////////
//Hilfsfunktion um eine Variable zu lesen und wenn diese nicht existiert,
//den Defaultwert zurückzugeben
function TScriptParser.ReadVariable(Key : Longstring; DefaultVal : Variant):Variant;
begin
     case (vartype(DefaultVal)) of
          varInteger : begin
                            result := aIntVariables[Key];
                            if (not aIntVariables.OK) then result:=DefaultVal;
                       end;     
          varString  : begin
                            result := aStrVariables[Key];
                            if (not aStrVariables.OK) then result:=DefaultVal;
                       end;
          varBoolean : begin
                            result := Boolean(aBoolVariables[Key]);
                            if (not aBoolVariables.OK) then result:=DefaultVal;
                       end;
     end;

end;

//Prüfen, ob eine Variable existiert
function TScriptParser.FindVariable(Key : Longstring): unsigned32;
begin
     result:=varError;
     //Einfach alle Variablenspeicher abfragen und das Ergebnis entsprechend setzen
     aIntVariables[Key];
     if (aIntVariables.OK) then
        begin
             result:=varInteger;
        end;

     aStrVariables[Key];
     if (aStrVariables.OK) then
        begin
             result:=varString;
        end;

     aBoolVariables[Key];
     if (aBoolVariables.OK) then
        begin
             result:=varBoolean;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Verarbeitung vorbereiten
procedure TScriptParser.SetInput();
var
   MyToken  : TToken;
   pMyToken : PToken;
begin
     //Tokenizer laden //Immer ein Exit anhängen
     Tokenizer.Input:=sInput+#$d+#$a+'exit';

     //Den Tokenbuffer leeren
     Self.Clear;

     //Und alles Tokenisieren
     while (Tokenizer.GetNextToken(MyToken)) do
           begin
                //Keine Formatierungen mitnehmen
                if (GetGroupID(MyToken.ID) < ID_TEXT) then
                   begin
                        //Neues Token erzeugen
                        new (pMyToken);

                        //Daten übernehmen
                        pMyToken^:=MyToken;

                        //Variablen direkt konvertieren
                        if (GetGroupID(pMyToken^.ID)=ID_VAR) then
                           begin
                                //Alle Variablen sind in "eingehüllt"
                                //und werden hier ausgepackt
                                pMyToken^.sValue:=String_Mid(pMyToken^.sValue,2,String_Length(pMyToken^.sValue)-2);

                                //Integer initialisieren
                                pMyToken^.u32Value:=0;

                                //Und hier holen wir die Werte raus
                                case (pMyToken^.ID) of
                                     ID_VAR_INTEGER : pMyToken^.u32Value:=StrToIntDef(pMyToken^.sValue,0)
                                end;
                           end;

                        //Alle gefundenen Token in den Puffer schreiben
                        TokenBuffer.Add(pMyToken);
                   end;
           end;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Parserfunktionen
///
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Das Skript ausführen
procedure TScriptParser.Run();
var
   u32Count  : unsigned32;
   bOK       : Boolean;
begin
     //Ausgaben löschen
     slOutput.Clear;
     slDebug.Clear;

     //Eingabe parsern
     Self.SetInput;

     //Nun arbeiten wir einfach alles durch
     u32Count:=0;
     bExit:=FALSE;
     while (u32Count < TokenBuffer.Size-1) and
           (not bExit)                     do
           begin
                //Befehl abarbeiten
                bOK:=ProcessCommand(u32Count);

                //Ausgabe verarbeiten
                if (bOK) then
                   begin
                   end
                else
                   begin
                        slOutput.Add('ERROR');
                   end;
                inc(u32Count);
           end;

end;
////////////////////////////////////////////////////////////////////////////////
//Den Befehl an Position u32Index ausführen
function TScriptParser.ProcessCommand(var u32Index:unsigned32):Boolean;
var
   pWorkToken : pToken;
   WorkPack   : TProcessPackage;
   sOutput    : longstring;
begin
     //Rückgaben initialisieren
     Result :=TRUE;

     sOutput:='';

     //Token holen
     pWorkToken:=TokenBuffer.Get(u32Index);

     //Token OK ?
     if (pWorkToken<>nil) then
        begin
             //Und verarbeiten
             case (pWorkToken^.ID) of
                  //Interne Funktionen
                  ID_ECHO      : result:=ProcessEcho  (u32Index,sOutput);
                  ID_LOOPSTART : result:=ProcessLoop  (u32Index,sOutput);
                  ID_IFSTART   : result:=ProcessIf    (u32Index,sOutput);
                  ID_IFELSE    : result:=ProcessELSE  (u32Index,sOutput);
                  ID_SEED      : result:=ProcessSeed  (u32Index,sOutput);
                  ID_INC       : result:=ProcessCalc  (u32Index,sOutput,1);
                  ID_DEC       : result:=ProcessCalc  (u32Index,sOutput,-1);
                  ID_SET       : result:=ProcessSet   (u32Index,sOutput);
                  ID_RESET     : result:=ResetFunction();
                  ID_EXIT      : bExit:=TRUE;
                  ID_CLEAR     : begin
                                      sOutPut:='';
                                      slOutput.Clear;
                                 end;
                  //Externe Funktionen
                  ID_FUNC_VALUE: result:=ProcessValue (u32Index,sOutput);
                  ID_FUNC_TIME : result:=ProcessTime (u32Index,sOutput);
             end;
        end;

     //Token erzeugen
     WorkPack.Index :=u32Index;
     WorkPack.ID    :=pWorkToken^.ID;
     WorkPack.Name  :=pWorkToken^.sValue;
     WorkPack.Output:='';

     //Ausgabe machen
     if (sOutput<>'') then
        begin
             slOutput.Add(sOutput);
             WorkPack.Output:=sOutput;
        end;


     //Und die Hauptfunktion pollen
     if (PollFunction(not bExit,WorkPack) = FALSE) then
        begin
             bExit:=TRUE;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// Befehlsfunktionen
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Den Befehl Loop ausführen
function TScriptParser.ProcessLoop(var u32Index:unsigned32; var sOutput : longstring):Boolean;
var
   pWorkToken : pToken;
   WorkToken  : TToken;
   u32Start   : unsigned32;
   u32Stop    : unsigned32;
begin
     result:=FALSE;

     //Nach einem Loop muß ein Zähler kommen
     //Token nach dem aktuellen holen
     inc(u32Index);
     pWorkToken:=TokenBuffer.Get(u32Index);

     if (pWorkToken <> nil) then
        begin
             //Token in lokalen Kontext ziehen,
             //um die Zähler nicht zu verstellen
             WorkToken:=pWorkToken^;

             //Eine Variable ?
             if (GetGroupID(WorkToken.ID) = ID_VAR) then
                begin
                     //Das Ende der Schleife finden
                     inc(u32Index);
                     u32Start:=u32Index;
                     u32Stop :=u32Start;
                     result  :=FindClose(u32Stop,ID_LOOPSTART,ID_LOOPEND);

                     //Tokenzähler laden
                     WorkToken.u32Value:=Self.ConvertVarsToInteger(pWorkToken);

                     //Debugausgabe
                     slDebug.Add(Format('looping %d times',[WorkToken.u32Value]));

                     //Der Zähler schon null ?
                     while (WorkToken.u32Value>0) and
                           (not bExit)            do
                        begin
                             //LoopCounter veröffentlichen
                             Self.IntVariables['loop']:=pWorkToken^.u32Value-WorkToken.u32Value;

                             //Innere Befehle ausführen
                             while (u32Start < u32Stop) do
                                   begin
                                        ProcessCommand(u32Start);

                                        //Position eins hoch
                                        inc(u32Start);
                                   end;

                             //Zähler eins runter
                             dec (WorkToken.u32Value);

                             //Dann von vorn
                             u32Start:=u32Index;
                        end;
                     //Ende setzen
                     u32Index:=u32Stop;
                end;
        end;
end;



////////////////////////////////////////////////////////////////////////////////
//Den Befehl Echo ausführen
function TScriptParser.ProcessEcho(var u32Index:unsigned32; var sOutput : longstring):Boolean;
var
   pWorkToken : pToken;
begin
     result :=FALSE;

     //Token nach dem aktuellen holen
     inc(u32Index);
     pWorkToken:=TokenBuffer.Get(u32Index);

     //Was gefunden ?
     if (pWorkToken<>nil) then
        begin
             //Ist es eine Variable
             if (Self.GetGroupID(pWorkToken^.ID) = ID_VAR) then
                begin
                     //Ergebnis zurückgeben
                     sOutput:=sOutput + ConvertVarsToString(pWorkToken);

                     //Debugausgabe
                     slDebug.Add(Format('echo %s',[pWorkToken^.sValue]));

                     result:=TRUE;
                end;
        end;

end;


////////////////////////////////////////////////////////////////////////////////
//IF ausführen
function TScriptParser.ProcessIF  (var u32Index:unsigned32; var sOutput:LongString):Boolean;
var
   pVar1 : pToken;
   pCmp  : pToken;
   pVar2 : pToken;
   bTRUE : Boolean;
   bInt  : Boolean;
   u32Else : unsigned32;
   u32IF   : unsigned32;
begin
     //Nach einem If müssen drei Werte kommen
     if ( (u32Index + 3) >= TokenBuffer.Size ) then
        begin
             //Fehler
             result:=FALSE;
             exit;
        end;

     //Werte holen
     pVar1:=TokenBuffer.Get(u32Index+1);
     pCmp :=TokenBuffer.Get(u32Index+2);
     pVar2:=TokenBuffer.Get(u32Index+3);
     //Position anpassen
     inc(u32Index,3);

     //Typ prüfen
     if ( Self.GetGroupID(pVar1^.ID) = ID_VAR ) and
        ( Self.GetGroupID(pVar2^.ID) = ID_VAR ) and
        ( Self.GetGroupID( pCmp^.ID) = ID_COMPARE) then
          begin
               //Debugausgabe
               slDebug.Add(Format('if %s %s %s',[pVar1^.sValue,pCmp^.sValue,pVar2^.sValue]));

               //Checken, was für einen Typ wir haben
               bInt:= (Self.FindVariable(pVar1.sValue)=varInteger) or
                      (pVar1^.ID=ID_VAR_INTEGER) or
                      (pVar1^.ID=ID_VAR_INTEGER);

               bTRUE:=FALSE;
               //Vergleich durchführen
               if (bInt)then
                  begin
                       case (pCmp^.ID) of
                            ID_CMP_GREATER  : bTRUE := ConvertVarsToInteger(pVar1) >   ConvertVarsToInteger(pVar2);
                            ID_CMP_LESSER   : bTRUE := ConvertVarsToInteger(pVar1) <   ConvertVarsToInteger(pVar2);
                            ID_CMP_EQUAL    : bTRUE := ConvertVarsToInteger(pVar1) =   ConvertVarsToInteger(pVar2);
                            ID_CMP_NOTEQUAL : bTRUE := ConvertVarsToInteger(pVar1) <>  ConvertVarsToInteger(pVar2);
                       end;
                  end
               else
                  begin
                       case (pCmp^.ID) of
                            ID_CMP_GREATER  : bTRUE := ConvertVarsToString(pVar1) >   ConvertVarsToString(pVar2);
                            ID_CMP_LESSER   : bTRUE := ConvertVarsToString(pVar1) <   ConvertVarsToString(pVar2);
                            ID_CMP_EQUAL    : bTRUE := ConvertVarsToString(pVar1) =   ConvertVarsToString(pVar2);
                            ID_CMP_NOTEQUAL : bTRUE := ConvertVarsToString(pVar1) <>  ConvertVarsToString(pVar2);
                       end;
                  end;

               //Ergebnis analysieren
               if (not bTRUE) then
                  begin
                       //Vergleich nicht OK, dann den nächsten Befehl suchen
                       u32Else := u32Index;
                       u32If   := u32Index;
                       FindClose(u32Else,ID_IFSTART,ID_IFELSE);
                       FindClose(u32IF  ,ID_IFSTART,ID_IFEND);

                       //Welcher ist näher ?
                       if (u32Else < u32IF) then
                          begin
                               u32Index:=u32Else;
                          end
                       else
                          begin
                               u32Index:=u32If;
                          end;
                  end;
          end;
     result:=TRUE;
end;


////////////////////////////////////////////////////////////////////////////////
//Bei einem Else muß nur zum nächsten Fi gesprungen werden
function TScriptParser.ProcessELSE(var u32Index:unsigned32; var sOutput:LongString):Boolean;
begin
     FindClose(u32Index,ID_IFSTART,ID_IFEND);
     result:=TRUE;
end;


////////////////////////////////////////////////////////////////////////////////
//Den Befehl Seed ausführen
function TScriptParser.ProcessSEED  (var u32Index:unsigned32; var sOutput:LongString):Boolean;
var
   pWorkToken : pToken;
   s32Data    : signed32;
begin
     //Keine Ausgabe
     sOutput :='';
     result :=FALSE;

     //Token nach dem aktuellen holen
     inc(u32Index);
     pWorkToken:=TokenBuffer.Get(u32Index);

     if (pWorkToken<>nil) then
        begin
             //Eine Integerzahl
             if (Self.GetGroupID(pWorkToken^.ID) = ID_VAR) then
                begin
                     s32Data:=Self.ConvertVarsToInteger(pWorkToken);
                     //Dann extern weitergeben
                     slDebug.Add(Format('seeding value %d',[s32Data]));

                     RND_RandomSeed(unsigned32(s32Data));
                     result:=TRUE;
                end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Eine Variable erhöhen oder verringern
function TScriptParser.ProcessCALC  (var u32Index:unsigned32; var sOutput:LongString; Value:signed32):Boolean;
var
   pWorkToken : pToken;
   s32Data    : signed32;
begin
     //Keine Ausgabe
     sOutput :='';
     result :=FALSE;

     //Token nach dem aktuellen holen
     inc(u32Index);
     pWorkToken:=TokenBuffer.Get(u32Index);

     if (pWorkToken<>nil) then
        begin
             //Eine Variable ?
             if (pWorkToken^.ID = ID_VAR_CONST) then
                begin
                     //Funktioniert nur bei Integer
                     if ( Self.FindVariable(pWorkToken^.sValue)=varInteger) then
                        begin
                             //Wert holen
                             s32Data:=Self.IntVariables[pWorkToken^.sValue];
                             //Berechnen
                             s32Data:=s32Data+Value;
                             //Wert speichern
                             Self.IntVariables[pWorkToken^.sValue]:=s32Data;

                             //Dann extern weitergeben
                             slDebug.Add(Format('setting %s from %d to %d',[pWorkToken^.sValue,s32Data,s32Data+1]));
                        end;
                     result:=TRUE;
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Eine Variable setzen
function TScriptParser.ProcessSET (var u32Index:unsigned32; var sOutput:LongString):Boolean;
var
   pVar1   : pToken;
   pVar2   : pToken;
begin
     //Keine Ausgabe
     sOutput :='';
     result :=FALSE;

     //Token nach dem aktuellen holen
     inc(u32Index);
     pVar1:=TokenBuffer.Get(u32Index);
     inc(u32Index);
     pVar2:=TokenBuffer.Get(u32Index);

     if (pVar1<>nil) and (pVar2<>nil) then
        begin
             //Vorne steht der Bezeichner
             //und danach de zuzuordnende Wert

             //Eine Variable ?
             if ( Self.GetGroupID(pVar2^.ID) = ID_VAR) then
                begin
                     //evtl. Vorhandenen Eintrag löschen
                     aIntVariables.Delete(pVar1^.sValue);
                     aStrVariables.Delete(pVar1^.sValue);
                     aBoolVariables.Delete(pVar1^.sValue);


                     //Selbst eine Variable ?
                     if ( Self.FindVariable(pVar2^.sValue)<>varError) then
                        begin

                             //Dann die neue entsprechend zurordnen
                             case (Self.FindVariable(pVar2^.sValue)) of
                                  varInteger : Self.IntVariables [pVar1^.sValue] := Self.IntVariables [pVar2^.sValue];
                                  varString  : Self.StrVariables [pVar1^.sValue] := Self.StrVariables [pVar2^.sValue];
                                  varBoolean : Self.BoolVariables[pVar1^.sValue] := Self.BoolVariables[pVar2^.sValue];
                             end;
                        end
                     else
                        begin
                             //Keine Variable, dann den Wert zuordnen
                             case (pVar2^.ID) of
                                  ID_VAR_STRING   : Self.StrVariables [pVar1.sValue] := Self.ConvertVarsToString (pVar2);
                                  ID_VAR_BOOL     : Self.BoolVariables[pVar1.sValue] := String_StringToBoolean(pVar2^.sValue);
                                  ID_VAR_INTEGER  : Self.IntVariables [pVar1.sValue] := Self.ConvertVarsToInteger(pVar2);
                                  ID_VAR_RND10    : Self.IntVariables [pVar1.sValue] := Self.ConvertVarsToInteger(pVar2);
                                  ID_VAR_RND100   : Self.IntVariables [pVar1.sValue] := Self.ConvertVarsToInteger(pVar2);
                                  ID_VAR_RND1000  : Self.IntVariables [pVar1.sValue] := Self.ConvertVarsToInteger(pVar2);
                                  ID_VAR_RND10000 : Self.IntVariables [pVar1.sValue] := Self.ConvertVarsToInteger(pVar2);
                                  ID_VAR_DATE     : Self.StrVariables [pVar1.sValue] := Self.ConvertVarsToString (pVar2);
                                  ID_VAR_TIME     : Self.StrVariables [pVar1.sValue] := Self.ConvertVarsToString (pVar2);
                             end;

                        end;

                     //Extern weitergeben
                     slDebug.Add(Format('initting %s',[pVar1^.sValue]));
                     result:=TRUE;
             end;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///
/// "Funktionsfunktionen"
///
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Den Befehl SetValue abarbeiten und das Ergebnis an die eingehängte Funktion senden
function TScriptParser.ProcessValue (var u32Index:unsigned32; var soutput:LongString):Boolean;
var
   pWorkToken : pToken;
   s32Data    : signed32;
begin
     //Keine Ausgabe
     sOutput :='';
     result :=FALSE;

     //Token nach dem aktuellen holen
     inc(u32Index);
     pWorkToken:=TokenBuffer.Get(u32Index);

     if (pWorkToken<>nil) then
        begin
             //Eine Integerzahl
             if (Self.GetGroupID(pWorkToken^.ID) = ID_VAR) then
                begin

                     //Dann extern weitergeben
                     s32Data:=Self.ConvertVarsToInteger(pWorkToken);
                     //Und das Ergebnis in den Variablenpuffer schieben
                     Self.BoolVariables['state']:=SendValueFunction(s32Data);

                     slDebug.Add(Format('sending value %d',[s32Data]));

                     //Und den Wert speichern, wenn das Ergebnis true war
                     if (Self.BoolVariables['state']=TRUE) then
                        begin
                             Self.IntVariables['value']:=pWorkToken^.u32Value;
                        end;

                     //Fertig
                     result:=TRUE;
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Den Befehl SetTime abarbeiten und das Ergebnis an die eingehängte Funktion senden
function TScriptParser.ProcessTime  (var u32Index:unsigned32; var soutput:LongString):Boolean;
var
   pWorkToken : pToken;
   s32Data    : signed32;
begin
     //Keine Ausgabe
     sOutput :='';
     result :=FALSE;

     //Token nach dem aktuellen holen
     inc(u32Index);
     pWorkToken:=TokenBuffer.Get(u32Index);

     if (pWorkToken<>nil) then
        begin
             //Eine Integerzahl
             if (Self.GetGroupID(pWorkToken^.ID) = ID_VAR) then
                begin
                     //Dann extern weitergeben
                     s32Data:=Self.ConvertVarsToInteger(pWorkToken);

                     slDebug.Add(Format('sending time %d',[s32Data]));

                     //Dann extern weitergeben
                     SendTimeFunction(s32Data);

                     //Fertig
                     result:=TRUE;
                end;
        end;
end;


end.
