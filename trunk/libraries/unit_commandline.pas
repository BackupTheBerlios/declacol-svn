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
unit Unit_Commandline;
//////////////////////////////////////////////////////////////////////////////////////////
///
/// Simple Unit, um einigermaßen elegant die Kommandozeile zu parsen
///
/// (c) 2004 Borg@Sven-of-Nine.de
///
//////////////////////////////////////////////////////////////////////////////////////////

interface
uses
     Unit_TypeDefs
     ,Sysutils        //Wegen Expandfilename
     ,Windows          //Wegen Handles, WriteToConsole, WriteFile, CloseHandle, TFILETIME
//     variants,
       ;
var
   //Handle für Messageout
   hConsole : THND = INVALID_HANDLE_VALUE;


type TclType = byte;

const
     clPATH     = TclType(1);           // z.B /source=c:\honk
     clBOOL     = TclType(2);           // z.B /verbose=TRUE /verbose=OFF
     clEXIST    = TclType(4);           // z.B /s
     clSTRING   = TclType(8);           // z.B /name=test
     clINT      = TclType(16);          // z.B /retry=10
     clSWITCHES = [clPATH,clBOOL,clEXIST,clSTRING,clINT];



//Checken, ob es Switches gitb
function CL_HaveSwitches():Boolean;

//Einen Switch abfragen und bei Fehler einen Defaultwert zurückgeben
function CL_GetSwitchValue(sSwitch:LongString;clType:TclType;vDefault:variant):variant;

//Einen Pfad aus der Commandozeile holen und bei Fehler Default zurückgeben
function CL_GetPathValue(u32Search:LongWord;sDefault:LongString):LongString;
                                         
//Textausgabe auf der Console
procedure CL_MessageOut(sText:LongString;bLinebreak:boolean=TRUE);

//Hande zu Console schließen
procedure CL_MessageClose();

//Abfage auf der Console
function CL_QueryUser(Text:LongString):Boolean;

//Ausgabeparser
function CL_ParsePath(sPath:LongString):Longstring;

//Ein paar interne Windowsfunktionen
function CL_GetUser():LongString;

//Ein paar Laufzeitfunktionen
procedure CL_StartRuntime ();
function  CL_ReadRuntime  ():shortfloat;


implementation
uses
    Unit_Strings
    ;

var
   //Hilfsvariable zur Zeitmessung
   u32RunTime : unsigned32;


//////////////////////////////////////////////////////////////////////////////////////////
//Prüfen, ob es Commandlineswitches gibt
//////////////////////////////////////////////////////////////////////////////////////////
function CL_HaveSwitches():Boolean;
begin
     //Gibt es mehr Switches als der Localdesc.
     result := ParamCount() > 0;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// Den x-ten Eintrag zurückgeben der der nach einem Pfad aussieht
//////////////////////////////////////////////////////////////////////////////////////////
function CL_GetPathValue(u32Search:Unsigned32;sDefault:LongString):LongString;
var
   u32Index : unsigned32;
   u32Found : unsigned32;
begin
     //Als Rückgabe Default annehmen
     result:=sDefault;

     //Fehler abfangen
     if (ParamCount()<1) then Exit;

     u32Found:=0;
     //Durch alle Parameter durchcyclen
     u32Index:=1;
     while (u32Index <= unsigned32(ParamCount()) ) do
           begin
                //Alle Pfade (?) mitzählen
                //Wir nehmen Pfade an, wenn kein Switchzeichen kommt
                if (Pos('-',ParamStr(u32Index))<>1) and
                   (Pos('/',ParamStr(u32Index))<>1) then
                   begin
                        //Gewünschter Index gefunden ?
                        if (u32Search=u32Found) then
                           begin
                                //Dann String merken und aus Schleife
                                //ausbrechen
                                result:=ParamStr(u32Index);
                                break;
                           end;
                        //Pfadindex erhöhen
                        inc(u32Found);
                   end;
                inc (u32Index);
           end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
//Einen Commandline-Switch abfragen und bei Fehler einen Defaultwert zurückgeben
//////////////////////////////////////////////////////////////////////////////////////////
function CL_GetSwitchValue(sSwitch:LongString;clType:TclType;vDefault:variant):variant;
var
   u32Index : unsigned32;
   sTemp    : LongString;
begin
     //Auf jeden Fall den Default-Wert zurückgeben
     result:=vDefault;

     //Anzahl der Parameter holen
     u32Index:=ParamCount();

     //Fehler abfangen
     //Kein Commandozeile übergeben ?
     if (u32Index<1) then Exit;

     //Kein valider Switchtyp angefragt ?
     if not (clType in clSWITCHES) then Exit;

     //Suchstrings evtl aufarbeiten, falls der User schludrig war
     case (clType) of
          //Ein Bool-Wert sollte so aussehen        /verbose=true
          //Ein String-Wert sollte so aussehen      /name=Sven
          //Ein Path-Wert sollte so aussehen        /path=./subdir/test.txt
          clBOOL    : sSwitch:=String_Append(sSwitch,'=');  // = anhängen
          clSTRING  : sSwitch:=String_Append(sSwitch,'=');  // = anhängen
          clPATH    : sSwitch:=String_Append(sSwitch,'=');  // = anhängen
          clINT     : sSwitch:=String_Append(sSwitch,'=');  // = anhängen
     end;

     //Alle Switches durchcyclen und evtl zurückgeben
     while (u32Index > 0) do
           begin
                //String rausholen, um dem Compiler optimierung zu erleichtern
                sTemp:=ParamStr(u32Index);
                //Mit pos die abfrage machen (1. ist im System 2. CaseInsensitive)
                //Kein Case, da Pascal keine SwitchCase-Konstrukte mit Strings unterstützt
                if (pos(sSwitch,sTemp)=1) then
                   begin
                        //Schalter abschneiden
                        sTemp:=Copy(sTemp,Length(sSwitch)+1,Length(sTemp));
                        //Was gefunden, dann je nach Abfrage den Wert holen
                        case (clType) of
                             clBOOL     : begin
                                               //Den Wert zurückgeben
                                               result:=String_StringToBoolean(sTemp);
                                          end;
                             clEXIST    : begin
                                               //Wenn der Schalter existiert, dann TRUE
                                               result:=TRUE;
                                          end;
                             clPATH     : begin
                                               //Pfad expandiert zurückgeben
                                               result:=ExpandFileName(sTemp);
                                          end;
                             clSTRING   : begin
                                               //String einfach so zurückgeben
                                               result:=sTemp;
                                          end;
                             clINT      : begin
                                               //String einfach so zurückgeben
                                               result:=StrToIntDef(sTemp,vDefault);
                                          end;
                        end;
                   end;
                //Nächster EIntrag
                dec(u32Index);
           end;
end;


//////////////////////////////////////////////////////////////////////////////////////////
//Standardausgabekanal
procedure CL_MessageOut(sText:LongString;bLinebreak:boolean=TRUE);
var
{$IFDEF FPC}
        //Einstellungen für den GPC
        u32Temp : Unsigned32;
{$ELSE}
         //Workaround für die Windows-Header die hier Cardinal als Typ verlangen
         u32Temp : Cardinal;
{$ENDIF}
begin
     //Console verfügbar ?
     if (hConsole<>INVALID_HANDLE_VALUE) then
        begin
{$IFDEF DEBUG}
              //Consolen darstellung abgrenzen
             sText:='con : '+sText;
{$ENDIF}
             //Linefeed ankleben
             if (bLinebreak) then sText:=sText+#13+#10;

             //Ja, dann auf die Console schreiben
             u32Temp:=Length(sText);
             WriteFile(hConsole,sText[1],u32Temp,u32Temp,nil);
        end
     else
        begin
{$IFDEF DEBUG}
              //Consolen darstellung abgrenzen
             sText:='wri : '+sText;
{$ENDIF}
             //Nein, standardfunktion nutzen
             if (bLinebreak) then
                begin
                     sText:=sText+#10+#13;
                end;
             Write(sText);
        end;
end;

//Schließt die Konsole (INVALID_HANDLE natürlich nicht notwendig)
procedure CL_MessageClose();
begin
     if (hConsole<>INVALID_HANDLE_VALUE) then
        begin
             CloseHandle(hConsole);
        end;
end;


/////////////////////////////////////////////////////////////////////////////////
//Userabfrag machen
function CL_QueryUser(Text:LongString):Boolean;
var
   chInput : Char;
const
     YESSET = ['y','Y'];
     NOSET  = ['n','N'];
   INPUTSET = YESSET+NOSET;
begin
     //Text ausgeben
     write(Text+ '(Y/N)');

     //Eingabe lesen
     repeat
           read(chInput);
     until (chInput in INPUTSET);

     //Und auswerten
     result:=chInput in YESSET;
end;




//////////////////////////////////////////////////////////////////////////////////////////
//Ein Paar Variablen in einem Pfad ersetzen
function CL_ParsePath(sPath:LongString):Longstring;
begin
     //Username ersetzten
     result:=String_Replace(sPath,'%user%',CL_GetUser());
     //Datum
     result:=String_Replace(result,'%time%',FormatDateTime('hh-nn-ss',Now()));
     //Uhrzeit
     result:=String_Replace(result,'%date%',FormatDateTime('yymmdd',Now()));
end;



//////////////////////////////////////////////////////////////////////////////////////////
//Den aktuellen User ausgeben
function CL_GetUser():Longstring;
var
   aUser   : array of Char;
{$IFDEF FPC}
        //Einstellungen für den GPC
        u32Size : Unsigned32;
{$ELSE}
         //Workaround für die Windows-Header die hier Cardinal als Typ verlangen
         u32Size : Cardinal;
{$ENDIF}
begin
     u32Size:=0;
     //User größe holen
     GetUserName(nil,u32Size);

     //Array passend machen
     SetLength(aUser,u32Size);

     //Und Name wirklich holen
     GetUserName(PChar(aUser),u32Size);

     //Ausgeben (letztes Zeichen ist 0, daher abschneiden)
     result:=Copy(LongString(aUser),1,u32Size-1);
end;



//////////////////////////////////////////////////////////////////////////////////////////
//Laufzeitmessungen auf der Kommandozeile vereinfachen
procedure CL_StartRuntime();
begin
     u32Runtime:=GetTickCount();
end;

function  CL_ReadRuntime():ShortFloat;
begin
     //Auf Sekunden umrechen und null verhindern
     result:=(GetTickCount() - u32Runtime +1) / 1000;
end;

end.
