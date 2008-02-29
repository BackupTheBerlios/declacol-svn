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
//////////////////////////////////////////////////////////////////////////////////////////
///
/// Zeitfunktionen (Unix-Timestamp / SecToStr / MSecToStr etc.)
///
/// (c) 2005 Borg@Sven-of-Nine.de
///
//////////////////////////////////////////////////////////////////////////////////////////
unit Unit_Time;

interface
uses SysUtils,Unit_TypeDefs;

//////////////////////////////////////////////////////////////////////////////////////////
/// Einige Grundwerte
//////////////////////////////////////////////////////////////////////////////////////////
const
     //Grundwerte
     SecondsPerMinute = 60;
     MinutesPerHour   = 60;
     HoursPerDay      = 24;
     DaysPerWeek      = 7;
     DaysPerMonth     = 30;
     DaysPerYear      = 365;
     WeeksPerMonth    = 4;
     MonthPerYear     = 12;
     UNIXStartYear    = 1970;
     UNIXStartMonth   = 1;
     UNIXStartDay     = 1;
     UNIXStartHour    = 1;
     UNIXStartMinute  = 0;
     UNIXStartSecond  = 0;
     UNIXStartDate    = 25569.0416666666666667;  // 01:00:00 1.1.1970

     JANUAR           = 1;
     FEBRUAR          = 2;
     MAERZ            = 3;
     APRIL            = 4;
     MAI              = 5;
     JUNI             = 6;
     JULI             = 7;
     AUGUST           = 8;
     SEPTEMBER        = 9;
     OKTOBER          = 10;
     NOVEMBER         = 11;
     DEZEMBER         = 12;

     MilliSecondsPerSecond = 1000;

     //Sekundenwerte
     SecondsPerHour   = SecondsPerMinute * MinutesPerHour;
     SecondsPerDay    = SecondsPerHour   * HoursPerDay;
     SecondsPerWeek   = SecondsPerDay    * DaysPerWeek;
     SecondsPerMonth  = SecondsPerDay    * DaysPerMonth;
     SecondsPerYear   = SecondsPerDay    * DaysPerYear;

     //MinutenWerte
     MinutesPerDay    = MinutesPerHour    * HoursPerDay;
     MinutesPerWeek   = MinutesPerDay     * DaysPerWeek;
     MinutesPerMonth  = MinutesPerDay     * DaysPerMonth;
     MinutesPerYear   = MinutesPerDay     * DaysPerYear;


//Datum in Timestamp wandeln und umgekehrt
function Time_DateTimeToUnixTimeStamp(date      : TDateTime):Timestamp;
function Time_UnixTimeStampToDateTime(stamp     : Timestamp):TDateTime;

//Einen Timestamp formatieren
function Time_FormatUnixTimeStamp(sFormatString:Longstring;Stamp:Timestamp):Longstring;

//Einzelne Werte extrahieren
function Time_UnixYear   (stamp:Timestamp):unsigned16;
function Time_UnixMonth  (stamp:Timestamp):unsigned16;
function Time_UnixDay    (stamp:Timestamp):unsigned16;
function Time_UnixHour   (stamp:Timestamp):unsigned16;
function Time_UnixMinute (stamp:Timestamp):unsigned16;
function Time_UnixSecond (stamp:Timestamp):unsigned16;

//Aus Sekunden / MSekunden einen String machen
function Time_SecToStr (seconds      : Unsigned32):LongString;
function Time_MSecToStr(milliseconds : Unsigned32):LongString;


//Und hier noch ein Typ zur Berechnung der Restlaufzeit
type TRuntime = Class(TObject)
     protected
              tiStarttime : Timestamp; //Wann wurde gestartet
              tiStopTime  : Timestamp; //Wann wurde gestoppt
              tiRun       : Timestamp; //Wie lange laufen wir schon
              sRun        : Longstring;//Als String
              tiETA       : Timestamp; //Wann werden wir fertig sein
              sETA        : Longstring;//Als String
              tiLeft      : Timestamp; //Wielange wird es noch dauern
              sLeft       : Longstring;//Als String;
              tiMid       : Timestamp; //Mittlere Dauer eines Events      
              u32Count    : unsigned32;//Wieviele Event hatten wir schon
              u32Events   : unsigned32;//Wieviele Event wird es geben
              u32Percent  : unsigned32;//Wieviel Prozent fertig
     private
     public
           function Start(u32Events:unsigned32):Timestamp;
           function Progress(s32Events:Signed32):Timestamp;
           function Stop():Timestamp;

           property Starttime     : Timestamp  read tiStarttime;
           property Stopttime     : Timestamp  read tiStoptime;

           property Events        : unsigned32 read u32Events;
           property EventsDone    : unsigned32 read u32Count write u32Count;
           property PercentDone   : unsigned32 read u32percent;


           property Mid_Ti        : Timestamp  read tiMid;

           property ETA_Ti        : Timestamp  read tiETA;
           property ETA_Str       : Longstring read sETA;

           property Run_Ti        : Timestamp  read tiRun;
           property Run_Str       : Longstring read sRun;

           property Left_Ti       : Timestamp  read tiLeft;
           property Left_Str      : Longstring read sLeft;
end;

//////////////////////////////////////////////////////////////////////////////////////////
implementation
uses windows;

//Hier wird er Start signalisiert
function TRuntime.Start(u32Events:unsigned32):unsigned32;
begin
     //Startposition merken
     tiStarttime:=GetTickCount();

     //Anzahl der Events merken
     Self.u32Events:=u32Events;

     //Eventzähler resetten
     u32Count:=0;

     //Und Zeit zurückgeben
     result:=tiStarttime;
end;

//Fortschritt signalisieren
function TRuntime.Progress(s32Events:Signed32):unsigned32;
var
   tiNow : Timestamp;
begin
     //Eventzähler anpassen
     inc(u32Count,s32Events);

     //Wieviel Uhr haben wir ?
     tiNow:=GetTickCount();

     //Wielange laufen wir schon
     Self.tiRun:=tiNow-tiStarttime;

     //Initialisieren
     u32Percent:=0;

     //Durch die schon abgearbeiteten Events teilen
     if (u32Count>0) then
        begin
             //Mittlere Zeit pro Event
             Self.tiMid:=Self.tiRun div Self.u32Count;

             //Prozent bestimmen
             Self.u32Percent:=trunc ( Self.u32Count / Self.u32Events * 100 );

        end;

     //Wie lange dauert es gesamt
     tiETA :=Self.u32Events * Self.tiMid;

     //Wie lange läuft es noch
     if (tiETA > tiRun) then
        begin
             tiLeft:=tiETA-tiRun;
        end
     else
        begin
             tiLeft:=0;
        end;


     //Und alle Werte nochmal als String
     sETA :=Time_SecToStr(tiETA  div 1000);
     sRun :=Time_SecToStr(tiRun  div 1000);
     sLeft:=Time_SecToStr(tiLeft div 1000);


     //Fertig
     result:=tiLeft;
end;

//Ende
function TRuntime.Stop():unsigned32;
begin
     //Aktuelle Zeit holen
     tiStoptime:=Time_DateTimeToUnixTimeStamp(now());

     //Und alles neu berechnen
     Self.Progress(0);

     //Fertig
     result:=tiRun;
end;


//////////////////////////////////////////////////////////////////////////////////////////
// Datetime <= => UnixTimestamp
function Time_DateTimeToUnixTimeStamp(date:TDateTime):Timestamp;
begin
     Result:=Round(( date - UNIXStartDate) * SecondsPerDay);
end;

//////////////////////////////////////////////////////////////////////////////////////////
function Time_UnixTimeStampToDateTime(Stamp:Timestamp):TDateTime;
begin
     Result:=(Stamp / SecondsPerDay)+UNIXStartDate;
end;

//////////////////////////////////////////////////////////////////////////////////////////
//Einen Timestamp formatieren
function Time_FormatUnixTimeStamp(sFormatString:Longstring;Stamp:Timestamp):Longstring;
begin
     //Einfach die internen Funktionen benutzern
     result:=FormatDateTime(sFormatString,Time_UnixTimeStampToDateTime(Stamp));
end;


//////////////////////////////////////////////////////////////////////////////////////////
function Time_UnixYear   (stamp:Timestamp):unsigned16;
var
   //Ein bißchen komisch, aber Delphi unterscheidet wohl intern
   //zwischen internem Typ Word und (identisch definierten) extern definiertem Typ Word
   u16Dummy  : system.word;
begin
     DecodeDate(Time_UnixTimestampToDateTime(stamp),System.Word(result),u16Dummy,u16Dummy);
end;
//////////////////////////////////////////////////////////////////////////////////////////
function Time_UnixMonth  (stamp:Timestamp):unsigned16;
var
   u16Dummy  : system.word;
begin
     DecodeDate(Time_UnixTimestampToDateTime(stamp),u16Dummy,System.Word(result),u16Dummy);
end;
//////////////////////////////////////////////////////////////////////////////////////////
function Time_UnixDay    (stamp:Timestamp):unsigned16;
var
   u16Dummy  : system.word;
begin
     DecodeDate(Time_UnixTimestampToDateTime(stamp),u16Dummy,u16Dummy,System.Word(result));
end;
//////////////////////////////////////////////////////////////////////////////////////////
function Time_UnixHour   (stamp:Timestamp):unsigned16;
var
   u16Dummy  : system.word;
begin
     DecodeTime(Time_UnixTimestampToDateTime(stamp),System.Word(result),u16Dummy,u16Dummy,u16Dummy);
end;
//////////////////////////////////////////////////////////////////////////////////////////
function Time_UnixMinute (stamp:Timestamp):unsigned16;
var
   u16Dummy  : system.word;
begin
     DecodeTime(Time_UnixTimestampToDateTime(stamp),u16Dummy,System.Word(result),u16Dummy,u16Dummy);
end;
//////////////////////////////////////////////////////////////////////////////////////////
function Time_UnixSecond (stamp:Timestamp):unsigned16;
var
   u16Dummy  : system.word;
begin
     DecodeTime(Time_UnixTimestampToDateTime(stamp),u16Dummy,u16Dummy,System.Word(result),u16Dummy);
end;


//////////////////////////////////////////////////////////////////////////////////////////
//Aus Millisekunden einen String bilden
function Time_MSecToStr(milliseconds:Unsigned32):LongString;
var
   h,m,s,ms : integer;
begin
     ms:=Milliseconds;
     //Sekunden ausrechnen
     s:=ms    div MilliSecondsPerSecond;
     //Restmilisekunden holen
     ms:=ms - (s * MilliSecondsPerSecond);

     //Stunden rausrechnen
     h :=s div (SecondsPerHour);
     //Restsekunden holen
     s :=s  - (h * SecondsPerHour);

     //Minuten rausrechnen
     m :=s div (MinutesPerHour);
     s :=s -   (m * MinutesPerHour);

     //Ausgabestring zusammenbauen
     result:='';
     if (h>0) then Result:=Result+IntToStr(h)+'h:';
     if (m>0)  or (h>0) then Result:=Result+IntToStr(m)+'m:';
     if (s>0)  or (m>0) or (h>0) then Result:=Result+IntToStr(s)+'s:';
     if (ms>0) or (s>0) or (m>0) or (h>0) then Result:=Result+IntToStr(ms)+'ms';

end;

//////////////////////////////////////////////////////////////////////////////////////////
//Aus Sekunden einen String bilden
function Time_SecToStr(seconds:Unsigned32):LongString;
var
   h,m,s : integer;
begin
     //Sekunden
     s :=seconds;
     //Stunden rausrechnen
     h :=s div (SecondsPerHour);
     //Restsekunden
     s :=s   - (h*SecondsPerHour);
     //Minuten rausrechnen
     m :=s div (MinutesPerHour);
     //Restsekunden
     s :=s  -  (m*MinutesPerHour);

     //String zusammensetzen
     result:='';
     if (h>0) then Result:=Result+IntToStr(h)+'h:';
     if (m>0) or (h>0) then Result:=Result+IntToStr(m)+'m:';
     if (s>0) or (m>0) or (h>0) then Result:=Result+IntToStr(s)+'s';
end;

end.
