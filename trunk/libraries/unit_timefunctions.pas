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

//////////////////////////////////////////////////////////////////////////////////////////
///
/// Zeitfunktionen (Unix-Timestamp / SecToStr / MSecToStr etc.)
///
///
///
///
///
///
///
///
///
///
//////////////////////////////////////////////////////////////////////////////////////////
unit Unit_TimeFunctions;

interface
uses SysUtils;

function DateTimeToTimeStamp(date:TDateTime):LongInt;
function TimeStampToDateTime(timestamp:LongInt):TDateTime;
function NTPStampToDateTime(ntpstamp:LongInt):TDateTime;
function SecToStr(seconds:int64):String;
function MSecToStr(milliseconds:int64):String;

implementation
const
     UNIXStartDate : TDateTime=255690.0;
     NTPStartDate  : TDateTime=255690.0;

function DateTimeToTimeStamp(date:TDateTime):LongInt;
begin
     UNIXStartDate:=EncodeDate(1970,1,1)+EncodeTime(2,0,0,0);
     Result:=Round(( date - UNIXStartDate)*86400);
end;

function TimeStampToDateTime(timestamp:LongInt):TDateTime;
begin
     UNIXStartDate:=EncodeDate(1970,1,1)+EncodeTime(2,0,0,0);
     Result:=(timestamp / 86400)+UNIXStartDate;
end;


function NTPStampToDateTime(NTPStamp:LongInt):TDateTime;
begin
     NTPStartDate:=EncodeDate(1900,1,1);//+EncodeTime(2,0,0,0);
     Result:=(NTPStamp / 86400)+UNIXStartDate;
end;


function MSecToStr(milliseconds:int64):String;
var
   h,m,s,ms : integer;
begin
     s:=milliseconds div 1000;
     ms:=milliseconds-s*1000;

     //Stunden rausrechnen
     h :=s div (60*60);
     s :=s-(h * 3600);

     //Minuten rausrechnen
     m :=s div (60);
     s :=s-(m*60);

     result:='';
     if (h>0) then Result:=Result+IntToStr(h)+'h:';
     if (m>0)  or (h>0) then Result:=Result+IntToStr(m)+'m:';
     if (s>0)  or (m>0) or (h>0) then Result:=Result+IntToStr(s)+'s:';
     if (ms>0) or (s>0) or (m>0) or (h>0) then Result:=Result+IntToStr(ms)+'ms';

end;

function SecToStr(seconds:int64):String;
var
   h,m,s : integer;
begin
     s :=seconds;
     h :=s div (60*60);
     s :=s-(h*60*60);
     m :=s div (60);
     s :=s-(m*60);
     result:='';
     if (h>0) then Result:=Result+IntToStr(h)+'h:';
     if (m>0) or (h>0) then Result:=Result+IntToStr(m)+'m:';
     if (s>0) or (m>0) or (h>0) then Result:=Result+IntToStr(s)+'s';
end;

end.
