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

//////////////////////////////////////////////////////////////////////
//
// Simple Klasse um Restlaufzeiten zu berechnen
//
//////////////////////////////////////////////////////////////////////
unit class_runtime;

interface

uses unit_typedefs,unit_timefunctions,windows;

type TRuntime = class(tobject)
protected
  eventsmax  : unsigned32;
  eventsdone : unsigned32;

  timestart  : unsigned32;
  timeleft   : unsigned32;
  seta       : longstring;

  brunning   : boolean; 

  function gettimedone():unsigned32;
  function gettimeleft():unsigned32;
  function geteta():longstring;

private
public

  constructor create();

  procedure start();
  procedure triggerevent(count : unsigned32);
  procedure stop();

  property events_max  : unsigned32 read eventsmax  write eventsmax;
  property events_done : unsigned32 read eventsdone;

  property time_done   : unsigned32 read gettimedone;
  property time_left   : unsigned32 read gettimeleft;
  property eta         : longstring read geteta;

  property running     : boolean    read brunning;
end;

implementation
constructor TRuntime.create();
begin
  eventsmax :=0;
  eventsdone:=0;
  brunning:=FALSE;
end;

procedure TRuntime.Start();
begin
  Self.timestart :=getcurrenttime();
  Self.eventsdone:=0;
  Self.brunning:=TRUE;
end;

procedure TRuntime.Stop();
begin
  Self.brunning:=FALSE;
end;

procedure TRuntime.TriggerEvent(count : unsigned32);
var
  diff : unsigned32;
begin
  inc (Self.eventsdone,count);
  if (Self.eventsdone > Self.eventsmax) then
    begin
      Self.eventsmax:=Self.eventsdone;
    end;

  if (eventsdone > 0) then
    begin
      diff:=self.time_done;
      diff:=diff div eventsdone;
    end
  else
    begin
      diff:=1;
    end;

  //Restzeit in Millisekunden
  Self.timeleft:=(Self.eventsmax - Self.eventsdone) * diff;
  Self.seta:=SecToStr(Self.timeleft);
end;

function Truntime.geteta():longstring;
begin
  Self.TriggerEvent(0);
  result:=Self.seta;
end;

function Truntime.gettimeleft():unsigned32;
begin
  Self.TriggerEvent(0);
  result:=Self.timeleft
end;

function TRuntime.gettimedone():unsigned32;
begin
  result:=( getcurrenttime() - Self.timestart ) shr 10 ;
end;

end.
