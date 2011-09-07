unit unit_names;
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
/////////////////////////////////////////////////////////////////////////////////
///
/// get your application a name
///
/// getname(TRUE) returns a randomly selected name
/// getname() return a name derived from the exe-file name
///
/////////////////////////////////////////////////////////////////////////////////

interface

uses unit_typedefs,sysutils;

function getname( randomized : boolean = FALSE ):longstring;

implementation

var
  names : array[0..44] of longstring =
  (
  'SilentBob','Wintermute','ScreamingFist','Deepthought','Mandelbrot',
  'EvilAdmin','Marvin','Beeblebrox','Gandalf','Rumo',
  'BabylonRocker','RedSector','Ganymed','Wooster','Bumblebee',
  'Groucho','Dogbert','Garou','LockJaw','Concrete',
  'Kosmonauta','Persephone','Ramayana','Gemini','Bluntman',
  'Tugboat','Tiny','0x40','RagDoll','Machine Head',
  'Anonymous','Genesis','Xenon','Poseidon','Torch',
  'JoeCool','Leviathan','Kang','Spongebob','DarkNet',
  'Uplink','Romeo','Egghead','Ghost','Case'
  );

const
  NAMECOUNT = length(names);

/////////////////////////////////////////////////////////////////////////////////
//convert a string into some kind of unique number
/////////////////////////////////////////////////////////////////////////////////
function stringtonumber(input : string; max : unsigned32):unsigned32;
var
  u32number : unsigned32;
  u32index  : unsigned32;
begin
  u32number:=length(input);
  u32index:=1;
  while (u32index <= unsigned32(length(input))) do
    begin
      inc(u32number,u32index * ord(input[u32index]));
      inc(u32index);
    end;
  result:=u32number mod max;
end;

/////////////////////////////////////////////////////////////////////////////////
//exported function
/////////////////////////////////////////////////////////////////////////////////
function getname( randomized : boolean = FALSE ):longstring;
begin
  if (randomized = FALSE) then
    begin
      result:=names[stringtonumber(extractfilename(paramstr(0)),NAMECOUNT-1)];
    end
  else
    begin
      randomize();
      result:=names[random(NAMECOUNT)];
    end;
end;

end.
