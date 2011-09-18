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

const
  NAMETYPE_DEFAULT  = 0;
  NAMETYPE_DB       = 1;
  NAMETYPE_JAPANESE = 2;


function getname(nametype : unsigned32 = NAMETYPE_DEFAULT; randomized : boolean = FALSE ):longstring;

implementation

function stringtonumber(input : string):unsigned32; forward;
function internalrand(seed : unsigned32):unsigned32; forward;

function getdbname(seed:unsigned32):longstring; forward;
function getjapanesename(seed:unsigned32):longstring; forward;

var
  namedb : array[0..44] of longstring =
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

  jap_sil : array[0..43] of longstring =
  (
  'a','e','i','o','u',
  'ka','ki','ku','ke','ko',
  'sa','shi','su','se','so',
  'ta','chi','tsu','te','to',
  'na','ni','nu','ne','no',
  'ha','hi','fu','he','ho',
  'ma','mi','mu','me','mo',
  'ya','yu','yo',
  'ra','ri','ru','re','ro',
  'wa'
  );

const
  NAMEDB_COUNT  = length(namedb);
  JAPAN_COUNT   = length(jap_sil);

/////////////////////////////////////////////////////////////////////////////////
//exported function
/////////////////////////////////////////////////////////////////////////////////
function getname(nametype : unsigned32 = NAMETYPE_DEFAULT; randomized : boolean = FALSE ):longstring;
var
  u32seed : unsigned32;
begin
  if (randomized = FALSE) then
    begin
      u32seed:=stringtonumber(extractfilename(paramstr(0)));
    end
  else
    begin
      randomize();
      u32seed:=random(high(signed32));
    end;

  case (nametype) of
    NAMETYPE_DB       : result:=getdbname(u32seed);
    NAMETYPE_JAPANESE : result:=getjapanesename(u32seed);
  else
    //default
    result:=getdbname(u32seed);
  end;
end;


/////////////////////////////////////////////////////////////////////////////////
// an internal random generator
/////////////////////////////////////////////////////////////////////////////////
function internalrand(seed : unsigned32):unsigned32;
begin
  result:=134775813 * seed + 1;
end;

/////////////////////////////////////////////////////////////////////////////////
//convert a string into some kind of unique number
/////////////////////////////////////////////////////////////////////////////////
function stringtonumber(input : string):unsigned32;
var
  u32index  : unsigned32;
begin
  result:=length(input);
  u32index:=1;
  while (u32index <= unsigned32(length(input))) do
    begin
      inc(result,u32index * ord(input[u32index]));
      inc(u32index);
    end;
end;

/////////////////////////////////////////////////////////////////////////////////
//get a name from the database
/////////////////////////////////////////////////////////////////////////////////
function getdbname(seed:unsigned32):longstring;
begin
  result:=namedb[seed mod (NAMEDB_COUNT - 1) ];
end;

/////////////////////////////////////////////////////////////////////////////////
//create a pseudo-japanese name
/////////////////////////////////////////////////////////////////////////////////
function getjapanesename(seed:unsigned32):longstring;
var
  u32size : unsigned32;
begin
  //create random size
  u32size:=seed mod 5;
  if (u32size < 2) then u32size:=3;

  result:='';
  while (u32size > 0) do
    begin
      seed:=internalrand(seed);
      result:=result +  jap_sil[seed mod (JAPAN_COUNT - 1)];
      dec(u32size);
    end;
end;

end.
