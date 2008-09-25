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
///
/// Hilfsunit für Devicespezifische Funktionen
///
////////////////////////////////////////////////////////////////////////////////
unit unit_hddhelper;

interface
uses unit_typedefs,sysutils;

//Die Sektoren und Köpfe bestimmen
procedure GetCHSSizes    (Size : unsigned64; var heads    : unsigned32; var Sectors : unsigned32);
function  GetCHSParameter(Size : unsigned64; var cylinder : unsigned64; var heads : unsigned32; var sectors : unsigned32; SectorSize : unsigned32 ):Boolean;

implementation

////////////////////////////////////////////////////////////////////////////////
//Die Sektoren und Köpfe bestimmen
procedure GetCHSSize(Size : unsigned64; var heads : unsigned32; var sectors : unsigned32);
var
   u32Mark : unsigned32;
begin
     //GB runterbrechen
     u32Mark := Size shr (10 + 10 + 10);

     if (u32Mark > 1 ) then
        begin
             if (u32Mark > 2) then
                begin
                     //> 2 GB
                     Heads  :=255;
                     Sectors:=63;
                end
             else
                begin
                     // >1gb <= 2GB
                     Heads  :=128;
                     Sectors:=32;
                end;
        end
     else
        begin
             // <= 1GB
             Heads  :=64;
             Sectors:=32;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
function GetCHSParameter(Size : unsigned64;var cylinder : unsigned64; var heads : unsigned32; var sectors : unsigned32; SectorSize : unsigned32 ):Boolean;
begin
     //Die Kopfverteilung lesen
     GetCHSSize(Size,heads,sectors);

     //Und die Cylinder berechnen
     cylinder:=Size div (heads * sectors * sectorsize);

     result:=TRUE;
end;

end.
