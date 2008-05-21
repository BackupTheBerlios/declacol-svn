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

function GetCHSParameter(Size : unsigned64;var cylinder : unsigned64; var heads : unsigned32; var sectors : unsigned32; SectorSize : unsigned32 ):Boolean;

implementation


function GetCHSParameter(Size : unsigned64;var cylinder : unsigned64; var heads : unsigned32; var sectors : unsigned32; SectorSize : unsigned32 ):Boolean;
begin

end;

end.
