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
/// Einfaches Hilfsunit um die Architektur per defines und Consts zu holen
///
/// Stark von Jedis Code inspiriert
///
////////////////////////////////////////////////////////////////////////////////

unit unit_compiler;

interface
{$DEFINE UNKNOWNDELPHI}

{$IFDEF VER80}
  {$DEFINE DELPHI1}
  {$DEFINE RTL80}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE IMPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE DELPHI2}
  {$DEFINE RTL90}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE IMPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE DELPHI3}
  {$DEFINE RTL100}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE IMPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE DELPHI4}
  {$DEFINE RTL120}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE IMPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE DELPHI5}
  {$DEFINE RTL130}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE IMPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE DELPHI6}
  {$DEFINE RTL140}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE EXPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE DELPHI7}
  {$DEFINE RTL150}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE EXPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER160}
  {$DEFINE DELPHI8}
  {$DEFINE RTL160}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE EXPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI2005} // synonym to DELPHI9
  {$DEFINE RTL170}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE EXPLICIT_VARIANT}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE DELPHI11}
  {$DEFINE RTL185}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE EXPLICIT_VARIANT}
{$ENDIF}

{$IFDEF UNKNOWNDELPHI}
  {$DEFINE DELPHI11}
  {$DEFINE RTL185}
  {$UNDEF UNKNOWNDELPHI}
  {$DEFINE EXPLICIT_VARIANT}
{$ENDIF}


implementation

end.
