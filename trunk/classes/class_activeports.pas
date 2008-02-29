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

unit class_activeports;

interface

uses unit_typedefs,
     sysutils,
     Winsock;

////////////////////////////////////////////////////////////////////////////////
//   IP helper library
////////////////////////////////////////////////////////////////////////////////
const
  IPHLPAPI                      =  'IPHLPAPI.DLL';


type TPort = record
     Name      : Longstring;
     LocalIP   : Longstring;
     LocalPort : unsigned16;

     TargetIP  : Longstring;
     TargetPort: unsigned16;
end;

type TActiveports = class(TObject)
private
      function IP4ToString(MyIP:TIP):Longstring;
      function StringToIP4(MyIP:Longstring):TIP; 
protected
public
      constructor create();

      procedure refresh();
end;

implementation

constructor TActivePorts.create();
begin
end;



procedure TActivePorts.Refresh();
begin
end;


////////////////////////////////////////////////////////////////////////////////
/// Hilfsfunktionen
////////////////////////////////////////////////////////////////////////////////
//IP zu String und zurück
function TActivePorts.IP4TOString(MyIP:TIP):Longstring;
begin
     result:=format('%d.%d.%d.%d',[MyIP.A,MyIP.B,MyIP.C,MyIP.D]);
end;

function TActivePorts.StringToIP4(MyIP:Longstring):TIP;
var
   IPTemp   : Array[0..3] of Byte;
   u32Index : unsigned32;
   u32Pos   : unsigned32;
begin
     //Array initialisieren
     IPTemp[0]:=0;IPTemp[1]:=0;IPTemp[2]:=0;IPTemp[3]:=0;

     u32Index:=0;
     u32Pos:=Pos('.',MyIP);
     while (Pos('.',MyIP)>0) AND (u32Index < 4) do
           begin
                IPTemp[u32Index]:=StrToIntDef(Copy(MyIP,1,u32Pos-1),0);
                Delete(MyIP,1,u32Pos);
                u32Pos:=Pos('.',MyIP);
                inc(u32Index);
           end;
     //Der Rest
     Result.A:=IPTemp[0];
     Result.B:=IPTemp[1];
     Result.C:=IPTemp[2];
     Result.D:=StrToIntDef(MyIP,0);
end;

end.
