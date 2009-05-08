unit unit_hex;
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
/// Funktionen zum direkten Arbeiten mit ByteArrays
///
/////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs,unit_filesystem,sysutils,windows;

//In einem Array suchen und ersetzen
//Zurückgegeben wir die Anzahl der Ersetzungen
function searchandreplace(var data : array of byte; search : array of byte; replace : array of byte):unsigned32;

function hextofile(filename : longstring; data : pointer; size : unsigned32):boolean;

implementation



function searchandreplace(var data : array of byte; search : array of byte; replace : array of byte):unsigned32;
var
  u32SLen   : unsigned32;
  u32HLen   : unsigned32;

  u32HIndex : unsigned32;
  u32SIndex : unsigned32;

  u32Temp   : unsigned32;
begin
  result:=0;

  //Größen holen, um dem Compiler bei der Optimierung zu helfen
  //Immer die kleinere größe bei SAR nehmen
  if (length(search) > length(replace)) then
    begin
      u32SLen:=unsigned32(Length(replace));
    end
  else
    begin
      u32SLen:=unsigned32(Length(search));
    end;

  u32HLen:=unsigned32(Length(data));

  //Längenfeler abfangen
  if (u32HLen > 0) and (u32SLen > 0) then
    begin
      u32HIndex:=0;

      //Eins abziehen da wir das Offset brauchen
      dec(u32SLen);

      //Alle Zeichen durchgehen
      while (u32HIndex < (u32HLen - u32SLen) ) do
        begin
          //Stimmt das erste Zeichen?
          u32SIndex:=0;
          if (data[u32HIndex] = search[u32SIndex]) then
            begin
              //Nun vergleichen wir von hinten, ob die Zeichen gleich sind
              //haben wir einen Unterschied, können wir abbrechen und den
              //Vergleich von dieser Position neu starten
              u32Temp:=u32SLen;
              while (u32Temp > 0) and
                    (data[u32HIndex + u32Temp] = search[u32Temp]) do
                begin
                  dec(u32Temp);
                end;

              //Ist u32Temp=0 ist die ganze Sequenz gleich
              if (u32Temp=0) then
                begin
                  //Die Sequenz ersetzen
                  for u32Temp:=0 to u32SLen do
                    begin
                      data[u32HIndex + u32Temp]:=replace[u32Temp];
                    end;
                  inc(result);
                end
              else
                begin
                  //Nicht gefunden, dann überspringen
                  inc(u32HIndex,u32SLen);
                  inc(u32HIndex);
                end;
            end;
          //Ganz normal weitersuchen
          inc(u32HIndex);
        end;
    end;
end;

function hextofile(filename : longstring; data : pointer; size : unsigned32):boolean;
var
  hFile : THandle;
begin
  if (directoryexists(extractfilepath(Filename))=FALSE) then
    begin
      ForceDirectories(extractfilepath(Filename));
    end;

  hFile:=filecreate(Filename);

  if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      filewrite(hFile,data^,size);
      closehandle(hFile);
      result:=TRUE;
    end
  else
    begin
      result:=FALSE;
    end;


end;


end.
