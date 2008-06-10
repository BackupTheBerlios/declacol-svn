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
// Einfache Programm um alles in einem Verzeichnis auf Kleinschrift umzustellen
////////////////////////////////////////////////////////////////////////////////

program all2lower;
{$APPTYPE CONSOLE}
uses
  unit_typedefs,windows,sysutils;

////////////////////////////////////////////////////////////////////////////////
//Wenn die Parameter fehlen oder falsh sind die hilfe anzeigen
procedure showhelp();
begin
     writeln('all2lower v0.1 (c) 2008 Borg@Sven-of-Nine.de');
     writeln('--------------------------------------------');
     writeln('renames all files to lowercase');
     writeln('');
     writeln('usage :');
     writeln('all2lower  [path] [--recursive] [--help]');
     writeln('examples :');
     writeln('alltolower *.exe');
     writeln('alltolower *.doc --recursive');
     writeln('alltolower c:\windows\');
     writeln('alltolower c:\windows\*.dll --recursive');

     writeln('');
     writeln('[press return to continue]');
     readln;
end;

////////////////////////////////////////////////////////////////////////////////
//Eingabeparameter prüfen
function CheckToken(Parameter : Longstring; Token : Longstring; Default : Boolean):Boolean;
var
   u32Index : unsigned32;
   sTemp    : longstring;
   sToken   : longstring;
begin
     Result:=FALSE;

     u32Index:=1;
     while (u32Index <= unsigned32(ParamCount)) do
           begin
                sTemp :=ParamStr(u32Index);
                sToken:=Copy(sTemp,1,Length(Token));
                sTemp :=Copy(sTemp,Length(Token)+1,Length(sTemp));

                if ( sTemp = Parameter) AND (sToken = Token) then
                   begin
                        Result:=TRUE;
                   end;
                inc(u32Index);
           end;
end;

////////////////////////////////////////////////////////////////////////////////
//Ein NichtToken suchen
function CheckNoToken(Default : longstring; Token : longstring; Offset : unsigned32 = 1 ):longstring;
var
   u32Index : unsigned32;
begin
     Result:=Default;

     u32Index:=Offset;
     while (u32Index < unsigned32(ParamCount)) do
           begin
                //Ein freier Pfad enthält kein Token
                if (Copy(ParamStr(u32Index),1,Length(Token)) <> Token) then
                   begin
                        Result:=ParamStr(u32Index);
                   end;
                inc(u32Index);
           end;
end;


////////////////////////////////////////////////////////////////////////////////
//Ein Objekt umbenennen
function RenameObject(OldName,NewName : Longstring):Boolean;
begin
     Result:=MoveFile(PChar(OldName),PChar(NewName));
     if (Result = FALSE) then
        begin
             WriteLn('unable to rename ' + OldName);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Ein Verzeichnis umbenennen
procedure RenameTarget(Path : longstring; Filter : longstring; Recursion : Boolean);
var
   rSearch   : TSearchRec;
   s32Result : signed32;
begin
     WriteLn('processing ' + Path);

     //Pfad immer mit Backslash enden lassen
     Path:=IncludeTrailingBackSlash(Path);

     //Alle passenden Dateien umbenennen
     rSearch.Name:=Filter;
     s32Result:=FindFirst(Path + Filter,faAnyFile,rSearch);

     while (s32Result = 0) do
           begin
                //Objekt umbenennen wenn es keine Traverse ist
                if (Copy(rSearch.Name,1,1) <> '.') then
                   begin
                        RenameObject(Path + rSearch.Name, LowerCase(Path + rSearch.Name) );
                   end;

                s32Result:=FindNext(rSearch);
           end;
     //Done
     FindClose(rSearch);

     //Alle Verzeichnisse prüfen (Wenn gewünscht)
     if (Recursion = TRUE) then
        begin
             s32Result:=FindFirst(Path + '*.*',faDirectory,rSearch);

             while (s32Result = 0) do
                   begin
                        if ((rSearch.Attr AND faDirectory) <> 0) then
                           begin
                                if (Copy(rSearch.Name,1,1) <> '.') then
                                   begin
                                        RenameTarget(Path + rSearch.Name,Filter, Recursion);
                                   end;
                           end;
                        s32Result:=FindNext(rSearch);
                   end;
             //Done
             FindClose(rSearch);
           end;

end;


////////////////////////////////////////////////////////////////////////////////
///Hauptfunktion
////////////////////////////////////////////////////////////////////////////////
const
   CMD_TOKEN = '--';

var
   bHelp    : boolean;
   bRecurse : boolean;
   sPath    : longstring;
   sFilter  : longstring;
begin
     //Nix angegeben, dann hilfe zeigen
     bHelp:=paramcount() = 0;

     //Parameter auswerten
     bRecurse:=CheckToken('recursive',CMD_TOKEN,FALSE);

     //Pfad holen
     sPath  :=CheckNoToken('',CMD_TOKEN);

     sPath  :=ExpandFileName (sPath);
     sFilter:=ExtractFileName(sPath);
     sPath  :=ExtractFilePath(sPath);

     //Wenn kein Pfad gegebn wurde nehmen wir den eigenen
     if (sPath='') then sPath:=ExtractFilePath(ParamStr(0));

     //Ausführen
     if (bHelp=TRUE) then
        begin
             ShowHelp();
        end
     else
        begin
             WriteLn('path : '+sPath);
             WriteLn('mask : '+sFilter);
             RenameTarget(sPath,sFilter,bRecurse);
        end;
end.
