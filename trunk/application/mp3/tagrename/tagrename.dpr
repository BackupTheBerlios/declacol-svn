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
/// Simple ID3 Tag renamer.
/// if the class_id3 is improved it'll be able to rename id3.2 as well
///
////////////////////////////////////////////////////////////////////////////////

{$IFDEF FPC}
        //Einstellungen für den GPC
        {$mode objfpc}{$H+}
{$ENDIF}
program tagrename;

{$APPTYPE CONSOLE}

uses
  unit_typedefs,unit_commandline,unit_strings,unit_filefunctions,class_id3,SysUtils,classes;

procedure showhelp();
begin
  CL_MessageOut('usage :');
  CL_MessageOut(' tagrename.exe path rename="namescheme" [options]');
  CL_MessageOut('');
  CL_MessageOut('namescheme tags :');
  CL_MessageOut(' %A => Artist');
  CL_MessageOut(' %T => Tracknumber');
  CL_MessageOut(' %L => Album');
  CL_MessageOut(' %N => Title');
  CL_MessageOut(' %Y => Year');
  CL_MessageOut(' %G => Genre');
  CL_MessageOut(' %M => MD5');
  CL_MessageOut('');
  CL_MessageOut('options');
  CL_MessageOut(' -L => lowercase');
  CL_MessageOut(' -S => use _ instead of space');
  CL_MessageOut(' -R => recursive');
  CL_MessageOut('');
  CL_MessageOut('examples');
  CL_MessageOut('');
  CL_MessageOut('tagrename c:\mymp3\ rename="%A--%L-(%T)-%N"');
  CL_MessageOut('tagrename c:\mymp3\ rename="%A - %N" -L -S');
  CL_MessageOut('');
end;

var
  sSource  : longstring;
  sPath    : longstring;
  sScheme  : longstring;
  u32Index : unsigned32;
  u32Ren   : unsigned32;
  u32Fail  : unsigned32;
  slFiles  : TStringlist;
  sName    : Longstring;
  sExt     : Longstring;
  bLower   : Boolean;
  bSpace   : Boolean;
  bRecurse : Boolean;
  bHelp    : Boolean;
  ID3      : TID3;
  TAG      : TID3Tag;


begin
  CL_MessageOut('tagrename 0.1 (c) 2009 Borg@Sven-of-nine.de');
  CL_MessageOut('===========================================');

  //Alle Parameter holen
  sPath   := CL_GetPathValue(0,'?');
  sScheme := CL_GetSwitchValue('rename',clSTRING,'%A - %L - %N');
  bLower  := CL_GetSwitchValue('-L',clEXIST,FALSE);
  bSpace  := CL_GetSwitchValue('-S',clEXIST,FALSE);
  bRecurse:= CL_GetSwitchValue('-R',clEXIST,FALSE);

  sSource := ExpandFilename(IncludeTrailingPathDelimiter(sPath));

  //Alles OK? sonst hilfe anzeigen
  bHelp:= (not CL_HaveSwitches()) OR
          (sPath = '?') OR
          (not DirectoryExists(sSource));

  if (bHelp=FALSE) then
    begin
      u32Ren:=0;
      u32Fail:=0;

      //Alle MP3s suchen
      if (bRecurse=TRUE) then
        begin
          slFiles:=FileScan(sPath,'*.mp3','',8192);
        end
      else
        begin
          slFiles:=FileScan(sPath,'*.mp3','',0);
        end;

      CL_MessageOut(IntToStr(slFiles.Count)+' files found');

      ID3:=TID3.Create();
      //MD5 angefragt?
      ID3.calccrc:=Pos('%M',sScheme)>0;

      while (slFiles.Count > 0) do
        begin
          TAG:=ID3.read(slFiles[0]);

          if (Tag.Valid) then
            begin
              sName:=String_Replace(sScheme,'"','');

              //Tags ersetzen
              sName:=String_Replace(sName,'%A',Tag.Artist);
              sName:=String_Replace(sName,'%G',Tag.Genre);
              sName:=String_Replace(sName,'%Y',Tag.Year);
              sName:=String_Replace(sName,'%L',Tag.Album);
              sName:=String_Replace(sName,'%G',Tag.Genre);
              sName:=String_Replace(sName,'%T',Tag.Track);
              sName:=String_Replace(sName,'%N',Tag.Title);
              sName:=String_Replace(sName,'%M',Tag.MD5);

              //Böse Zeichen ersetzen
              sName:=String_Replace(sName,'\','');
              sName:=String_Replace(sName,'/','');
              sName:=String_Replace(sName,'?','');
              sName:=String_Replace(sName,'*','');
              sName:=String_Replace(sName,'"','');
              sName:=String_Replace(sName,':','');
              sName:=String_Replace(sName,'<','');
              sName:=String_Replace(sName,'>','');
              sName:=String_Replace(sName,'|','');

              //Optionen
              if (bSpace=TRUE) then sName:=String_Replace(sName,' ','_');
              if (bLower=TRUE) then sName:=LowerCase(sName);

              //Zielname bauen
              sName:=Copy(slFiles[0],1,Length(sPath)+1)+sName;
              u32Index:=1;
              sExt:='.mp3';

              //Name auf jeden Fall richten
              while (fileexists(sName+sExt)=TRUE) AND
                    (sName + sExt <> slFiles[0]) DO
                begin
                  sExt:='.('+IntToHex(u32Index,3)+').mp3';
                  inc(u32Index);
                end;

              //Und umbenennen
              if (renamefile(slFiles[0],sName+sExt)=TRUE) then
                begin
                  //CL_MessageOut('renamed '+ExtractFilename(slFiles[0])+' to ' + sName+sExt);
                  write('.');
                  inc(u32Ren);
                end
              else
                begin
                  CL_MessageOut('');
                  CL_MessageOut('failed to rename '+ExtractFilename(slFiles[0])+' to '+sName+sExt);
                  inc(u32Fail);
                end;
            end
          else
            begin
              CL_MessageOut('');
              CL_MessageOut('no valid tags found in '+ExtractFilename(slFiles[0]));
            end;
          slFiles.Delete(0);
        end;
      CL_MessageOut('');
      CL_MessageOut(IntToStr(u32Ren)+ ' renamed');
      CL_MessageOut(IntToStr(u32Fail) + ' failed');

      ID3.Free();
    end
  else
    begin
      ShowHelp();
    end;
  readln;
end.
