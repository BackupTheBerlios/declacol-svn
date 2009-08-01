program tagrename;

{$APPTYPE CONSOLE}

uses
  unit_typedefs,unit_commandline,unit_strings,unit_filefunctions,class_id3,SysUtils,classes;

procedure showhelp();
begin
  CL_MessageOut('tagrename 0.1 (c) 2009 Borg@Sven-of-nine.de');
  CL_MessageOut('===========================================');
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
end;

var
  sSource  : longstring;
  sPath    : longstring;
  sScheme  : longstring;
  u32Index : unsigned32;
  slFiles  : TStringlist;
  sName    : Longstring;
  sExt     : Longstring;
  bLower   : Boolean;
  bSpace   : Boolean;
  bHelp    : Boolean;
  ID3      : TID3;
  TAG      : TID3Tag;


begin

  //Alle Parameter holen
  sPath   := CL_GetPathValue(0,'?');
  sScheme := CL_GetSwitchValue('rename',clSTRING,'%A - %L - %N');
  bLower  := CL_GetSwitchValue('-L',clEXIST,FALSE);
  bSpace  := CL_GetSwitchValue('-S',clEXIST,FALSE);

  sSource := ExpandFilename(IncludeTrailingPathDelimiter(sPath));

  //Alles OK? sonst hilfe anzeigen
  bHelp:= (not CL_HaveSwitches()) OR
          (sPath = '?') OR
          (not DirectoryExists(sSource));

  if (bHelp=FALSE) then
    begin
      slFiles:=FileScan(sPath,'*.mp3','',8192);
      CL_MessageOut(IntToStr(slFiles.Count)+' files found');

      ID3:=TID3.Create();

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

              //Optionen 
              if (bSpace=TRUE) then sName:=String_Replace(sName,' ','_');
              if (bLower=TRUE) then sName:=LowerCase(sName);

              //Zielname bauen
              sName:=Copy(slFiles[0],1,Length(sPath)+1)+sName;
              u32Index:=1;
              sExt:='.mp3';

              //Name auf jeden Fall richten
              sName:=String_Filter(sName,sftPath);
              while (fileexists(sName+sExt)=TRUE) AND
                    (sName + sExt <> slFiles[0]) DO
                begin
                  sExt:='.('+IntToHex(u32Index,3)+').mp3';
                  inc(u32Index);
                end;

              //Und umbenennen
              CL_MessageOut('renaming '+ExtractFilename(slFiles[0])+' to ' + sName+sExt);
              renamefile(slFiles[0],sName+sExt);
            end
          else
            begin
              CL_MessageOut('no valid tags found in '+ExtractFilename(slFiles[0]));
            end;
          slFiles.Delete(0);
        end;
    end
  else
    begin
      ShowHelp();
    end;
  readln;
end.
