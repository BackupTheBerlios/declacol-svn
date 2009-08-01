program m3u;

{$APPTYPE CONSOLE}

uses
  unit_typedefs,unit_filefunctions,SysUtils,classes;

var
  sSource  : longstring;
  sFiles   : tstringlist;
  u32Index : unsigned32;

begin
  if (ParamCount() > 0) then
    begin

      sSource:=ExpandFileName(ParamStr(1));
      if ( DirectoryExists(sSource) ) then
        begin
          sSource:=IncludeTrailingPathDelimiter(sSource);
          sFiles:=FileScan(sSource,'*.mp3','',8192);
          sFiles.AddStrings(FileScan(sSource,'*.ogg','',8192));
          sFiles.AddStrings(FileScan(sSource,'*.wma','',8192));

          if (sFiles.Count > 0) then
            begin
              sFiles.Sort();
              for u32Index:=0 to sFiles.Count -1 do
                begin
                  sFiles[u32Index]:=copy(sFiles[u32Index],Length(sSource) + 1,Length(sFiles[u32Index]));
                end;


              sFiles.SaveToFile(sSource+'!playlist.m3u');
              writeln('done');
            end
          else
            begin
              writeln('no files found');
            end;
          sFiles.Free();
        end
      else
        begin
          writeln(sSource+' is not a directory');
        end;
    end
  else
    begin
      writeln('need a sourcedirectory');
    end;
end.
 