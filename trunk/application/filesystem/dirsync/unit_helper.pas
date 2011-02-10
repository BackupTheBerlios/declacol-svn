unit unit_helper;

interface

uses unit_typedefs,sysutils,unit_filefunctions,windows;

function compare_files_normal  (source :longstring; target : longstring):boolean;
function compare_files_paranoid(source :longstring; target : longstring):boolean;

function sync_files(source:longstring; target:longstring):boolean;

implementation

//einfacher vergleich zweier dateien
//attribute ignorieren wir
function compare_files_normal  (source :longstring; target : longstring):boolean;
begin
  result:=                 (fileage(source) = fileage(target));
  result:=result AND   (filegetattr(source) = filegetattr(target));
  result:=result AND (getfilesizeex(source) = getfilesizeex(target));
end;

//hardcore-vergleich (crc)
function compare_files_paranoid(source :longstring; target : longstring):boolean;
begin
  result:=compare_files_normal(source,target);
  //TODO CRC
end;

//datei kopieren und den timestamp/attribute synchronisieren
function sync_files(source:longstring; target:longstring):boolean;
var
  crc16   : unsigned16;
  hsource : THandle;
  htarget : THandle;
begin
  copydataunfragged(source,target,crc16);
  hsource:=fileopen(source,fmOPENREAD);
  if (hsource <> INVALID_HANDLE_VALUE) then
    begin
      htarget:=fileopen(target,fmOPENWRITE);
      if (htarget <> INVALID_HANDLE_VALUE) then
        begin
          filesetdate(htarget,filegetdate(hsource));
          closehandle(htarget);
        end;
      closehandle(hsource);
    end;
  filesetattr(target,filegetattr(source));
  result:=compare_files_normal(source,target);
end;

end.
