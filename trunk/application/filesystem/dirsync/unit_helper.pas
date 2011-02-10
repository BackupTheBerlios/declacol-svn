unit unit_helper;

interface

uses unit_typedefs,sysutils,unit_filefunctions,windows,class_checksum;

function compare_files_normal  (source :longstring; target : longstring):boolean;
function compare_files_paranoid(source :longstring; target : longstring):boolean;

function sync_files(source:longstring; target:longstring; unfragged : boolean):boolean;

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
var
  adler32   : TAdler32;
  sourcecrc : TAdler32Struct;
  targetcrc : TAdler32Struct;
begin
  result:=compare_files_normal(source,target);

  //crc nur wenn der restliche vergleich ok ist
  if (result = TRUE) then
    begin
      adler32:=TAdler32.create();
      sourcecrc:=adler32.fromfile(source);
      targetcrc:=adler32.fromfile(target);
      adler32.free();
      result:=result AND (sourcecrc.u32Checksum = targetcrc.u32Checksum);
    end;
end;

//datei kopieren und den timestamp/attribute synchronisieren
function sync_files(source:longstring; target:longstring; unfragged : boolean):boolean;
var
  crc16   : unsigned16;
  hsource : THandle;
  htarget : THandle;
begin
  if (unfragged = TRUE) then
    begin
      copydataunfragged(source,target,crc16);
    end
  else
    begin
      copydata(source,target);
    end;

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
