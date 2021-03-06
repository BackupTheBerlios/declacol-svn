program hashcalc;

{$APPTYPE CONSOLE}

uses
  unit_typedefs,
  SysUtils,
  class_checksum;

const
  cCRC32   = 1;
  cADLER32 = 2;
  cMD5     = 4;

var
  sFilename : longstring;
  sTemp     : longstring;
  u8Mode    : unsigned8;
  brename   : boolean;

procedure showhelp();
begin
  writeln('hashcalc 0.1 (c) 2009 Borg@Sven-of-Nine.de');
  writeln('usage :');
  writeln('  hashcalc filename [mode] [options]');
  writeln('mode :');
  writeln('  --crc32 (default)');
  writeln('  --adler32');
  writeln('  --md5');
  writeln('options :');
  writeln('  --rename');
  writeln('example');
  writeln('  hashcalc myfile.bin --md5');
  writeln('  hashcalc myfile.bin --md5 --rename');
end;

function calchash(filename:longstring;Mode : unsigned8):Longstring;
var
  CRC32   : TCRC32;
  ADLER32 : TADLER32;
  MD5     : TMD5;
  sClear  : Longstring;
begin
  result:='?';
  sClear:='?';

  case (Mode) of
    cCRC32 : begin
              CRC32 :=TCRC32.create();
              Result:=CRC32.FromFile(filename).sChecksum;
              sClear:=CRC32.FromString('').sChecksum;
              CRC32.Free();
            end;
    cADLER32 : begin
              ADLER32:=TADLER32.create();
              Result:=ADLER32.FromFile(filename).sChecksum;
              sClear:=ADLER32.FromString('').sChecksum;
              ADLER32.Free();
            end;
    cMD5     : begin
              MD5:=TMD5.create();
              Result:=MD5.FromFile(filename).sChecksum;
              sClear:=MD5.FromString('').sChecksum;
              MD5.Free();
            end;
  end;

  //Fehler zurückmelden
  if (result = sClear) then result:='?';
end;

begin
  sFilename:=ExpandFilename(ParamStr(1));

  u8Mode:=cCRC32;
  sTemp:=LowerCase(ParamStr(2));
  if (sTemp='--adler32') then u8Mode:=cADLER32;
  if (sTemp='--md5')     then u8Mode:=cMD5;

  brename:=(ParamStr(2)='--rename') OR (ParamStr(3)='--rename');


  if (sFilename<>'') then
    begin
      sTemp:=CalcHash(sFilename,u8Mode);
      if (sTemp<>'?') then
        begin
          write(LowerCase(sTemp));
          ExitCode:=0;

          if (bRename=TRUE) then
            begin
              sTemp:=extractfilepath(sFilename)+'\'+sTemp;
              renamefile(sFilename,sTemp);
              if (fileexists(sTemp)=FALSE) then
                begin
                  writeln('');
                  writeln('unable to rename file');
                  ExitCode:=2;
                end;
            end;

        end
      else
        begin
          writeln('unable to calculate hash (file locked?)');
          ExitCode:=1;
        end;
    end
  else
    begin
      showhelp();
      ExitCode:=255;
    end;
end.
 