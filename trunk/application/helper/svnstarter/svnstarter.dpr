program svnstarter;


uses
  unit_typedefs,
  SysUtils,
  unit_processfunctions,
  inifiles;

var
  param    : longstring;
  root     : longstring;
  svnexe   : longstring;
  ini      : tinifile;

begin
  ini:=tinifile.Create(ChangeFileExt(ParamStr(0),'.ini'));

  svnexe:= ini.ReadString('config','exe'    ,'svnserve.exe');
  param := ini.ReadString('config','params' ,'--daemon --foreground');
  root  := ini.ReadString('config','root'   ,'repository');

  ini.WriteString('config','exe'   ,svnexe);
  ini.WriteString('config','root'  ,root);
  ini.WriteString('config','params',param);

  param:=param + ' --root "'+root+'"';

  svnexe:=ExpandFilename(svnexe);

  if (fileexists(svnexe)=TRUE) then
    begin
      execute(svnexe,param,TRUE,FALSE);
    end;

  ini.free;
end.
