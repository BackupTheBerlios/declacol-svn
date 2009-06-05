program language_compiler;

{$APPTYPE CONSOLE}

uses
  unit_typedefs,SysUtils,class_language,windows,classes,unit_hex;

procedure showhead();
begin
  writeln('dingoo language-compiler 0.2');
  writeln('-----------------------------');
  writeln('(c) 2009 Borg@Sven-of-Nine.de');
  writeln('');
end;

procedure showhelp();
begin
  writeln('compile resource');
  writeln('language_compiler.exe languagefile [outfile]');
  writeln('');
  writeln('decompile resource');
  writeln('language_compiler.exe extract languagefile [outfile]');
  writeln('');
end;

procedure createinifile(infile : longstring;outfile : longstring);
var
  sLng      : Tlanguageencoder;
  items     : tstringlist;
  data      : array of byte;
  u32size   : unsigned32;
  u32index  : unsigned32;
begin
  try
    //Größe des Array bestimmen
    if (filetohex(infile,data,u32size) = FALSE) AND (u32Size > 0) then
      begin
        setlength(data,u32size);
        filetohex(infile,data,u32size);

        sLng:=TLanguageEncoder.Create();
        items:=tstringlist.Create();

        sLng.unpack(data,items);

        u32index:=0;
        while (u32Index < items.Count) do
          begin
            items[u32index]:=items[u32index] + '   ==    ' + items[u32index];
            inc(u32index);
          end;

        items.SaveToFile(outfile);

        sLng.Free();
        items.Free();

        writeln('languagefile created');
      end
    else
      begin
        writeln('unable to open '+infile);
      end;


  except
    writeln('unable to create '+extractfilename(outfile));
  end;
end;

procedure compile(sIn : longstring; sOut : longstring);
var
  slIn      : TStringlist;
  slOut     : TStringlist;
  sLng      : Tlanguageencoder;
  u32Index  : unsigned32;
  aData     : array of byte;
begin
  writeln('compiling '+sIn);

  //Alle Strings lesen
  slIn:=TSTringlist.Create();
  slIn.LoadFromFile(sIn);

  //Nur dir rechte Seite holen
  slOut:=TStringList.Create();
  u32Index := 0;
  while (u32Index < unsigned32(slIn.Count)) do
    begin
      slOut.Add(trim(copy(slIn[u32Index],pos('==',slIn[u32Index])+2,Length(slIn[u32Index]))));
      inc(u32Index);
    end;

  sLNG:=TLanguageencoder.Create();
  setlength(aData,0);
  sLNG.pack(aData,slOut);
  setlength(aData,sLNG.memneeded);
  sLNG.pack(aData,slOut);
  sLNG.Free();

  hextofile(sOut,addr(aData[0]),length(aData));

  writeln('done');
end;

var
  sIn  : longstring;
  sOut : longstring;
begin
  showhead();

  sIn:=expandfilename(lowercase(paramstr(1)));

  //Eingabedatei nicht gefunden
  if (fileexists(sIn)=FALSE) then
    begin
      //Soll decompiled werde?
      if (paramstr(1)='extract') then
        begin
          //Dateien angegeben?
          if (paramstr(2) <> '') then
            begin
              //Pfade holen
              sIn :=expandfilename(lowercase(paramstr(2)));

              if (paramstr(3)='') then
                begin
                  sOut:=ChangeFileExt(sIn,'.txt');
                end
              else
                begin
                  sOut:=expandfilename(lowercase(paramstr(3)));
                end;

              //Und los
              createinifile(sIn,sOut);
            end
          else
            begin
              writeln('in or outfile not given');
            end;
        end
      else
        begin
          showhelp();
        end;
    end
  else
    begin
      //Es soll compiliert werden
      if (paramstr(2) <> '') then
        begin
          //Ausgabedatei angegeben?
          sOut:=expandfilename(lowercase(paramstr(2)));
        end
      else
        begin
          sOut:=ChangeFileExt(sIn,'.dlx');
        end;
      compile(sIn,sOut);
    end;
end.
