program screengrabber;

uses
  unit_typedefs,
  SysUtils,
  unit_grafix,
  unit_processfunctions,
  unit_stringfunctions,
  graphics,
  jpeg,
  unit_log
  ;

const
  Delay       = 10000; //Default delay between two screenshots
  jpg_prefix  = 'image-';
  u32Compress = 30; //JPEG compression factor

var
  bscreen : TBitmap;
  jfile   : TJPegImage;

  PID       : unsigned32;
  u32Delay  : unsigned32;
  sTarget   : longstring;
  sFile     : longstring;
  u32Index  : unsigned32;
  sLog      : longstring;

begin
  //init a few parameter
  PID  := ProcessIDByFile(ParamStr(0));
  sLog := ParamStr(0)+'.log';
  bScreen:=TBitmap.Create();
  jFile:=TJPEGImage.Create();
  jFile.CompressionQuality:=u32Compress;

  //Param1 = outputdir
  //Param2 = Delay
  sTarget:=ExpandFilename(IncludeTrailingBackslash(ParamStr(1)));
  u32Delay:=StrToIntDef(ParamStr(2),10000);

  forcedirectories(sTarget);
  sTarget:=IncludeTrailingBackSlash(sTarget) + jpg_prefix;

  //log header
  Log_Add(sLog,'------------------------------------------------------------------------------------------');
  Log_Add(sLog,'screengrabber started');
  Log_Add(sLog,'------------------------------------------------------------------------------------------');
  Log_Add(sLog,'Targetpath : '+sTarget);
  Log_Add(sLog,'PID        : '+IntToStr(PID));
  Log_Add(sLog,'Delay      : '+IntToStr(Delay));

  //pid file
  Str2File(ParamStr(0)+'.pid',IntToStr(PID));

  //main loop
  u32index:=0;
  while (TRUE) do
    begin

      //find free filename
      repeat
        sFile:=LowerCase(sTarget + IntToHex(u32Index,8) + '.jpg');
        inc(u32Index);
      until (fileexists(sFile)=FALSE);

      //get screen
      if (Bitmap_Screenshot(0,bScreen)=TRUE) then
        begin
          //convert to jpeg
          Log_Add(sLog,'screen ' + sFile);
          try
            jFile.Assign(bScreen);
//            jFile.Compress();
            jFile.SaveToFile(sFile);
          except
            Log_Add(sLog,'convertion to jpg failed');
          end;
        end
      else
        begin
          Log_Add(sLog,'unable to get screen');
        end;
      sleep(u32Delay);
    end;
end.
 