program language_compiler;

{$APPTYPE CONSOLE}

uses
  unit_typedefs,SysUtils,class_language,windows,classes,unit_hex;

procedure showhead();
begin
  writeln('dingoo language-compiler 0.1');
  writeln('-----------------------------');
  writeln('(c) 2009 Borg@Sven-of-Nine.de');
  writeln('');
end;

procedure showhelp();
begin
  writeln('compile resource');
  writeln('language_compiler.exe languagefile [outfile]');
  writeln('');
  writeln('create default languagefile');
  writeln('language_compiler.exe create languagefile');
  writeln('');
end;

procedure createinifile(filename : longstring);
var
  fileout : TFileStream;

  procedure saveline(text : longstring);
  begin
    text:=text+'   ==    '+text+ #13;
    fileout.write(text[1],length(text));
  end;


begin
  try
    fileout:=TFileStream.Create(filename,fmCreate);

    saveline('Main Interface');
    saveline('Music');
    saveline('Video');
    saveline('FLASH  Play');
    saveline('Picture');
    saveline('Radio');
    saveline('Record');
    saveline('E-Book');
    saveline('Playing');
    saveline('Browser');
    saveline('Games');
    saveline('System Set');
    saveline('Blight Time');
    saveline('About');
    saveline('Important Notice');
    saveline('Main Menu');
    saveline('Display');
    saveline('Button Sound');
    saveline('Date & Time');
    saveline('Language');
    saveline('Auto Off');
    saveline('Auto Off');
    saveline('USB Mode');
    saveline('Default Set');
    saveline('Blight Time');
    saveline('Background Lights');
    saveline('Background Transparency');
    saveline('Fonts Colour');
    saveline('Choice Strip Color');
    saveline('Back To Default Background');
    saveline('5 secs');
    saveline('10 secs');
    saveline('15 secs');
    saveline('20 secs');
    saveline('30 secs');
    saveline('Always On');
    saveline('Off');
    saveline('On');
    saveline('>');
    saveline('OK');
    saveline('Cancel');
    saveline('White');
    saveline('Gray');
    saveline('Dark White');
    saveline('Blue');
    saveline('Green');
    saveline('Violet');
    saveline('Orange');
    saveline('Pink');
    saveline('Delete');
    saveline('Play');
    saveline('Add to current playlist');
    saveline('Add to my playlist');
    saveline('Connect to USB');
    saveline('Play Rec');
    saveline('Start Rec');
    saveline('Recording current Radio station');
    saveline('Listen Radio');
    saveline('FM List');
    saveline('Updata FM Playlist');
    saveline('Choose FM Playlist');
    saveline('Stereo Switch');
    saveline('File Name');
    saveline('Title');
    saveline('Artist');
    saveline('Album');
    saveline('Turn off order');
    saveline('Sampling Rate');
    saveline('Bit Rate');
    saveline('Unknown');
    saveline('3D Games');
    saveline('GBA Games');
    saveline('GBA Sound Settings');
    saveline('GBA Display Setting');
    saveline('Original Size');
    saveline('Zoom with scale');
    saveline('Full Screen Display');
    saveline('Signal Power Adjust');
    saveline('Low');
    saveline('Mid');
    saveline('High');
    saveline('Internal');
    saveline('External');
    saveline('All Songs');
    saveline('Playlist');
    saveline('Album');
    saveline('Play Orders');
    saveline('Update Jukebox');
    saveline('Play Mode');
    saveline('EQ Select');
    saveline('All Repeat');
    saveline('Order');
    saveline('Shuffle');
    saveline('Repeat Once');
    saveline('Once');
    saveline('Rencent Videos');
    saveline('Video Settings');
    saveline('Listen Radio');
    saveline('Save This Station');
    saveline('Background Play');
    saveline('Close');
    saveline('Normal');
    saveline('Jazz');
    saveline('Pop');
    saveline('Rock');
    saveline('Classic');
    saveline('Vocal');
    saveline('Treble');
    saveline('Bass');
    saveline('Video Times');
    saveline('Display current time');
    saveline('Display full times');
    saveline('Show all time');
    saveline('Turn off display time');
    saveline('10 min');
    saveline('30 min');
    saveline('1 hr.');
    saveline('Switch Off');
    saveline('No File Found!');
    saveline('Video Playlist');
    saveline('USB Connecting');
    saveline('USB Connected...');
    saveline('UDisk Memory');
    saveline('UDisk Free');
    saveline('Model   SDK');
    saveline('Version V1.03');
    saveline('(c) 2007-2008 CHINACHIP');
    saveline('File List');
    saveline('Music Playlist');
    saveline('U Disk');
    saveline('MINISD Card');
    saveline('Browse All Files');
    saveline('Theme Choose');
    saveline('Theme One');
    saveline('Theme Two');
    saveline('Theme Three');
    saveline('Theme Four');
    saveline('No FM station info!');
    saveline('Flash Playlist');
    saveline('MINISD card out!');
    saveline('Unkown music format!');
    saveline('File doesn'+#39+'t exist!');
    saveline('Unknown Tite');
    saveline('Unkown Artist');
    saveline('Unkown Album');
    saveline('White');
    saveline('Red');
    saveline('DarkRed');
    saveline('Green');
    saveline('Dark Green');
    saveline('Blue');
    saveline('Dark Blue');
    saveline('Dark White');
    saveline('Green Blue');
    saveline('Violet');
    saveline('Dark Violet');
    saveline('Yellow');
    saveline('Dark Yellow');
    saveline('Gray');
    saveline('Black');
    saveline('GBA Game Playlist');
    saveline('Not enough space!');
    saveline('No File Found!');
    saveline('Snakes');
    saveline('No File Found!');
    saveline('Folder Once');
    saveline('MINISD card out!');
    saveline('Speaker');
    saveline('No HeadPhone,No Fm Update!');
    saveline('No HeadPhone,No listen!');
    saveline('Please plug the USB calbe!');
    saveline('Press \"MENU\" to quit!');
    saveline('Press \"Left\" to quit!');
    saveline('No File Found!');
    saveline('Not Enough Space!');
    saveline('Picture module error!');
    saveline('Music Background Pic');
    saveline('FM radio background pic');
    saveline('Recording Background Pic');
    saveline('E-Book Background Pic');
    saveline('Main Menu Background Pic');
    saveline('Slide Show Switch Times');
    saveline('Open slide show');
    saveline('Close slide show');
    saveline('2 secs');
    saveline('3 secs');
    saveline('5 secs');
    saveline('8 secs');
    saveline('15 secs');
    saveline('Unknow media format!');
    saveline('Read media format error!');
    saveline('Continue Play');
    saveline('China');
    saveline('Europe');
    saveline('U.S.A');
    saveline('Japan');
    saveline('FM  Region');
    saveline('MINISD Memory');
    saveline('MINISD Free');
    saveline('Erros');
    saveline('File doesn'+#39+'t exist,can'+#39+'t open!');
    saveline('No enough memory,can'+#39+'t open!');
    saveline('Empty file,can'+#39+'t Read!');
    saveline('File too big, can'+#39+'t open!');
    saveline('No File Found!');
    saveline('Not Enough Memory!');
    saveline('Can'+#39+'t use bookmark!');
    saveline('Set  1st bookmark');
    saveline('Set  2nd bookmark');
    saveline('Set  3rd bookmark');
    saveline('Set  4th bookmark');
    saveline('Jump to 1st bookmark');
    saveline('Jump to 2nd bookmark');
    saveline('Jump to 3rd bookmark');
    saveline('Jump to 4th bookmark');
    saveline('TTS engine loading...');
    saveline('25 secs');
    saveline('35 secs');
    saveline('Auto Next Page');
    saveline('Bookmark');
    saveline('Auto Play Set');
    saveline('TTS read');
    saveline('No Bookmark!');
    saveline('End!');
    saveline('Top!');
    saveline('Run from last');
    saveline('Yes');
    saveline('No');
    saveline('Finish Record and Saved!');
    saveline('Press any button to quit');
    saveline('Not Enough Space!');
    saveline('Recording time remaining:');
    saveline('FM Stations');
    saveline('Hardware Error!');
    saveline('Please check FM module line!');
    saveline('Update FM playlist first!');
    saveline('Manual adjust Radio frequency');
    saveline('FM Station Serching .....');
    saveline('Recording...');
    saveline('Continue from last time?');
    saveline('Disk Is Full,Can'+#39+'t use bookmark');
    saveline('Animation Picture');
    saveline('1 min');
    saveline('3 min');
    saveline('10 min ');
    saveline('Close');
    saveline('Candy House');
    saveline('Brick');
    saveline('E-Book fonts');
    saveline('Big Font');
    saveline('Small Font');
    saveline('Customize');
    saveline('File saving...');
    saveline('Initializing...');
    saveline('Rec Quality');
    saveline('Low');
    saveline('Middle');
    saveline('High');
    saveline('100 ms');
    saveline('200 ms');
    saveline('300 ms');
    saveline('400 ms');
    saveline('500 ms');
    saveline('Deleting File');
    saveline('Deleting file!');
    saveline('Please open FM!');
    saveline('FM Record');
    saveline('Normal');
    saveline('FillBox');
    saveline('FullScreen');
    saveline('Zoom Mode');
    saveline('Low Battery!');
    saveline('No music is playing!');
    saveline('Right direction');
    saveline('Left direction');
    saveline('Screen direction');
    saveline('Automatically search radio');
    saveline('Main menu style');
    saveline('List');
    saveline('Icon');
    saveline('TV standard ');
    saveline('TV screens set up ');
    saveline('TV screen ');
    saveline('TV ');
    saveline('P system ');
    saveline('N system ');
    saveline('16:9 ');
    saveline('4:3 ');
    saveline('TV ');
    saveline('Screen');
    saveline('Broswer');
    saveline('Main choice');
    saveline('Animated icon');
    saveline('General icon');
    saveline('List');
    saveline('TV types');
    saveline('LCD');
    saveline('General');
    saveline('Off');
    saveline('Concert Hall');
    saveline('Small room');
    saveline('Large room');
    saveline('Scene');
    saveline('Stadium');
    saveline('Virtual sound field');
    saveline('Campus Channel');
    saveline('Udisk immunity');
    saveline('Theme five');
    saveline('EQ ');
    saveline('TV setup');
    saveline('Music setup');
    saveline('Video setup');
    saveline('Radio setup');
    saveline('Record setup');
    saveline('System setup');
    saveline('Open/close TV');
    saveline('Connect TV');
    saveline('cClose TV');
    saveline('GAME LIST');
    saveline('Game center');
    saveline('Music hall ');
    saveline('Movie theater');
    saveline('Picture room');
    saveline('Library');
    saveline('Record');
    saveline('Albums');
    saveline('Artist');
    saveline('Songs');
    saveline('Search music');
    saveline('3D game');
    saveline('Interesting game');
    saveline('Press right key to search music ');
    saveline('Search result');
    saveline('Sound output setting');
    saveline('3D processing ');
    saveline('Automatic volume balance ');
    saveline('Bass');
    saveline('High-frequency compensation ');
    saveline('3D audio settings ');
    saveline('General headphones ');
    saveline('Hi-fi headphones ');
    saveline('General Speaker ');
    saveline('Larger speakers ');
    saveline('Close');
    saveline('Weak ');
    saveline('Medium');
    saveline('Strong');
    saveline('Simulator not exit,download please!');
    saveline('Copy');
    saveline('Paste');
    saveline('Suspended recording');
    saveline('Need No Bookmark');
    saveline('Background Set');
    saveline('Jump Read');
    saveline('Jump Backward 20 Pages');
    saveline('Jump To Page100');
    saveline('Jump To Page250');
    saveline('Jump To Page500');
    saveline('Jump To Page1000');
    saveline('Continue Playing?');
    saveline('Stop recording');
    saveline('File exists,replace it?');
    saveline('Add to favorites');
    saveline('Can not set fontsize');
    saveline('Can not set fontcolor');
    saveline('Text To Sound');
    saveline('Model:');
    saveline('A320');
    saveline('File name is too long!');
    saveline('Favorites');
    saveline('Pedometer');
    saveline('Start');
    saveline('Stride Setting');
    saveline('Record List');
    saveline('To delete the record ');
    saveline('To delete all records ');
    saveline('Was deleted');
    saveline('Walk state	');
    saveline('State quickly ');
    saveline('Normal state');
    saveline('Records list');
    saveline('Update Finished');
    saveline('Total');
    saveline('Select');
    saveline('Do not put out sd card');
    saveline('');
    saveline('');
    saveline('');
    saveline('');

    fileout.Free();

    writeln('languagefile created');
  except
    writeln('unable to create '+extractfilename(filename));
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

  if (fileexists(sIn)=FALSE) then
    begin
      //Stringliste erstellen
      if (paramstr(1)='create') then
        begin
          if (paramstr(2) <> '') then
            begin
              sOut:=expandfilename(lowercase(paramstr(2)));
              createinifile(sOut);
            end
          else
            begin
              writeln('translationfile exists => stopping');
            end;
        end
      else
        begin
          showhelp();
        end;
    end
  else
    begin
      if (paramstr(2) <> '') then
        begin
          sOut:=expandfilename(lowercase(paramstr(2)));
        end
      else
        begin
          sOut:=expandfilename('.\с╒нд.dlx')
        end;
      compile(sIn,sOut);
    end;
  readln;
end.
