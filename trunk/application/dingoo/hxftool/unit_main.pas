unit unit_main;

interface

uses
  unit_typedefs,unit_strings,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_filefunctions,unit_filesystem,class_hxf, ComCtrls,
  Grids, Menus, ExtCtrls, ExtDlgs,unit_grafix,class_language, Mask,unit_hex;

type
  TfmMain = class(TForm)
    dgOpenHXF: TOpenDialog;
    pcMain: TPageControl;
    tsFiles: TTabSheet;
    tsPatches: TTabSheet;
    tsBootscreens: TTabSheet;
    tsLanguage: TTabSheet;
    tsDefaults: TTabSheet;
    tsLog: TTabSheet;
    gbHexviewer: TGroupBox;
    lbFiles: TListBox;
    GroupBox2: TGroupBox;
    lbLog: TListBox;
    sgHex: TStringGrid;
    sbHex: TScrollBar;
    mmMain: TMainMenu;
    File1: TMenuItem;
    mOpen: TMenuItem;
    mClose: TMenuItem;
    mExit: TMenuItem;
    N1: TMenuItem;
    pcScreens: TPageControl;
    tsLogon: TTabSheet;
    tsLogoff: TTabSheet;
    gbLogoff: TGroupBox;
    pnLogoff: TPanel;
    btImportLogoff: TButton;
    imLogoff: TImage;
    gbLogOn: TGroupBox;
    pnLogon: TPanel;
    imLogon: TImage;
    btImportLogon: TButton;
    dgOpenBitmap: TOpenPictureDialog;
    N2: TMenuItem;
    About1: TMenuItem;
    gbLanguages: TGroupBox;
    lbLanguage: TListBox;
    btSaveWord: TButton;
    lbLanguageSize: TLabel;
    edLanguage: TEdit;
    gbDefault: TGroupBox;
    cbLanguage: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cbPoweroff: TComboBox;
    Label3: TLabel;
    cbTheme: TComboBox;
    gbPatches: TGroupBox;
    btDirNamesToLower: TButton;
    btDisabelAutoRunProtection: TButton;
    btThemepatch: TButton;
    cbBrightness: TComboBox;
    Label4: TLabel;
    pmLanguage: TPopupMenu;
    dumptofile1: TMenuItem;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cbAvLanguage1: TComboBox;
    cbAvLanguage2: TComboBox;
    cbAvLanguage3: TComboBox;
    cbLanguages: TComboBox;
    procedure mOpenClick(Sender: TObject);
    procedure mCloseClick(Sender: TObject);
    procedure lockfunctions();
    procedure unlockfunctions();

    procedure lbFilesClick(Sender: TObject);

    procedure loadscreen(Filename : longstring; Bitmap:TBitmap);
    procedure savescreen(Filename : longstring; BitmapFile:longstring);
    procedure btImportLogoffClick(Sender: TObject);
    procedure btImportLogonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tsLogonShow(Sender: TObject);
    procedure tsLogoffShow(Sender: TObject);

    procedure enumlanguage(filename : string);
    procedure tsLanguageShow(Sender: TObject);
    procedure lbLanguageClick(Sender: TObject);
    procedure btSaveWordClick(Sender: TObject);
    procedure tsDefaultsShow(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure cbPoweroffChange(Sender: TObject);
    procedure cbThemeChange(Sender: TObject);
    procedure btDirNamesToLowerClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure sbHexChange(Sender: TObject);
    procedure mExitClick(Sender: TObject);
    procedure edLanguageKeyPress(Sender: TObject; var Key: Char);
    procedure btDisabelAutoRunProtectionClick(Sender: TObject);
    procedure btThemepatchClick(Sender: TObject);
    procedure cbBrightnessChange(Sender: TObject);
    procedure dumptofile1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tsPatchesShow(Sender: TObject);
    procedure cbAvLanguage1Change(Sender: TObject);
    procedure cbLanguagesChange(Sender: TObject);
    procedure cbAvLanguage2Change(Sender: TObject);
    procedure cbAvLanguage3Change(Sender: TObject);

  private
    hxf       : Thxfreader;
    hxfdata   : thxfrecord;
    sLangfile : longstring;

    { Private-Deklarationen }
    procedure addlog(txt : longstring);
    procedure setconfig(offset:unsigned32; data : byte);
    procedure sar (filename : longstring; search : longstring; replace : longstring); overload;
    procedure sar(filename : longstring; search : array of byte; replace : array of byte); overload;
    procedure addsignature();
    procedure savelanguagesettings(langindex : unsigned32; languageid : unsigned32);

  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;
  bHXF : boolean = FALSE;
  aLangID  : array of unsigned32;
  aLangPos : array of unsigned32;

implementation

uses unit_about;

{$R *.dfm}


/////////////////////////////////////////////////////////////////////////////////
/// Formularfunktionen
/////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.FormCreate(Sender: TObject);
begin
  lockfunctions();
end;


/////////////////////////////////////////////////////////////////////////////////
/// Men�funktionen
/////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.mOpenClick(Sender: TObject);
var
  data : thxfrecord;
  bOK  : boolean;
begin
  with (dgOpenHXF) do
    begin
      if (execute=TRUE) then
        begin
          lbFiles.Clear();

          addlog('open '+filename);

          //Alles analysieren und Dateien listen
          hxf:=thxfreader.create(filename);

          if (hxf.createchecksum() <> hxf.crc) then
            begin
              if (hxf.ignoreerrors=TRUE) then
                begin
                  addlog('checksum invalid but reading anyway');
                end
              else
                begin
                  addlog('checksum invalid');
                end;
            end;

          bOK:=hxf.getfirst(data);
          while (bOK=TRUE) do
            begin
              lbFiles.Items.Add(data.filename);

              bOK:=hxf.getnext(data);
              application.processmessages();
            end;

          addlog(inttostr(hxf.count)+' files found');

          if(hxf.count > 0) then
            begin
              unlockfunctions();
            end
          else
            begin
              mCloseClick(Self);
            end;
        end;
    end;
end;

procedure TfmMain.mCloseClick(Sender: TObject);
begin
  lockfunctions();

  addsignature();

  hxf.free();
  addlog('hxf closed');
end;

procedure TfmMain.mExitClick(Sender: TObject);
begin
  Close();
end;


procedure TfmMain.lockfunctions();
begin
  mClose.Enabled:=FALSE;
  mOpen.Enabled:=not mClose.Enabled;

  lbFiles.Clear();

  //Tabs sperren
  tsPatches.TabVisible     :=FALSE;
  tsBootScreens.TabVisible :=FALSE;
  tsLanguage.TabVisible    :=FALSE;
  tsDefaults.TabVisible    :=FALSE;
end;

procedure TfmMain.unlockfunctions();
begin
  //Close freigeben
  mClose.Enabled:=hxf.count > 0;
  mOpen.Enabled:=not mClose.Enabled;

  //Die einzelnen Tabs je nach Funktionalit�t aktivieren
  tsFiles.TabVisible       := hxf.count > 0;
  tsPatches.TabVisible     := hxf.exists('ccpmp.bin');
  tsBootscreens.TabVisible := hxf.exists('user_data\logon.ani') and
                              hxf.exists('user_data\logoff.ani');
  tsLanguage.TabVisible    := hxf.exists('system\nls\Ӣ��.dlx');
  tsDefaults.TabVisible    := hxf.exists('user_data\ccpmp.cfg') and
                              hxf.exists('user_data\default.cfg');

  addlog('');
  if (tsPatches.TabVisible)     then addlog('firmware found');
  if (tsBootscreens.TabVisible) then addlog('bootscreens found');
  if (tsLanguage.TabVisible)    then addlog('english languagefile found');
  if (tsDefaults.TabVisible)    then addlog('defaultconfiguration found');

  addlog('');
  addlog('header data');
  addlog('id        : '+ hxf.header.id);
  addlog('version   : '+ hxf.header.version);
  addlog('timestamp : '+ hxf.header.timestamp);
  addlog('size      : '+ inttostr(hxf.header.size));
  addlog('checksum  : '+ inttohex(hxf.header.crc,12));
  addlog('');
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Datei auf jeden Fall immer schlie�en
  if (mClose.Enabled) then
    begin
      mCloseClick(Self);
    end;
end;


/////////////////////////////////////////////////////////////////////////////////
// Hexviewer
/////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.lbFilesClick(Sender: TObject);
begin
  sbHex.Enabled:=FALSE;
  sbHex.Max:=0;
  sbHex.Min:=0;
  sbHex.Position:=0;

  if (lbFiles.ItemIndex > -1) then
    begin
      if (hxf.get(lbfiles.ItemIndex,hxfdata)=TRUE) then
        begin
          sbHex.Max:=hxfdata.size;
          sbHexChange(Self);
        end;
    end;
end;

procedure TfmMain.sbHexChange(Sender: TObject);
var
  u32Index : unsigned32;
  u32row   : unsigned32;
  u32col   : unsigned32;
  sTemp    : longstring;
begin
  u32row := 0;
  u32col := 0;
  for u32Index:=sbHex.Position to sbHex.Position + ((sgHex.ColCount * sgHex.RowCount)-1)  do
    begin
      sgHex.Cells[u32Col,u32Row]:=inttohex(hxfdata.buffer[u32Index],2);
      stemp:=stemp + chr(hxfdata.buffer[u32Index]);

      inc(u32col);
      if (u32col >= unsigned32(sgHex.ColCount) - 1) then
        begin
          sgHex.Cells[u32Col,u32Row]:=stemp;
          u32col:=0;
          stemp:='';
          inc(u32row);
        end;
    end;
end;

/////////////////////////////////////////////////////////////////////////////////
// Bootscreens
/////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.tsLogonShow(Sender: TObject);
begin
  loadscreen('user_data\logon.ani' ,imLogon.Picture.Bitmap);
  imLogon.Invalidate();
end;

procedure TfmMain.tsLogoffShow(Sender: TObject);
begin
  loadscreen('user_data\logoff.ani',imLogOff.Picture.Bitmap);
  imLogoff.Invalidate();
end;


procedure TfmMain.btImportLogonClick(Sender: TObject);
begin
  with (dgOpenBitmap) do
    begin
      if (execute=TRUE) then
        begin
          savescreen('user_data\logon.ani',filename);
          tsLogOnShow(Self);
        end;
    end;
end;

procedure TfmMain.btImportLogoffClick(Sender: TObject);
begin
  with (dgOpenBitmap) do
    begin
      if (execute=TRUE) then
        begin
          savescreen('user_data\logoff.ani',filename);
          tsLogOffShow(Self);
        end;
    end;
end;


procedure TfmMain.loadscreen(Filename : longstring; Bitmap:TBitmap);
var
  u32X     : unsigned32;
  u32Y     : unsigned32;
  u32Index : unsigned32;
  aTemp    : Thxfrecord;
  pLine    : ^Byte;
begin
  //Immer 16VBt
  Bitmap.PixelFormat:=pf16Bit;

  if (hxf.get(Filename,aTemp)) then
    begin
      u32Index:=48;
      for u32y:=0 to Bitmap.Height -1 do
        begin
          pLine:=Bitmap.ScanLine[u32Y];
          for u32x:=0 to (Bitmap.Width * 2) - 1 do
            begin
              pLine^:=(aTemp.buffer[u32Index]);
              inc(pLine);
              inc(u32Index);
            end;
        end;
    end;

  //Buffer freigeben
  setlength(aTemp.buffer,0);
end;

procedure TfmMain.savescreen(Filename : longstring; Bitmapfile:Longstring);
var
  u32X     : unsigned32;
  u32Y     : unsigned32;
  u32Index : unsigned32;
  aTemp    : Thxfrecord;
  pLine    : ^Byte;
  Bitmap   : TBitmap;
begin
  if (hxf.get(Filename,aTemp)) then
    begin
      addlog('changing screen '+filename+' to '+bitmapfile); 

      //Bitmap laden und aufarbeiten
      Bitmap:=TBitmap.Create();
      Bitmap.LoadFromFile(Bitmapfile);

      if (Bitmap.Width <> 320) AND (Bitmap.Height <> 240) then
        begin
          bitmap_resample(Bitmap,320,240);
          addlog('resampling bitmap');
        end;
      bitmap_changecolordeepth(Bitmap,16);

      //In den Buffer schreiben
      u32Index:=48;
      for u32y:=0 to Bitmap.Height -1 do
        begin
          pLine:=Bitmap.ScanLine[u32Y];
          for u32x:=0 to (Bitmap.Width * 2)- 1 do
            begin
              aTemp.buffer[u32Index]  :=pline^;
              inc(pLine);
              inc(u32Index);
            end;
        end;

      //abspeichern
      if (hxf.put(aTemp)=TRUE) then
        begin
          addlog('done');
        end
      else
        begin
          addlog('failed');
        end;

      //Fertig
      Bitmap.Free();
    end
  else
    begin
      addlog(filename+' not found in hxf');
    end;

  //Buffer freigeben
  setlength(aTemp.buffer,0);
end;

/////////////////////////////////////////////////////////////////////////////////
// Languageviewer
/////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.tsLanguageShow(Sender: TObject);
var
  u32Index : unsigned32;
begin
  //Sprachen enumerieren
  cbLanguages.Clear();

  u32Index:=0;
  while (u32Index < unsigned32(length(LanguageData))) do
    begin
      if (hxf.exists(LanguageData[u32Index].filepath)=TRUE) then
        begin
          cbLanguages.items.add(LanguageData[u32Index].realname);
        end;
      inc(u32Index);
    end;

  //Und alles klarmachen
  if (cbLanguages.Items.count > 0) then
    begin
      cbLanguages.ItemIndex:=0;
      cbLanguagesChange(Self);
      gbLanguages.Enabled:=TRUE;
    end
  else
    begin
      gbLanguages.Enabled:=FALSE;
    end;
end;

procedure TfmMain.cbLanguagesChange(Sender: TObject);
var
  sLang    : longstring;
  u32Index : unsigned32;
begin
    if (cbLanguages.ItemIndex >=0) then
      begin
        sLang:=cbLanguages.Items[cbLanguages.ItemIndex];
      end;

    u32Index:=0;
    while(u32Index < unsigned32(length(LanguageData))) do
      begin
        if (sLang = LanguageData[u32Index].realname) then
          begin
            //Sprache laden
            enumlanguage(LanguageData[u32Index].filepath);
            edLanguage.Text:='';
            btSaveWordClick(Self);

            //Und abbrechen
            u32Index:=Length(LanguageData);
          end;
        inc(u32Index);
      end;
end;

procedure TfmMain.lbLanguageClick(Sender: TObject);
begin
  //Text in die Box �bernhemen, aber maximale L�nge begrenzen
  if (lbLanguage.ItemIndex >= 0) then
    begin
      edLanguage.Text := lbLanguage.Items[lbLanguage.ItemIndex];
    end;
end;

//Neuen Text abspeichern. Dabei wird direkt gespeichert und danach neu eingelesen
procedure TfmMain.btSaveWordClick(Sender: TObject);
var
  aTemp    : Thxfrecord;
  lng      : tlanguageencoder;
begin
  if (lbLanguage.ItemIndex >= 0) then
    begin
      lbLanguage.Items[lbLanguage.ItemIndex] := edLanguage.Text;
      addlog('replacing '+lbLanguage.Items[lbLanguage.ItemIndex]+' with '+edLanguage.Text);
    end;

  //Und abspeichern
  if (hxf.get(sLangfile,aTemp)=TRUE) then
    begin
      lng:=tlanguageencoder.Create();
      if (lng.pack(aTemp.buffer,TStringList(lbLanguage.items))=TRUE) then
        begin
          addlog('writing language data');
          hxf.put(aTemp);
        end
      else
        begin
          addlog('no more space to write language data');
        end;
      lbLanguageSize.Caption:='Bytes left : '+IntToStr(aTemp.size - lng.memneeded);
      lng.free();
    end
  else
    begin
      addlog('unable to write to hxf-file');
    end;
end;


//Alle Eintr�ge ablegen
procedure tfmMain.enumlanguage(filename:string);
var
  aTemp    : Thxfrecord;
  lng      : tlanguageencoder;
  strings  : TStringlist;
begin
  sLangfile:='?';

  if (hxf.get(filename,aTemp)=TRUE) then
    begin
      strings:=tStringlist.Create;
      lng:=tlanguageencoder.Create();
      lng.unpack(aTemp.buffer,strings);
      lng.free();

      lbLanguage.Clear();
      lbLanguage.Items.AddStrings(strings);
      strings.free();

      sLangfile:=filename;
    end;
end;

//Bei Return speichern und den n�chsten Eintrag holen
procedure TfmMain.edLanguageKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = chr(13)) then
    begin
      btSaveWordClick(Self);
      if (lbLanguage.Items.Count > lbLanguage.Itemindex - 1) then
        begin
          lbLanguage.Itemindex:=lbLanguage.Itemindex + 1;
          lbLanguageClick(Self);
        end;
    end;
end;

//Die Textlist abspeichern
procedure TfmMain.dumptofile1Click(Sender: TObject);
begin
  try
    lbLanguage.Items.SaveToFile('language_dump.txt');
  except
  end;
end;

/////////////////////////////////////////////////////////////////////////////////
/// Defaults
/////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.tsDefaultsShow(Sender: TObject);
var
  aTemp : Thxfrecord;
begin
  if (hxf.get('user_data\ccpmp.cfg',aTemp)=TRUE) then
    begin
      cbLanguage.ItemIndex:=aTemp.buffer[$2e];
      cbPoweroff.ItemIndex:=aTemp.buffer[$1b];
      cbBrightness.ItemIndex:=aTemp.buffer[$2f];
    end;
end;

procedure TfmMain.cbLanguageChange(Sender: TObject);
begin
  setconfig($2e,cbLanguage.ItemIndex);
end;

procedure TfmMain.cbPoweroffChange(Sender: TObject);
begin
  setconfig($1b,cbLanguage.ItemIndex);
end;

procedure TfmMain.cbBrightnessChange(Sender: TObject);
begin
  setconfig($2f,cbBrightness.ItemIndex);
end;

procedure TfmMain.cbThemeChange(Sender: TObject);
begin
//  setconfig($1b,cbLanguage.ItemIndex);
end;


procedure TfmMain.setconfig(offset:unsigned32; data : byte);
var
  aTemp : Thxfrecord;
begin
  if (hxf.get('user_data\ccpmp.cfg',aTemp)=TRUE) then
    begin
      aTemp.buffer[offset]:=data;
      hxf.put(aTemp)
    end;

  if (hxf.get('user_data\default.cfg',aTemp)=TRUE) then
    begin
      aTemp.buffer[offset]:=data;
      hxf.put(aTemp)
    end;
end;

/////////////////////////////////////////////////////////////////////////////////
/// Vorgegebene Patches
/////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.tsPatchesShow(Sender: TObject);
var
  u32Index    : unsigned32;
  u32FoundPos : unsigned32;
  u32Temp     : unsigned32;
  aTemp       : thxfrecord;
begin
  //Alles l�schen
  setlength(aLangPos,0);
  setlength(aLangID,0);
  cbAVLanguage1.Clear();
  cbAVLanguage2.Clear();
  cbAVLanguage3.Clear();

  //Firmware laden
  hxf.get('ccpmp.bin',aTemp);


  //Da die Sprachverkn�pfungen nicht immer am selben Platz liegen,
  //suchen wir sie
  u32Index:=0;
  while (u32Index < unsigned32(length(LanguageData))) do
    begin
      //Auf die Harte Tour die erste Sprache suche
      if (hexsearch(aTemp.buffer,LanguageData[u32Index].Configb,0,u32FoundPos)=TRUE) then
        begin
          //Array erweitern und Sprache/ID ablegen
          setlength(aLangPos,Length(aLangPos)+1);
          aLangPos[Length(aLangPos)-1]:=u32foundpos;

          setlength(aLangID,Length(aLangID)+1);
          aLangID[Length(aLangID)-1]:=LanguageData[u32Index].id;
        end;

      //Sprachbezeichner in die Komboboxen kopieren
      cbAVLanguage1.Items.Add(LanguageData[u32Index].realname);
      cbAVLanguage2.Items.Add(LanguageData[u32Index].realname);
      cbAVLanguage3.Items.Add(LanguageData[u32Index].realname);
      inc(u32Index);
    end;

  //Jetzt haben wir alle vergebenen Sprachen in aLanguages
  //Kurz sortieren
  u32Index:=0;
  while (u32Index < unsigned32(Length(aLangPos))) do
    begin
      u32FoundPos:=0;
      while (u32Foundpos < unsigned32(Length(aLangPos)-1)) do
        begin
          if (aLangPos[u32FoundPos] > aLangPos[u32FoundPos+1]) then
            begin
              u32Temp:=aLangPos[u32FoundPos+1];
              aLangPos[u32FoundPos+1]:=aLangPos[u32FoundPos];
              aLangPos[u32FoundPos]:=u32temp;

              u32Temp:=aLangID[u32FoundPos+1];
              aLangID[u32FoundPos+1]:=aLangID[u32FoundPos];
              aLangID[u32FoundPos]:=u32temp;
            end;
          inc(u32Foundpos);
        end;
      inc(u32Index);
    end;

  //Comboboxen zuordnen
  cbAVLanguage1.ItemIndex:=aLangID[0];
  cbAVLanguage2.ItemIndex:=aLangID[1];
  cbAVLanguage3.ItemIndex:=aLangID[2];

end;

procedure TfmMain.savelanguagesettings(langindex : unsigned32; languageid : unsigned32);
var
  aTemp : Thxfrecord;
  u32index : unsigned32;
begin
  //Neue Sprachkonfiguration schreibem
  if (hxf.get('ccpmp.bin',aTemp)=TRUE) then
    begin
      u32index:=0;
      while (u32index < sizeof(LanguageData[0].Configb)) do
        begin
          aTemp.buffer[aLangPos[langindex] + u32Index] := LanguageData[languageid].configb[u32Index];
          inc(u32index);
        end;
      hxf.put(aTemp);
      addlog('changing language to '+LanguageData[languageid].realname);
    end;
end;


procedure TfmMain.cbAvLanguage1Change(Sender: TObject);
begin
  if (cbAVLanguage1.ItemIndex >= 0) then
    begin
      savelanguagesettings(0,cbAVLanguage1.ItemIndex);
    end;
end;

procedure TfmMain.cbAvLanguage2Change(Sender: TObject);
begin
  if (cbAVLanguage2.ItemIndex >= 0) then
    begin
      savelanguagesettings(1,cbAVLanguage2.ItemIndex);
    end;
end;

procedure TfmMain.cbAvLanguage3Change(Sender: TObject);
begin
  if (cbAVLanguage3.ItemIndex >= 0) then
    begin
      savelanguagesettings(2,cbAVLanguage3.ItemIndex);
    end;
end;


procedure TfmMain.btDirNamesToLowerClick(Sender: TObject);
begin
  sar('ccpmp.bin','A:\GAME'   ,'a:\game');
  sar('ccpmp.bin','A:\FLASH'  ,'a:\flash');
  sar('ccpmp.bin','A:\MUSIC'  ,'a:\music');
  sar('ccpmp.bin','A:\PICTURE','a:\picture');
  sar('ccpmp.bin','A:\RECORD' ,'a:\record');
  sar('ccpmp.bin','A:\TXT'    ,'a:\txt');
  sar('ccpmp.bin','A:\VIDEO'  ,'a:\video');

  sar('ccpmp.bin','a:\GAME'   ,'a:\game');
  sar('ccpmp.bin','a:\FLASH'  ,'a:\flash');
  sar('ccpmp.bin','a:\MUSIC'  ,'a:\music');
  sar('ccpmp.bin','a:\PICTURE','a:\picture');
  sar('ccpmp.bin','a:\RECORD' ,'a:\record');
  sar('ccpmp.bin','a:\TXT'    ,'a:\txt');
  sar('ccpmp.bin','a:\VIDEO'  ,'a:\video');

  sar('ccpmp.bin','A:\game'   ,'a:\game');
  sar('ccpmp.bin','A:\flash'  ,'a:\flash');
  sar('ccpmp.bin','A:\music'  ,'a:\music');
  sar('ccpmp.bin','A:\picture','a:\picture');
  sar('ccpmp.bin','A:\record' ,'a:\record');
  sar('ccpmp.bin','A:\txt'    ,'a:\txt');
  sar('ccpmp.bin','A:\video'  ,'a:\video');
end;

procedure TfmMain.btDisabelAutoRunProtectionClick(Sender: TObject);
begin
  sar('ccpmp.bin','a:\autorun.txt'  ,'a:\autorin.inf');
  sar('ccpmp.bin','b:\autorun.txt'  ,'b:\autorin.inf');
end;

procedure TfmMain.btThemepatchClick(Sender: TObject);
var
  aTemp   : thxfrecord;
  stemp   : longstring;
  u32pos1 : unsigned32;
  u32pos2 : unsigned32;
  u32pos3 : unsigned32;
begin
  sar('ccpmp.bin','z:\system\res\clock.bmp'    ,'a:\system\res\clock.bmp');
  sar('ccpmp.bin','z:\system\res\warning.bmp'  ,'a:\system\res\warning.bmp');
  sar('ccpmp.bin','z:\system\res\didian_1.bmp' ,'a:\system\res\didian_1.bmp');
  sar('ccpmp.bin','z:\system\res\didian_2.bmp' ,'a:\system\res\didian_2.bmp');
  sar('ccpmp.bin','z:\system\res\didian_3.bmp' ,'a:\system\res\didian_3.bmp');
  sar('ccpmp.bin','z:\system\res\didian_4.bmp' ,'a:\system\res\didian_4.bmp');
  sar('ccpmp.bin','z:\system\res\guanji_1.bmp' ,'a:\system\res\guanji_1.bmp');
  sar('ccpmp.bin','z:\system\res\guanji_2.bmp' ,'a:\system\res\guanji_2.bmp');
  sar('ccpmp.bin','z:\system\res\guanji_3.bmp' ,'a:\system\res\guanji_3.bmp');
  sar('ccpmp.bin','z:\system\res\guanji_4.bmp' ,'a:\system\res\guanji_4.bmp');
  sar('ccpmp.bin','z:\system\res\demo.tar'     ,'a:\system\res\demo.tar');

  sar('ccpmp.bin','z:\system\font\'+#00        ,'a:\system\font\'+#00);
  sar('ccpmp.bin','z:\system\res\'+#00         ,'a:\system\res\'+#00);

  sTemp:=string_append(extractfilepath(hxf.hxffile),'\');
  sTemp:=sTemp + 'hxf_dump\';
  //Dateien exportieren
  if (hxf.getfirst(aTemp)=TRUE) then
    begin
      repeat
        //Buffer schreiben wenn es die passenden Dateien sind
        u32pos1:=pos('system\res\' ,aTemp.filename);
        u32pos2:=pos('system\font\',aTemp.filename);
        u32pos3:=pos('emulator'    ,aTemp.filename);


        if (( u32pos1 > 0) and (u32pos1 <= 2)) or
           (( u32pos2 > 0) and (u32pos2 <= 2)) or
           (( u32pos3 > 0) and (u32pos3 <= 2)) then
           begin
            hextofile(sTemp + aTemp.filename,addr(aTemp.buffer[0]),aTemp.size);
           end;
      until hxf.getnext(aTemp)=FALSE;
    end;

  messagebox(0,'files exported to hxf_dump. after the firmware update copy the contents of hxf_dump into the rootdir of your dingoo.','info',MB_OK); 
end;

procedure TfmMain.sar(filename : longstring; search : longstring; replace : longstring);
var
  aSearch  : array of byte;
  aReplace : array of Byte;
begin
  setlength(aSearch,length(search));
  move(search[1],aSearch[0],length(search));

  setlength(aReplace,length(Replace));
  move(replace[1],aReplace[0],length(replace));

  sar(filename,aSearch,aReplace);
end;

procedure TfmMain.sar(filename : longstring; search : array of byte; replace : array of byte);
var
  aTemp     : thxfrecord;
begin
  screen.Cursor:=crHourglass;

  //Datei holen und L�ngenfeler abfangen
  if (hxf.get(filename,aTemp)=TRUE) then
    begin
      searchandreplace(aTemp.buffer,search,replace);

      //Und abspeichern
      hxf.put(aTemp);
    end;
  screen.Cursor:=crDefault;
end;

/////////////////////////////////////////////////////////////////////////////////
/// Logfunktionen
/////////////////////////////////////////////////////////////////////////////////
procedure tfmMain.addlog(txt : longstring);
begin
  lbLog.ItemIndex:=lbLog.Items.Add(txt);
  application.processmessages();
end;

//ein kleine Signatur in die Firmware schreiben
procedure tfmmain.addsignature();
begin
  sar('ccpmp.bin','VERDOR ID','SO9 v0.6 ');
end;


procedure TfmMain.About1Click(Sender: TObject);
begin
  fmAbout.ShowModal();
end;


end.

