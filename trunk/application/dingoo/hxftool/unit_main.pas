unit unit_main;

interface

uses
  unit_typedefs,unit_strings,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_filefunctions,unit_filesystem,class_hxf, ComCtrls,
  Grids, Menus, ExtCtrls, ExtDlgs,unit_grafix,class_language, Mask;

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
    GroupBox1: TGroupBox;
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
    procedure mOpenClick(Sender: TObject);
    procedure mCloseClick(Sender: TObject);
    procedure lockfunctions();
    procedure unlockfunctions();
    procedure setconfig(offset:unsigned32; data : byte);
    procedure sar(filename : longstring; search : array of byte; replace : array of byte);



    procedure lbFilesClick(Sender: TObject);


    procedure loadscreen(Filename : longstring; Bitmap:TBitmap);
    procedure savescreen(Filename : longstring; BitmapFile:longstring);
    procedure btImportLogoffClick(Sender: TObject);
    procedure btImportLogonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tsLogonShow(Sender: TObject);
    procedure tsLogoffShow(Sender: TObject);

    procedure enumlanguage();
    procedure tsLanguageShow(Sender: TObject);
    procedure lbLanguageClick(Sender: TObject);
    procedure btSaveWordClick(Sender: TObject);
    procedure tsDefaultsShow(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure cbPoweroffChange(Sender: TObject);
    procedure cbThemeChange(Sender: TObject);
    procedure btDirNamesToLowerClick(Sender: TObject);
    procedure About1Click(Sender: TObject);

  private
    hxf     : Thxfreader;
    hxfdata : thxfrecord;

    { Private-Deklarationen }
    procedure addlog(txt : longstring);
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;
  bHXF : boolean = FALSE;

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
/// Menüfunktionen
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
  hxf.free();
  addlog('hxf closed');
  lockfunctions();
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

  //Die einzelnen Tabs je nach Funktionalität aktivieren
  tsFiles.TabVisible       := hxf.count > 0;
  tsPatches.TabVisible     := hxf.exists('ccpmp.bin');
  tsBootscreens.TabVisible := hxf.exists('user_data\logon.ani') and
                              hxf.exists('user_data\logoff.ani');
  tsLanguage.TabVisible    := hxf.exists('system\nls\Ó¢ÎÄ.dlx');
  tsDefaults.TabVisible    := hxf.exists('user_data\ccpmp.cfg') and
                              hxf.exists('user_data\default.cfg');
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
begin
  enumlanguage();
  edLanguage.Text:='';
end;

procedure TfmMain.lbLanguageClick(Sender: TObject);
begin
  //Text in die Box übernhemen, aber maximale Länge begrenzen
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
  if (hxf.get(LNG_ENGLISH,aTemp)=TRUE) then
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

//Alle Einträge ablegen
procedure tfmMain.enumlanguage();
var
  aTemp    : Thxfrecord;
  lng      : tlanguageencoder;
  strings  : TStringlist;
begin
  if (hxf.get(LNG_ENGLISH,aTemp)=TRUE) then
    begin
      strings:=tStringlist.Create;
      lng:=tlanguageencoder.Create();
      lng.unpack(aTemp.buffer,strings);
      lng.free();

      lbLanguage.Clear();
      lbLanguage.Items.AddStrings(strings);
      strings.free();
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
procedure TfmMain.btDirNamesToLowerClick(Sender: TObject);
begin
// sar('ccpmp.bin','A:\MUSIC','a:\music');
end;

procedure TfmMain.sar(filename : longstring; search : array of byte; replace : array of byte);
var
  aTemp    : thxfrecord;
  u32Hay   : unsigned32;
  u32Needle: unsigned32;
  u32Found : unsigned32;
begin
  if (hxf.get(filename,aTemp)=TRUE) then
    begin
      u32Hay:=0;
      //Das ganze Heu durchsuchen
      while (u32Hay < aTemp.size) do
        begin
          u32Needle:=0;
          //Vergleichen
          if (aTemp.buffer[u32Hay] = search[u32Needle]) then
            begin
              //Anfang gefunden
              u32Found:=u32Hay;

              //Stimmt auch der Rest?
              while (aTemp.buffer[u32Hay] = search[u32Needle]) and
                    (u32Needle < unsigned32(length(search))) and
                    (u32Hay    < aTemp.size) do
                    begin
                      inc(u32Hay);
                      inc(u32Needle);
                    end;

              //Was gefunden?
              if (u32Needle = unsigned32(Length(Search))) then
                begin

                end;
            end;
          inc(u32Hay);
          inc(u32Needle);
        end;
      //Speichern
      hxf.put(aTemp);
    end;
end;

/////////////////////////////////////////////////////////////////////////////////
/// Logfunktionen
/////////////////////////////////////////////////////////////////////////////////
procedure tfmMain.addlog(txt : longstring);
begin
  lbLog.ItemIndex:=lbLog.Items.Add(txt);
  application.processmessages();
end;














procedure TfmMain.About1Click(Sender: TObject);
begin
  fmAbout.ShowModal();
end;

end.

