unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, LogBox, BrowseDialog, CheckLst, unit_typedefs,
  unit_filefunctions,class_checksum,unit_strings,unit_time;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    Scanner: TTabSheet;
    Remover: TTabSheet;
    btScan: TButton;
    GroupBox1: TGroupBox;
    edFolder: TEdit;
    btFolder: TButton;
    lbFolder: TLabel;
    lbFilter: TLabel;
    edFilter: TEdit;
    GroupBox2: TGroupBox;
    rbHyperspeed: TRadioButton;
    rbFast: TRadioButton;
    rbSlow: TRadioButton;
    rbParanoid: TRadioButton;
    GroupBox3: TGroupBox;
    dgBrowse: TBrowseDialog;
    lbLog: TLogBox;
    lbFound: TCheckListBox;
    pbChecksum: TProgressBar;
    procedure btFolderClick(Sender: TObject);
    procedure btScanClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private-Deklarationen }
    function GetChecksum(filename : longstring):longstring;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  bLoaded : Boolean = FALSE;
  u32Mode : unsigned32;

const
     CHECK_SIZE      = 1;
     CHECK_ADLER     = 2;
     CHECK_MD5       = 4;
     CHECK_PARANOID  = CHECK_ADLER or CHECK_MD5;

implementation

{$R *.DFM}

procedure TForm1.FormActivate(Sender: TObject);
begin
     if (not bLoaded) then
        begin
             edFolder.Text:=GetCurrentDir();
             bLoaded:=TRUE;
        end;
end;


procedure TForm1.btFolderClick(Sender: TObject);
begin
     with (dgBrowse) do
          begin
               if (execute = TRUE) then
                  begin
                       edFolder.Text:=Folder;
                  end;
          end;
end;

procedure TForm1.btScanClick(Sender: TObject);
var
   slFiles  : TStringlist;
   slFilter : TStringlist;
   slCRC    : TStringList;
   u32Index : unsigned32;
   u32Found : unsigned32;
   u32double: unsigned32;
   ETA      : TRuntime; 
begin
     lbFound.Clear();

     //Prüfmodus setzen
     if (rbHyperspeed.Checked) then u32Mode:=CHECK_SIZE;
     if (rbFast.Checked)       then u32Mode:=CHECK_ADLER;
     if (rbSlow.Checked)       then u32Mode:=CHECK_MD5;
     if (rbParanoid.Checked)   then u32Mode:=CHECK_PARANOID;

     //Die Dateifilter einzeln verarbeiten
     slFilter:=TSTringlist.Create();
     String_Explode(edFilter.Text,';',slFilter,TRUE);

     //Jeden Filter einzeln scannen und die Treffer zufügen
     slFiles:=TStringlist.Create();
     while (slFilter.Count > 0) do
           begin
                slFiles.Addstrings(FileScan(edFolder.Text,slFilter[0],'',8192));
                slFilter.Delete(0);
           end;

     lbLog.AddLog(Format('%d file(s) found',[slFiles.Count]));

     //Für jede Datei erzeugen wir nun die Prüfsumme
     if (slFiles.Count > 0) then
        begin
             ETA:=TRuntime.Create();
             lbLog.AddLog('calculating checksums');
             pbChecksum.Max:=slFiles.Count - 1;

             slCRC:=TStringList.Create();

             ETA.Start(slFiles.Count - 1);
             for u32Index:=0 to slFiles.Count - 1 do
                 begin
                      ETA.Progress(1);

                      pbChecksum.Position:=u32Index;
                      Application.ProcessMessages();
                      slCRC.Add(Self.GetCheckSum(slFiles[u32Index]));

                      Caption:=ETA.Left_Str;
                 end;
             ETA.Stop();
             lbLog.AddLog('done in '+ETA.Run_str);

             lbLog.AddLog('scanning for clones');
             //Jetzt haben wir zwei Listen
             //eine mit den Dateinamen und eine mit den Prüfsummen.
             //Nun gehen wir einfach alle Prüfsummen durch
             ETA.Start(slCRC.Count - 1);
             u32double:=0;
             for u32Index:=0 to slCRC.Count - 1 do
                 begin
                      ETA.Progress(1);
                      pbChecksum.Position:=u32Index;

                      for u32Found:=u32Index + 1 to slCRC.Count - 1 do
                          begin
                               if (slCRC[u32Index][1] = slCRC[u32Found][1]) then
                                  begin
                                       if ( slCRC[u32Index] = slCRC[u32Found] ) then
                                          begin
                                               lbFound.Items.Add(slCRC[u32Index] + ' : ' + slFiles[u32Index] +' => ' + slFiles[u32Found]);
                                               inc(u32double);
                                          end;
                                  end;
                          end;
                      Application.ProcessMessages();
                      Caption:=ETA.Left_Str;
                 end;
             lbFound.Sorted:=TRUE;

             lbLog.AddLog(format('%d clone(s) found',[u32double]));
             lbLog.AddLog('done');
             ETA.Free();
             slCRC.Free();
        end
     else
        begin
             lbLog.AddLog('nothing to do');
        end;

     slFiles.Free();
     slFilter.Free();
end;


//Für jeden erwünschten Check die Prüfsumme an das Ergebnis hängen
function TForm1.GetChecksum(filename : longstring): longstring;
var
   adler32 : TAdler32;
   md5     : TMD5;
begin
     result:='';

     if (u32Mode AND CHECK_SIZE > 0) then
        begin
             result:=result + inttostr(getfilesizeex(filename)); 
        end;

     if (u32Mode AND CHECK_ADLER > 0) then
        begin
            adler32:=TAdler32.Create();
            result:=result + adler32.fromfile(filename).sChecksum;
            adler32.Free();
        end;

     if (u32Mode AND CHECK_MD5 > 0) then
        begin
            md5:=TMD5.Create();
            result:=result + md5.fromfile(filename).sChecksum;
            md5.Free();
        end;
end;


end.
