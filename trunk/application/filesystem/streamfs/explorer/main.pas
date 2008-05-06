unit main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,class_streamfs,
  Grids, StdCtrls, DragDrop, DropTarget, DragDropFile,ShellApi, Menus;

type
  TfmMain = class(TForm)
    GroupBox1: TGroupBox;
    sgFiles: TStringGrid;
    dIncoming: TDropFileTarget;
    GroupBox2: TGroupBox;
    btOpen: TButton;
    btExport: TButton;
    btDelete: TButton;
    GroupBox3: TGroupBox;
    btChkDsk: TButton;
    btDefrag: TButton;
    btFormat: TButton;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    mmFileInfo: TMemo;
    mmFS: TMemo;
    dgSave: TSaveDialog;
    GroupBox6: TGroupBox;
    rbPlain: TRadioButton;
    rbCrypt: TRadioButton;                                                                               
    sgTitle: TStringGrid;
    pmFiles: TPopupMenu;
    Open1: TMenuItem;
    Export1: TMenuItem;
    Delte1: TMenuItem;
    DeleteAll1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure UpdateFileList();
    procedure FormActivate(Sender: TObject);
    procedure dIncomingDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);
    function getactivefilename():longstring;
    procedure sgFilesClick(Sender: TObject);

    procedure openstreamfile();
    procedure deletestreamfile();
    procedure exportstreamfile();
    procedure defragstream();
    procedure checkstream();
    procedure formatstream();

    procedure showinfo(text:longstring);
    procedure btOpenClick(Sender: TObject);
    procedure btExportClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btChkDskClick(Sender: TObject);
    procedure btDefragClick(Sender: TObject);
    procedure btFormatClick(Sender: TObject);

    procedure ClearGrid();
    procedure rbPlainClick(Sender: TObject);
    procedure rbCryptClick(Sender: TObject);
    function  createtempfilename():Longstring;

    procedure StartBusy(Title:longstring);
    procedure StopBusy();
    procedure DeleteAll1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain   : TfmMain;
  StreamFS : TStreamFS;
  bLoaded  : Boolean = FALSE;
  bBusy    : Boolean = FALSE;

implementation

uses progress;

{$R *.DFM}

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.FormCreate(Sender: TObject);
begin
     StreamFS:=TStreamFS.Create();

     //Ein paar Einstellungen
     StreamFS.crypt:=CRYPT_XOR;
     StreamFS.cryptheader:=TRUE;
     StreamFS.password:=extractfilename(ParamStr(0));
     StreamFS.streamtype:=STREAM_FILE;

     Caption:=StreamFS.PassWord;

     //FS öffnen
     StreamFS.Open(ParamStr(0)+'.dat');

     if (StreamFS.Error <> FSDB_ERROR_NONE) then
        begin
             ShowInfo('error in fsdb '+IntToStr(StreamFS.Error)); 
        end;
     //Alle Schreibzugriffe blocken wenn wir Readonly sind
     btDelete.Enabled:=StreamFS.Writable;
     btDefrag.Enabled:=StreamFS.Writable;
     btFormat.Enabled:=StreamFS.Writable;

     btOpen.Enabled:=StreamFS.Readable;
     btExport.Enabled:=StreamFS.Readable;
     btChkdsk.Enabled:=StreamFS.Readable;

     pmFiles.Autopopup:=StreamFS.Writable;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.FormActivate(Sender: TObject);
begin
     if (not bLoaded) then
        begin
             bLoaded:=TRUE;
             Self.UpdateFileList();
             StreamFS.OnProgress:=fmProgress.doprogress;
        end;

     case (StreamFS.crypt) of
               CRYPT_NONE : rbPlain.Checked:=TRUE;
               CRYPT_XOR  : rbCrypt.Checked:=TRUE;
          end;
end;

procedure TfmMain.UpdateFileList();
var
   slFiles : TStringList;
   fsEntry : TFSEntry;
   u32Row  : unsigned32;
   u64Size : unsigned64;
   u32Temp : unsigned32;
   sTemp   : longstring;
begin
     u32Temp:=sgFiles.Row;

     slFiles:=StreamFS.EnumFiles();

     sTemp:='';
     if (StreamFS.readable) then sTemp:=sTemp+'r';
     if (StreamFS.writable) then sTemp:=sTemp+'w';

     //Bei der Gelegenheit setzen wir die FileSystemInfo
     mmFS.Text:=Format('%s'+#13+#10+
                       '--------------------------------'+#13+#10+
                       'Mode       : %s'+#13+#10+
                       'Streamtype : %u'+#13+#10+
                       'Files      : %u'+#13+#10+
                       'StreamSize : %u Byte'+#13+#10,
                       [ExtractFileName(StreamFS.Streamname),sTemp,StreamFS.StreamType,slFiles.Count,StreamFS.StreamSize]);



     //Alles löschen
     Self.ClearGrid();
     sgFiles.RowCount:=0;

     //Spaltenbeschriftung
     sgTitle.Cells[0,0]:='Filename';
     sgTitle.Cells[1,0]:='Size';
     sgTitle.Cells[2,0]:='Adler32';
     sgTitle.Cells[3,0]:='Crypt';

     u32Row:=0;
     u64Size:=0;
     sgFiles.DoubleBuffered:=TRUE;

     while (slFiles.Count > 0) do
           begin
                fsEntry:=StreamFS.info(slFiles[0]);
                if (StreamFS.Error = FSDB_ERROR_NONE) then
                   begin
                        sgFiles.Cells[0,u32Row]:=fsEntry.aFilename;
                        sgFiles.Cells[1,u32Row]:=IntToStr(fsEntry.u64FileSize);
                        sgFiles.Cells[2,u32Row]:=IntToHex(fsEntry.u32Hash,8);
                        sgFiles.Cells[3,u32Row]:=IntToStr(fsEntry.u32Method);

                        sgFiles.RowCount:=sgFiles.RowCount + 1;
                        inc(u64Size,fsEntry.u64FileSize);
                        inc(u32Row);
                   end;
                slFiles.Delete(0);
           end;

     mmFS.Text:=mmFS.Text+'Filesize   : '+IntToStr(u64Size)+' Byte';


     sgFiles.RowCount:=u32Row;

     if (sgFiles.RowCount > 0) AND (u32Temp > 0) then
        begin
             if (u32Temp >= unsigned32(sgFiles.RowCount) ) then
                begin
                     u32Temp:=sgFiles.RowCount-1;
                end;
             sgFiles.Row:=u32Temp;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.dIncomingDrop(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
   sTemp : longstring;
begin
     if (StreamFS.writable) then
        begin
             StartBusy('importing');
             while (dIncoming.Files.Count > 0) do
                   begin
                        sTemp:=ExtractFilename(dIncoming.Files[0]);
                        fmProgress.Caption:='importing '+sTemp;
                        StreamFS.Store(dIncoming.Files[0],sTemp);
                        dIncoming.Files.Delete(0);
                   end;
             StopBusy();
        end
     else
        begin
             ShowInfo('filesystem is readonly');
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TfmMain.getactivefilename():longstring;
begin
     result:='';
     if (sgFiles.Row >= 0) then
        begin
             result:=sgFiles.Cells[0,sgFiles.Row];
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.sgFilesClick(Sender: TObject);
var
   sTemp   : longstring;
   fsEntry : TFSEntry;
begin
     sTemp:=Self.GetActiveFileName();
     if (sTemp <> '') then
        begin
             fsEntry:=StreamFS.Info(sTemp);
             if (StreamFS.Error = FSDB_ERROR_NONE) then
                begin
                     mmFileInfo.Text:=Format('%s '+#13+#10+
                                             '--------------------------------'+#13+#10+
                                             'Size    : %u Byte'+#13+#10+
                                             'Adler32 : %x'+#13+#10+
                                             'Crypt   : %u'
                                             ,[fsEntry.aFilename,fsEntry.u64FileSize,fsEntry.u32Hash,fsEntry.u32Method]);
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.openstreamfile();
var
   sTemp : longstring;
begin
     if (not bBusy) then
        begin
             sTemp:=CreateTempFileName() + GetActiveFilename();

             StartBusy('open');

             StreamFS.Restore(GetActiveFilename(),sTemp);

             StopBusy();

             ShellExecute(0,'open',PChar(sTemp),'',PChar(ExtractFilePath(sTemp)),SW_SHOW);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.deletestreamfile();
begin
     if (not bBusy) then
        begin
             StartBusy('delete');
             StreamFS.Delete(Self.GetActiveFilename());
             StopBusy();
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.exportstreamfile();
begin
     if (not bBusy) then
        begin
             with dgSave do
                  begin
                       Filename:=GetActiveFilename();
                       if (Execute) then
                          begin
                               StartBusy('export');
                               StreamFS.Restore(GetActiveFilename(),Filename);
                               StopBusy();
                          end;
                  end;
          end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.defragstream();
begin
     if (not bBusy) then
        begin
             StartBusy('defrag');
             if StreamFS.Defrag() then
                begin
                     StopBusy();
                     ShowInfo('defrag done');
                end
             else
                begin
                     StopBusy();
                     ShowInfo('defrag failed');
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.checkstream();
var
   slFiles : TStringList;
   sTemp   : Longstring;
begin
     if (not bBusy) then
        begin
             StartBusy('checking files');
             slFiles:=StreamFS.EnumFiles();
             sTemp:='';
             while (slFiles.Count > 0) do
                   begin
                        if (StreamFS.check(slFiles[0])<>TRUE) then
                           begin
                                sTemp:=sTemp + slFiles[0] + #13+#10;
                           end;
                        slFiles.Delete(0);
                   end;
             StopBusy();

             if (sTemp <> '') then
                begin
                     ShowInfo('corrupt files :'+#13+#10+sTemp);
                end
             else
                begin
                     ShowInfo('filesystem looks fine');
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.formatstream();
begin
     if (not bBusy) then
        begin
             StartBusy('format');
             StreamFS.Format();
             StopBusy();
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.showinfo(text:longstring);
begin
     MessageBox(fmMain.Handle,PChar(Text),'message',MB_OK);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btOpenClick(Sender: TObject);
begin
     openstreamfile();
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btExportClick(Sender: TObject);
begin
     exportstreamfile();
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btDeleteClick(Sender: TObject);
begin
     deletestreamfile();
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btChkDskClick(Sender: TObject);
begin
     checkstream();
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btDefragClick(Sender: TObject);
begin
     defragstream();
end;

////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btFormatClick(Sender: TObject);
begin
     formatstream();
end;

procedure TfmMain.ClearGrid();
var
  i,j:integer;
begin
  for i := sgFiles.FixedCols to sgFiles.ColCount - 1 do
  begin
    for j := sgFiles.FixedRows to sgFiles.RowCount - 1 do
    begin
      sgFiles.Cells[i,j] := '';
    end;
  end;
end;

procedure TfmMain.rbPlainClick(Sender: TObject);
begin
     StreamFS.crypt:=CRYPT_NONE;
end;

procedure TfmMain.rbCryptClick(Sender: TObject);
begin
     StreamFS.crypt:=CRYPT_XOR;
end;

function TfmMain.createtempfilename():Longstring;
var
   aTemp : array[0..MAX_PATH] of Char;
begin
     if (GetTempPath(MAX_PATH,aTemp) > 0) then
        begin
             if (GetTempFileName(aTemp,'sf_',0,aTemp) > 0) then
                begin
                     result:=trim(string(aTemp));
                end;
        end;
end;

procedure TfmMain.StartBusy(Title: Longstring);
begin
     fmProgress.Caption:=Title;
     fmProgress.lbInfo.Caption:='';
     fmProgress.Show();
     bBusy:=TRUE;
end;

procedure TfmMain.StopBusy();
begin
     fmProgress.lbInfo.Caption:='working';
     fmProgress.Hide();
     Self.UpdateFileList();
     bBusy:=FALSE;
end;


procedure TfmMain.DeleteAll1Click(Sender: TObject);
var
   slFiles  : TStringList;
   u32Count : unsigned32;
   u32Max   : unsigned32;
begin
     slFiles:=StreamFS.EnumFiles();

     u32Count:=0;
     u32Max:=slFiles.Count;

     StartBusy('delete all');
     while (slFiles.Count > 0) do
           begin
                fmProgress.doprogress( (u32Count * 100) div u32Max,'delete '+slFiles[0]);

                StreamFS.Delete(slFiles[0]);
                slFiles.Delete(0);
                inc(u32Count);
           end;
     StopBusy();


     slFiles.Free();
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
     StreamFS.Free();
     CanClose:=TRUE;
end;

end.
