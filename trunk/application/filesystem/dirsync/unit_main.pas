unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_helper,unit_filefunctions,unit_stringfunctions,unit_strings,unit_log,
  ExtCtrls,class_ini;

type
  TfmMain = class(TForm)
    gblog: TGroupBox;
    lbLog: TListBox;
    gbpath: TGroupBox;
    edsource: TEdit;
    btbrowsesource: TButton;
    lbsource: TLabel;
    lbtarget: TLabel;
    edtarget: TEdit;
    btbrowsetarget: TButton;
    gboptions: TGroupBox;
    btstart: TButton;
    cblowercase: TCheckBox;
    cbtest: TCheckBox;
    btstop: TButton;
    cbunfragged: TCheckBox;
    cbcrc: TCheckBox;
    Timer1: TTimer;
    cbverbose: TCheckBox;
    cbautoclose: TCheckBox;
    procedure btstartClick(Sender: TObject);
    procedure btbrowsesourceClick(Sender: TObject);
    procedure btbrowsetargetClick(Sender: TObject);
    procedure btstopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
    btest  : boolean;
    bBusy  : boolean;
    bLoaded: boolean;
    procedure clearlog();
    procedure addlog(text : longstring);

    procedure prepare_dirs (source:longstring; target:longstring);
    procedure prepare_files(source:longstring; target:longstring);

    procedure dosync(source : longstring; target : longstring);
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation
{$R *.dfm}

procedure TfmMain.btbrowsesourceClick(Sender: TObject);
begin
  edsource.Text:=browsedialog('source path');
end;

procedure TfmMain.btbrowsetargetClick(Sender: TObject);
begin
  edtarget.Text:=browsedialog('target path');
end;

procedure TfmMain.btstartClick(Sender: TObject);
begin
  btStart.Visible:=FALSE;
  clearlog();

  bTest:=cbtest.checked;
  bBusy:=TRUE;

  addlog('syncing '+edsource.Text + ' to ' +edtarget.text);
  if (DirectoryExists(edsource.text)=TRUE) AND (DirectoryExists(edtarget.Text)=TRUE) then
    begin
      dosync(edsource.text,edtarget.Text);
    end
  else
    begin
      addlog('wrong path');
    end;
  addlog('done');

  bBusy:=FALSE;
  btStart.Visible:=TRUE;

  //autostop?
  if (cbautoclose.checked=TRUE) then
    begin
      self.close();
    end;

end;

procedure TfmMain.btstopClick(Sender: TObject);
begin
  bBusy:=FALSE;
end;


procedure TfmMain.dosync(source : longstring; target : longstring);
var
  status : signed32;
  search : TSearchRec;
begin
  source:=IncludeTrailingPathDelimiter(source);
  target:=IncludeTrailingPathDelimiter(target);

  if (cbverbose.checked=TRUE) then addlog('processing '+source);
  
  if (cblowercase.checked=TRUE) then
    begin
      source:=lowercase(source);
      target:=lowercase(target);
    end;

  prepare_dirs(source,target);
  prepare_files(source,target);

  //quellverzeichnisse durchgehen
  status:=FindFirst(source+'*.*',faANYFILE,search);
  while (status=0) AND (bBusy=TRUE)  do
    begin
      //verzeichnis ?
      if ( (search.attr AND faDirectory) <> 0 ) AND
           (search.name <> '.') AND
           (search.name <> '..') then
        begin
          Application.ProcessMessages();
          //recursion
          dosync(source+search.Name,target+search.Name);
        end;
      status:=FindNext(search);
    end;
  FindClose(search);
end;




//Alle �berfl�ssigen Verzeichnisse entfernen
procedure TfmMain.prepare_dirs (source:longstring; target:longstring);
var
  slsource : TStringlist;
  sltarget : TStringlist;

  index    : unsigned32;
  found    : signed32;
begin
  source:=IncludeTrailingPathDelimiter(source);
  target:=IncludeTrailingPathDelimiter(target);

  if (cblowercase.checked=TRUE) then
    begin
      source:=lowercase(source);
      target:=lowercase(target);
    end;

  //verzeichnisse vergleichen
  slsource:=DirScan(source,'*.*','',0);
  sltarget:=DirScan(target,'*.*','',0);

  //aus allen treffern den absoluten pfad l�schen
  index:=0;
  while (index < unsigned32(slsource.Count)) do
    begin
      slsource[index]:=string_replace(slsource[index],source,'');
      if (cblowercase.checked=TRUE) then
        begin
          slsource[index]:=lowercase(slsource[index]);
        end;
      inc(index);
    end;

  index:=0;
  while (index < unsigned32(sltarget.Count)) do
    begin
      sltarget[index]:=string_replace(sltarget[index],target,'');
      if (cblowercase.checked=TRUE) then
        begin
          sltarget[index]:=lowercase(sltarget[index]);
        end;
      inc(index);
    end;

  slsource.sort();
  sltarget.sort();

  //alle nicht in source vorhandenen verzeichnisse entfernen
  index:=0;
  while (index < unsigned32(slsource.count)) do
    begin
      found:=sltarget.IndexOf(slsource[index]);
      if (found<>-1) then
        begin
          slsource[index]:='';
          sltarget.delete(found);
        end;
      inc(index);
    end;

  //was noch in target ist kann gel�scht werden
  while (sltarget.count > 0) do
    begin
      addlog('clean ' + target + sltarget[0]);
      if (not bTest) then deltree(target + sltarget[0]);
      sltarget.delete(0);
    end;

  sltarget.free();
  slsource.free();
end;

procedure TfmMain.prepare_files(source:longstring; target:longstring);
var
  slsource : TStringlist;
  sltarget : TStringlist;

  index    : unsigned32;
  found    : signed32;

  status   : boolean;
begin
  source:=IncludeTrailingPathDelimiter(source);
  target:=IncludeTrailingPathDelimiter(target);

  if (cblowercase.checked=TRUE) then
    begin
      source:=lowercase(source);
      target:=lowercase(target);
    end;

  //Dateien vergleichen
  slsource:=FileScan(source,'*.*','',0);
  sltarget:=FileScan(target,'*.*','',0);

  //aus allen treffern den absoluten pfad l�schen
  index:=0;
  while (index < unsigned32(slsource.Count)) do
    begin
      slsource[index]:=string_replace(slsource[index],source,'');
      if (cblowercase.checked=TRUE) then
        begin
          slsource[index]:=lowercase(slsource[index]);
        end;
      inc(index);
    end;

  index:=0;
  while (index < unsigned32(sltarget.Count)) do
    begin
      sltarget[index]:=string_replace(sltarget[index],target,'');
      if (cblowercase.checked=TRUE) then
        begin
          sltarget[index]:=lowercase(sltarget[index]);
        end;
      inc(index);
    end;

  slsource.sort();
  sltarget.sort();

  //alle nicht in source vorhandenen dateien entfernen
  index:=0;
  while (index < unsigned32(slsource.count)) do
    begin
      Application.ProcessMessages();

      found:=sltarget.IndexOf(slsource[index]);
      if (found<>-1) then
        begin
          if (cbverbose.Checked=TRUE) then addlog('check '+source+slsource[index]);

          //sind die dateien gleich eintrag entfernen
          if (cbcrc.Checked=TRUE) then
            begin
              status:=compare_files_paranoid(source + slsource[index],target + sltarget[found])
            end
          else
            begin
              status:=compare_files_normal(source + slsource[index],target + sltarget[found])
            end;

          if (status = TRUE) then
            begin
              slsource[index]:='';
              sltarget.delete(found);
            end;
        end;
      inc(index);
    end;

  //was noch in target ist kann gel�scht werden
  while (sltarget.count > 0) do
    begin
      addlog('clean ' + target + sltarget[0]);
      if (not bTest) then deldata(target + sltarget[0]);
      sltarget.delete(0);
    end;

  while (slsource.Count > 0) do
    begin
      if (slsource[0] <> '') then
        begin
          if (not bTest) then
            begin
              status:=sync_files(source + slsource[0], target + slsource[0],cbunfragged.checked)
            end
          else
            begin
              status:=TRUE;
            end;

          if (status=TRUE) then
            begin
              addlog('sync ' + source + slsource[0]);
            end
          else
            begin
              addlog('sync error ' + source + slsource[0]);
            end;
        end;
        slsource.delete(0);
    end;

  sltarget.free();
  slsource.free();
end;

procedure TfmMain.addlog(text : longstring);
begin
  lbLog.ItemIndex:=lbLog.Items.Add(text);
  Log_Add(paramstr(0)+'.log',text);
  while(lbLog.Count > 250000) do lbLog.Items.Delete(0);
end;

procedure TfmMain.clearlog();
begin
  lblog.clear();
end;

procedure TfmMain.Timer1Timer(Sender: TObject);
begin
  if (bBusy) then
    begin
      Application.ProcessMessages();
    end;
end;

procedure TfmMain.FormActivate(Sender: TObject);
var
  ini   : TIni;
  stemp : longstring;
  btemp : boolean;
begin
  if (bLoaded = FALSE) then
    begin
      bLoaded:=TRUE;

      ini := TIni.Create();
      ini.LoadFromFile(paramstr(0)+'.ini');

      ini.Read('options','source','',stemp);
      edsource.Text:=stemp;

      ini.Read('options','target','',stemp);
      edtarget.Text:=stemp;

      ini.Read('options','crccheck' ,FALSE ,btemp);
      cbcrc.checked:=btemp;

      ini.Read('options','lowercase',TRUE  ,btemp);
      cblowercase.checked:=btemp;

      ini.Read('options','testrun'  ,FALSE ,btemp);
      cbtest.checked:=btemp;

      ini.Read('options','unfragged',FALSE ,btemp);
      cbunfragged.checked:=btemp;

      ini.Read('options','verbose'  ,TRUE  ,btemp);
      cbverbose.checked:=btemp;

      ini.Read('options','autoclose',FALSE  ,btemp);
      cbautoclose.checked:=btemp;

      ini.Free();

      //autorun?
      if (paramstr(1)='run') then
        begin
          btstartclick(self);
        end;
    end;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ini   : TIni;
begin
  ini := TIni.Create();
  ini.Write('options','source',edsource.Text);
  ini.Write('options','target',edtarget.Text);

  ini.Write('options','crccheck' ,cbcrc.checked);
  ini.Write('options','lowercase',cblowercase.checked);
  ini.Write('options','testrun'  ,cbtest.checked);
  ini.Write('options','unfragged',cbunfragged.checked);
  ini.Write('options','verbose'  ,cbverbose.checked);
  ini.Write('options','autoclose',cbautoclose.checked);

  ini.SaveToFile(paramstr(0)+'.ini');
  ini.Free();
end;

end.
