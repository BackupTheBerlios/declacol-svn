unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_helper,unit_filefunctions,unit_stringfunctions,unit_strings,unit_log;

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
    procedure btstartClick(Sender: TObject);
    procedure btbrowsesourceClick(Sender: TObject);
    procedure btbrowsetargetClick(Sender: TObject);
    procedure btstopClick(Sender: TObject);
  private
    { Private-Deklarationen }
    btest  : boolean;
    bBusy  : boolean;
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

  addlog('processing '+source);
  
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




//Alle überflüssigen Verzeichnisse entfernen
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

  //aus allen treffern den absoluten pfad löschen
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

  //was noch in target ist kann gelöscht werden
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

  //aus allen treffern den absoluten pfad löschen
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
      found:=sltarget.IndexOf(slsource[index]);
      if (found<>-1) then
        begin
          //sind die dateien gleich eintrag entfernen
          if (compare_files_normal(source + slsource[index],target + sltarget[found]) = TRUE) then
            begin
              slsource[index]:='';
              sltarget.delete(found);
            end;
        end;
      inc(index);
    end;

  //was noch in target ist kann gelöscht werden
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
              status:=sync_files(source + slsource[0], target + slsource[0])
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





end.
