unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_helper,unit_filefunctions,unit_stringfunctions,unit_strings;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    lbLog: TListBox;
    GroupBox2: TGroupBox;
    edsource: TEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edtarget: TEdit;
    Button2: TButton;
    GroupBox3: TGroupBox;
    Button3: TButton;
    cblowercase: TCheckBox;
    cbtest: TCheckBox;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  Form1: TForm1;

implementation
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  edsource.Text:=browsedialog('source path');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  edtarget.Text:=browsedialog('target path');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  clearlog();

  bTest:=cbtest.checked;
  bBusy:=TRUE;

  addlog('syncing');
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
end;


procedure TForm1.dosync(source : longstring; target : longstring);
var
  status : signed32;
  search : TSearchRec;
begin
  source:=IncludeTrailingPathDelimiter(source);
  target:=IncludeTrailingPathDelimiter(target);

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
procedure TForm1.prepare_dirs (source:longstring; target:longstring);
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

procedure TForm1.prepare_files(source:longstring; target:longstring);
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

procedure TForm1.addlog(text : longstring);
begin
  lbLog.ItemIndex:=lbLog.Items.Add(text);
  while(lbLog.Count > 250000) do lbLog.Items.Delete(0);
end;

procedure TForm1.clearlog();
begin
  lblog.clear();
end;




end.
