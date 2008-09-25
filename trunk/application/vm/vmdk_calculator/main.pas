unit main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,unit_log,unit_filefunctions,unit_hddhelper,unit_strings;

type
  TForm1 = class(TForm)
    btGo: TButton;
    Memo1: TMemo;
    btExit: TButton;
    dgOpen: TOpenDialog;
    procedure btGoClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.btGoClick(Sender: TObject);
var
   sInput         : Longstring;
   sOutput        : Longstring;
   u64FileSize    : unsigned64;
   u32SectorSize  : unsigned32;
   u32SectorCount : unsigned32;
   u32HeadCount   : unsigned32;
   u64CylCount    : unsigned64;
begin
     with (dgOpen) do
          begin
               if (execute()=TRUE) then
                  begin
                       //Datei evtl. konform umbenennen
                       if (pos ('-flat.vmdk',Filename)=0) then
                          begin
                               sInput :=ChangeFileExt(Filename,'-flat.vmdk');
                               sOutput:=ChangeFileExt(Filename,'.vmdk');
                               renamefile(filename,sinput);
                          end
                       else
                          begin
                               sInput:=Filename;
                               sOutput:=string_replace(Filename,'-flat.vmdk','.vmdk');
                          end;  

                       //Daten berechnen
                       u32SectorSize:=512;
                       u64FileSize:=GetFileSizeEx(sInput);

                       GetCHSParameter(u64FileSize,u64CylCount,u32HeadCount,u32SectorCount,u32SectorSize);

                       Log_Clear(sOutput);
                       randomize();
                       Log_Add(sOutput,'#Disk Descriptorfile / Borg@Sven-of-Nine.de',FALSE);
                       Log_Add(sOutput,'version=1',FALSE);
                       Log_Add(sOutput,Format('CID=%s',[IntToHex(random(High(Integer)),8)]),FALSE);
                       Log_Add(sOutput,'parentCID=ffffffff',FALSE);
                       Log_Add(sOutput,'createType="monolithicFlat"',FALSE);

                       Log_Add(sOutput,'# Extent description',FALSE);
                       Log_Add(sOutput,Format('RW %d FLAT "%s" 0',[u64FileSize div u32SectorSize,ExtractFilename(sInput)]),FALSE);

                       Log_Add(sOutput,'# The Disk Data Base',FALSE);
                       Log_Add(sOutput,'#DDB',FALSE);

                       Log_Add(sOutput,'ddb.virtualHWVersion = "4"',FALSE);
                       Log_Add(sOutput,Format('ddb.geometry.cylinders = "%d"',[u64CylCount]),FALSE);
                       Log_Add(sOutput,Format('ddb.geometry.heads = "%d"',[u32HeadCount]),FALSE);
                       Log_Add(sOutput,Format('ddb.geometry.sectors = "%d"',[u32SectorCount]),FALSE);
                       Log_Add(sOutput,'ddb.adapterType = "ide"',FALSE);

                       MessageBox(Self.Handle,'Done','info',MB_OK);
                  end;
          end;

end;

procedure TForm1.btExitClick(Sender: TObject);
begin
     Self.Close();
end;

end.
