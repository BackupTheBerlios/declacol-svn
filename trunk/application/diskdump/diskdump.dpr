program DiskDump;

uses
  Forms,
  DDMain in 'DDMain.pas' {fmMain},
  class_ramdisk in 'class_ramdisk.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DiskDumper';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
