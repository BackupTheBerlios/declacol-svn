program DiskDump;

uses
  Forms,
  ddmain in 'ddmain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DiskDumper';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
