program DiskDump;

uses
  Forms,
  DDMain in 'DDMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DiskDumper';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
