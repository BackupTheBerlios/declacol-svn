program dirsync;

uses
  Forms,
  unit_main in 'unit_main.pas' {fmMain},
  unit_helper in 'unit_helper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DirSync';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
