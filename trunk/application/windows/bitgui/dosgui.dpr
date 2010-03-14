program dosgui;

uses
  Forms,
  unit_main in 'unit_main.pas' {fmMain},
  unit_failure in 'unit_failure.pas' {fmFailure};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bitdefender GUI';
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmFailure, fmFailure);
  Application.Run;
end.
