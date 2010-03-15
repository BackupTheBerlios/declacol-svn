program dosgui;

uses
  Forms,
  unit_main in 'unit_main.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Bitdefender GUI';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
