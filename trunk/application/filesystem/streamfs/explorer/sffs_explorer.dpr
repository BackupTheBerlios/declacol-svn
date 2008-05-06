program sffs_explorer;

uses
  Forms,
  main in 'main.pas' {fmMain},
  progress in 'progress.pas' {fmProgress};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmProgress, fmProgress);
  Application.Run;
end.
