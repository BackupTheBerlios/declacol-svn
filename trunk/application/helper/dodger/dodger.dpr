program dodger;

uses
  Forms,
  main in 'main.pas' {MyOwnDodger};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Dodger';
  Application.CreateForm(TMyOwnDodger, MyOwnDodger);
  Application.Run;
end.
