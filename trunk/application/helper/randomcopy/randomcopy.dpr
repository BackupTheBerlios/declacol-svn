program randomcopy;

uses
  Forms,
  unit_main in 'unit_main.pas' {Form1},
  class_runtime in '..\..\..\classes\class_runtime.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
