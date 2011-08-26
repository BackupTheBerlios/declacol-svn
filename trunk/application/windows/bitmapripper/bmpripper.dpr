program bmpripper;

uses
  Forms,
  unit_main in 'unit_main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
