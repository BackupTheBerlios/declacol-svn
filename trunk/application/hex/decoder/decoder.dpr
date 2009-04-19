program decoder;

uses
  Forms,
  unit_main in 'unit_main.pas' {Form1},
  unit_xor in 'unit_xor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Decoder';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
