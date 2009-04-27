program animaker;

uses
  Forms,
  unit_main in 'unit_main.pas' {Form1},
  unit_fileformat in 'unit_fileformat.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
