program FileJoin;

uses
  Forms,
  unit_main in 'unit_main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'FileJoin';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
