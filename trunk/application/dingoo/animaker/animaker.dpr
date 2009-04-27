program animaker;

uses
  Forms,
  unit_main in 'unit_main.pas' {Form1},
  unit_fileformat in 'unit_fileformat.pas',
  unit_about in 'unit_about.pas' {fmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'animaker';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.Run;
end.
