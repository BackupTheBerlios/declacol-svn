program cleanhdd;

uses
  Forms,
  main in 'main.pas' {Form1},
  progress in 'progress.pas' {FM_PausenClown};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TempKill (c) 2005 Borg@Sven-of-Nine.de';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFM_PausenClown, FM_PausenClown);
  Application.Run;
end.
