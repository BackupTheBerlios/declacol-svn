program proclist;

uses
  Forms,
  main in 'main.pas' {FM_Main};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'procex (c) 2005 Borg@Sven-of-Nine.de';
  Application.CreateForm(TFM_Main, FM_Main);
  Application.Run;
end.
