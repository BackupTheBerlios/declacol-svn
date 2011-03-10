program usbchecker;

uses
  Forms,
  main in 'main.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DeviceChekcer';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
