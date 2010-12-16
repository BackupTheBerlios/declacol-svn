program usbchecker;

uses
  Forms,
  main in 'main.pas' {fmMain},
  iotask in 'iotask.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
