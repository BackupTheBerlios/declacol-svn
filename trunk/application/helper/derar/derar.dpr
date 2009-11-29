program derar;

uses
  Forms,
  unit_main in 'unit_main.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'RAR-Unpacker';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
