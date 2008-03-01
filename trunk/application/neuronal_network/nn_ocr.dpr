program nn_ocr;

uses
  Forms,
  ocr_main in 'ocr_main.pas' {FM_Main};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFM_Main, FM_Main);
  Application.Run;
end.
