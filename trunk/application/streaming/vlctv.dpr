program vlctv;

uses
  Forms,
  unit_main in 'unit_main.pas' {Form1},
  unit_edit in 'unit_edit.pas' {fmEditChannel};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfmEditChannel, fmEditChannel);
  Application.Run;
end.
