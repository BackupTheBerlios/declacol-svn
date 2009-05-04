program hxftool;

uses
  Forms,
  unit_main in 'unit_main.pas' {fmMain},
  class_hxf in 'class_hxf.pas',
  class_language in 'class_language.pas',
  unit_patches in 'unit_patches.pas',
  unit_about in 'unit_about.pas' {fmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.Run;
end.
