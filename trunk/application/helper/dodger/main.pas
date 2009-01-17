unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,ShellAPI,unit_password;

type
  TMyOwnDodger = class(TForm)
    Button1: TButton;
    edEMail: TEdit;
    cbClipboard: TCheckBox;
    cbBrowser: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  MyOwnDodger: TMyOwnDodger;

implementation

{$R *.dfm}

procedure TMyOwnDodger.Button1Click(Sender: TObject);
var
  sURL   : string;
  sEMail : string;
begin
  sEMail:=Pass_CreateSyntheticString(14,5);
  edEMail.Text:=sEMail + '@dodgit.com';

  if (cbClipboard.Checked) then
    begin
      edEMail.SelectAll();
      edEMail.CopyToClipboard();
    end;

  if (cbBrowser.Checked) then
    begin
      sURL:='http://dodgit.com/run/checkmail?mailbox='+sEMail;
      ShellExecute(self.WindowHandle,'open',PChar(sURL),nil,nil, SW_SHOWNORMAL);
    end;
end;

end.
