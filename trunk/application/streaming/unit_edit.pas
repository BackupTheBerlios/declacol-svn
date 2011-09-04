unit unit_edit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_processfunctions;

type
  TfmEditChannel = class(TForm)
    edChannel: TEdit;
    edURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btOK: TButton;
    btTest: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btTestClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    sChannel : string;
    sURL     : string;
    sVLC     : string;
    sCMD     : string;
    bChanged : boolean;
  end;

var
  fmEditChannel: TfmEditChannel;

implementation

{$R *.dfm}

procedure TfmEditChannel.FormActivate(Sender: TObject);
begin
  edChannel.Text:=sChannel;
  edURL.Text:=sURL;
end;

procedure TfmEditChannel.btOKClick(Sender: TObject);
begin
  bChanged:=TRUE;
  self.close();
end;

procedure TfmEditChannel.btTestClick(Sender: TObject);
begin
  application.Minimize();
  execute(sVLC,sCMD + ' ' + edURL.Text,TRUE);
  application.Restore();
end;

procedure TfmEditChannel.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (bChanged=FALSE) then
    begin
      sURL:='?';
    end
  else
    begin
      sChannel:=UpperCase(edChannel.Text);
      sURL:=edURL.Text;
    end;
end;

end.
