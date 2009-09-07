unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Dialogs,
  StdCtrls, EditBtn,md5;

type

  { TForm1 }

  TForm1 = class(TForm)
    FileNameEdit1: TFileNameEdit;
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }


procedure TForm1.FileNameEdit1AcceptFileName(Sender: TObject; var Value: String
  );
var
   md5 : string;
begin
     md5:=lowercase(MD5Print(MD5File(utf8tosys(value))));
     application.MessageBox(pchar(md5),'MD5',0);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
   sTemp : String;
begin
  if (ParamStr(1)<>'') AND (FileExists(ParamStr(1))=TRUE) then
     begin
          sTemp:=ParamStr(1);
          FileNameEdit1AcceptFileName(Self,sTemp);
          Application.Terminate();
     end;
end;

initialization
  {$I unit_main.lrs}

end.

