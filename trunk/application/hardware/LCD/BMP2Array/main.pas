unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, TFlatButtonUnit, TFlatGroupBoxUnit,
  TFlatTitlebarUnit, TFlatPanelUnit, TFlatEditUnit, TFlatSpinEditUnit,
  Unit_LogFunctions,Unit_Grafix;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    FlatPanel1: TFlatPanel;
    FlatGroupBox2: TFlatGroupBox;
    FlatButton1: TFlatButton;
    FlatButton2: TFlatButton;
    FlatGroupBox1: TFlatGroupBox;
    IM_Char: TImage;
    Title: TFlatTitlebar;
    FlatButton3: TFlatButton;
    FlatGroupBox3: TFlatGroupBox;
    SE_NewWidth: TFlatSpinEditInteger;
    SE_NewHeight: TFlatSpinEditInteger;
    FlatButton4: TFlatButton;
    Label1: TLabel;
    Label2: TLabel;
    SE_Level: TFlatSpinEditInteger;
    Label3: TLabel;
    FlatButton5: TFlatButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FlatButton3Click(Sender: TObject);
    procedure FlatButton4Click(Sender: TObject);
    procedure FlatButton5Click(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure SaveAsArray(Filename:String);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  clFront    : TColor=$0040d040;
  clBack     : TColor=$00000000;

implementation

{$R *.DFM}
procedure TForm1.SaveAsArray(Filename:String);
var
   iCountX: integer;
   iCountY: integer;
   sLine  : String;
begin
     //Titel schreiben
     AddToLog(Filename,'//BMP2Pas (Borg@Sven-of-Nine)//',FALSE);
     AddToLog(Filename,'////////////////////////////////',FALSE);
     AddToLog(Filename,Format('aFont : array[0..%d - 1] of Byte =(',[IM_Char.Picture.Bitmap.Width*IM_Char.Picture.Bitmap.Height]),FALSE);

     //Zeile initialisieren
     sLine:='';

     //Und Bytes bilden
     for iCountY:=0 to IM_Char.Picture.Bitmap.Height-1 do
         begin
              sLine:='';
              for iCountX:=0 to IM_Char.Picture.Bitmap.Width-1 do
                  begin
                       if (IM_Char.Picture.Bitmap.Canvas.Pixels[iCountX,iCountY]<>clBlack) then
                          begin
                               //Weiﬂ
                               sLine:=sLine+'0,';
                          end
                       else
                          begin
                               //Schwarz
                               sLine:=sLine+'1,';
                          end;
                  end;
              //Letztes Komma rausnehmen
              if (iCountY=IM_Char.Picture.Bitmap.Height-1) then
                 begin
                      sLine:=Copy(sLine,1,Length(sLine)-1);
                 end;
              //Eine Zeile komplett, dann speichern
              AddToLog(Filename,sLine,FALSE);
         end;
     AddToLog(Filename,');',FALSE);
end;




procedure TForm1.Button1Click(Sender: TObject);
begin
     With (OpenDialog1) do
          begin
               if (Execute) then
                  begin
                       IM_Char.Picture.LoadFromFile(Filename);
                  end;
          end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     with (SaveDialog1) do
          begin
               Filter:='Text|*.txt';
               if (Execute) then
                  begin
                       DeleteFile(Filename);
                       SaveAsArray(Filename);
                  end;
          end;
end;

procedure TForm1.FlatButton3Click(Sender: TObject);
begin
     Self.Close;
end;

procedure TForm1.FlatButton4Click(Sender: TObject);
begin
     with (IM_Char.Picture) do
          begin
               if (Bitmap.Width <> SE_NewWidth.Value) or
                  (Bitmap.Height<> SE_NewHeight.Value) then
                  begin
                       ResampleBitmap(Bitmap,SE_NEWWidth.Value,SE_NewHeight.Value);

                       //Schwarz/Weiﬂ machen
                       BinaryScale(Bitmap,SE_Level.Value);

                  end;
          end;
     IM_CHar.Invalidate;
end;

procedure TForm1.FlatButton5Click(Sender: TObject);
begin
     Invert(IM_Char.Picture.Bitmap);
     IM_CHar.Invalidate;
end;

end.
