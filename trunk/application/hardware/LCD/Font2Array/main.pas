unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, TFlatButtonUnit, TFlatGroupBoxUnit,
  TFlatTitlebarUnit, TFlatPanelUnit, TFlatEditUnit, TFlatSpinEditUnit,
  Unit_LogFunctions;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    FlatPanel1: TFlatPanel;
    FlatGroupBox2: TFlatGroupBox;
    FlatButton1: TFlatButton;
    FlatButton2: TFlatButton;
    FlatGroupBox1: TFlatGroupBox;
    IM_Test: TImage;
    IM_Char: TImage;
    Title: TFlatTitlebar;
    FlatButton3: TFlatButton;
    FlatGroupBox3: TFlatGroupBox;
    SE_Width: TFlatSpinEditInteger;
    SE_Height: TFlatSpinEditInteger;
    SE_Cols: TFlatSpinEditInteger;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    FlatButton4: TFlatButton;
    FlatButton5: TFlatButton;
    FlatButton6: TFlatButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FlatButton3Click(Sender: TObject);
    procedure SE_WidthChange(Sender: TObject);
    procedure FlatButton4Click(Sender: TObject);
    procedure FlatButton5Click(Sender: TObject);
    procedure FlatButton6Click(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure SaveAsArray(Filename:String);
    procedure WriteText(XPos,YPos:Integer;Text:String);
    procedure WriteChar(Bitmap:TBitmap;Xpos,YPos:Integer;Sign:Char;Back,Front:TColor);
    procedure ShowTest();
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
   iIndex : Integer;
   iXPos  : Integer;
   iYPos  : Integer;
   iCountX: integer;
   iCountY: integer;
   sLine  : String;
   bByte  : Byte;
begin
     //Titel schreiben
     AddToLog(Filename,'//Font2Pas (Borg@Sven-of-Nine)//',FALSE);
     AddToLog(Filename,'////////////////////////////////',FALSE);
     AddToLog(Filename,Format('aFont : array[0..256 * %d - 1] of Byte =(',[SE_Height.Value]),FALSE);

     //Alle Zeichen verarbeiten
     for iIndex:=0 to 255 do
         begin
              //Spalte und Zeile bestimmen
              iXPos:=(iIndex mod SE_Cols.Value);
              iYPos:=(iIndex div SE_Cols.Value);

              //Koordinaten bestimmen
              iXPos:=iXPos * SE_Width.Value;
              iYPos:=iYPos * SE_Height.Value;

              //Zeile initialisieren
              sLine:='';
              for iCountY:=0 to SE_Height.Value-1 do
              //Und Bytes bilden
                  begin
                       bByte:=0;
                       for iCountX:=0 to SE_Width.Value-1 do
                           begin
                                if (iXPos+iCountX < IM_Char.Picture.Bitmap.Width) and
                                   (iYPos+iCountY < IM_CHar.Picture.Bitmap.Height) then
                                   begin
                                        //Pixel nicht Wei?
                                        if (IM_Char.Picture.Bitmap.Canvas.Pixels[iXPos+iCountx,iYPos+iCounty]<>clWhite) then
                                           begin
                                                bByte:=(bByte shl 1) or 1;
                                           end
                                        else
                                            begin
                                                bByte:=(bByte shl 1) or 0;
                                            end;
                                   end;
                           end;
                       //Eine Zeile komplett, dann speichern
                       sLine:=sLine+IntToStr(bByte)+',';
                  end;
              //Zeichen komplett, dann in Datei schreiben
              //Wenn wir beim letzten Zeichen sind, dann letztes Komme rausnehmen
              if (iIndex=255) then
                 begin
                      sLine:=Copy(sLine,1,Length(sLine)-1);
                 end;
              AddToLog(Filename,#09+#09+#09+sLine,FALSE);
         end;
     AddToLog(Filename,');',FALSE);
end;

procedure TForm1.WriteChar(Bitmap:TBitmap;Xpos,YPos:Integer;Sign:Char;Back,Front:TColor);
var
   iXPos   : integer;
   iYPos   : integer;
   iCountX : integer;
   iCountY : integer;
begin
     //Position des Zeichens im Bitmap bestimmen

     //Spalte und Zeile bestimmen
     iXPos:=(Ord(Sign) mod SE_Cols.Value);
     iYPos:=(Ord(Sign) div SE_Cols.Value);


     //Koordinaten bestimmen
     iXPos:=iXPos * SE_Width.Value;
     iYPos:=iYPos * SE_Height.Value;

     //Und einzeichnen
     for iCountX:=0 to SE_Width.Value-1 do
         begin
              for iCountY:=0 to SE_Height.Value-1 do
                 begin
                      if (iXPos+iCountX < IM_Char.Picture.Bitmap.Width) and
                         (iYPos+iCountY < IM_Char.Picture.Bitmap.Height) then
                         begin
                              if (IM_Char.Picture.Bitmap.Canvas.Pixels[iXPos+iCountx,iYPos+iCounty]<>clWhite) then
                                 begin
                                      Bitmap.Canvas.Pixels[XPos+iCountX,YPos+iCountY]:=Back;
                                 end
                              else
                                 begin
                                      Bitmap.Canvas.Pixels[XPos+iCountX,YPos+iCountY]:=Front;
                                 end;
                         end;

                 end;
         end;
end;

procedure TForm1.WriteText(XPos,YPos:Integer;Text:String);
var
   iIndex : integer;
   iXPos  : integer;
   iYPos  : integer;
begin
     iXPos:=XPos;
     iYPos:=YPos;
     for iIndex:=1 to Length (Text)do
         begin
              WriteChar(IM_Test.Picture.Bitmap,iXPos,iYPos,Text[iIndex],clBack,clFront);
              inc(iXPos,SE_Width.Value+1);
         end;
end;

procedure TForm1.ShowTest();
var
   BMP : TBitmap;
begin
     //Passende Bitmap erzeugen
     BMP:=TBitmap.Create;
     BMP.Width :=(SE_Width.Value+1)*26;
     BMP.Height:=(SE_Height.Value+1)*8;
     BMP.PixelFormat:=pf8Bit;
     BMP.Canvas.Brush.Color:=clFront;
     BMP.Canvas.FillRect(BMP.Canvas.ClipRect);
     IM_Test.Picture.Assign(BMP);
     BMP.Free;

     WriteText(0,0                ,'abcdefghijklmnopqrstuvwxyz');
     WriteText(0,(SE_Height.Value+1)*1,'ABCDEFGHIJKLMNOPQRSTUVWXYZ');
     WriteText(0,(SE_Height.Value+1)*2,'0123456789');
     WriteText(0,(SE_Height.Value+1)*3,'!"^$%&/()=?#*+-,.;:');
     WriteText(0,(SE_Height.Value+1)*4,#160+#161+#162+#163);
     IM_Test.Invalidate;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
     With (OpenDialog1) do
          begin
               if (Execute) then
                  begin
                       IM_Char.Picture.LoadFromFile(Filename);
                       ShowTest;
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

procedure TForm1.SE_WidthChange(Sender: TObject);
begin
     ShowTest();
end;

procedure TForm1.FlatButton4Click(Sender: TObject);
var
   iXPosL: Integer;
   iYPosL: Integer;
   iCount: Integer;
begin
     //Alle Großbuchstaben über die Kleinbuchstaben kopieren
     for iCount:=Ord('A') to Ord('Z') do
         begin
              //Spalte und Zeile bestimmen
              iXPosL:=( (iCount + ( Ord('a') - Ord('A') ) ) mod SE_Cols.Value);
              iYPosL:=( (iCount + ( Ord('a') - Ord('A') ) ) div SE_Cols.Value);


              //Koordinaten bestimmen (Des Kleinbuchstabens)
              iXPosL:=iXPosL * SE_Width.Value;
              iYPosL:=iYPosL * SE_Height.Value;

              //Und rumkopieren
              WriteChar(IM_Char.Picture.Bitmap,iXPosL,iYPosl,Chr(iCount),clBlack,clWhite);
         end;
     ShowTest();
end;

procedure TForm1.FlatButton5Click(Sender: TObject);
var
   iXPosL: Integer;
   iYPosL: Integer;
   iCount: Integer;
begin
     //Alle Großbuchstaben über die Kleinbuchstaben kopieren
     for iCount:=Ord('a') to Ord('z') do
         begin
              //Spalte und Zeile bestimmen
              iXPosL:=( (iCount - ( Ord('a') - Ord('A') ) ) mod SE_Cols.Value);
              iYPosL:=( (iCount - ( Ord('a') - Ord('A') ) ) div SE_Cols.Value);


              //Koordinaten bestimmen (Des Kleinbuchstabens)
              iXPosL:=iXPosL * SE_Width.Value;
              iYPosL:=iYPosL * SE_Height.Value;

              //Und rumkopieren
              WriteChar(IM_Char.Picture.Bitmap,iXPosL,iYPosl,Chr(iCount),clBlack,clWhite);
         end;
     ShowTest();
end;

procedure TForm1.FlatButton6Click(Sender: TObject);
begin
     with SaveDialog1 do
          begin
               Filter:='BMP|*.bmp';
               if (Execute) then
                  begin
                       IM_Char.Picture.SaveToFile(Filename);
                  end;
          end;
end;

end.
