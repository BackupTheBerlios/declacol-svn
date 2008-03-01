unit ocr_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,Unit_NeuralNet, Spin, ImgList;

type
  TFM_Main = class(TForm)
    GB_Eingabe: TGroupBox;
    IM_1: TImage;
    IM_2: TImage;
    IM_4: TImage;
    IM_3: TImage;
    IM_5: TImage;
    IM_6: TImage;
    IM_7: TImage;
    IM_9: TImage;
    IM_8: TImage;
    GroupBox2: TGroupBox;
    LB_Result: TLabel;
    GroupBox3: TGroupBox;
    ListBox1: TListBox;
    Button1: TButton;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    ImageList: TImageList;
    Button2: TButton;
    Button3: TButton;
    IM_Out: TImage;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Label2: TLabel;
    Button7: TButton;
    Button8: TButton;
    procedure FormCreate(Sender: TObject);
    procedure IM_1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure SendBMPToNet(BMP:TBitmap;bUpdate:Boolean=TRUE);
    procedure SetImages();
  public
    { Public-Deklarationen }
  end;

var
  FM_Main : TFM_Main;
  Neuro : TNeuralNetWork;
  Output: array[0..8] of String=('A','Fragezeichen','Hut','I','Invader','Mann','Smiley','Tasse','X'); 

implementation

{$R *.DFM}

procedure TFM_Main.FormCreate(Sender: TObject);
begin
     //64 Eingaben weil 8x8 Bit pro Icon
     // 9 Ausgaben, weil neun Icons
     Neuro:=TNeuralNetwork.Create(64,64,32,9);
     SetImages;
end;

procedure TFM_Main.SendBMPToNet(BMP:TBitmap;bUpdate:Boolean);
var
   iIndex1 : integer;
   iIndex2 : integer;
   iPos    : integer;
   sFound  : single;
begin
     //Die Pixel des Bitmaps auf die Eingabeknoten des Netzwerkes legen
     iPos:=0;
     for iIndex1:=0 to 7 do
         begin
              for iIndex2:=0 to 7 do
                  begin
                       Neuro.Input[iPos]:=BMP.Canvas.Pixels[iIndex1,iIndex2];
                       inc(iPos);
                  end;
         end;
     //Das Netzwerk ausrechnen
     Neuro.CalcNetwork;

     //Und die Ausgabe checken
     //Der höchste Wert ist unser Ergebnis
     sFound:=-255;
     iPos  :=0;
     for iIndex1:=0 to 8 do
         begin
              if (Neuro.Output[iIndex1]>sFound) then
                 begin
                      sFound:=Neuro.Output[iIndex1];
                      iPos:=iIndex1;
                 end;
         end;

     if (bUpdate) then
        begin
             //Und den Text ausgeben
             LB_Result.Caption:=Output[iPos];

             //Grafik ausgeben
             IM_Out.Picture.Assign(BMP);
        end;
end;

procedure TFM_Main.SetImages();
var
   BMP : TBitmap;
begin
     //Die Bitmaps den Buttons zuordnen
     BMP:=TBitmap.Create;
     ImageList.GetBitmap(0,BMP); IM_1.Picture.Assign(BMP);
     BMP.Free;

     BMP:=TBitmap.Create;
     ImageList.GetBitmap(1,BMP); IM_2.Picture.Assign(BMP);
     BMP.Free;

     BMP:=TBitmap.Create;
     ImageList.GetBitmap(2,BMP); IM_3.Picture.Assign(BMP);
     BMP.Free;

     BMP:=TBitmap.Create;
     ImageList.GetBitmap(3,BMP); IM_4.Picture.Assign(BMP);
     BMP.Free;

     BMP:=TBitmap.Create;
     ImageList.GetBitmap(4,BMP); IM_5.Picture.Assign(BMP);
     BMP.Free;

     BMP:=TBitmap.Create;
     ImageList.GetBitmap(5,BMP); IM_6.Picture.Assign(BMP);
     BMP.Free;

     BMP:=TBitmap.Create;
     ImageList.GetBitmap(6,BMP); IM_7.Picture.Assign(BMP);
     BMP.Free;

     BMP:=TBitmap.Create;
     ImageList.GetBitmap(7,BMP); IM_8.Picture.Assign(BMP);
     BMP.Free;

     BMP:=TBitmap.Create;
     ImageList.GetBitmap(8,BMP); IM_9.Picture.Assign(BMP);
     BMP.Free;
end;

procedure TFM_Main.IM_1Click(Sender: TObject);
begin
     with Sender as TImage do
          begin
               SendBMPToNet(Picture.Bitmap);
          end;
end;

procedure TFM_Main.Button1Click(Sender: TObject);
var
   iIndex1: integer;
   iIndex2: integer;
   iComp  : integer;
   BMP    : TBitmap;
   sError : Double;
begin
     Screen.Cursor:=crHourGlass;
     sError:=0;
     iIndex1:=0;
     while (iIndex1 < SpinEdit1.Value) do
         begin
              //Eine Zufällige Grafik auswählen
              iComp:=Random(ImageList.Count);
              BMP:=TBitmap.Create;
              ImageList.GetBitmap(iComp,BMP);

              //An das Netz schicken
              SendBMPToNet(BMP,FALSE);
              BMP.Free;

              //Und ausrechnen
              Neuro.CalcNetwork;

              //Richtige Ausgabe setzen
              for iIndex2:=0 to 8 do
                  begin
                       Neuro.OutputWish[iIndex2]:=0;
                  end;
              //Und den gewünschten setzen
              Neuro.OutputWish[iComp]:=1;

              //Fehler berechnen
              Neuro.CalcError;
              sError:=sError+Neuro.TeachError;
              //Lernen
              Neuro.TeachNetwork;

              inc(iIndex1);
         end;
     //Fehler ausgeben
     ListBox1.ItemIndex:=ListBox1.Items.Add(FloatToStr(sError / SpinEdit1.Value));

     Screen.Cursor:=crDefault;
end;

procedure TFM_Main.Button3Click(Sender: TObject);
begin
     SetImages;
end;

procedure TFM_Main.Button2Click(Sender: TObject);
procedure Noise(BMP:TBitmap);
var
   iX : integer;
   iY : integer;
begin
     for iX:=0 to 7 do
         begin
              for iY:=0 to 7 do
                  begin
                       if (random(100)>95) then
                          begin
                               BMP.Canvas.Pixels[iX,iY]:=clBlack;
                          end;
                  end;
         end;
end;
begin
     Noise(IM_1.Picture.Bitmap);
     Noise(IM_2.Picture.Bitmap);
     Noise(IM_3.Picture.Bitmap);
     Noise(IM_4.Picture.Bitmap);
     Noise(IM_5.Picture.Bitmap);
     Noise(IM_6.Picture.Bitmap);
     Noise(IM_7.Picture.Bitmap);
     Noise(IM_8.Picture.Bitmap);
     Noise(IM_9.Picture.Bitmap);
end;

procedure TFM_Main.Button4Click(Sender: TObject);
begin
     if (Neuro.Save(Paramstr(0)+'.neuro')) then
        begin
             ListBox1.ItemIndex:=ListBox1.Items.Add('Speichern OK');
        end
     else
        begin
             ListBox1.ItemIndex:=ListBox1.Items.Add('Speichern fehlgeschlagen');
        end;
end;

procedure TFM_Main.Button5Click(Sender: TObject);
begin
     if (Neuro.load(Paramstr(0)+'.neuro')) then
        begin
             ListBox1.ItemIndex:=ListBox1.Items.Add('Laden OK');
        end
     else
        begin
             ListBox1.ItemIndex:=ListBox1.Items.Add('Laden fehlgeschlagen');
        end;
end;

procedure TFM_Main.Button6Click(Sender: TObject);
begin
     Neuro.NeuroFunction:=0;
     ListBox1.ItemIndex:=ListBox1.Items.Add('Rechteckfunktion');
end;

procedure TFM_Main.Button7Click(Sender: TObject);
begin
     Neuro.NeuroFunction:=1;
     ListBox1.ItemIndex:=ListBox1.Items.Add('Exp-funktion');
end;

procedure TFM_Main.Button8Click(Sender: TObject);
begin
     Neuro.NeuroFunction:=2;
     ListBox1.ItemIndex:=ListBox1.Items.Add('Tan-funktion');
end;

end.
