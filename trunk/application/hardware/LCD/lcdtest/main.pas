unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,Unit_LCD;

type
  TForm1 = class(TForm)
    IM_Test: TImage;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1  : TForm1;
  LCD    : TLCDDisplay;
implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
     LCD:=TLCDDisplay.Create;
     with IM_Test.Picture do
          begin
               Bitmap:=TBitmap.Create;
               Bitmap.Width :=96;
               Bitmap.Height:=32;
               Bitmap.PixelFormat:=pf8Bit;
          end;
     LCD.LPTPort:=$03bc;
     LCD.Open;
     LCD.Contrast:=14;
     LCD.DisplayOn;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
     with (IM_Test.Picture) do
          begin
               LCD.ClearBitmap(Bitmap);
               LCD.FontStyle:=0;
               LCD.PaintText(Bitmap,0, 0,'ABCDEFGH 123456');
               LCD.FontStyle:=1;
               LCD.PaintText(Bitmap,0, 6,'ABCDEFGH 123456');
               LCD.FontStyle:=2;
               LCD.PaintText(Bitmap,0,12,'ABCDEFGH 123456');
               LCD.FontStyle:=3;
               LCD.PaintText(Bitmap,0,18,'ABCDEFGH 123456');
               LCD.FontStyle:=4;
               LCD.PaintText(Bitmap,0,24,'ABCDEFGH 123456');
          end;
     IM_Test.Invalidate;
     LCD.WriteBitmap(IM_Test.Picture.Bitmap);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
   iIndex : integer;
begin
     with (IM_Test.Picture) do
          begin
               LCD.ClearBitmap(Bitmap);
               LCD.FontStyle:=Random(4);

               for iIndex:=0 to 4 do
                   begin
                        LCD.PaintText(Bitmap,1, iIndex*(LCD.FontHeight+1)+1,'BAR');
                        LCD.PaintBar (Bitmap,20,iIndex*(LCD.FontHeight+1)+1,75,5,random(100));
                   end;

          end;
     IM_Test.Invalidate;
     LCD.WriteBitmap(IM_Test.Picture.Bitmap);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
   iIndex : integer;
begin
     with (IM_Test.Picture) do
          begin
               LCD.ClearBitmap(Bitmap);
               LCD.FontStyle:=Random(4);

               for iIndex:=0 to 13 do
                   begin
                        LCD.PaintText(Bitmap,iIndex*(LCD.FontWidth+2),32-LCD.FontHeight,Chr(Ord('A')+iIndex));
                        LCD.PaintBar (Bitmap,iIndex*(LCD.FontWidth+2),0,LCD.FontWidth,32-LCD.FontHeight-1,random(100),FALSE);
                   end;

          end;
     IM_Test.Invalidate;
     LCD.WriteBitmap(IM_Test.Picture.Bitmap);
end;


procedure TForm1.Button4Click(Sender: TObject);
begin
     with (IM_Test.Picture) do
          begin
               LCD.ClearBitmap(Bitmap);
               LCD.FontStyle:=2;

               //CPU
               LCD.PaintText(Bitmap,1,32-LCD.FontHeight,'CPU');
               LCD.PaintBar (Bitmap,1,0,17,26,random(100),FALSE);

               //LAN
               LCD.PaintText(Bitmap,20,32-LCD.FontHeight,'LAN');
               LCD.PaintBar (Bitmap,20,0,17,26,random(100),FALSE);

               //MEM
               LCD.PaintText(Bitmap,39,32-LCD.FontHeight,'MEM');
               LCD.PaintBar (Bitmap,39,0,17,26,random(100),FALSE);

               //HDD
               LCD.PaintText(Bitmap,58,32-LCD.FontHeight,'HD1');
               LCD.PaintBar (Bitmap,58,0,17,26,random(100),FALSE);

               LCD.PaintText(Bitmap,77,32-LCD.FontHeight,'HD2');
               LCD.PaintBar (Bitmap,77,0,17,26,random(100),FALSE);

          end;
     IM_Test.Invalidate;
     LCD.WriteBitmap(IM_Test.Picture.Bitmap);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
     with (IM_Test.Picture) do
          begin
               LCD.ClearBitmap(Bitmap);
               LCD.FontStyle:=1;

               LCD.PaintText(Bitmap, 1, 1,'CPU');
               LCD.PaintBar (Bitmap,20, 1,75,5,random(100));

               LCD.PaintText(Bitmap, 1, 7,'LAN');
               LCD.PaintBar (Bitmap,20, 7,75,5,random(100));

               LCD.PaintText(Bitmap, 1, 13,'MEM');
               LCD.PaintBar (Bitmap,20, 13,75,5,random(100));

               LCD.PaintText(Bitmap, 1, 19,'HD1');
               LCD.PaintBar (Bitmap,20, 19,75,5,random(100));

               LCD.PaintText(Bitmap, 1, 25,'HD2');
               LCD.PaintBar (Bitmap,20, 25,75,5,random(100));
          end;
     IM_Test.Invalidate;
     LCD.WriteBitmap(IM_Test.Picture.Bitmap);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     LCD.Close;
     LCD.Free;
end;



end.
