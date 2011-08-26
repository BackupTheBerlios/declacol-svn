unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    btUp: TButton;
    btLeft: TButton;
    btRight: TButton;
    btDown: TButton;
    RadioButton1: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    procedure array2bmp(data : pByte; size : unsigned32; bmp : TBitmap);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure RadioButton8Click(Sender: TObject);
    procedure RadioButton5Click(Sender: TObject);
    procedure RadioButton7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btUpClick(Sender: TObject);
    procedure btDownClick(Sender: TObject);

    function getlinesize(bmp : TBitmap):unsigned32;
    procedure repaintbmp();
    procedure btRightClick(Sender: TObject);
    procedure btLeftClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
    aData : array of byte;
    u32Offset   : unsigned32;
    u32LineSize : unsigned32;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  u32Index : unsigned32;
begin
  //fill random data
  setlength (aData, 1024 *1024 * 10 );
  for u32Index:=0 to Length(aData) - 1 do
    begin
      aData[u32Index]:=random(high(byte));
    end;

  //prepare bitmap
  image1.Picture.Bitmap.PixelFormat:=pf8bit;
  u32linesize:=getlinesize(image1.Picture.Bitmap);
  u32offset:=0;
end;

function TForm1.getlinesize(bmp : TBitmap):unsigned32;
begin
  //calculate number of bytes
  case bmp.PixelFormat of
    pf1Bit  : result:=width div 8;
    pf4Bit  : result:=width div 2;
    pf8Bit  : result:=width div 1;
    pf15Bit : result:=width * 2;
    pf16Bit : result:=width * 2;
    pf24Bit : result:=width * 3;
    pf32Bit : result:=width * 4;
  else
    result:=width;
  end;
end;

procedure TForm1.repaintbmp();
begin
  array2bmp(@aData[u32Offset],length(aData) - u32Offset,image1.Picture.Bitmap);
  image1.Invalidate();
end;

procedure TForm1.array2bmp(data : pByte; size : unsigned32; bmp : TBitmap);
var
  u32y     : unsigned32;
  u32height: unsigned32;
  pLine    : pScanline;
begin
  //recalc scanlinesize
  u32linesize:=getlinesize(bmp);

  //calculate maximum height
  u32height:=size div u32linesize;
  if (unsigned32(bmp.Height) < u32height) then u32height:=bmp.Height - 1;

  //copy data to bitmap
  for u32y:=0 to u32height do
    begin
      pLine:=bmp.scanline[u32y];
      move(data^,pLine^,u32linesize);
      inc(data,u32linesize);
    end;

  //fill all other with zero
  inc(u32height);
  while (u32height < unsigned32(bmp.Height)) do
    begin
      pLine:=bmp.scanline[u32Height];
      fillchar(pLine^,u32linesize,0);
      inc(u32Height);
    end;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  Image1.Picture.Bitmap.PixelFormat:=pf1Bit;
  repaintbmp();
end;

procedure TForm1.RadioButton3Click(Sender: TObject);
begin
  Image1.Picture.Bitmap.PixelFormat:=pf4Bit;
  repaintbmp();
end;

procedure TForm1.RadioButton4Click(Sender: TObject);
begin
  Image1.Picture.Bitmap.PixelFormat:=pf8Bit;
  repaintbmp();
end;

procedure TForm1.RadioButton8Click(Sender: TObject);
begin
  Image1.Picture.Bitmap.PixelFormat:=pf16Bit;
  repaintbmp();
end;

procedure TForm1.RadioButton5Click(Sender: TObject);
begin
  Image1.Picture.Bitmap.PixelFormat:=pf24Bit;
  repaintbmp();
end;

procedure TForm1.RadioButton7Click(Sender: TObject);
begin
  Image1.Picture.Bitmap.PixelFormat:=pf32Bit;
  repaintbmp();
end;


procedure TForm1.btUpClick(Sender: TObject);
begin
  if (u32Offset >= u32linesize) then
    begin
      dec(u32Offset,u32linesize);
    end
  else
    begin
      u32Offset:=0;
    end;

  repaintbmp();
end;

procedure TForm1.btDownClick(Sender: TObject);
begin
  if (u32Offset < (Image1.Picture.Bitmap.Height - 2) * u32linesize ) then
    begin
      inc(u32Offset,u32linesize);
    end
  else
    begin
      u32Offset:=(Image1.Picture.Bitmap.Height - 1) * u32linesize;
    end;

  repaintbmp();
end;

procedure TForm1.btRightClick(Sender: TObject);
begin
  if (u32Offset < (Image1.Picture.Bitmap.Height - 1) * u32linesize) then
    begin
      inc(u32Offset);
    end;

  repaintbmp();
end;

procedure TForm1.btLeftClick(Sender: TObject);
begin
  if (u32Offset > 1) then
    begin
      dec(u32Offset);
    end;

  repaintbmp();
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (key='w') then btUpClick(Self);
  if (key='a') then btLeftClick(Self);
  if (key='s') then btDownClick(Self);
  if (key='d') then btRightClick(Self);

  if (key='W') then btUpClick(Self);
  if (key='A') then btLeftClick(Self);
  if (key='S') then btDownClick(Self);
  if (key='D') then btRightClick(Self);
end;

end.
