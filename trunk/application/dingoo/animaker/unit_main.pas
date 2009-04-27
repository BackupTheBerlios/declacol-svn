unit unit_main;

interface

uses
  Windows,unit_typedefs, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,unit_grafix, ExtDlgs,unit_fileformat;

type
  TForm1 = class(TForm)
    gbPreview: TGroupBox;
    imPreview: TImage;
    gbCommands: TGroupBox;
    btOpenBitmap: TButton;
    btSaveAni: TButton;
    btAbout: TButton;
    dgSaveAni: TSaveDialog;
    dgOpenBitmap: TOpenPictureDialog;
    procedure btOpenBitmapClick(Sender: TObject);
    procedure btSaveAniClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btOpenBitmapClick(Sender: TObject);
var
  Bitmap : TBitmap;
begin
  with (dgOpenBitmap) do
    begin
      if (Execute) then
        begin
          Bitmap:=TBitmap.Create;

          //Bild laden
          Bitmap.LoadFromFile(Filename);
          Bitmap.PixelFormat:=pf24Bit;

          //Skalieren wenn es notwendig ist
          if (Bitmap.Width  <> 320) AND
             (Bitmap.Height <> 240) then
             begin
                bitmap_resample(Bitmap,320,240);
             end;

          //bitmap_changeto16bit(bitmap);
          bitmap_changeto16bit(Bitmap);
          imPreview.Picture.Bitmap.Assign(Bitmap);
          imPreview.Picture.Bitmap.PixelFormat:=pf16Bit;

          btSaveAni.Enabled:=TRUE;

          Bitmap.Free();
        end;
    end;
  imPreview.Invalidate();
end;

procedure TForm1.btSaveAniClick(Sender: TObject);
var
  u32Y      : unsigned32;
  u32X      : unsigned32;
  StreamOut : TFileStream;
  pLine     : pWordArray;
  u16Pixel  : unsigned16;
begin
  with (dgSaveAni) do
    begin
      if (Execute) then
        begin
          if (extractfileext(filename)<>'.ani') then
            begin
              filename:=filename + '.ani';
            end;

          StreamOut:=TFileStream.Create(filename,fmCreate or fmOpenWrite);

          try
            //Header
            StreamOut.WriteBuffer(aAniHeader,Length(aAniheader));

            for u32Y:=0 to imPreview.Picture.Bitmap.Height - 1 do
              begin
                pLine:=pWordArray(imPreview.Picture.Bitmap.ScanLine[u32Y]);

                StreamOut.WriteBuffer(pLine^,imPreview.Picture.Bitmap.Width * 2);
              end;

          finally
            StreamOut.Free();
          end;

        end;
    end;
end;



end.
