unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,unit_xor, ComCtrls;

type
  TForm1 = class(TForm)
    gbOptions: TGroupBox;
    edSearch: TEdit;
    gbFiles: TGroupBox;
    edSource: TEdit;
    btBrowse: TButton;
    Label1: TLabel;
    gbCommands: TGroupBox;
    btGo: TButton;
    rbSimpleXor: TRadioButton;
    rbChainedXOR: TRadioButton;
    odInput: TOpenDialog;
    mmLog: TMemo;
    pbProgress: TProgressBar;
    procedure btBrowseClick(Sender: TObject);
    procedure btGoClick(Sender: TObject);
  private
    { Private-Deklarationen }
    Reader  : TFilestream;
    Decoder : TDecoderXOR;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btBrowseClick(Sender: TObject);
begin
  with (odInput) do
    begin
      if (execute = TRUE) then
        begin
          edSource.Text:=lowercase(Filename);
        end;
    end;
end;

procedure TForm1.btGoClick(Sender: TObject);
var
  aTemp     : array of Byte;
  aSearch   : array of Byte;
  u32Size   : unsigned32;
  u32Pointer: unsigned32;
  bFound    : Boolean;
  u32Hits   : unsigned32;
begin
  //Sucharrays initialisieren
  SetLength(aTemp  ,Length(edSearch.Text));
  SetLength(aSearch,Length(edSearch.Text));
  copymemory(addr(aSearch[0]),addr(edSearch.Text[1]),Length(aSearch));

  //Ausgabe initialisieren
  MMLog.Clear();

  if (FileExists(edSource.Text) = TRUE) and (Length(aSearch) > 0) then
    begin
      Reader:=TFileStream.Create(edSource.Text,fmOpenRead OR fmShareDenyNone	);

      pbProgress.Max:= integer(Reader.Size);

      //Straight-Decoder initilisieren
      Decoder:=TDecoderXOR.Create();
      Decoder.init();

      //Und los
      repeat
        //Zur Sicherheit nochmal positionieren
        Reader.Seek(0,0);

        mmLog.Lines.Add('scanning code '+Decoder.code);
        u32Hits:=0;
        pbProgress.Position:=0;

          //Die ganze Datei durchsuchen
          repeat
            //Erstes Byte vergleichen
            u32Pointer:=0;
            repeat
              u32Size:=Reader.Read(aTemp[u32Pointer],1);
              aTemp[u32Pointer]:=Decoder.decode(aTemp[u32Pointer]);
            until ( aTemp[u32Pointer] = aSearch[u32Pointer] ) or (u32Size = 0);

            pbProgress.Position:=integer(Reader.Position);
            Application.ProcessMessages();

            //Gefunden ?
            if (u32Size > 0) then
              begin
                //Den Rest vergleichen
                repeat
                  inc(u32Pointer);
                  u32Size:=Reader.Read(aTemp[u32Pointer],1);
                  aTemp[u32Pointer]:=Decoder.decode(aTemp[u32Pointer]);
                  bFound:= aTemp[u32Pointer] = aSearch[u32Pointer];
                until (bFound = FALSE) or
                      (u32Size = 0) or
                      (u32Pointer + 1 >= unsigned32(Length(aSearch)));

                //Gefunden ?
                if ( bFound ) then
                  begin
                    mmLog.Lines.Add('found at 0x'+IntToHEx(Reader.Position - u32Pointer ,12));
                    inc(u32Hits);
                  end;
              end;
          until (u32Size=0);

        if (u32Hits = 0) then
          begin
            mmLog.Lines.Delete(mmLog.Lines.Count-1);
          end
        else
          begin
            mmLog.Lines.Add(IntToStr(u32Hits) + ' position(s) found');
            mmLog.Lines.Add('');
          end;

          //Ein Durchlauf beendet
      until ( decoder.cycledone() );

      Decoder.Free();
      Reader.Free();
    end;
end;

end.
