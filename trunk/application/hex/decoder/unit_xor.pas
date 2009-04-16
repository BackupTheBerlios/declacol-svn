unit unit_xor;

interface
uses unit_typedefs,sysutils;

type TDecoderXOR = class(TObject)
  private
    u8Key : unsigned8;

    //Den aktuellen Code als String holen
    function getcode():longstring;
  protected
  public

    //Initialisiert alle internen Variablen
    procedure init();

    //Dekodiert ein Byte
    function decode(input : byte):byte;

    //Einen Durchlauf als Abgeschlossen ansehen
    //Bei FALSE wird ein weitere Durchlauf benötigt, ansonsten sind wir fertig
    function cycledone():boolean;

    property code : longstring read getcode;

end;

implementation

//Bei XOR gibt es 255 Möglichkeiten, wir fangen bei 0 an
procedure TDecoderXOR.init();
begin
  self.u8Key:=0;
end;

function TDecoderXOR.decode(input : byte):byte;
begin
  //Dekodieren
  result:=input xor self.u8key;
end;

function TDecoderXOR.cycledone():boolean;
begin
  //Key weiterzählen
  self.u8Key:=Self.u8Key + 1;

  //Kommt noch was?
  result:=Self.u8Key=0;
end;

function TDecoderXOR.getcode():longstring;
begin
  result:=IntToStr(Self.u8Key);
end;


end.
