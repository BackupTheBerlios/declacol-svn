unit class_dingoo;
////////////////////////////////////////////////////////////////////////////////
/// Klasse um alle möglichen Einstellungen an einer HXF-Datei zu machen
/// Hier werden alle unterklassen zusammengefasst und eine einheitliche
/// Schnittstelle geboten
///
////////////////////////////////////////////////////////////////////////////////


interface

uses unit_typedefs,class_hxf,class_language,unit_kernel,graphics,sysutils,unit_hex;

type TDingoo = class (TObject)
  private
    //Alle in der Firmware hinterlegten Sprachdateien
    aKernelLNG : array of tlanguagedata;
    //Alle in der HXF-Datei Verfügbaren sprachdateien
    aAvailLNG  : array of tlanguagedata;

    //Die LogOn-Bitmaps
    bmpLogon   : TBitmap;
    bmpLogOff  : TBitmap;

    //Alle verfügbaren Optionen
    bPatches   : boolean;
    bDefaults  : boolean;
    bBMPLogOn  : boolean;
    bBMPLogOff : boolean;
    bTranslate : boolean;
    bFiles     : boolean;

    //Array mit allen verfügbaren Defaulteinstellungen
    aDefaults  : array of TDefaultOptions;

  protected
  public
    hxf        : THXFReader;

    constructor create(hxffile:longstring);
end;

implementation

constructor TDingoo.create(hxffile:longstring);
var
  u32index : unsigned32;
  u32FoundPos : unsigned32;
  u32Temp     : unsigned32;
  aTemp       : thxfrecord;
  aLNG        : array[0..SIZEOF_CONFIG-1] of byte;
begin
  hxf:=THXFReader.create(hxffile);

  //Sind wir überhaupt eine HXF-Datei?
  bFiles     :=hxf.count > 0;

  //Nun suchen wir alle Dateien die wir für das Interface brauchen
  bDefaults  :=hxf.exists('user_data\default.cfg')  AND bFiles;
  bPatches   :=hxf.exists('ccpmp.bin')              AND bFiles;
  bBMPLogOn  :=hxf.exists('user_data\logon.ani')    AND bFiles;
  bBMPLogOff :=hxf.exists('user_data\logoff.ani')   AND bFiles;

  //Alle Sprachen im Archiv suchen und auf verfügbarkeit prüfen
  u32index:=0;
  SetLength(aAvailLNG,0);
  while (u32index < unsigned32(length(LanguageData))) do
    begin
      if (hxf.exists(LanguageData[u32Index].filepath)=TRUE) then
        begin
          SetLength(aAvailLNG,Length(aAvailLNG)+1);
          aAvailLNG[Length(aAvailLNG)-1]:=LanguageData[u32Index];
        end;
      inc(u32Index);
    end;
  bTranslate:=length(aAvailLNG) > 0;

  //Kernelsprachen suchen
  setlength(aKernelLNG,0);

  //Firmware laden
  hxf.get('ccpmp.bin',aTemp);

  //Da die Sprachverknüpfungen nicht immer am selben Platz liegen,
  //suchen wir die erste und lesen alle weiteren die sich anschließen
  u32Index:=0;
  while (u32Index < unsigned32(length(LanguageData))) do
    begin
      if (hexsearch(aTemp.buffer,LanguageData[u32Index].Configb,0,u32Temp)=TRUE) and
         (u32Temp < u32FoundPos) then
          begin
            u32FoundPos:=u32Temp;
          end;
      inc(u32Index);
    end;

  //Nun holen wir alle Sprachen die sich anschließen
  

    

end;


end.
