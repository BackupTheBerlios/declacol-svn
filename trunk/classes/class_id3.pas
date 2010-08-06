unit class_id3;
////////////////////////////////////////////////////////////////////////////////
///
/// Klasse zum verarbeiten von ID3 Tags
///
/// Kann im Moment nur ID3v1.1 lesen
////////////////////////////////////////////////////////////////////////////////
interface
uses unit_typedefs,sysutils,class_checksum,classes;

//Eregbnisstruct des readaufrufes
type  pID3Tag = ^TID3Tag;
      TID3Tag = record
      Valid   : Boolean;
      MD5     : string[32];
      ID      : string[3];
      Artist  : string[30];
      Album   : string[30];
      Year    : string[4];
      Track   : string[4];
      Title   : string[30];
      Comment : string[30];
      Genre   : string[30];
      GenreID  : unsigned8;
  end;

type TID3 = class(TObject)
  protected
    bMD5 : Boolean;
  private
    function read_id3v1(Filename: longstring): TID3Tag;
    function read_id3v2(Filename: longstring): TID3Tag;
  public
    function read(filename:string):TID3Tag;

    property calccrc : boolean read bMD5 write bMD5 default true;
end;



implementation

////////////////////////////////////////////////////////////////////////////////
//Speichermap der TagDaten
{
  Byte 1-3 = ID 'TAG'
  Byte 4-33 = Titel / Title
  Byte 34-63 = Artist
  Byte 64-93 = Album
  Byte 94-97 = Jahr / Year
  Byte 98-127 = Kommentar / Comment
  Byte 128 = Genre
}

type pTagBuff = ^TTagBuff;
     TTagBuff = packed record
 ID     : array[0..2]  of Char;
 Title  : array[0..29] of Char;
 Artist : array[0..29] of Char;
 Album  : array[0..29] of Char;
 Year   : array[0..3]  of Char;
 Text   : array[0..29] of Char;
 GenreID: Byte;
end;

////////////////////////////////////////////////////////////////////////////////
const
//Tags für id3v2.2
 V2TITLE   = 'TAL';
 V2ARTIST  = 'TP1';
 V2ALBUM   = 'TAL';
 V2YEAR    = 'TYE';
 V2COMMENT = 'COM';
 V2TRACK   = 'TRK';
 V2GENRE   = 'TCO';

//Tags für id3v2.3


//Alle verfügbaren Genres
 Genres : array[0..146] of string =
    ('Blues','Classic Rock','Country','Dance','Disco','Funk','Grunge',
    'Hip- Hop','Jazz','Metal','New Age','Oldies','Other','Pop','R&B',
    'Rap','Reggae','Rock','Techno','Industrial','Alternative','Ska',
    'Death Metal','Pranks','Soundtrack','Euro-Techno','Ambient',
    'Trip-Hop','Vocal','Jazz+Funk','Fusion','Trance','Classical',
    'Instrumental','Acid','House','Game','Sound Clip','Gospel','Noise',
    'Alternative Rock','Bass','Punk','Space','Meditative','Instrumental Pop',
    'Instrumental Rock','Ethnic','Gothic','Darkwave','Techno-Industrial','Electronic',
    'Pop-Folk','Eurodance','Dream','Southern Rock','Comedy','Cult','Gangsta',
    'Top 40','Christian Rap','Pop/Funk','Jungle','Native US','Cabaret','New Wave',
    'Psychadelic','Rave','Showtunes','Trailer','Lo-Fi','Tribal','Acid Punk',
    'Acid Jazz','Polka','Retro','Musical','Rock & Roll','Hard Rock','Folk',
    'Folk-Rock','National Folk','Swing','Fast Fusion','Bebob','Latin','Revival',
    'Celtic','Bluegrass','Avantgarde','Gothic Rock','Progressive Rock',
    'Psychedelic Rock','Symphonic Rock','Slow Rock','Big Band','Chorus',
    'Easy Listening','Acoustic','Humour','Speech','Chanson','Opera',
    'Chamber Music','Sonata','Symphony','Booty Bass','Primus','Porn Groove',
    'Satire','Slow Jam','Club','Tango','Samba','Folklore','Ballad',
    'Power Ballad','Rhytmic Soul','Freestyle','Duet','Punk Rock','Drum Solo',
    'Acapella','Euro-House','Dance Hall','Goa','Drum & Bass','Club-House',
    'Hardcore','Terror','Indie','BritPop','Negerpunk','Polsk Punk','Beat',
    'Christian Gangsta','Heavy Metal','Black Metal','Crossover','Contemporary C',
    'Christian Rock','Merengue','Salsa','Thrash Metal','Anime','JPop','SynthPop');

////////////////////////////////////////////////////////////////////////////////
//Globaler read
function TID3.read(Filename : longstring): TID3Tag;
var
  MD5    : TMD5;
begin
  //Gibt es einen ID3v2?
  result:=Self.read_id3v2(filename);

  //Dann ID3v1 probieren
  if (result.Valid = FALSE) then
    begin
      result:=Self.read_id3v1(filename);
    end;

  //Prüfsumme erzeugen
  if (Self.bMD5 = TRUE) then
    begin
      MD5:=TMD5.create();
      result.MD5:=MD5.FromString(Filename).sChecksum;
      MD5.Free();
    end
  else
    begin
      Result.MD5:='';
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//Versucht ID3v1.1 zu lesen
function TID3.read_id3v1(Filename: longstring): TID3Tag;
var
  Input  : TFileStream;
  Buffer : TTagBuff;
begin
  //Fehler annehmen
  FillChar(Result,SizeOf(Result),0);
  result.Valid:=FALSE;

  //Einfach per Stream lesen
  Input := TFileStream.Create(Filename, fmOPENREAD or fmSHAREDENYWRITE);
  try
    //Hinten anfangen
    Input.Seek(-128, soFromEnd);

    //Speicher füllen
    Input.Read(Buffer, 128);

    //Wirklich ein Tag ?
    if (string(Buffer.ID) = 'TAG') then
      begin
        //Und ablegen
        with Result do
          begin
            Valid   := TRUE;
            ID      := string(Buffer.ID);
            Title   := string(Buffer.Title);
            Artist  := string(Buffer.Artist);
            Album   := string(Buffer.Album);
            Year    := string(Buffer.Year);
            Comment := string(Buffer.Text);
            GenreID := Buffer.GenreID;
            Track   := IntToStr(Ord(Buffer.Text[29]));

            //Genre anhand der ID zuordnen
            if (GenreID < Length(Genres)) then
              begin
                Genre := Genres[GenreID];
              end
                else
              begin
                Genre := 'unknown';
              end;
        end;
    end;
  finally
    Input.Free();
  end;

end;

////////////////////////////////////////////////////////////////////////////////
//Versucht ID3v2 zu lesen
function TID3.read_id3v2(Filename: longstring): TID3Tag;
begin
  result.valid:=FALSE;
end;


end.
