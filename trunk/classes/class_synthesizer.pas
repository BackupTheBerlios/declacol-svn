unit class_synthesizer;

interface
uses unit_typedefs,MMSystem,classes,windows,unit_errorcodes,messages;


//Callback für die Events
type TWaveCallback = procedure(WaveHandle : THandle) of object;

//Hauptklasse
type TWavePlayer = class(TObject)
     private
            hWaveOut     : THandle;
            rWaveHead    : TWaveHdr;
            rWaveFormat  : TWaveFormatEx;
            u32Channels  : unsigned32;
            u32Samples   : unsigned32;
            u8Bits       : unsigned8;
            u32Error     : unsigned32;
            bActive      : boolean;
            bPlaying     : boolean;
            bPaused      : boolean;

            WaveMem      : TMemoryStream;
            
            fOnOpen      : TWaveCallback;
            fOnClose     : TWaveCallback;
            fOnStartplay : TWaveCallback;
            fOnStopPlay  : TWaveCallback;
            fOnPause     : TWaveCallback;
            fOnResume    : TWaveCallback;

            //CallBack für den PlayerStatus
            procedure WaveOutProc(WaveHandle : THandle; MSG : unsigned32; Instance : unsigned32; Param1 : unsigned32; Param2 : unsigned32) stdcall;
            procedure SaveCallback(fCall : TWaveCallback);


            //Synthesizefunktionen
            function GetSound(Index : unsigned32; Maximum : unsigned32; Frequency : unsigned32): unsigned8;
     public
            constructor create();
            destructor  free();

            function  Open ():Boolean;
            procedure Close();

            procedure Sound(Frequency : unsigned32; Duration : unsigned32);
            function  Play(WaveData : Pointer; WaveDataSize : unsigned32):boolean;
            procedure Pause();
            procedure Resume();
            procedure Stop();

            //Mono / Stereo
            property Channels    : unsigned32 read u32Channels write u32Channels;

            // 8000, 11025, 22050, or 44100
            property Samplerate  : unsigned32 read u32Samples  write u32Samples;

            //Im Moment werden nur 8 Bits unterstützt
            property Bits        : unsigned8  read u8Bits      write u8Bits;

            //Flags
            property Lasterror   : unsigned32 read u32Error;
            property Active      : boolean    read bActive;
            property Playing     : boolean    read bPlaying;
            property Paused      : boolean    read bPaused;

            //Callbacks
            property OnOpen      : TWaveCallback read fOnOpen      write fOnOpen;
            property OnClose     : TWaveCallback read fOnClose     write fOnClose;
            property OnStartplay : TWaveCallback read fOnStartplay write fOnStartplay;
            property OnStopPlay  : TWaveCallback read fOnStopPlay  write fOnStopPlay;
            property OnPause     : TWaveCallback read fOnPause     write fOnPause;
            property OnResume    : TWaveCallback read fOnResume    write fOnResume; 
end;


//Klassenloser Stub um aus dem Callback in die Instanz einspringen zu können 
procedure _WaveOutProc(WaveHandle : THandle; MSG : unsigned32; Instance : unsigned32; Param1 : unsigned32; Param2 : unsigned32) stdcall;

implementation


////////////////////////////////////////////////////////////////////////////////
constructor TWavePlayer.create();
begin
     //Defaults erzwingen
     Self.bActive        :=FALSE;
     Self.bPlaying       :=FALSE;
     Self.bPaused        :=FALSE;
     Self.Channels       :=2;
     Self.SampleRate     :=22050;
     Self.Bits           :=8;

     //Speichreablage erzeugen
     Self.WaveMem:=TMemoryStream.Create();

     inherited create;
end;

////////////////////////////////////////////////////////////////////////////////
destructor TWavePlayer.free();
begin
     Self.WaveMem.Free();
     Self.Close();
     inherited free();
end;

////////////////////////////////////////////////////////////////////////////////
///
/// Einen Kanal des WaveMappers öffnen
/// und dabei das WaveFormat initialiseren
///
////////////////////////////////////////////////////////////////////////////////
function TWavePlayer.Open():Boolean;
begin
     result:=FALSE;

     //Waveformat setzen
     rWaveFormat.wFormatTag      := WAVE_FORMAT_PCM;
     rWaveFormat.nChannels       := Self.u32Channels;
     rWaveFormat.nSamplesPerSec  := Self.u32Samples; // 8000, 11025, 22050, or 44100
     rWaveFormat.wBitsPerSample  := 8;
     rWaveFormat.nBlockAlign     := (rWaveFormat.nChannels * rWaveFormat.wBitsPerSample) div rWaveFormat.wBitsPerSample;
     rWaveFormat.nAvgBytesPerSec := rWaveFormat.nSamplesPerSec * rWaveFormat.nBlockAlign;
     rWaveFormat.cbSize          := 0;

     //WaveDevices checken
     if (waveOutGetNumDevs() > 0) then
        begin
             //Prüfen, ob die Soundkarte das gewünschte Format unterstützt
             if (waveOutOpen(@hWaveOut, 0,@rWaveFormat, 0, 0, WAVE_FORMAT_QUERY) = 0) then
                begin
                     u32Error:=waveOutOpen( @hWaveOut,
                                            WAVE_MAPPER,
                                            @rWaveFormat,
                                            cardinal(@_WaveOutProc),
                                            cardinal(Self),
                                            CALLBACK_FUNCTION);

                     result:=u32Error=MMSYSERR_NOERROR;
                end;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
///
/// Aufräumen und schließen
///
////////////////////////////////////////////////////////////////////////////////
procedure TWavePlayer.Close();
begin
     //Evtl. laufendes Sample beenden
     Self.Stop();
end;

////////////////////////////////////////////////////////////////////////////////
//Einfach einen beliebigen Buffer als Sample interpretieren und abspielen
function TWavePlayer.Play(WaveData : Pointer; WaveDataSize : unsigned32):boolean;
begin
     //Zustand prüfen
     if (Self.bActive = TRUE) and (Self.bPlaying = FALSE) then
        begin
             //Waveheader initialisieren
             Self.rWaveHead.lpData:=WaveData;
             Self.rWaveHead.dwBufferLength:=WaveDataSize;
             Self.rWaveHead.dwBytesRecorded:=0;
             Self.rWaveHead.dwUser:=0;
             Self.rWaveHead.dwFlags:=0;
             Self.rWaveHead.dwLoops:=0;
             
             //Hier könnte man auf einen nachfolgenden Buffer verweisen
             //Somit wäre ein lückenloses abspielen mit nur zwei zyklischen
             //Puffern möglich
             //Self.rWaveHead.lpNext:=nil;

             waveOutPrepareHeader(hWaveOut,@Self.rWaveHead,SizeOf(Self.rWaveHead));
             u32Error:=waveOutWrite(hWaveOut,@Self.rWaveHead,SizeOf(Self.rWaveHead));

             Self.bPlaying:=TRUE;
             Self.bPaused:=FALSE;
             result:=u32Error = MMSYSERR_NOERROR;

             if (result) then
                begin
                     Self.SaveCallback(Self.fOnStartplay);
                end;
        end
     else
        begin
             result:=FALSE;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TWavePlayer.Pause();
begin
     if (Self.bPlaying=TRUE) AND (Self.bPaused=FALSE) then
        begin
             waveOutPause(Self.hWaveOut);
             Self.bPaused:=TRUE;
             Self.SaveCallback(Self.fOnPause);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TWavePlayer.Resume();
begin
     if (Self.bPlaying=TRUE) AND (Self.bPaused=TRUE) then
        begin
             waveOutRestart(Self.hWaveOut);
             Self.bPaused:=FALSE;
             Self.SaveCallback(Self.fOnResume);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
//Abspielen beenden
procedure TWavePlayer.Stop();
begin
     if (Self.bPlaying) then
        begin
             waveOutReset(Self.hWaveOut);
             Self.bPlaying:=FALSE;
             Self.bPaused :=FALSE;
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Events sicher aufrufen
procedure TWavePlayer.SaveCallback(fCall : TWaveCallback);
begin
     if (Assigned(fCall)) then
        begin
             fCall(Self.hWaveOut);
        end;
end;


////////////////////////////////////////////////////////////////////////////////
//Callback nach einem Abspielen um alles zu entladen
//Klassenlose Callbackfunktion um keine Verwirrung mit Instanzen zu bekommen
procedure _WaveOutProc(WaveHandle : THandle; MSG : unsigned32; Instance : unsigned32; Param1 : unsigned32; Param2 : unsigned32) stdcall;
begin
     //Und hier in die Instanz reinspringen
     TWavePlayer(Instance).WaveOutProc(WaveHandle,MSG,Instance,Param1,Param2);
end;


////////////////////////////////////////////////////////////////////////////////
procedure TWavePlayer.WaveOutProc(WaveHandle : THandle; MSG : unsigned32; Instance : unsigned32; Param1 : unsigned32; Param2 : unsigned32);
begin
     //Nachrichten verarbeiten
     case (MSG) of
          //Device ist geöffnet
          WOM_OPEN    : begin
                             Self.bActive := TRUE;
                             Self.bPlaying:= FALSE;
                             Self.bPaused := FALSE;

                             Self.SaveCallback(Self.fOnOpen);
                        end;

          //Device ist geschlossen
          WOM_CLOSE   : begin
                             Self.bActive :=FALSE;
                             Self.bPlaying:=FALSE;
                             Self.bPaused :=FALSE;

                             Self.SaveCallback(Self.fOnClose);
                        end;

          //Buffer ist abgespielt
          WOM_DONE    : begin
                             //Buffer freigeben
                             waveOutUnPrepareHeader(WaveHandle,pointer(Param1),SizeOf(TWaveHdr));

                             Self.bPlaying:=FALSE;
                             Self.bPaused :=FALSE;

                             Self.SaveCallback(Self.fOnStopPlay);
                        end;     
     end;
end;


////////////////////////////////////////////////////////////////////////////////
//Einen Memorystream bauen der einen Ton als Sample enthält
procedure TWavePlayer.Sound(Frequency : unsigned32; Duration : unsigned32);
var
  u32Index : unsigned32;
  u32Count : unsigned32;

  u16Data  : unsigned16;
  u8Left   : unsigned8;
  u8Right  : unsigned8;
begin
     u32Count := (Duration * Self.u32Samples) div 1000; // sound data
     dec (u32Count);

     WaveMem.Position:=0;

     //Linker Kanal
     for u32Index := 0 to u32Count do
     begin
          //Die Kanäle synthetisieren
          u8Right:= Self.GetSound(u32Index, u32Count ,Frequency);
          u8Left := Self.GetSound(u32Index, u32Count ,Frequency shl 1);

          //Stereopaket bauen und abspeichern
          u16Data:=u8Right + (u8Left shl 8);
          WaveMem.Write(u16Data, SizeOf(u16Data) );
     end;

    Self.Play(WaveMem.Memory,WaveMem.Size);
end;



////////////////////////////////////////////////////////////////////////////////
//Ab hier die Synthesizerfunktionen
function TWavePlayer.GetSound(Index : unsigned32; Maximum : unsigned32; Frequency : unsigned32): unsigned8;
var
  Omega    : Double;
begin
     //Einfach den Buffer füllen
     Omega:=2 * Pi * Frequency;

     // wt = w *i /SampleRate
     result:= 127 + trunc(127 * sin(Index * Omega / Self.SampleRate));
end;

end.
