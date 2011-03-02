unit main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,class_diskio,class_blockio, CheckLst, ExtCtrls, class_checksum,
  ComCtrls,unit_log,inifiles;

type
  TfmMain = class(TForm)
    GroupBox1: TGroupBox;
    cbDrives: TCheckListBox;
    GroupBox2: TGroupBox;
    rbCRC: TRadioButton;
    rbSector00: TRadioButton;
    rbSectorFF: TRadioButton;
    rbSectorAA: TRadioButton;
    GroupBox3: TGroupBox;
    lbLog: TListBox;
    GroupBox4: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    btStop: TButton;
    tiRefresh: TTimer;
    cbEndless: TCheckBox;
    pbProgress: TProgressBar;
    Burst: TEdit;
    UpDown: TUpDown;
    Label1: TLabel;

    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tiRefreshTimer(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
  private
    { Private-Deklarationen }
    procedure ScanDevices();
    procedure AddLog(text : string);
    procedure DeviceChange(var Message: TMESSAGE); message WM_DEVICECHANGE;
    function getcrc(index : unsigned32):string;

    procedure checkcrc();

  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;
  bBusy   : Boolean = FALSE;
  bLoaded : Boolean = FALSE;
  ini     : TInifile;

implementation

{$R *.dfm}

//Refreshbutton
procedure TfmMain.Button1Click(Sender: TObject);
begin
  Self.ScanDevices()
end;

//Beim ersten Start scannen
procedure TfmMain.FormActivate(Sender: TObject);
begin
  if (not bLoaded) then
    begin
      bLoaded:=TRUE;
      Self.ScanDevices();
      ini:=TIniFile.Create(paramstr(0)+'.ini');
      updown.Position:=Ini.ReadInteger('options','buffersize',1);
    end;
end;


procedure TfmMain.AddLog(text : string);
begin
  while (lbLog.Items.Count > 300000) do
    begin
      lbLog.Items.Delete(0);
    end;

  lbLog.ItemIndex:=lbLog.Items.Add(timetostr(time()) + ' : ' +  text);

  //Und Datei erzeugen
  Log_Add(paramstr(0)+'.txt',text,TRUE);
end;


//Meldet das System einen Devicewechsel neu scannen
procedure TfmMain.DeviceChange(var Message: TMESSAGE);
begin
  //Nur scannen wenn wir nicht gerade was tun
  if ( not bBusy ) then
    begin
      Self.ScanDevices();
    end;
end;

//Alle Devices enumerieren und als Objekt der Listbox zuordnen
procedure TfmMain.ScanDevices();
var
  Disk      : TDiskIO;
  Counter   : Byte;
  OldDevice : signed16;
begin
//  Self.AddLog('devicescan');

  //Altes device merken
  if (cbDrives.ItemIndex >=0) then
     begin
          OldDevice:=TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]).DeviceNumber;
     end
  else
     begin
          OldDevice:=-1;
     end;

  //Alle evtl. schon bestehenden Objekte entladen
  while (cbDrives.Items.Count > 0) do
    begin
      cbDrives.Items.Objects[0].Free;
      cbDrives.Items.Delete(0);
    end;

  //Und neu enumerieren  
  for Counter:=0 to 255 do
    begin
      Disk:=TDiskIO.Create(Counter);
      if (Disk.valid=TRUE) then
        begin
          //Zu jedem gefundenen Objekt die Klasse ablegen
          if (Disk.Removable = TRUE) then
            begin
              cbDrives.Items.AddObject('Device ' + IntToStr(Disk.Devicenumber) + ' (' + Disk.SizeString + ')' ,Disk);
            end;

          //Markierung wieder herstellen
          if (Disk.Devicenumber = OldDevice) then
             begin
                  cbDrives.ItemIndex:=cbDrives.Items.Count - 1;
             end;
        end
      else
        begin
          Disk.Free();
        end;
    end;
end;

procedure TfmMain.tiRefreshTimer(Sender: TObject);
begin
  if (bBusy = TRUE) then
    begin
    Application.ProcessMessages();
    end;
end;

procedure TfmMain.btStopClick(Sender: TObject);
begin
  bBusy:=FALSE;
  btStop.Visible:=FALSE;
  AddLog('user break');  
end;

procedure TfmMain.Button2Click(Sender: TObject);
begin
  btStop.Visible:=TRUE;

  if (rbCRC.Checked = TRUE) then
    begin
      checkcrc();
    end;

  btStop.Visible:=FALSE;
end;



procedure TfmMain.checkcrc();
var
  index    : unsigned32;
  cycle    : unsigned32;
  maxcycle : unsigned32;
begin
//Als erstes holen wir für jedes Laufwerk die Checksumme
bbusy:=TRUE;
index:=0;
while (index < unsigned32(cbDrives.Items.Count)) and (bBusy) do
  begin
    if (cbDrives.checked[index] = TRUE) then
      begin
        addlog('buffersize set to '+Burst.Text);
        addlog('initial read '+cbDrives.Items[index]);
        //Lesen und abspeichern
        TDiskIO(cbDrives.Items.Objects[index]).Data:=getcrc(index);
      end;
    inc(index);
  end;

if (cbEndless.Checked) then maxcycle:=high(unsigned32) else maxcycle:=1;
cycle:=0;
while (cycle < maxcycle) do
  begin
    index:=0;
    while (index < unsigned32(cbDrives.Items.Count)) and (bBusy) do
      begin
        if (cbDrives.checked[index] = TRUE) then
          begin
            addlog('compare '+cbDrives.Items[index]);
            //Lesen und vergleichen
            if (TDiskIO(cbDrives.Items.Objects[index]).Data <> getcrc(index)) then
              begin
                addlog ('crc different at cycle '+IntToStr(cycle));
              end;
          end;
        inc(index);
      end;

    //Userbreak
    if (bBusy=FALSE) then
      begin
        maxcycle:=0;
      end
    else
      begin
        inc(cycle);
      end;
  end;
addlog('done');
end;


function TfmMain.getcrc(Index : unsigned32):string;
var
  Buffer   : array of Byte;
  u32Read  : unsigned32;
  u32Burst : unsigned32;
  u64Sector: unsigned32;
  MD5      : TMD5;
begin
  pbProgress.Position:=0;
  if (assigned(cbDrives.Items.Objects[index])) then
    begin
      with TDiskIO(cbDrives.Items.Objects[index]) do
        begin
          //Single sector
          u32Burst := UpDown.Position;

          MD5:=TMD5.create();
          MD5.init();

          //Buffer Setzen
          SetLength(Buffer, (SectorSize * u32Burst) );

          //Und los
          u64Sector:=0;
          while (u64Sector < SectorCount) and (bBusy) do
             begin
               if (Read(u64Sector,Addr(Buffer[0]),u32Burst,u32Read)=TRUE) then
                 begin
                   pbProgress.Position:=signed32(trunc(u64Sector / unsigned32(SectorCount) * 1024));

                   caption:=inttostr(u64Sector);
                   //Prüfsumme bauen
                   MD5.add(Addr(Buffer[0]),u32Read * SectorSize);

                   //Um die gelesenen Sekotoren vorschieben
                   inc(u64Sector,u32Read);
                 end
               else
                 begin
                   AddLog(Format('error reading sector  %d',[u64Sector]));
                   inc(u64Sector,1);
                 end;
               Application.ProcessMessages();
             end;

          result:=MD5.Finalize().sChecksum;
          MD5.Free();
        end;
    end;
  //Speicher freigeben
  SetLength(Buffer,0);
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  u32size : unsigned32;
begin
  u32size:=updown.Position;
  ini.WriteInteger('options','buffersize',updown.Position);
  ini.UpdateFile();
  ini.Free();
end;

procedure TfmMain.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin

  AllowChange:=TRUE;
  if (NewValue < UpDown.Min) then
    begin
      UpDown.Position:=UpDown.Min;
      AllowChange:=FALSE;
    end;
    
  if (NewValue > UpDown.Max) then
    begin
      UpDown.Position:=UpDown.Max;
      AllowChange:=FALSE;
    end;

  if (AllowChange=TRUE) then
    begin
      Burst.Text:=IntToStr(NewValue);
    end;
end;

end.


