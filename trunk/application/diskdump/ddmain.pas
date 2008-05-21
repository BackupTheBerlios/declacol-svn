{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
Author: Sven Lorenz / Borg@Sven-of-Nine.de
}

////////////////////////////////////////////////////////////////////////////////
///
/// Tool zum direkten lesen und schreiben von Devices (USB-Stick, HDD, CF,SD)
///
////////////////////////////////////////////////////////////////////////////////

unit ddmain;

interface

uses
  unit_compiler,
//Delphi 7 Kompatibilität
{$IfDef EXPLICIT_VARIANT}
  Variants,
{$EndIf}

  Unit_typedefs,Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,class_diskio, StdCtrls,Spin, Grids, ComCtrls, ExtCtrls,class_checksum,class_random,
  unit_hddhelper, Menus;

type
  TfmMain = class(TForm)
    pcMain: TPageControl;
    tsSectorViewer: TTabSheet;
    tsReader: TTabSheet;
    pnSectorViewer: TPanel;
    sgHex: TStringGrid;
    seSector: TSpinEdit;
    gbDeviceControl: TGroupBox;
    gbMountedDevices: TGroupBox;
    cbDrives: TListBox;
    gbData: TGroupBox;
    lbMediaPre: TLabel;
    lbSizePre: TLabel;
    lbSectorPre: TLabel;
    lbMediaType: TLabel;
    lbSize: TLabel;
    lbSectors: TLabel;
    lbDevicePre: TLabel;
    lbDevice: TLabel;
    lbSectorSizePre: TLabel;
    lbSectorSize: TLabel;
    gbOptions: TGroupBox;
    cbWriteprotected: TCheckBox;
    cbRealSize: TCheckBox;
    pnReader: TPanel;
    tsWriter: TTabSheet;
    pnWrite: TPanel;
    dgLoad: TOpenDialog;
    lbDumpfile: TLabel;
    edTargetFile: TEdit;
    btBrowseTarget: TButton;
    dgSave: TSaveDialog;
    btDump: TButton;
    cbReadBurstMode: TCheckBox;
    seReadBurstMode: TSpinEdit;
    lbReadBurstMode: TLabel;
    pbRead: TProgressBar;
    lbReadLog: TListBox;
    lbImageFIle: TLabel;
    edSourceFile: TEdit;
    btBrowseSource: TButton;
    cbWriteBurstMode: TCheckBox;
    seWriteBurstMode: TSpinEdit;
    btWriteImage: TButton;
    lbWriteLog: TListBox;
    lbWriteBurstMode: TLabel;
    pbWrite: TProgressBar;
    tsProperties: TTabSheet;
    pnProperties: TPanel;
    mmProperties: TMemo;
    tsBenchmark: TTabSheet;
    pnBenchmark: TPanel;
    mmBenchmark: TMemo;
    btStartBenchmark: TButton;
    lbReadSpeedPre: TLabel;
    lbReadSpeed: TLabel;
    lbWriteSpeedPre: TLabel;
    lbWriteSpeed: TLabel;
    tsCHSCalculator: TTabSheet;
    pnCHSCalculator: TPanel;
    lbCHSSIze: TLabel;
    lbCylinder: TLabel;
    lbHeads: TLabel;
    lbSetors: TLabel;
    seCHSCylinder: TSpinEdit;
    seCHSHeads: TSpinEdit;
    seCHSSectors: TSpinEdit;
    edCHSSize: TEdit;
    lbCHSSector: TLabel;
    edCHSSectorSize: TEdit;
    lbCHSStatusPre: TLabel;
    lbCHSStatus: TLabel;
    btCHSReset: TButton;
    tsDummyFiles: TTabSheet;
    pnDummy: TPanel;
    lbCreateDummy: TLabel;
    lbDummySizePre: TLabel;
    pbDummy: TProgressBar;
    edDummyFile: TEdit;
    btBrowseDummy: TButton;
    btCreateDummyFile: TButton;
    lbDummyLog: TListBox;
    edDummySize: TEdit;
    tiRememberRefresh: TTimer;
    btBreak: TButton;
    pmDevices: TPopupMenu;
    miRefresh: TMenuItem;
    tsWipe: TTabSheet;
    pnWiper: TPanel;
    lbWiper: TLabel;
    pbWipe: TProgressBar;
    btWipeDevice: TButton;
    rbWipe00: TRadioButton;
    rbWipeFF: TRadioButton;
    rbWipeRandom: TRadioButton;
    lbWipeLog: TListBox;
    lbWipeSpeedPre: TLabel;
    lbWipeSpeed: TLabel;
    tsCRC: TTabSheet;
    pnCRC: TPanel;
    lbCRC: TLabel;
    lbCRCSpeedPre: TLabel;
    lbCRCSpeed: TLabel;
    pbCRC: TProgressBar;
    btCRC: TButton;
    lbCRCLog: TListBox;
    lbCRCLogpre: TListBox;
    cbCreatePartition: TCheckBox;
    procedure cbDrivesClick(Sender: TObject);
    procedure cbWriteprotectedClick(Sender: TObject);
    procedure cbRealSizeClick(Sender: TObject);
    procedure ScanDevices();
    procedure RefreshList();
    procedure DisableUser();
    procedure EnableUser();
    procedure FormActivate(Sender: TObject);
    procedure seSectorChange(Sender: TObject);
    procedure btBrowseTargetClick(Sender: TObject);
    procedure btDumpClick(Sender: TObject);
    procedure cbReadBurstModeClick(Sender: TObject);
    procedure btBrowseSourceClick(Sender: TObject);
    procedure cbWriteBurstModeClick(Sender: TObject);
    procedure btWriteImageClick(Sender: TObject);
    procedure tsSectorViewerEnter(Sender: TObject);
    procedure tsPropertiesShow(Sender: TObject);
    procedure btStartBenchmarkClick(Sender: TObject);
    procedure edCHSSizeChange(Sender: TObject);
    procedure btCHSResetClick(Sender: TObject);
    procedure btBrowseDummyClick(Sender: TObject);
    procedure btCreateDummyFileClick(Sender: TObject);
    procedure DeviceChange(var Message: TMESSAGE); message WM_DEVICECHANGE;
    procedure SettingChange(var Message: TMESSAGE); message WM_SETTINGCHANGE;
    procedure tiRememberRefreshTimer(Sender: TObject);
    procedure btBreakClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure btWipeDeviceClick(Sender: TObject);
    procedure btCRCClick(Sender: TObject);

  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;
  bLoaded : Boolean=FALSE;
  bBusy   : Boolean=FALSE;

implementation

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
/// Hilfsfunktionen
////////////////////////////////////////////////////////////////////////////////

//Userzugriff ein oder ausschalten
procedure TfmMain.DisableUser();
begin
  Screen.Cursor:=crHourGlass;
  bBusy:=TRUE;
end;

procedure TfmMain.EnableUser();
begin
  Screen.Cursor:=crDefault;
  btBreak.Hide();
  bBusy:=FALSE;
end;

//Bei Devicewechsel neu enumerieren
procedure TfmMain.SettingChange(var Message: TMESSAGE);
begin
  Self.DeviceChange(Message);
end;

procedure TfmMain.DeviceChange(var Message: TMESSAGE);
begin
  //Nur scannen wenn wir nicht gerade was tun
  if ( not bBusy ) then
    begin
      Self.ScanDevices();
      tiRememberRefresh.Enabled:=FALSE;
    end
  else
    begin
      //Den Updatetime aktivieren, um die Änderung nicht zu vergessen
      tiRememberRefresh.Enabled:=TRUE;
    end;
end;

//Alle Devices enumerieren und als Objekt der Listbox zuordnen
procedure TfmMain.ScanDevices();
var
  Disk    : TDiskIO;
  Counter : Byte;
begin
  Self.DisableUser();

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
          cbDrives.Items.AddObject('Device',Disk);
        end
      else
        begin
          Disk.Free();
        end;
    end;
  Self.RefreshList();
  Self.EnableUser();
end;

//Die Anzeige der Liste refreshen
procedure TfmMain.RefreshList();
var
  Index : unsigned32;
begin
  Index:=0;
  while (Index < unsigned32(cbDrives.Items.Count)) do
    begin
      with TDiskIO(cbDrives.Items.Objects[Index]) do
          if (Removable) then
            begin
              cbDrives.Items[Index]:='[removable] ' + SizeString;
            end
          else
            begin
              cbDrives.Items[Index]:='[fixed] ' + SizeString;
            end;
      inc(Index);
    end;
    //Update der Datenbox erzwingen
    Self.cbDrivesClick(Self);
end;


//Bei einem Click auf das Laufwerk alls aktualisieren
procedure TfmMain.cbDrivesClick(Sender: TObject);
begin
  if (cbDrives.ItemIndex>=0) then
    begin
      //Wenn ein Laufwerk gewählt ist dessen Daten ausgeben
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          //Mediadaten updaten
          if (Removable=TRUE) then lbMediatype.Caption:='removable' else lbMediatype.Caption:='fixed';
          lbSize.Caption          := SizeString;
          lbSectors.Caption       := IntToStr(SectorCount);
          lbDevice.Caption        := IntToStr(DeviceNumber);
          lbSectorSize.Caption    := IntToStr(SectorSize)+' Bytes';
          cbWriteProtected.Checked:= WriteProtected;
          cbRealSize.Checked      := RealSize;

          //Sectorviewer resetten
          seSector.MaxValue       := SectorCount-1;
          seSector.Value          := 0;
          Self.seSectorChange(Self);

          //Properties ersetten
          tsPropertiesShow(Self);

          //CHS-Rechner resetten
          seCHSCylinder.Value :=Geometry.Cylinders;
          edCHSSectorSize.Text:=IntToStr(SectorSize);
          edCHSSize.Text:=IntToStr(Size);
          edDummySize.Text:=IntToStr(Size);
        end;
    end;
end;

//Writeprotect Checkbox
procedure TfmMain.cbWriteprotectedClick(Sender: TObject);
begin
  if (cbDrives.ItemIndex>=0) then
    begin
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          WriteProtected:=cbWriteProtected.Checked;
        end;
    end;
end;

//RealSize Checkbox
procedure TfmMain.cbRealSizeClick(Sender: TObject);
begin
  if (cbDrives.ItemIndex>=0) then
    begin
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
           if (RealSize <> cbRealSize.Checked) then
            begin
              Self.DisableUser();
              RealSize:=cbRealSize.Checked;
              RefreshList();
              Self.EnableUser();
            end;
        end;
    end;
end;

//Beim Start nach Devices Suchen
procedure TfmMain.FormActivate(Sender: TObject);
begin
  //Zufall zur Sichreheit initialisieren   
  randomize();

  if (bLoaded=FALSE) then
    begin
      Self.ScanDevices();
      bLoaded:=TRUE;
      pcMain.ActivePageIndex:=0;
    end;
end;


////////////////////////////////////////////////////////////////////////////////
/// Funktionen des Sektorviewers
////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.tsSectorViewerEnter(Sender: TObject);
begin
    //Sectoransicht neu laden
    seSectorChange(Self);
end;

//Sektorenansicht
procedure TfmMain.seSectorChange(Sender: TObject);
var
  Buffer : array of byte;
  Dummy  : unsigned32;
  Index  : unsigned32;
  Col    : unsigned32;
  Row    : unsigned32;
  sRow   : String;
begin
  DisableUser();
  if (cbDrives.ItemIndex>=0) then
    begin
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          SetLength(Buffer,SectorSize);

          //Nur einen Sektor lesen
          Read(unsigned64(seSector.Value),Addr(Buffer[0]),1,Dummy);

         //Grid an die Sektoren anpassen
         sgHex.RowCount:=signed32(SectorSize) div sgHex.ColCount;

         //Und den ganzen Sektor im Hexviewer ausgeben
         Row:=0;
         Col:=0;
         sRow:=' ';
         for Index:=0 to SectorSize - 1 do
            begin
              //Die Zelle
              sgHex.Cells[Col,Row]:=IntToHex(Buffer[Index],2);
              //Der Text
              sRow:=sRow+Chr(Buffer[Index]);
              inc (Col);

              //Am Ende der Zeile den Text einfügen
              if (Col >= unsigned32(sgHex.ColCount - 1) ) then
                begin
                  //Text darstellen
                  sgHex.Cells[Col,Row]:=sRow;
                  sRow:=' ';
                  Col:=0;
                  Inc(Row);
                end;
             end;
         end;
     end;
  EnableUser();
end;

////////////////////////////////////////////////////////////////////////////////
/// Funktionen des Readers
////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btBrowseTargetClick(Sender: TObject);
begin
  with (dgSave) do
    begin
      if (Execute) then
        begin
          edTargetFile.Text:=Filename;
        end;
    end;
end;

procedure TfmMain.cbReadBurstModeClick(Sender: TObject);
begin
  seReadBurstMode.Enabled:=cbReadBurstMode.Checked;
  lbReadBurstMode.Enabled:=cbReadBurstMode.Checked;
end;

procedure TfmMain.btDumpClick(Sender: TObject);
var
  hFile    : Signed32;
  Buffer   : array of Byte;
  u32Read  : unsigned32;
  u32Burst : unsigned32;
  u64Sector: unsigned32;
  MD5      : TMD5;
  sMD5     : longstring;
begin
  DisableUser();
  btBreak.Show();
  lbReadLog.Clear();
  if (cbDrives.ItemIndex>=0) then
    begin

      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          lbReadLog.Items.Add(Format('reading device%d',[Devicenumber]));

          //BurstMode
          if (cbReadBurstMode.Checked) then
            begin
              u32Burst := seReadBurstMode.Value;
            end
          else
            begin
              u32Burst := 1;
            end;

          //Evtl. Existierende Datei löschen
          if (FileExists(edTargetFile.Text)) then
            begin
              lbReadLog.Items.Add(Format('removing old file %s',[edTargetFile.Text]));
              deletefile(edTargetFile.Text);
            end;

          //Datei erzeugen
          hFile:=FileCreate(edTargetFile.Text);
          if (hFile >= 0) then
            begin
              lbReadLog.Items.Add(Format('creating file %s',[edTargetFile.Text]));

              MD5:=TMD5.create();
              MD5.init();

              //Buffer Setzen
              SetLength(Buffer, (SectorSize * u32Burst) );
              lbReadLog.Items.Add(Format('sector buffer set to %d',[u32Burst]));
              lbReadLog.Items.Add(Format('%d sectors found',[SectorCount]));

              //Und los
              u64Sector:=0;
              while (u64Sector < SectorCount) and (bBusy) do
                begin
                  if (Read(u64Sector,Addr(Buffer[0]),u32Burst,u32Read)=TRUE) then
                    begin
                      pbRead.Position:=signed32(trunc(u64Sector / SectorCount * 1024));

                      //Prüfsumme bauen
                      MD5.add(Addr(Buffer[0]),u32Read * SectorSize);

                      //In Datei speichern
                      FileWrite(hFile,Buffer[0],u32Read * SectorSize);
                      lbReadSpeed.Caption:=Format('%f',[Speed]);

                      //Um die gelesenen Sekotoren vorschieben
                      inc(u64Sector,u32Read);
                    end
                  else
                    begin
                      lbReadLog.Items.Add(Format('error reading sector  %d',[u64Sector]));
                      inc(u64Sector,1);
                    end;
                  Application.ProcessMessages();
                end;

              sMD5:=MD5.Finalize().sChecksum;

              lbReadLog.Items.Add(Format('%d sectors read ',[u64Sector]));
              lbReadLog.Items.Add(Format('MD5 : %s ',[sMD5]));
              lbReadLog.Items.Add('done');

              //Auf die CRC-Seite spiegeln um die Daten vorzuhalten
              lbCRCLogPre.Items.Add(Format('Read File : %s => MD5 : %s ',[edTargetFile.Text,sMD5]));

              MD5.Free();
              CloseHandle(hFile);
            end
          else
            begin
              lbReadLog.Items.Add(Format('unable to create file %s',[edTargetFile.Text]));
            end;
        end;
    end
  else
    begin
      lbReadLog.Items.Add('no device selected');
    end;
  //Speicher freigeben
  SetLength(Buffer,0);
  EnableUser();
end;

////////////////////////////////////////////////////////////////////////////////
/// Funktionen des Writers
////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btBrowseSourceClick(Sender: TObject);
begin
  with (dgLoad) do
    begin
      if (Execute) then
        begin
          edSourceFile.Text:=Filename;
        end;
    end;
end;

procedure TfmMain.cbWriteBurstModeClick(Sender: TObject);
begin
  seWriteBurstMode.Enabled:=cbWriteBurstMode.Checked;
  lbWriteBurstMode.Enabled:=cbWriteBurstMode.Checked;
end;

procedure TfmMain.btWriteImageClick(Sender: TObject);
var
  hFile    : Signed32;
  Buffer   : array of Byte;
  u32Write : unsigned32;
  u32Burst : unsigned32;
  u64Sector: unsigned32;
  MD5      : TMD5;
  sMD5     : longstring;
begin
  DisableUser();
  btBreak.Show();

  lbWriteLog.Clear();

  if (cbDrives.ItemIndex>=0) then
    begin

      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          lbWriteLog.Items.Add(Format('writing device%d',[DeviceNumber]));

          //Schreibschutz
          if (WriteProtected) then
            begin
              lbWriteLog.Items.Add('device ist writeprotected');
              EnableUser();
              Exit;
            end;

          //BurstMode
          if (cbWriteBurstMode.Checked) then
            begin
              u32Burst := seWriteBurstMode.Value;
            end
          else
            begin
              u32Burst := 1;
            end;

          //Datei erzeugen
          lbWriteLog.Items.Add(Format('opening file %s',[edSourceFile.Text]));
          hFile:=FileOpen(edSourceFile.Text,fmOPENREAD);
          if (hFile >= 0) then
            begin
              //Checksummer initialisieren
              MD5:=TMD5.create();
              MD5.init();

              //Buffer Setzen
              SetLength(Buffer, (SectorSize * u32Burst) );
              lbWriteLog.Items.Add(Format('sector buffer set to %d',[u32Burst]));
              lbWriteLog.Items.Add(Format('%d sectors found',[SectorCount]));

              //Ab die Post
              u64Sector:=0;
              while (u64Sector < SectorCount) and (bBusy) do
                begin
                  //Buffer flushen um den Sektor immer mit nullen aufzufüllen
                  FillChar(Buffer[0],Length(Buffer),#00);

                  //Datei lesen
                  u32Write:=unsigned32(FileRead(hFile,Buffer[0],SectorSize * u32Burst));
                  if (u32Write > 0) then
                    begin
                      //Prüfsumme bauen
                      MD5.add(Addr(Buffer[0]),u32Write);

                      //Sektoren schreiben
                      if (Write(u64Sector,Addr(Buffer[0]),u32Burst,u32Write)=FALSE) then
                        begin
                          lbWriteLog.Items.Add(Format('unable to write sector %d - %d',[u64Sector,u64Sector+u32Burst]));
                          inc(u64Sector,1);
                        end
                      else
                        begin
                          inc(u64Sector,u32Write);
                        end;

                      lbWriteSpeed.Caption:=Format('%f',[Speed]);
                      //Fortschritt
                      pbWrite.Position:=signed32(trunc(u64Sector / SectorCount * 1024));
                      Application.ProcessMessages();
                    end;
                end;

              sMD5:=MD5.Finalize().sChecksum;
              lbWriteLog.Items.Add(Format('%d sectors written',[u64Sector]));
              lbWriteLog.Items.Add(Format('MD5 : %s ',[sMD5]));
              lbWriteLog.Items.Add('done');

              lbCRCLogPre.Items.Add(Format('Write File : %s => MD5 : %s ',[edTargetFile.Text,sMD5]));
              
              MD5.Free();
              CloseHandle(hFile);
            end
          else
            begin
              lbWriteLog.Items.Add(Format('unable to open file %s',[edSourceFile.Text]));
            end;
        end;
    end
  else
    begin
      lbWriteLog.Items.Add('no device selected');
    end;
  //Speicher freigeben
  SetLength(Buffer,0);
  EnableUser();
end;


////////////////////////////////////////////////////////////////////////////////
/// Funktionen der Properties
////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.tsPropertiesShow(Sender: TObject);
var
  sType : Longstring;
begin
  mmProperties.Clear;
  if (cbDrives.ItemIndex>=0) then
    begin
      //Wenn ein Laufwerk gewählt ist dessen Daten ausgeben
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          if (Removable=TRUE) then
            begin
              sType:='removable';
            end
          else
            begin
              sType:='fixed';
            end;

          //Mediadaten updaten
          mmProperties.Lines.Add(Format('Device     : %d',[DeviceNumber]));
          mmProperties.Lines.Add(Format('Type       : %s',[sType]));
          mmProperties.Lines.Add(Format('Size       : %s',[SizeString]));
          mmProperties.Lines.Add(Format('Size       : %d',[Size]));
          mmProperties.Lines.Add('');
          mmProperties.Lines.Add(Format('Cylinder   : %d',[Geometry.Cylinders]));
          mmProperties.Lines.Add(Format('Tracks     : %d',[Geometry.TracksPerCylinder]));
          mmProperties.Lines.Add(Format('Sectors    : %d',[Geometry.SectorsPerTrack]));
          mmProperties.Lines.Add(Format('SectorSize : %d Bytes',[Geometry.BytesPerSector]));
          mmProperties.Lines.Add(Format('SectorMax  : %d',[SectorCount]));
          mmProperties.Lines.Add('');
        end;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Funktionen des Benchmarks
////////////////////////////////////////////////////////////////////////////////

procedure TfmMain.btStartBenchmarkClick(Sender: TObject);
var
  sType    : Longstring;
  sSpeed   : Longstring;
  u32Burst : unsigned32;
  u32Count : unsigned32;
  u32Loop  : unsigned32;
  u32Read  : unsigned32;
  u64Sector: unsigned64;
  fSpeed   : Single;
  bOK      : Boolean;
  Buffer   : Array of Byte;
  Random   : TRandom;
begin
  DisableUser();
  mmBenchmark.Clear;
  Random:=TRandom.Create();

  if (cbDrives.ItemIndex>=0) then
    begin
      //Wenn ein Laufwerk gewählt ist dessen Daten ausgeben
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          if (Removable=TRUE) then
            begin
              sType:='removable';
            end
          else
            begin
              sType:='fixed';
            end;

          //Mediadaten updaten
          mmBenchmark.Lines.Add(Format('Device     : %d [%s]',[DeviceNumber,sType]));
          mmBenchmark.Lines.Add(Format('Size       : %s',[SizeString]));
          mmBenchmark.Lines.Add('');

          mmBenchmark.Lines.Add('Linear Read');
          mmBenchmark.Lines.Add('Sectors  /  KB/s');
          u32Burst:=1;
          for u32Count:=0 to 12 do
            begin
              SetLength(Buffer, u32Burst * SectorSize);
              bOK   :=TRUE;
              fSpeed:=0;
              u64Sector:=0;

              //Mehrmals lesen, um einen Mittelwert zu bekommen
              for u32Loop:=0 to 5 do
                begin
                  bOK:=bOK and Read(u64Sector,Addr(Buffer[0]),u32Burst,u32Read);
                  inc(u64Sector,u32Burst);
                  fSpeed:=fSpeed + Speed;
                end;

              if (bOK=TRUE) then
                begin
                  if (u32Read=u32Burst) then
                    begin
                      sSpeed:=Format('%f',[fSpeed / 5]);
                    end
                  else
                    begin
                      sSpeed:='not enough sectors on device';
                    end;
                end
              else
                begin
                  sSpeed:='readerror';
                end;

              mmBenchmark.Lines.Add(Format('%0:-8d -  %s',[u32Burst,sSpeed]));
              Application.ProcessMessages();
              u32Burst:=u32Burst shl 1;
            end;

          mmBenchmark.Lines.Add('Random Read');
          mmBenchmark.Lines.Add('Sectors  /  KB/s');
          u32Burst:=1;
          for u32Count:=0 to 12 do
            begin
              SetLength(Buffer, u32Burst * SectorSize);
              bOK   :=TRUE;
              fSpeed:=0;

              //Mehrmals lesen, um einen Mittelwert zu bekommen
              for u32Loop:=0 to 5 do
                begin
                  //Zufälligen Sector bestimmen
                  u64Sector:=Random.GetQuadWord();
                  if (u64Sector >= SectorCount) then
                    begin
                      u64Sector:=SectorCount - u32Burst;
                    end;

                  //Und lesen
                  bOK:=bOK and Read(u64Sector,Addr(Buffer[0]),u32Burst,u32Read);
                  fSpeed:=fSpeed + Speed;
                end;

              if (bOK=TRUE) then
                begin
                  if (u32Read=u32Burst) then
                    begin
                      sSpeed:=Format('%f',[fSpeed / 5]);
                    end
                  else
                    begin
                      sSpeed:='not enough sectors on device';
                    end;
                end
              else
                begin
                  sSpeed:='readerror';
                end;

              mmBenchmark.Lines.Add(Format('%0:-8d -  %s',[u32Burst,sSpeed]));
              Application.ProcessMessages();
              u32Burst:=u32Burst shl 1;
            end;
        end;
    end;
    //Speicher freigeben
    SetLength(Buffer,0);
    Random.Free();
    EnableUser();
end;

////////////////////////////////////////////////////////////////////////////////
/// Funktionen des Dummycreators
////////////////////////////////////////////////////////////////////////////////

procedure TfmMain.btBrowseDummyClick(Sender: TObject);
begin
  with (dgSave) do
    begin
      if (Execute) then
        begin
          edDummyFile.Text:=Filename;
        end;
    end;
end;

procedure TfmMain.btCreateDummyFileClick(Sender: TObject);
var
  hFile    : Signed32;
  u64Size  : unsigned64;
  Buffer   : array of Byte;
begin
  DisableUser();
  btBreak.Show();
  lbDummyLog.Clear();
  if (cbDrives.ItemIndex>=0) then
    begin
      lbDummyLog.Items.Add('creating dummy');

      //Evtl. Existierende Datei löschen
      if (FileExists(edDummyFile.Text)) then
         begin
           lbDummyLog.Items.Add(Format('removing old file %s',[edDummyFile.Text]));
           deletefile(edDummyFile.Text);
         end;

      //Datei erzeugen
      hFile:=FileCreate(edDummyFile.Text);
      if (hFile >= 0) then
         begin

           with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
              begin
                u64Size:=Size;
              end;

           lbDummyLog.Items.Add(Format('creating file %s',[edTargetFile.Text]));

           //Buffer Setzen
           SetLength(Buffer,1024 * 1024);
           FillChar(Buffer[0],Length(Buffer),#00);

           pbDummy.Max:=u64Size div Length(Buffer);
           pbDummy.Position:=0;

           while (u64Size > Length(Buffer)) and (bBusy) do
             begin
                  //In Datei speichern
                  FileWrite(hFile,Buffer[0],Length(Buffer));
                  dec (u64Size , Length(Buffer));

                  //Fortschritt anzeigen
                  pbDummy.Position:=pbDummy.Position + 1;
                  Application.ProcessMessages();
             end;

          //Und den Rest schreiben
          FileWrite(hFile,Buffer[0],unsigned32(u64Size));

          lbDummyLog.Items.Add('done');

          CloseHandle(hFile);
        end
      else
        begin
           lbReadLog.Items.Add(Format('unable to create file %s',[edTargetFile.Text]));
        end;
    end
  else
    begin
      lbReadLog.Items.Add('no device selected');
    end;
  //Speicher freigeben
  SetLength(Buffer,0);
  EnableUser();
end;

////////////////////////////////////////////////////////////////////////////////
/// Funktionen des Wipers
////////////////////////////////////////////////////////////////////////////////
procedure TfmMain.btWipeDeviceClick(Sender: TObject);
var
  Buffer   : array of Byte;
  u32Write : unsigned32;
  u32Burst : unsigned32;
  u64Sector: unsigned32;
  MD5      : TMD5;
  sMD5     : Longstring;
begin
  DisableUser();
  btBreak.Show();

  lbWipeLog.Clear();

  if (cbDrives.ItemIndex>=0) then
    begin
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          lbWipeLog.Items.Add(Format('writing device%d',[DeviceNumber]));

          //Schreibschutz
          if (WriteProtected) then
            begin
              lbWriteLog.Items.Add('device ist writeprotected');
              EnableUser();
              Exit;
            end;

          //BurstMode aus Write holen
          if (cbWriteBurstMode.Checked) then
            begin
              u32Burst := seWriteBurstMode.Value;
            end
          else
            begin
              u32Burst := 1;
            end;

          //Checksummer initialisieren
          MD5:=TMD5.create();
          MD5.init();

          //Buffer Setzen
          SetLength(Buffer, (SectorSize * u32Burst) );
          lbWipeLog.Items.Add(Format('sector buffer set to %d',[u32Burst]));
          lbWipeLog.Items.Add(Format('%d sectors found',[SectorCount]));

          //Buffer initialisieren
          if (rbWipe00.Checked = TRUE) then
             begin
                  FillChar(Buffer[0],Length(Buffer),#00);
             end;
          if (rbWipeFF.Checked = TRUE) then
             begin
                  FillChar(Buffer[0],Length(Buffer),#255);
             end;

          //Ab die Post
          u64Sector:=0;
          while (u64Sector < SectorCount) and (bBusy) do
            begin
              //Buffer flushen (wenn random gewhält ist)
              if (rbWipeRandom.Checked = TRUE) then
                 begin
                      for u32Write:=0 to Length(Buffer) - 1 do
                          begin
                               Buffer[u32Write]:=random(256);
                          end;
                 end;

              //Sektoren schreiben
              if (Write(u64Sector,Addr(Buffer[0]),u32Burst,u32Write)=FALSE) then
                 begin
                   lbWipeLog.Items.Add(Format('unable to write sector %d - %d',[u64Sector,u64Sector+u32Burst]));
                   inc(u64Sector,1);
                 end
              else
                 begin
                   //Prüfsumme bauen
                   MD5.add(Addr(Buffer[0]),SectorSize * u32Write);
                   inc(u64Sector,u32Write);
                 end;

              lbWipeSpeed.Caption:=Format('%f',[Speed]);

              //Fortschritt
              pbWipe.Position:=signed32(trunc(u64Sector / SectorCount * 1024));
              Application.ProcessMessages();
            end;

          sMD5:=MD5.Finalize().sChecksum;
          lbWipeLog.Items.Add(Format('%d sectors written',[u64Sector]));
          lbWipeLog.Items.Add(Format('MD5 : %s ',[sMD5]));
          lbWipeLog.Items.Add('done');

          lbCRCLogPre.Items.Add(Format('Wipe => MD5 : %s ',[sMD5]));

          MD5.Free();

          //Soll auch Partitioniert werden ?
          if (cbCreatePartition.Checked=TRUE) then
            begin
                 CreatePartition();
            end;

        end;
    end
  else
    begin
      lbWipeLog.Items.Add('no device selected');
    end;
  //Speicher freigeben
  SetLength(Buffer,0);


  EnableUser();
end;

////////////////////////////////////////////////////////////////////////////////
/// Funktionen des CHS-Kalkulators
////////////////////////////////////////////////////////////////////////////////

procedure TfmMain.btCHSResetClick(Sender: TObject);
begin
  seCHSHeads.Value  :=32;
  seCHSSectors.Value:=63;

  //Und neu berechnen
  edCHSSizeChange(Self);
end;

procedure TfmMain.edCHSSizeChange(Sender: TObject);
var
  a : unsigned64;
  b : unsigned32;
  c : unsigned32;
begin
  if (cbDrives.ItemIndex>=0) then
    begin
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          a:=0;
          b:=seCHSHeads.Value;
          c:=seCHSSectors.Value;

          if (GetCHSParameter(Size,a,b,c,SectorSize)=TRUE) then
            begin
              lbCHSStatus.Caption:='OK';
            end
          else
            begin
              lbCHSStatus.Caption:='inconsistent';
            end;

          if (unsigned32(seCHSCylinder.Value) <> a ) then seCHSCylinder.Value:=a;
          if (unsigned32(seCHSHeads.Value)    <> b ) then seCHSHeads.Value:=b;
          if (unsigned32(seCHSSectors.Value)  <> c ) then seCHSSectors.Value:=c;
        end;
      end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Funktionen des CRC-Checkers
////////////////////////////////////////////////////////////////////////////////

procedure TfmMain.btCRCClick(Sender: TObject);
var
  Buffer   : array of Byte;
  u32Read  : unsigned32;
  u32Burst : unsigned32;
  u64Sector: unsigned32;
  MD5      : TMD5;
  sMD5     : longstring;
begin
  DisableUser();
  btBreak.Show();
  lbReadLog.Clear();
  if (cbDrives.ItemIndex>=0) then
    begin
      with TDiskIO(cbDrives.Items.Objects[cbDrives.ItemIndex]) do
        begin
          lbCRCLog.Items.Add(Format('reading device%d',[DeviceNumber]));
          //BurstMode
          if (cbReadBurstMode.Checked) then
            begin
              u32Burst := seReadBurstMode.Value;
            end
          else
            begin
              u32Burst := 1;
            end;

          MD5:=TMD5.create();
          MD5.init();

          //Buffer Setzen
          SetLength(Buffer, (SectorSize * u32Burst) );
          lbCRCLog.Items.Add(Format('sector buffer set to %d',[u32Burst]));
          lbCRCLog.Items.Add(Format('%d sectors found',[SectorCount]));

          //Und los
          u64Sector:=0;
          while (u64Sector < SectorCount) and (bBusy) do
             begin
               if (Read(u64Sector,Addr(Buffer[0]),u32Burst,u32Read)=TRUE) then
                 begin
                   pbCRC.Position:=signed32(trunc(u64Sector / SectorCount * 1024));

                   //Prüfsumme bauen
                   MD5.add(Addr(Buffer[0]),u32Read * SectorSize);

                   //Um die gelesenen Sekotoren vorschieben
                   inc(u64Sector,u32Read);
                 end
               else
                 begin
                   lbCRCLog.Items.Add(Format('error reading sector  %d',[u64Sector]));
                   inc(u64Sector,1);
                 end;

               lbCRCSpeed.Caption:=Format('%f',[Speed]);
               Application.ProcessMessages();
             end;

          sMD5:=MD5.Finalize().sChecksum;
          lbCRCLog.Items.Add(Format('%d sectors read ',[u64Sector]));
          lbCRCLog.Items.Add(Format('MD5 : %s ',[sMD5]));
          lbCRCLog.Items.Add('done');

          MD5.Free();
        end;
    end
  else
    begin
      lbCRCLog.Items.Add('no device selected');
    end;
  //Speicher freigeben
  SetLength(Buffer,0);
  EnableUser();
end;


//Polling Timer um einen Devicewechsel nicht zu verpassen
procedure TfmMain.tiRememberRefreshTimer(Sender: TObject);
var
  Dummy : TMESSAGE;
begin
  Self.DeviceChange(Dummy);
end;

//Abbruchbutton
procedure TfmMain.btBreakClick(Sender: TObject);
begin
  bBusy:=FALSE;
end;

procedure TfmMain.miRefreshClick(Sender: TObject);
var
  Dummy : TMESSAGE;
begin
  Self.DeviceChange(Dummy);
end;



end.
