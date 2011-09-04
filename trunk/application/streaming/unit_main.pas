unit unit_main;

interface

uses
  unit_typedefs,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,class_ini,unit_processfunctions, ExtCtrls, Menus;

type
  TForm1 = class(TForm)
    lbChannel: TListBox;
    btView: TButton;
    tiNoFreeze: TTimer;
    pnBusy: TPanel;
    pmEdit: TPopupMenu;
    pmEditChannel: TMenuItem;
    pmRemoveChannel: TMenuItem;
    N1: TMenuItem;
    pmChannelAdd: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure lbChannelClick(Sender: TObject);
    procedure btViewClick(Sender: TObject);
    procedure tiNoFreezeTimer(Sender: TObject);
    procedure pmRemoveChannelClick(Sender: TObject);
    procedure pmEditChannelClick(Sender: TObject);
    procedure pmChannelAddClick(Sender: TObject);
    procedure lbChannelDblClick(Sender: TObject);
  private
    { Private-Deklarationen }

    ini  : TIni;
    sVLC : string;
    sCMD : string;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  bLoaded : boolean = FALSE;

implementation

uses unit_edit;

{$R *.dfm}

procedure TForm1.FormActivate(Sender: TObject);
var
  slChannels : TStringlist;
begin
  if (not bLoaded) then
    begin
      ini:=TIni.Create();
      ini.LoadFromFile();

      slChannels:=TStringList.Create();
      ini.ReadKeys('channels',slChannels);
      slChannels.Sort();
      lbChannel.Items.AddStrings(slChannels);
      slChannels.Destroy();

      //zusaetzliche parameter fuer vlc
      ini.Read('options','cmd',' ',sCMD);

      if (ini.read('options','vlc','vlc.exe',sVLC)=FALSE) then
        begin
          showmessage('vlc-path not set');
        end;

      sVLC:=expandfilename(sVLC);

      if (fileexists(sVLC)=FALSE) then
        begin
          showmessage('wrong vlc path');
        end;

      //ini-file erzwingen
      ini.SaveToFile();

      fmEditChannel.sVLC:=sVLC;
      fmEditChannel.sCMD:=sCMD;

      bLoaded:=TRUE;
    end;
end;

procedure TForm1.lbChannelClick(Sender: TObject);
begin
  if (lbChannel.ItemIndex >= 0 ) then
    begin
      btView.enabled:=TRUE;
    end
  else
    begin
      btView.enabled:=FALSE;
    end;
end;

procedure TForm1.btViewClick(Sender: TObject);
var
  sURL : string;
begin

  if (ini.Read('channels',lbChannel.Items[lbChannel.ItemIndex],'?',sURL)=TRUE) then
    begin
      pnBusy.Visible:=TRUE;
      application.Minimize();
      execute(sVLC,sCMD + ' ' + sURL,TRUE);
      pnBusy.Visible:=FALSE;
      application.Restore();
    end
  else
    begin
      showmessage('channel-url not found');
    end;
end;

procedure TForm1.pmChannelAddClick(Sender: TObject);
begin
  //daten uebergeben
  fmEditChannel.sChannel:='newchannel';
  fmEditChannel.sURL:='?';

  //edit
  fmEditChannel.bChanged:=FALSE;
  fmEditChannel.ShowModal();

  if (fmEditChannel.bChanged = TRUE)then
    begin
      //write new one
      ini.Write('channels',fmEditChannel.sChannel,fmEditChannel.sURL);

      lbChannel.Items.Add(fmEditChannel.sChannel);
      lbChannel.Sorted:=TRUE;

      //save data
      ini.SaveToFile();
    end;

  //set position
  lbChannel.ItemIndex:=lbChannel.Items.IndexOf(fmEditChannel.sChannel);
  lbChannelClick(self);
end;


procedure TForm1.pmEditChannelClick(Sender: TObject);
begin
  if (lbChannel.ItemIndex >= 0) then
    begin
      //daten uebergeben
      fmEditChannel.sChannel:=lbChannel.Items[lbChannel.ItemIndex];
      ini.Read('channels',lbChannel.Items[lbChannel.ItemIndex],'?',fmEditChannel.sURL);

      //edit
      fmEditChannel.bChanged:=FALSE;
      fmEditChannel.ShowModal();

      if (fmEditChannel.bChanged = TRUE ) then
        begin
          //remove old one
          ini.DelKey('channels',lbChannel.Items[lbChannel.ItemIndex]);

          //write new one
          ini.Write('channels',fmEditChannel.sChannel,fmEditChannel.sURL);

          //save data
          ini.SaveToFile();
          lbChannel.Items[lbChannel.ItemIndex]:=fmEditChannel.sChannel;

          //set position
          lbChannel.ItemIndex:=lbChannel.Items.IndexOf(fmEditChannel.sChannel);
        end;
    end;
  lbChannelClick(self);
end;

procedure TForm1.pmRemoveChannelClick(Sender: TObject);
var
  u32Temp : unsigned32;
begin
  if (lbChannel.ItemIndex >= 0) then
    begin
      u32Temp:=lbChannel.ItemIndex;

      ini.DelKey('channels',lbChannel.Items[lbChannel.ItemIndex]);

      //save data
      ini.SaveToFile();
      lbChannel.Items.Delete(u32Temp);

      if (lbChannel.Items.Count > 0) then
        begin
          if (u32Temp > 0) then
            begin
              lbChannel.ItemIndex:=u32Temp - 1;
            end
          else
            begin
              lbChannel.ItemIndex:=0;
            end
        end;
    end;
  lbChannelClick(self);
end;

procedure TForm1.tiNoFreezeTimer(Sender: TObject);
begin
  Application.ProcessMessages();
end;

procedure TForm1.lbChannelDblClick(Sender: TObject);
begin
  btViewClick(Self);
end;

end.
