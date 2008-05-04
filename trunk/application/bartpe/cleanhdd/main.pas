unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst,Unit_FileFunctions, ExtCtrls,Unit_StringFunctions,
  ComCtrls;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    CLB_TempDirs: TCheckListBox;
    BT_Rescan: TButton;
    BT_Exit: TButton;
    BT_CleanUp: TButton;
    procedure BT_RescanClick(Sender: TObject);
    procedure BT_ExitClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BT_CleanUpClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function ScanForTempDirs():TStringList;
    function CleanDir(Startpath:String):Boolean;
    function SubTempScan(StartPath,Filter:String;Hide:String;SubDir:integer):TStringList;
    function DirEmpty(Startpath:String):Boolean;
    procedure StartPausenClown(Text:String);
    procedure StopPausenClown ();

  end;


var
  Form1: TForm1;
  
implementation

uses progress;
{$R *.DFM}


////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.StartPausenClown(Text:String);
begin
     With (FM_PausenClown) do
          begin
               Left:=Self.Left + (Self.Width  shr 1) - (Width  shr 1);
               Top :=Self.Top  + (Self.Height shr 1) - (Height shr 1);
               LB_Status.Caption:=Text;
               Show;
          end;
end;
procedure TForm1.StopPausenClown();
begin
     FM_PausenClown.Hide;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
function Tform1.DirEmpty(Startpath:String):Boolean;
var
   SRF     : TSearchRec;
   OK      : Integer;
   s       : String;
begin
     Result:=TRUE;
     try
     //Und nun die Verzeichnisse durchsuchen ?
     OK:=FindFirst(Startpath+'*.*',faANYFILE,SRF);
     while (OK=0) do
           begin
                //Was ausser den Traversen drin ?
                if (SRF.Name[1]<>'.') then
                   begin
                        OK:=-1;
                        Result:=FALSE;
                   end
                else
                   begin
                        OK:=FindNext(SRF);
                   end;
           end;
        FindClose(SRF);
     except
     end;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
function TForm1.SubTempScan(StartPath,Filter:String;Hide:String;SubDir:integer):TStringList;
var
   SRF     : TSearchRec;
   OK      : Integer;
   s       : String;
begin
     //Vorbereitungen
     Result:=TStringlist.Create;
     Result.Clear;
     Startpath:=AddBackSlash(Startpath);

     //Erstmal alle Verzeichnisse holen
     OK:=FindFirst(Startpath+Filter,faANYFILE,SRF);
     while (OK=0) do
           begin
                if (SRF.Name[1]<>Hide) then
                   begin
                        if (SRF.Name[1]<>'.') then
                           begin
                                if ((SRF.Attr and faDirectory) <> faDirectory) then
                                   begin
//                                        Result.Add(StartPath+SRF.Name);
                                   end
                                else
                                   begin
                                        s:=AddBackSlash(Startpath+SRF.Name);
                                        Result.Add(s);
                                   end;
                           end;
                   end;
                Application.ProcessMessages;
                OK:=FindNext(SRF);
           end;
     FindClose(SRF);

     //Und nun die Verzeichnisse durchsuchen ?
     if (SubDir>0) then
        begin
             OK:=FindFirst(Startpath+'*.*',faANYFILE,SRF);
             while (OK=0) do
                   begin
                        if (SRF.Name[1]<>Hide) then
                           begin
                                if (SRF.Name[1]<>'.') then
                                   begin
                                        if ((SRF.Attr and faDirectory) = faDirectory) then
                                           begin
                                                s:=AddBackSlash(Startpath+SRF.Name);
                                                Result.AddStrings(DirScan(s,Filter,Hide,SubDir-1));
                                           end;
                                   end;
                           end;
                        Application.ProcessMessages;
                        OK:=FindNext(SRF);
                   end;
             FindClose(SRF);
        end;
     Result.Sort;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Aus einer Verzeichnisstruktur alle Dateien löschen, auf die Filter passt. Und ggf.
//Die Verzeichnisse mitlöschen
function TForm1.CleanDir(Startpath:String):Boolean;
var
   SRF     : TSearchRec;
   OK      : Integer;
   s       : String;
begin
     Result:=TRUE;
     try
     //Und nun die Verzeichnisse durchsuchen ?
     OK:=FindFirst(Startpath+'*.*',faANYFILE,SRF);
     while (OK=0) do
           begin
                if (SRF.Name[1]<>'.') then
                //Verzeichnis ? dann Scannen
                if ((SRF.Attr and faDirectory) = faDirectory) then
                   begin
                        s:=AddBackSlash(Startpath+SRF.Name);
                        DelTree(s);
                        Result:=Result and RemoveDirectory(PChar(s));
                   end
                else
                   begin
                        //Ansonsten Dateien löschen
                        //READONLY-Attribut wird automatisch entfernt
                        DelData(StartPath+SRF.Name);
                   end;
                OK:=FindNext(SRF);
           end;
        FindClose(SRF);
     except
     end;
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Nach diversen Temp-Verzeichnissen suchen
function TForm1.ScanForTempDirs():TStringList;
var
   slDrives : TStringList;
   iCount   : Integer;
begin
     Result:=TStringList.Create;
     Result.Clear;
     Result.Sorted:=TRUE;
     Result.Duplicates:=DupIgnore;

     //Alle Festplattem holen
     slDrives:=EnumerateDrives(DRIVE_FIXED);

     while (slDrives.Count > 0) do
           begin
                StartPausenClown('scanning '+slDrives[0]);

                Application.ProcessMessages;

                //Temporäre Internet-Dateien
                Result.AddStrings(SubTempScan(slDrives[0],'Temporary Internet Files','',5));

                //Temp allgemein
                Result.AddStrings(SubTempScan(slDrives[0],'Temp','',5));

                //Mülleimer allgemein
                Result.AddStrings(SubTempScan(slDrives[0],'Recycler','',1));

                slDrives.Delete(0);
           end;

     //Jetzt nehmen wir alle raus, die leer sind
     iCount:=0;
     StartPausenClown('filtering');
     while (iCount < Result.Count) do
           begin
                //Leeres Verzeichnis ?
                if (DirEmpty(Result[iCount])) then
                   begin
                        //Dann aus der Liste nehmen
                        Result.Delete(iCount);
                        //Und neu starten
                        iCount:=-1;
                   end;

                inc(iCount);
           end;
     slDrives.Free;
end;


////////////////////////////////////////////////////////////////////////////////////////////////
//Nach Temporärern Verzeichnissen suchen
procedure TForm1.BT_RescanClick(Sender: TObject);
var
   iCount : integer;
begin
     //Verzeichnisse holen
     CLB_TempDirs.Clear;
     CLB_TempDirs.Items.AddStrings(ScanForTempDirs());

     //Alles anhaken
     iCount:=0;
     while (iCount < CLB_TempDirs.Items.Count) do
           begin
                CLB_TempDirs.Checked[iCount]:=TRUE;
                inc(iCount);
           end;
     StopPausenClown();
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Die angeklickten Verzeichnisse löschen
procedure TForm1.BT_CleanUpClick(Sender: TObject);
var
   iCount  : Integer;
begin
     //Alles anhaken
     iCount:=0;
     while (iCount < CLB_TempDirs.Items.Count) do
           begin
                CLB_TempDirs.ItemIndex:=iCount;
                Application.ProcessMessages();

                StartPausenClown('cleaning :'+CLB_TempDirs.Items[iCount]);
                //Angeforderte Dateien löschen
                if (CLB_TempDirs.Checked[iCount]) then
                   begin
                        CleanDir(CLB_TempDirs.Items[iCount]);
                   end;

                inc(iCount);
           end;
     StopPausenClown();
end;

////////////////////////////////////////////////////////////////////////////////////////////////
//Anwendung beenden
procedure TForm1.BT_ExitClick(Sender: TObject);
begin
     Self.Close;
end;



////////////////////////////////////////////////////////////////////////////////////////////////
procedure TForm1.FormActivate(Sender: TObject);
const
     bScanned : Boolean = FALSE;
begin
     //Nur einmal scannen
     if (not bScanned) then
        begin
             BT_RescanClick(Self);
             bScanned:=TRUE;
        end;
end;


end.
