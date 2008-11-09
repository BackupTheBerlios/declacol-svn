unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,unit_typedefs, ExtCtrls, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    TreeView1: TTreeView;
    cbEnable: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  bOn  : Boolean;

implementation

{$R *.dfm}

type
  PMyEnumParam = ^TMyEnumParam;
  TMyEnumParam = record
    Nodes: TTreeNodes;
    Current: TTreeNode;
  end;

function EnumWindowsProc(Wnd: HWND; Param: PMyEnumParam): BOOL; stdcall;
const
  MyMaxName = 64;
  MyMaxText = 64;
var
  ParamChild: TMyEnumParam;
  ClassName: string;
  WindowText: string;
begin
  Result := True;
  SetLength(ClassName, MyMaxName);
  SetLength(ClassName, GetClassName(Wnd, PChar(ClassName), MyMaxName));
  SetLength(WindowText, MyMaxText);
  SetLength(WindowText, SendMessage(Wnd, WM_GETTEXT, MyMaxText, lParam(PChar(WindowText))));
  ParamChild.Nodes   := Param.Nodes;

  if (IsWindowEnabled(Wnd)) then
    begin
      ParamChild.Current := Param.Nodes.AddChildObject(Param.Current,
      '[' + ClassName + '] On  "' + WindowText + '"' + ' Handle: ' + IntToStr(Wnd), Pointer(Wnd));
    end
  else
    begin
      ParamChild.Current := Param.Nodes.AddChildObject(Param.Current,
      '[' + ClassName + '] Off "' + WindowText + '"' + ' hWnd : ' + IntToStr(Wnd), Pointer(Wnd));
    end;

  if (bOn) then
    begin
      EnableWindow(Wnd,TRUE);
    end;
      if (ismenu(Wnd)) then
        begin
          MessageBox(0,'a','a',MB_OK);
        end;

  EnumChildWindows(Wnd, @EnumWindowsProc, lParam(@ParamChild));
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  Param: TMyEnumParam;
begin
  bOn:=Self.cbEnable.Checked;

  Param.Nodes := TreeView1.Items;
  Param.Current := TreeView1.TopItem;
  TreeView1.Items.BeginUpdate;
  EnumWindows(@EnumWindowsProc, lParam(@Param));
  TreeView1.Items.EndUpdate;
end;

end.
