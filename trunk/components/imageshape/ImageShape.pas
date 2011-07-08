unit ImageShape;

interface

uses
  Messages, Windows, SysUtils, Classes, Controls, Forms, Menus, Graphics,
  StdCtrls,ExtCtrls;
type
  TImageShape = class(TGraphicControl)
  private
    move:Tpoint;
    moveflag:boolean;
    FMovable:boolean;
    FPicture: TPicture;
    FOnProgress: TProgressEvent;
    FStretch: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FDrawing: Boolean;
    FProportional: Boolean;
    lrgn: Hrgn;
    function GetCanvas: TCanvas;
    procedure PictureChanged(Sender: TObject);
    procedure SetCenter(Value: Boolean);
    procedure SetPicture(Value: TPicture);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetProportional(Value: Boolean);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect: TRect;
    function DoPaletteChange: Boolean;
    function GetPalette: HPALETTE; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure Paint; override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Center: Boolean read FCenter write SetCenter default False;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
    property PopupMenu;
    property Proportional: Boolean read FProportional write SetProportional default false;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnStartDock;
    property OnStartDrag;
    property Movable:boolean read FMovable write FMovable;
  end;
procedure Register;
function GetFrgn(dc1:HDC;clientrect,destrect:Trect):HRGN;
implementation
uses Consts, Dialogs;
procedure TImageShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
        inherited MouseDown(Button,Shift,X,Y);
        moveflag:=true;
        getcursorpos(move);
end;
procedure TImageShape.MouseMove(Shift: TShiftState; X, Y: Integer);
var
        pos:Tpoint;
begin
        inherited MouseMove(Shift,X,Y);
        if(moveflag)and(FMovable)then
        begin
                getcursorpos(pos);
                parent.Left:=parent.Left+(pos.x-move.x);
                parent.Top:=parent.Top+(pos.y-move.y);
                move:=pos;
        end;
end;
procedure TImageShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
        inherited MouseUp(Button,Shift,X,Y);
        moveflag:=false;
end;
function GetFrgn(dc1:HDC;clientrect,destrect:Trect):HRGN;
var
        x,y,lx:integer;
        rgn,rgn2:hrgn;
        rec,rec1:Trect;
        dc:HDC;
        bkcolor:colorref;
begin
        dc:=Createcompatibledc(dc1);
        getclipbox(dc1,rec1);
        bkcolor:=getpixel(dc1,rec1.Left,rec1.Bottom-1);
        SelectObject(dc,
        CreateCompatibleBitmap(dc1,clientrect.Right-clientrect.Left,clientrect.Bottom-clientrect.Top)
        );
        StretchBlt(dc,destrect.Left,destrect.Top,
                   destrect.Right-destrect.Left,
                   destrect.Bottom-destrect.Top,
                   dc1,0,0,rec1.Right-rec1.Left,
                   rec1.Bottom-rec1.Top,
                   SRCCOPY);
        getclipbox(dc,rec);
        rgn:=CreateRectrgn(0,0,0,0);
        for y:=rec.Top to rec.Bottom  do
        begin
                x:=rec.Left;
                while(x<=rec.Right)do
                begin
                        lx:=x;
                        while((getpixel(dc,x,y)=bkcolor)and(x<=rec.Right)) do
                                inc(x);
                        combinergn(rgn,rgn,createrectrgn(lx,y,x,y+1),RGN_OR);
                        while((getpixel(dc,x,y)<>bkcolor)and(x<=rec.Right)) do
                                inc(x);

                end;
        end;
        rgn2:=CreateRectRgn(0,0,0,0);
        combinergn(rgn2,CreateRectRgnIndirect(rec),CreateRectRgnIndirect(destrect)
                   ,RGN_AND);
        combinergn(rgn,rgn2,rgn,RGN_DIFF);
        Deletedc(dc);
        GetFrgn:=rgn;
end;

constructor TImageShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMovable:=true;
  moveflag:=false;
  ControlStyle := ControlStyle + [csReplicatable];
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FPicture.OnProgress := Progress;
  Height := 105;
  Width := 105;
  moveflag:=false;
  lrgn:=CreateRectRgn(0,0,0,0);
end;

destructor TImageShape.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TImageShape.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
	Result := FPicture.Graphic.Palette;
end;

function TImageShape.DestRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := Picture.Width;
  h := Picture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
	if Proportional and (w > 0) and (h > 0) then
	begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if Center then
	OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;

procedure TImageShape.Paint;
var
  Save: Boolean;
begin
  if csDesigning in ComponentState then
	with inherited Canvas do
	begin
	  Pen.Style := psDash;
	  Brush.Style := bsClear;
	  Rectangle(0, 0, Width, Height);
	end;
  Save := FDrawing;
  FDrawing := True;
  try
	with inherited Canvas do
        begin
	  StretchDraw(DestRect, Picture.Graphic);
        end;
  finally
	FDrawing := Save;
  end;
end;

function TImageShape.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := Picture.Graphic;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
	(Tmp.PaletteModified) then
  begin
	if (Tmp.Palette = 0) then
	  Tmp.PaletteModified := False
	else
	begin
	  ParentForm := GetParentForm(Self);
	  if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
	  begin
		if FDrawing then
		  ParentForm.Perform(wm_QueryNewPalette, 0, 0)
		else
		  PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
		Result := True;
		Tmp.PaletteModified := False;
	  end;
	end;
  end;
end;

procedure TImageShape.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
  begin
	if DoPaletteChange then Update
	else Paint;
  end;
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

function TImageShape.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if Picture.Graphic = nil then
  begin
	Bitmap := TBitmap.Create;
	try
	  Bitmap.Width := Width;
	  Bitmap.Height := Height;
	  Picture.Graphic := Bitmap;
	finally
	  Bitmap.Free;
	end;
  end;
  if Picture.Graphic is TBitmap then
	Result := TBitmap(Picture.Graphic).Canvas
  else
	raise EInvalidOperation.Create(SImageCanvasNeedsBitmap);
end;

procedure TImageShape.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
	FCenter := Value;
	PictureChanged(Self);
  end;
end;

procedure TImageShape.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TImageShape.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
	FStretch := Value;
	PictureChanged(Self);
  end;
end;

procedure TImageShape.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
	FTransparent := Value;
	PictureChanged(Self);
  end;
end;

procedure TImageShape.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
	FProportional := Value;
	PictureChanged(Self);
  end;
end;

procedure TImageShape.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  prgn,wrgn:hrgn;
  exxoffset,exyoffset:integer;
  rec1,rec2:Trect;
  p:Tcanvas;
begin
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
	SetBounds(Left, Top, Picture.Width, Picture.Height);
  G := Picture.Graphic;
  if G <> nil then
  begin
	if not ((G is TMetaFile) or (G is TIcon)) then
	  G.Transparent := FTransparent;
	if (not G.Transparent) and Stretch and not Proportional then
	  ControlStyle := ControlStyle + [csOpaque]
	else  // picture might not cover entire clientrect
	  ControlStyle := ControlStyle - [csOpaque];
	if DoPaletteChange and FDrawing then
        begin
                  Update;
        end;
  end
  else ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then Invalidate;
  if ((G<>nil)and(not (csDesigning in ComponentState))) then
  begin
                          p:=Tcanvas.Create;
                          p.Handle:=CreateCompatibleDc(getdc(0));
                          SelectObject(p.Handle,CreateCompatibleBitmap(getdc(0),G.Width,G.Height));
                          p.Draw(0,0,G);
                          prgn:=getFrgn({self.picture.bitmap.canvas.handle}p.Handle,self.ClientRect,destrect);
                          p.Destroy;
                          windows.GetClientRect(parent.Handle,rec1);
                          windows.GetWindowRect(parent.Handle,rec2);
                          exxoffset:=(rec2.Right-rec1.Right+rec1.Left-rec2.Left)div 2;
                          exyoffset:=(rec2.Bottom-rec1.Bottom+rec1.Top-rec2.Top)-exxoffset;
                          offsetrgn(prgn,self.Left+exxoffset,self.Top+exyoffset);
                          if(GetEnvironmentvariable('vadim')='set')then
                          begin
                                  wrgn:=CreateRectRgn(0,0,0,0);
                                  GetWindowRgn(parent.Handle,wrgn);
                                  combinergn(wrgn,wrgn,lrgn,RGN_DIFF);
                                  combinergn(lrgn,prgn,0,RGN_COPY);
                                  combinergn(prgn,wrgn,prgn,RGN_OR);
                          end
                          else
                          begin
                                combinergn(lrgn,prgn,0,RGN_COPY);
                                SetEnvironmentVariable('vadim','set');
                          end;
                          Setwindowrgn(parent.Handle,prgn,true);
  end;
end;
function TImageShape.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and
    (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height;
  end;
end;

procedure Register;
begin
  RegisterComponents('Shape', [TImageShape]);
end;

end.
