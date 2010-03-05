object fmMain: TfmMain
  Left = 369
  Top = 200
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'StayBehind'
  ClientHeight = 50
  ClientWidth = 104
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClick = FormActivate
  OnDblClick = FormActivate
  OnHide = FormActivate
  OnMouseDown = FormMouseDown
  OnPaint = FormActivate
  OnResize = FormActivate
  OnShow = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu1: TPopupMenu
    Left = 32
    Top = 16
    object Black1: TMenuItem
      Caption = 'Black'
      OnClick = Black1Click
    end
    object Blue1: TMenuItem
      Caption = 'Blue'
      OnClick = Blue1Click
    end
    object Green1: TMenuItem
      Caption = 'Green'
      OnClick = Green1Click
    end
    object Red1: TMenuItem
      Caption = 'Red'
      OnClick = Red1Click
    end
    object Silver1: TMenuItem
      Caption = 'Silver'
      OnClick = Silver1Click
    end
    object White1: TMenuItem
      Caption = 'White'
      OnClick = White1Click
    end
    object N1: TMenuItem
      Caption = '_____'
    end
    object Exit1: TMenuItem
      Caption = 'Exit'
      OnClick = Exit1Click
    end
  end
end
