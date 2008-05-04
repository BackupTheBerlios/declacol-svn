object FM_PausenClown: TFM_PausenClown
  Left = 447
  Top = 407
  BorderStyle = bsNone
  Caption = 'FM_PausenClown'
  ClientHeight = 49
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 273
    Height = 49
    TabOrder = 0
    object LB_Status: TLabel
      Left = 8
      Top = 8
      Width = 3
      Height = 13
    end
    object PB_PausenClown: TProgressBar
      Left = 11
      Top = 25
      Width = 254
      Height = 16
      Min = 0
      Max = 100
      Smooth = True
      TabOrder = 0
    end
  end
  object TI_PausenClown: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TI_PausenClownTimer
    Left = 240
    Top = 16
  end
end
