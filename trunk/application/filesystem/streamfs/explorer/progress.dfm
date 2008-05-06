object fmProgress: TfmProgress
  Left = 6
  Top = 883
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'busy'
  ClientHeight = 50
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbInfo: TLabel
    Left = 8
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Defrag'
  end
  object pbProgress: TProgressBar
    Left = 8
    Top = 24
    Width = 321
    Height = 16
    Min = 0
    Max = 100
    Smooth = True
    TabOrder = 0
  end
end
