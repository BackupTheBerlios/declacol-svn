object Form1: TForm1
  Left = 720
  Top = 340
  BorderStyle = bsToolWindow
  Caption = 'Form1'
  ClientHeight = 170
  ClientWidth = 204
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object IM_Test: TImage
    Left = 8
    Top = 8
    Width = 192
    Height = 64
    Stretch = True
  end
  object Button1: TButton
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Teststring'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Bars'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Pillars'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 120
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Sysinfo 1'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 120
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Sysinfo 2'
    TabOrder = 4
    OnClick = Button5Click
  end
end
