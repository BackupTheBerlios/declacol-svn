object Form1: TForm1
  Left = 447
  Top = 247
  BorderStyle = bsToolWindow
  Caption = 'TorSwitch'
  ClientHeight = 168
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Switch'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 377
    Height = 121
    TabOrder = 1
  end
  object cbAutoswitch: TCheckBox
    Left = 160
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Autoswitch'
    TabOrder = 2
  end
  object ClientSocket: TClientSocket
    Active = False
    ClientType = ctNonBlocking
    Port = 0
    OnConnect = ClientSocketConnect
    OnRead = ClientSocketRead
    OnError = ClientSocketError
    Left = 96
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 320
    Top = 8
  end
end
