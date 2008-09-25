object Form1: TForm1
  Left = 510
  Top = 404
  BorderStyle = bsToolWindow
  Caption = 'VMDK-Creator / Borg@Sven-of-Nine.de'
  ClientHeight = 65
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btGo: TButton
    Left = 0
    Top = 40
    Width = 73
    Height = 25
    Caption = 'Go'
    TabOrder = 0
    OnClick = btGoClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 249
    Height = 33
    Lines.Strings = (
      'creates vmdk-files from flat hdd-images. Click "go", '
      'select target and wait...')
    TabOrder = 1
  end
  object btExit: TButton
    Left = 176
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 2
    OnClick = btExitClick
  end
  object dgOpen: TOpenDialog
    Filter = 'Raw Files|*.bin;*.raw;*.img;*.dsk;*-flat.vmdk|All Files|*.*'
    Left = 88
    Top = 32
  end
end
