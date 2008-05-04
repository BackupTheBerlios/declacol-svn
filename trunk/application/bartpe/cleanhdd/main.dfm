object Form1: TForm1
  Left = 327
  Top = 280
  BorderStyle = bsToolWindow
  Caption = 'Tempclean (c) 2005 Borg@Sven-of-Nine.de'
  ClientHeight = 256
  ClientWidth = 458
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 457
    Height = 233
    Caption = 'Temp Directories'
    TabOrder = 0
    object CLB_TempDirs: TCheckListBox
      Left = 8
      Top = 16
      Width = 441
      Height = 177
      ItemHeight = 13
      TabOrder = 0
    end
    object BT_Rescan: TButton
      Left = 8
      Top = 200
      Width = 75
      Height = 25
      Caption = 'Rescan'
      TabOrder = 1
      OnClick = BT_RescanClick
    end
    object BT_Exit: TButton
      Left = 376
      Top = 200
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 2
      OnClick = BT_ExitClick
    end
    object BT_CleanUp: TButton
      Left = 88
      Top = 200
      Width = 75
      Height = 25
      Caption = 'Cleanup'
      TabOrder = 3
      OnClick = BT_CleanUpClick
    end
  end
end
