object Form1: TForm1
  Left = 195
  Top = 106
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'Form1'
  ClientHeight = 352
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object gbOptions: TGroupBox
    Left = 8
    Top = 48
    Width = 361
    Height = 81
    Caption = 'Options'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 53
      Height = 13
      Caption = 'search for :'
    end
    object edSearch: TEdit
      Left = 64
      Top = 16
      Width = 241
      Height = 21
      TabOrder = 0
    end
    object rbSimpleXor: TRadioButton
      Left = 8
      Top = 40
      Width = 113
      Height = 17
      Caption = 'simple XOR'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object rbChainedXOR: TRadioButton
      Left = 8
      Top = 56
      Width = 113
      Height = 17
      Caption = 'chained XOR'
      TabOrder = 2
    end
    object cbUTF: TCheckBox
      Left = 312
      Top = 16
      Width = 41
      Height = 17
      Caption = 'UTF'
      TabOrder = 3
    end
  end
  object gbFiles: TGroupBox
    Left = 8
    Top = 0
    Width = 361
    Height = 49
    Caption = 'Files'
    TabOrder = 1
    object edSource: TEdit
      Left = 8
      Top = 16
      Width = 305
      Height = 21
      TabOrder = 0
    end
    object btBrowse: TButton
      Left = 320
      Top = 16
      Width = 35
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btBrowseClick
    end
  end
  object gbCommands: TGroupBox
    Left = 8
    Top = 296
    Width = 361
    Height = 49
    Caption = 'Command'
    TabOrder = 2
    object btGo: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Go'
      TabOrder = 0
      OnClick = btGoClick
    end
    object pbProgress: TProgressBar
      Left = 88
      Top = 16
      Width = 270
      Height = 25
      Smooth = True
      TabOrder = 1
    end
  end
  object mmLog: TMemo
    Left = 8
    Top = 136
    Width = 361
    Height = 161
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object odInput: TOpenDialog
    Left = 16
    Top = 144
  end
end
