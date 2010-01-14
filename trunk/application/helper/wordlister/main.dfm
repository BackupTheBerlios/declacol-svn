object Form1: TForm1
  Left = 215
  Top = 209
  Width = 289
  Height = 259
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object gbStats: TGroupBox
    Left = 8
    Top = 8
    Width = 169
    Height = 217
    Caption = 'Statistics'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 32
      Width = 28
      Height = 13
      Caption = 'words'
    end
    object lbWordCount: TLabel
      Left = 128
      Top = 32
      Width = 33
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 13
      Height = 13
      Caption = 'file'
    end
    object lbFile: TLabel
      Left = 155
      Top = 16
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '--'
    end
    object lbLog: TListBox
      Left = 8
      Top = 96
      Width = 153
      Height = 97
      ItemHeight = 13
      TabOrder = 0
    end
    object pbProgress: TProgressBar
      Left = 8
      Top = 192
      Width = 150
      Height = 16
      Smooth = True
      TabOrder = 1
    end
  end
  object GroupBox1: TGroupBox
    Left = 184
    Top = 8
    Width = 89
    Height = 217
    Caption = 'Commands'
    TabOrder = 1
    object Button1: TButton
      Left = 6
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Add Words'
      TabOrder = 0
      OnClick = Button1Click
    end
    object btUpper: TButton
      Left = 8
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Uppercase'
      TabOrder = 1
      OnClick = btUpperClick
    end
    object bTLower: TButton
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Lowercase'
      TabOrder = 2
      OnClick = bTLowerClick
    end
    object Button4: TButton
      Left = 8
      Top = 184
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 3
      OnClick = Button4Click
    end
    object btUnique: TButton
      Left = 8
      Top = 112
      Width = 75
      Height = 25
      Caption = 'Unique'
      TabOrder = 4
      OnClick = btUniqueClick
    end
    object btClear: TButton
      Left = 8
      Top = 144
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 5
      OnClick = btClearClick
    end
  end
  object dgAddFile: TOpenDialog
    Left = 136
    Top = 168
  end
  object Timer1: TTimer
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 24
    Top = 168
  end
  object dgSaveFile: TSaveDialog
    Left = 104
    Top = 168
  end
end
