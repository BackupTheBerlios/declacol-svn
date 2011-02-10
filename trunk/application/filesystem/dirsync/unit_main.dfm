object fmMain: TfmMain
  Left = 248
  Top = 130
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'DirSync 0.1'
  ClientHeight = 449
  ClientWidth = 434
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
  object gblog: TGroupBox
    Left = 0
    Top = 136
    Width = 433
    Height = 313
    Caption = 'log'
    TabOrder = 0
    object lbLog: TListBox
      Left = 8
      Top = 16
      Width = 409
      Height = 289
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object gbpath: TGroupBox
    Left = 0
    Top = 0
    Width = 433
    Height = 73
    Caption = 'path'
    TabOrder = 1
    object lbsource: TLabel
      Left = 8
      Top = 18
      Width = 32
      Height = 13
      Caption = 'source'
    end
    object lbtarget: TLabel
      Left = 8
      Top = 42
      Width = 27
      Height = 13
      Caption = 'target'
    end
    object edsource: TEdit
      Left = 48
      Top = 16
      Width = 353
      Height = 21
      ReadOnly = True
      TabOrder = 0
      Text = 'd:\test\a'
    end
    object btbrowsesource: TButton
      Left = 400
      Top = 16
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btbrowsesourceClick
    end
    object edtarget: TEdit
      Left = 48
      Top = 40
      Width = 353
      Height = 21
      ReadOnly = True
      TabOrder = 2
      Text = 'd:\test\b'
    end
    object btbrowsetarget: TButton
      Left = 400
      Top = 40
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = btbrowsetargetClick
    end
  end
  object gboptions: TGroupBox
    Left = 0
    Top = 80
    Width = 433
    Height = 49
    Caption = 'options'
    TabOrder = 2
    object btstop: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 3
      OnClick = btstopClick
    end
    object btstart: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = btstartClick
    end
    object cblowercase: TCheckBox
      Left = 136
      Top = 10
      Width = 73
      Height = 17
      Caption = 'lowercase'
      TabOrder = 1
    end
    object cbtest: TCheckBox
      Left = 136
      Top = 28
      Width = 97
      Height = 17
      Caption = 'testrun'
      TabOrder = 2
    end
    object cbunfragged: TCheckBox
      Left = 240
      Top = 10
      Width = 97
      Height = 17
      Caption = 'no fragments'
      TabOrder = 4
    end
    object cbcrc: TCheckBox
      Left = 240
      Top = 28
      Width = 97
      Height = 17
      Caption = 'crc compare'
      TabOrder = 5
    end
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 400
    Top = 96
  end
end
