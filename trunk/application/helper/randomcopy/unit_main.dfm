object Form1: TForm1
  Left = 325
  Top = 242
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'BorgCopy'
  ClientHeight = 226
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbRuntime: TLabel
    Left = 80
    Top = 200
    Width = 47
    Height = 13
    Caption = 'lbRuntime'
  end
  object btStop: TButton
    Left = 0
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    Visible = False
    OnClick = btStopClick
  end
  object gbPath: TGroupBox
    Left = 0
    Top = 0
    Width = 425
    Height = 81
    Caption = 'Pathes'
    TabOrder = 0
    object edSource: TEdit
      Left = 8
      Top = 16
      Width = 377
      Height = 21
      TabOrder = 0
      Text = 'd:\mp3\'
    end
    object edTarget: TEdit
      Left = 8
      Top = 48
      Width = 377
      Height = 21
      TabOrder = 1
      Text = 'd:\temp\'
    end
    object btBrowseSource: TButton
      Left = 392
      Top = 16
      Width = 27
      Height = 21
      Caption = '...'
      TabOrder = 2
      OnClick = btBrowseSourceClick
    end
    object btBrowseTarget: TButton
      Left = 390
      Top = 48
      Width = 27
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = btBrowseTargetClick
    end
  end
  object gbOption: TGroupBox
    Left = 0
    Top = 80
    Width = 425
    Height = 73
    Caption = 'Options'
    TabOrder = 1
    object cbSkipExistingFiles: TCheckBox
      Left = 8
      Top = 16
      Width = 105
      Height = 17
      Caption = 'skip existing files'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object rbHighPrio: TRadioButton
      Left = 312
      Top = 16
      Width = 105
      Height = 17
      Caption = 'high priority'
      TabOrder = 1
    end
    object rbLowPrio: TRadioButton
      Left = 312
      Top = 48
      Width = 105
      Height = 17
      Caption = 'low priority'
      TabOrder = 2
    end
    object rbNormalPrio: TRadioButton
      Left = 312
      Top = 32
      Width = 105
      Height = 17
      Caption = 'normal priority'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
    object cbRandom: TCheckBox
      Left = 8
      Top = 32
      Width = 113
      Height = 17
      Caption = 'randomized copy'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
  end
  object btStart: TButton
    Left = 0
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = btStartClick
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 152
    Width = 425
    Height = 41
    Caption = 'Status'
    TabOrder = 4
    object lbStatus: TLabel
      Left = 8
      Top = 16
      Width = 16
      Height = 13
      Caption = 'idle'
    end
  end
  object pbProgress: TProgressBar
    Left = 176
    Top = 200
    Width = 249
    Height = 25
    Smooth = True
    TabOrder = 5
    Visible = False
  end
  object tiStatusUpdate: TTimer
    OnTimer = tiStatusUpdateTimer
    Left = 400
    Top = 192
  end
end
