object fmMain: TfmMain
  Left = 248
  Top = 169
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'DeviceChecker v0.1'
  ClientHeight = 346
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 185
    Height = 217
    Caption = 'Mounted Devices'
    TabOrder = 0
    object cbDrives: TCheckListBox
      Left = 8
      Top = 16
      Width = 169
      Height = 193
      ItemHeight = 13
      TabOrder = 0
    end
    object pbProgress: TProgressBar
      Left = 8
      Top = 192
      Width = 169
      Height = 16
      Max = 1024
      Smooth = True
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 192
    Top = 0
    Width = 257
    Height = 129
    Caption = 'Mode'
    TabOrder = 1
    object Label1: TLabel
      Left = 80
      Top = 106
      Width = 45
      Height = 13
      Caption = 'buffersize'
    end
    object rbCRC: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'check crc'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbSector00: TRadioButton
      Left = 8
      Top = 32
      Width = 161
      Height = 17
      Caption = 'fill and check sector #00'
      Enabled = False
      TabOrder = 1
    end
    object rbSectorFF: TRadioButton
      Left = 8
      Top = 48
      Width = 161
      Height = 17
      Caption = 'fill and check sector #FF'
      Enabled = False
      TabOrder = 2
    end
    object rbSectorAA: TRadioButton
      Left = 8
      Top = 64
      Width = 161
      Height = 17
      Caption = 'fill and check sector #AA'
      Enabled = False
      TabOrder = 3
    end
    object cbEndless: TCheckBox
      Left = 8
      Top = 80
      Width = 97
      Height = 17
      Caption = 'endless'
      TabOrder = 4
    end
    object Burst: TEdit
      Left = 8
      Top = 104
      Width = 49
      Height = 17
      AutoSize = False
      ReadOnly = True
      TabOrder = 5
      Text = '1'
    end
    object UpDown: TUpDown
      Left = 56
      Top = 104
      Width = 16
      Height = 17
      Min = 1
      Max = 8192
      Increment = 32
      Position = 1
      TabOrder = 6
      OnChangingEx = UpDownChangingEx
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 216
    Width = 449
    Height = 129
    Caption = 'Log'
    TabOrder = 2
    object lbLog: TListBox
      Left = 8
      Top = 16
      Width = 433
      Height = 105
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object GroupBox4: TGroupBox
    Left = 192
    Top = 136
    Width = 257
    Height = 81
    Caption = 'Commands'
    TabOrder = 3
    object btRefresh: TButton
      Left = 8
      Top = 16
      Width = 105
      Height = 25
      Caption = 'Refresh Devicelist'
      TabOrder = 0
      OnClick = btRefreshClick
    end
    object btStart: TButton
      Left = 8
      Top = 48
      Width = 105
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = btStartClick
    end
    object btStop: TButton
      Left = 8
      Top = 48
      Width = 105
      Height = 25
      Caption = 'Stop'
      TabOrder = 2
      Visible = False
      OnClick = btStopClick
    end
  end
  object tiRefresh: TTimer
    OnTimer = tiRefreshTimer
    Left = 24
    Top = 256
  end
end
