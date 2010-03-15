object fmMain: TfmMain
  Left = 331
  Top = 178
  BorderStyle = bsToolWindow
  Caption = 'Main'
  ClientHeight = 386
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbLog: TGroupBox
    Left = 0
    Top = 200
    Width = 489
    Height = 185
    Caption = 'Log'
    TabOrder = 2
    object mmLog: TListBox
      Left = 8
      Top = 16
      Width = 473
      Height = 161
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object gbCommand: TGroupBox
    Left = 352
    Top = 0
    Width = 137
    Height = 201
    Caption = 'Commands'
    TabOrder = 0
    object cbUseHeuristic: TCheckBox
      Left = 8
      Top = 32
      Width = 121
      Height = 17
      Caption = 'use heuristic'
      TabOrder = 0
    end
    object cbShowFiles: TCheckBox
      Left = 8
      Top = 16
      Width = 121
      Height = 17
      Caption = 'show scanned files'
      TabOrder = 1
    end
    object cbIgnorePackedFiles: TCheckBox
      Left = 8
      Top = 48
      Width = 121
      Height = 17
      Caption = 'ignore packed files'
      TabOrder = 2
    end
    object cbScanArchives: TCheckBox
      Left = 8
      Top = 64
      Width = 121
      Height = 17
      Caption = 'scan archives'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cbScanBootsector: TCheckBox
      Left = 8
      Top = 80
      Width = 121
      Height = 17
      Caption = 'scan bootsector'
      TabOrder = 4
    end
    object cbDisinfect: TCheckBox
      Left = 8
      Top = 96
      Width = 121
      Height = 17
      Caption = 'disinfect files'
      TabOrder = 5
    end
    object cbRename: TCheckBox
      Left = 8
      Top = 112
      Width = 121
      Height = 17
      Caption = 'rename infected files'
      TabOrder = 6
    end
    object btUpdate: TButton
      Left = 8
      Top = 136
      Width = 121
      Height = 25
      Caption = 'run update'
      TabOrder = 7
      OnClick = btUpdateClick
    end
    object btScan: TButton
      Left = 8
      Top = 168
      Width = 121
      Height = 25
      Caption = 'scan'
      TabOrder = 8
      OnClick = btScanClick
    end
  end
  object Panel1: TPanel
    Left = 360
    Top = 16
    Width = 121
    Height = 177
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object lbinfected: TLabel
      Left = 0
      Top = 0
      Width = 44
      Height = 13
      Caption = 'infected :'
    end
    object lbinfectedcount: TLabel
      Left = 48
      Top = 0
      Width = 33
      Height = 13
      Caption = '0'
    end
    object btCancel: TButton
      Left = 0
      Top = 152
      Width = 121
      Height = 25
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = btCancelClick
    end
    object Animate1: TAnimate
      Left = 8
      Top = 40
      Width = 105
      Height = 73
      AutoSize = False
      CommonAVI = aviFindFolder
      StopFrame = 29
    end
  end
  object gbTarget: TGroupBox
    Left = 0
    Top = 0
    Width = 345
    Height = 201
    Caption = 'Target'
    TabOrder = 1
    object DriveComboBox1: TDriveComboBox
      Left = 8
      Top = 16
      Width = 329
      Height = 19
      DirList = dlbMain
      TabOrder = 0
    end
    object dlbMain: TDirectoryListBox
      Left = 8
      Top = 40
      Width = 329
      Height = 153
      ItemHeight = 16
      TabOrder = 1
    end
  end
  object DosCom: TDosCommand
    CommandLine = 
      'C:\DevDelphi\Projects\Selfmade\application\dingoo\language_compi' +
      'ler\language_compiler.exe'
    OnNewLine = DosComNewLine
    OnTerminated = DosComTerminated
    InputToOutput = False
    MaxTimeAfterBeginning = 0
    MaxTimeAfterLastOutput = 0
    Left = 16
    Top = 224
  end
end
