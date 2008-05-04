object fmMain: TfmMain
  Left = 304
  Top = 301
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DiskDumper (c) 2007 Borg@Sven-of-Nine.de'
  ClientHeight = 369
  ClientWidth = 713
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 184
    Top = 0
    Width = 529
    Height = 369
    ActivePage = tsDummyFiles
    TabOrder = 0
    object tsSectorViewer: TTabSheet
      Caption = 'Sector Viewer'
      OnEnter = tsSectorViewerEnter
      object pnSectorViewer: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Caption = 'pnSectorViewer'
        TabOrder = 0
        object sgHex: TStringGrid
          Left = 8
          Top = 32
          Width = 505
          Height = 297
          ColCount = 17
          DefaultColWidth = 20
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 30
          FixedRows = 0
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Fixedsys'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
          ColWidths = (
            20
            20
            20
            20
            20
            20
            20
            20
            20
            20
            20
            20
            20
            20
            20
            20
            159)
        end
        object seSector: TSpinEdit
          Left = 8
          Top = 8
          Width = 121
          Height = 22
          EditorEnabled = False
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = seSectorChange
        end
      end
    end
    object tsReader: TTabSheet
      Caption = 'Reader'
      ImageIndex = 1
      object pnReader: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 8
          Width = 41
          Height = 13
          Caption = 'Dumpfile'
        end
        object lbReadBurstMode: TLabel
          Left = 104
          Top = 82
          Width = 36
          Height = 13
          Caption = 'Sectors'
        end
        object lbReadSpeedPre: TLabel
          Left = 8
          Top = 292
          Width = 73
          Height = 13
          Caption = 'Speed [KB/s]  :'
        end
        object lbReadSpeed: TLabel
          Left = 88
          Top = 292
          Width = 45
          Height = 13
          Caption = '0             '
        end
        object pbRead: TProgressBar
          Left = 8
          Top = 312
          Width = 505
          Height = 16
          Max = 1024
          Smooth = True
          TabOrder = 5
        end
        object edTargetFile: TEdit
          Left = 8
          Top = 24
          Width = 465
          Height = 21
          TabOrder = 0
        end
        object btBrowseTarget: TButton
          Left = 480
          Top = 24
          Width = 27
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = btBrowseTargetClick
        end
        object btDump: TButton
          Left = 8
          Top = 120
          Width = 75
          Height = 25
          Caption = 'Save to File'
          TabOrder = 2
          OnClick = btDumpClick
        end
        object cbReadBurstMode: TCheckBox
          Left = 8
          Top = 56
          Width = 97
          Height = 17
          Hint = 'read more than one sector at a time'
          Caption = 'burstmode'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 3
          OnClick = cbReadBurstModeClick
        end
        object seReadBurstMode: TSpinEdit
          Left = 32
          Top = 80
          Width = 65
          Height = 22
          MaxValue = 32768
          MinValue = 1
          TabOrder = 4
          Value = 2048
        end
        object lbReadLog: TListBox
          Left = 8
          Top = 152
          Width = 505
          Height = 137
          ItemHeight = 13
          TabOrder = 6
        end
      end
    end
    object tsWriter: TTabSheet
      Caption = 'Writer'
      ImageIndex = 2
      object pnWrite: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object lbImageFIle: TLabel
          Left = 8
          Top = 8
          Width = 41
          Height = 13
          Caption = 'imagefile'
        end
        object lbWriteBurstMode: TLabel
          Left = 104
          Top = 82
          Width = 36
          Height = 13
          Caption = 'Sectors'
        end
        object lbWriteSpeedPre: TLabel
          Left = 8
          Top = 292
          Width = 73
          Height = 13
          Caption = 'Speed [KB/s]  :'
        end
        object lbWriteSpeed: TLabel
          Left = 88
          Top = 292
          Width = 45
          Height = 13
          Caption = '0             '
        end
        object edSourceFile: TEdit
          Left = 8
          Top = 24
          Width = 465
          Height = 21
          TabOrder = 0
        end
        object btBrowseSource: TButton
          Left = 480
          Top = 24
          Width = 27
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = btBrowseSourceClick
        end
        object cbWriteBurstMode: TCheckBox
          Left = 8
          Top = 56
          Width = 97
          Height = 17
          Hint = 'write more than one sector at a time'
          Caption = 'burstmode'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 2
          OnClick = cbWriteBurstModeClick
        end
        object seWriteBurstMode: TSpinEdit
          Left = 32
          Top = 80
          Width = 65
          Height = 22
          MaxValue = 32768
          MinValue = 1
          TabOrder = 3
          Value = 2048
        end
        object btWriteImage: TButton
          Left = 8
          Top = 120
          Width = 75
          Height = 25
          Caption = 'Write from File'
          TabOrder = 4
          OnClick = btWriteImageClick
        end
        object lbWriteLog: TListBox
          Left = 8
          Top = 152
          Width = 505
          Height = 137
          ItemHeight = 13
          TabOrder = 5
        end
        object pbWrite: TProgressBar
          Left = 8
          Top = 312
          Width = 505
          Height = 16
          Max = 1024
          Smooth = True
          TabOrder = 6
        end
      end
    end
    object tsProperties: TTabSheet
      Caption = 'Properties'
      ImageIndex = 3
      OnShow = tsPropertiesShow
      object pnProperties: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object mmProperties: TMemo
          Left = 8
          Top = 8
          Width = 505
          Height = 321
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Fixedsys'
          Font.Style = []
          Lines.Strings = (
            'mmProperties')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
    object tsBenchmark: TTabSheet
      Caption = 'Benchmark'
      ImageIndex = 4
      object pnBenchmark: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object mmBenchmark: TMemo
          Left = 8
          Top = 8
          Width = 505
          Height = 289
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Fixedsys'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object btStartBenchmark: TButton
          Left = 8
          Top = 304
          Width = 75
          Height = 25
          Caption = 'Start'
          TabOrder = 1
          OnClick = btStartBenchmarkClick
        end
      end
    end
    object tsCHSCalculator: TTabSheet
      Caption = 'CHS-Calculator'
      ImageIndex = 5
      object pnCHSCalculator: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object lbCHSSIze: TLabel
          Left = 8
          Top = 18
          Width = 20
          Height = 13
          Caption = 'Size'
        end
        object lbCylinder: TLabel
          Left = 8
          Top = 42
          Width = 37
          Height = 13
          Caption = 'Cylinder'
        end
        object lbHeads: TLabel
          Left = 8
          Top = 66
          Width = 31
          Height = 13
          Caption = 'Heads'
        end
        object lbSetors: TLabel
          Left = 8
          Top = 90
          Width = 36
          Height = 13
          Caption = 'Sectors'
        end
        object Label2: TLabel
          Left = 8
          Top = 114
          Width = 49
          Height = 13
          Caption = 'Sectorsize'
        end
        object lbCHSStatusPre: TLabel
          Left = 8
          Top = 144
          Width = 30
          Height = 13
          Caption = 'Status'
        end
        object lbCHSStatus: TLabel
          Left = 72
          Top = 144
          Width = 120
          Height = 13
          AutoSize = False
          Caption = '                                        '
        end
        object seCHSCylinder: TSpinEdit
          Left = 72
          Top = 40
          Width = 121
          Height = 22
          EditorEnabled = False
          Enabled = False
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 512
          OnChange = edCHSSizeChange
        end
        object seCHSHeads: TSpinEdit
          Left = 72
          Top = 64
          Width = 121
          Height = 22
          EditorEnabled = False
          MaxValue = 65535
          MinValue = 1
          TabOrder = 1
          Value = 32
          OnChange = edCHSSizeChange
        end
        object seCHSSectors: TSpinEdit
          Left = 72
          Top = 88
          Width = 121
          Height = 22
          EditorEnabled = False
          MaxValue = 65535
          MinValue = 1
          TabOrder = 2
          Value = 63
          OnChange = edCHSSizeChange
        end
        object edCHSSize: TEdit
          Left = 72
          Top = 16
          Width = 121
          Height = 21
          ReadOnly = True
          TabOrder = 3
          Text = '0'
          OnChange = edCHSSizeChange
        end
        object edCHSSectorSize: TEdit
          Left = 72
          Top = 112
          Width = 121
          Height = 21
          ReadOnly = True
          TabOrder = 4
          Text = '0'
        end
        object btCHSReset: TButton
          Left = 8
          Top = 168
          Width = 75
          Height = 25
          Caption = 'Reset'
          TabOrder = 5
          OnClick = btCHSResetClick
        end
      end
    end
    object tsDummyFiles: TTabSheet
      Caption = 'DummyFiles'
      ImageIndex = 6
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object Label3: TLabel
          Left = 8
          Top = 8
          Width = 82
          Height = 13
          Caption = 'Create Dummyfile'
        end
        object lbDummySizePre: TLabel
          Left = 8
          Top = 52
          Width = 26
          Height = 13
          Caption = 'Size :'
        end
        object pbDummy: TProgressBar
          Left = 8
          Top = 312
          Width = 505
          Height = 16
          Max = 1024
          Smooth = True
          TabOrder = 3
        end
        object edDummyFile: TEdit
          Left = 8
          Top = 24
          Width = 465
          Height = 21
          TabOrder = 0
        end
        object btBrowseDummy: TButton
          Left = 480
          Top = 24
          Width = 27
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = btBrowseDummyClick
        end
        object btCreateDummyFile: TButton
          Left = 8
          Top = 120
          Width = 75
          Height = 25
          Caption = 'Create File'
          TabOrder = 2
          OnClick = btCreateDummyFileClick
        end
        object lbDummyLog: TListBox
          Left = 8
          Top = 152
          Width = 505
          Height = 153
          ItemHeight = 13
          TabOrder = 4
        end
        object edDummySize: TEdit
          Left = 40
          Top = 48
          Width = 121
          Height = 21
          Enabled = False
          TabOrder = 5
          Text = '0'
        end
      end
    end
  end
  object gbDeviceControl: TGroupBox
    Left = 0
    Top = 0
    Width = 177
    Height = 369
    Caption = 'Device Control'
    TabOrder = 1
    object gbMountedDevices: TGroupBox
      Left = 8
      Top = 16
      Width = 161
      Height = 169
      Caption = 'Mounted Devices'
      TabOrder = 0
      object cbDrives: TListBox
        Left = 8
        Top = 16
        Width = 145
        Height = 145
        ItemHeight = 13
        PopupMenu = pmDevices
        TabOrder = 0
        OnClick = cbDrivesClick
      end
    end
    object gbData: TGroupBox
      Left = 8
      Top = 184
      Width = 161
      Height = 105
      Caption = 'Device Data'
      TabOrder = 1
      object lbMediaPre: TLabel
        Left = 16
        Top = 32
        Width = 49
        Height = 13
        Caption = 'Mediatype'
      end
      object lbSizePre: TLabel
        Left = 16
        Top = 48
        Width = 20
        Height = 13
        Caption = 'Size'
      end
      object lbSectorPre: TLabel
        Left = 16
        Top = 64
        Width = 36
        Height = 13
        Caption = 'Sectors'
      end
      object lbMediaType: TLabel
        Left = 75
        Top = 32
        Width = 78
        Height = 13
        AutoSize = False
        Caption = '                       '
      end
      object lbSize: TLabel
        Left = 75
        Top = 48
        Width = 78
        Height = 13
        AutoSize = False
        Caption = '                    '
      end
      object lbSectors: TLabel
        Left = 75
        Top = 64
        Width = 78
        Height = 13
        AutoSize = False
        Caption = '                  '
      end
      object lbDevicePre: TLabel
        Left = 16
        Top = 16
        Width = 34
        Height = 13
        Caption = 'Device'
      end
      object lbDevice: TLabel
        Left = 75
        Top = 16
        Width = 78
        Height = 13
        AutoSize = False
        Caption = '                           '
      end
      object lbSectorSizePre: TLabel
        Left = 16
        Top = 80
        Width = 54
        Height = 13
        Caption = 'Sectorssize'
      end
      object lbSectorSize: TLabel
        Left = 75
        Top = 80
        Width = 78
        Height = 13
        AutoSize = False
        Caption = '                  '
      end
    end
    object gbOptions: TGroupBox
      Left = 8
      Top = 288
      Width = 161
      Height = 73
      Caption = 'Options'
      TabOrder = 2
      object cbWriteprotected: TCheckBox
        Left = 16
        Top = 16
        Width = 137
        Height = 17
        Hint = 'prevent writing to device. (HDDs are writeprotected by default)'
        Caption = 'writeproteced'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = cbWriteprotectedClick
      end
      object cbRealSize: TCheckBox
        Left = 16
        Top = 40
        Width = 137
        Height = 17
        Hint = 'scan all readable sectors to calculate devicesize'
        Caption = 'force real size'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = cbRealSizeClick
      end
    end
  end
  object btBreak: TButton
    Left = 196
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Break'
    TabOrder = 2
    Visible = False
    OnClick = btBreakClick
  end
  object dgLoad: TOpenDialog
    Filter = 'all files|*.*'
    Left = 204
    Top = 280
  end
  object dgSave: TSaveDialog
    Filter = 'all files|*.*'
    Left = 236
    Top = 280
  end
  object tiRememberRefresh: TTimer
    Enabled = False
    OnTimer = tiRememberRefreshTimer
    Left = 268
    Top = 280
  end
  object pmDevices: TPopupMenu
    Left = 300
    Top = 280
    object Refresh1: TMenuItem
      Caption = 'Refresh'
      OnClick = Refresh1Click
    end
  end
end
