object fmMain: TfmMain
  Left = 229
  Top = 210
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DiskDumper (c) 2008 Borg@Sven-of-Nine.de'
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
    ActivePage = tsReader
    TabOrder = 0
    object tsSectorViewer: TTabSheet
      Caption = 'Viewer'
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
        object edSector: TEdit
          Left = 8
          Top = 8
          Width = 121
          Height = 21
          ReadOnly = True
          TabOrder = 1
          Text = '0'
        end
        object sbSector: TScrollBar
          Left = 128
          Top = 8
          Width = 16
          Height = 21
          Kind = sbVertical
          LargeChange = 10
          PageSize = 0
          TabOrder = 2
          OnChange = sbSectorChange
        end
      end
    end
    object tsReader: TTabSheet
      Caption = 'Read'
      ImageIndex = 1
      object pnReader: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object lbDumpfile: TLabel
          Left = 8
          Top = 8
          Width = 41
          Height = 13
          Caption = 'Dumpfile'
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
          TabOrder = 4
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
        end
        object lbReadLog: TListBox
          Left = 8
          Top = 152
          Width = 505
          Height = 137
          ItemHeight = 13
          TabOrder = 5
        end
      end
    end
    object tsWriter: TTabSheet
      Caption = 'Write'
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
        end
        object btWriteImage: TButton
          Left = 8
          Top = 120
          Width = 75
          Height = 25
          Caption = 'Write from File'
          TabOrder = 3
          OnClick = btWriteImageClick
        end
        object lbWriteLog: TListBox
          Left = 8
          Top = 152
          Width = 505
          Height = 137
          ItemHeight = 13
          TabOrder = 4
        end
        object pbWrite: TProgressBar
          Left = 8
          Top = 312
          Width = 505
          Height = 16
          Max = 1024
          Smooth = True
          TabOrder = 5
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
    object tsDummyFiles: TTabSheet
      Caption = 'DummyFiles'
      ImageIndex = 6
      object pnDummy: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object lbCreateDummy: TLabel
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
    object tsWipe: TTabSheet
      Caption = 'Wipe'
      ImageIndex = 7
      object pnWiper: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object lbWiper: TLabel
          Left = 8
          Top = 8
          Width = 79
          Height = 13
          Caption = 'wipe device with'
        end
        object lbWipeSpeedPre: TLabel
          Left = 8
          Top = 292
          Width = 73
          Height = 13
          Caption = 'Speed [KB/s]  :'
        end
        object lbWipeSpeed: TLabel
          Left = 88
          Top = 292
          Width = 45
          Height = 13
          Caption = '0             '
        end
        object pbWipe: TProgressBar
          Left = 8
          Top = 312
          Width = 505
          Height = 16
          Max = 1024
          Smooth = True
          TabOrder = 1
        end
        object btWipeDevice: TButton
          Left = 8
          Top = 120
          Width = 75
          Height = 25
          Caption = 'Wipe Device'
          TabOrder = 0
          OnClick = btWipeDeviceClick
        end
        object rbWipe00: TRadioButton
          Left = 8
          Top = 32
          Width = 113
          Height = 17
          Caption = '#00'
          Checked = True
          TabOrder = 2
          TabStop = True
        end
        object rbWipeFF: TRadioButton
          Left = 8
          Top = 48
          Width = 113
          Height = 17
          Caption = '#FF'
          TabOrder = 3
        end
        object rbWipeRandom: TRadioButton
          Left = 8
          Top = 64
          Width = 113
          Height = 17
          Caption = 'random'
          TabOrder = 4
        end
        object lbWipeLog: TListBox
          Left = 8
          Top = 152
          Width = 505
          Height = 137
          ItemHeight = 13
          TabOrder = 5
        end
        object cbCreatePartition: TCheckBox
          Left = 8
          Top = 88
          Width = 249
          Height = 17
          Caption = 'create partition (changes checksum)'
          TabOrder = 6
        end
      end
    end
    object tsCRC: TTabSheet
      Caption = 'CRC'
      ImageIndex = 8
      object pnCRC: TPanel
        Left = 0
        Top = 0
        Width = 521
        Height = 337
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object lbCRC: TLabel
          Left = 8
          Top = 8
          Width = 22
          Height = 13
          Caption = 'CRC'
        end
        object lbCRCSpeedPre: TLabel
          Left = 8
          Top = 292
          Width = 73
          Height = 13
          Caption = 'Speed [KB/s]  :'
        end
        object lbCRCSpeed: TLabel
          Left = 88
          Top = 292
          Width = 45
          Height = 13
          Caption = '0             '
        end
        object pbCRC: TProgressBar
          Left = 8
          Top = 312
          Width = 505
          Height = 16
          Max = 1024
          Smooth = True
          TabOrder = 1
        end
        object btCRC: TButton
          Left = 8
          Top = 120
          Width = 75
          Height = 25
          Caption = 'Create CRC'
          TabOrder = 0
          OnClick = btCRCClick
        end
        object lbCRCLog: TListBox
          Left = 8
          Top = 152
          Width = 505
          Height = 137
          ItemHeight = 13
          TabOrder = 2
        end
        object lbCRCLogpre: TListBox
          Left = 8
          Top = 24
          Width = 505
          Height = 89
          ItemHeight = 13
          TabOrder = 3
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
        Width = 49
        Height = 13
        Caption = 'Sectorsize'
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
    Filter = 'raw image|*.raw|packed raw image|*.paw|all files|*.*'
    Left = 204
    Top = 280
  end
  object dgSave: TSaveDialog
    Filter = 'raw image|*.raw|packed raw image|*.paw|all files|*.*'
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
    object miRefresh: TMenuItem
      Caption = 'Refresh'
      OnClick = miRefreshClick
    end
  end
end
