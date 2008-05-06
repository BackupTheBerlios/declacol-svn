object fmMain: TfmMain
  Left = 225
  Top = 199
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'StreamFS-Explorer v0.1 | (c) 2008 Borg@Sven-of-Nine.de'
  ClientHeight = 544
  ClientWidth = 802
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 609
    Height = 545
    Caption = 'Files'
    TabOrder = 0
    object sgFiles: TStringGrid
      Left = 8
      Top = 40
      Width = 593
      Height = 497
      ColCount = 4
      DefaultRowHeight = 18
      FixedCols = 0
      FixedRows = 0
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
      ParentFont = False
      PopupMenu = pmFiles
      ScrollBars = ssVertical
      TabOrder = 0
      OnClick = sgFilesClick
      OnDblClick = btOpenClick
      ColWidths = (
        367
        93
        71
        64)
    end
    object sgTitle: TStringGrid
      Left = 8
      Top = 16
      Width = 593
      Height = 22
      ColCount = 4
      DefaultRowHeight = 18
      FixedCols = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
      ParentFont = False
      ScrollBars = ssNone
      TabOrder = 1
      OnClick = sgFilesClick
      ColWidths = (
        366
        93
        71
        53)
    end
  end
  object GroupBox2: TGroupBox
    Left = 616
    Top = 0
    Width = 89
    Height = 129
    Caption = 'Files'
    TabOrder = 1
    object btOpen: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btOpenClick
    end
    object btExport: TButton
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Export'
      TabOrder = 1
      OnClick = btExportClick
    end
    object btDelete: TButton
      Left = 8
      Top = 96
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btDeleteClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 712
    Top = 0
    Width = 89
    Height = 129
    Caption = 'Maintenance'
    TabOrder = 2
    object btChkDsk: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Chkdsk'
      TabOrder = 0
      OnClick = btChkDskClick
    end
    object btDefrag: TButton
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Defrag'
      TabOrder = 1
      OnClick = btDefragClick
    end
    object btFormat: TButton
      Left = 8
      Top = 96
      Width = 75
      Height = 25
      Caption = 'Format'
      TabOrder = 2
      OnClick = btFormatClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 616
    Top = 352
    Width = 185
    Height = 193
    Caption = 'FS-Info'
    TabOrder = 3
    object mmFS: TMemo
      Left = 8
      Top = 16
      Width = 169
      Height = 169
      Font.Charset = OEM_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Terminal'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object GroupBox5: TGroupBox
    Left = 616
    Top = 160
    Width = 185
    Height = 193
    Caption = 'Fileinfo'
    TabOrder = 4
    object mmFileInfo: TMemo
      Left = 8
      Top = 16
      Width = 169
      Height = 169
      Font.Charset = OEM_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Terminal'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object GroupBox6: TGroupBox
    Left = 616
    Top = 128
    Width = 185
    Height = 33
    Caption = 'Storemode'
    TabOrder = 5
    object rbPlain: TRadioButton
      Left = 8
      Top = 12
      Width = 65
      Height = 17
      Caption = 'Plain'
      TabOrder = 0
      OnClick = rbPlainClick
    end
    object rbCrypt: TRadioButton
      Left = 112
      Top = 12
      Width = 65
      Height = 17
      Caption = 'Crypted'
      TabOrder = 1
      OnClick = rbCryptClick
    end
  end
  object dIncoming: TDropFileTarget
    Dragtypes = [dtCopy, dtLink]
    GetDataOnEnter = False
    OnDrop = dIncomingDrop
    ShowImage = True
    Target = sgFiles
    OptimizedMove = True
    AllowAsyncTransfer = False
    Left = 16
    Top = 488
  end
  object dgSave: TSaveDialog
    Filter = 'all files|*.*'
    Left = 48
    Top = 488
  end
  object pmFiles: TPopupMenu
    Left = 80
    Top = 488
    object Open1: TMenuItem
      Caption = 'Open'
      OnClick = btOpenClick
    end
    object Export1: TMenuItem
      Caption = 'Export'
      OnClick = btExportClick
    end
    object Delte1: TMenuItem
      Caption = 'Delete'
      OnClick = btDeleteClick
    end
    object DeleteAll1: TMenuItem
      Caption = 'Delete All'
      OnClick = DeleteAll1Click
    end
  end
end
