object Form1: TForm1
  Left = 329
  Top = 256
  BorderStyle = bsToolWindow
  Caption = 'DoubleKill (c) 2008 Bortg@Sven-of-Nine.de'
  ClientHeight = 385
  ClientWidth = 633
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 385
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 8
      Top = 8
      Width = 617
      Height = 369
      ActivePage = Scanner
      TabOrder = 0
      object Scanner: TTabSheet
        Caption = 'Scanner'
        object pbChecksum: TProgressBar
          Left = 80
          Top = 315
          Width = 529
          Height = 17
          Min = 0
          Max = 100
          Smooth = True
          TabOrder = 4
        end
        object btScan: TButton
          Left = 0
          Top = 312
          Width = 75
          Height = 25
          Caption = 'Scan'
          TabOrder = 0
          OnClick = btScanClick
        end
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 609
          Height = 81
          Caption = 'Source'
          TabOrder = 1
          object lbFolder: TLabel
            Left = 8
            Top = 18
            Width = 26
            Height = 13
            Caption = 'folder'
          end
          object lbFilter: TLabel
            Left = 8
            Top = 50
            Width = 19
            Height = 13
            Caption = 'filter'
          end
          object edFolder: TEdit
            Left = 56
            Top = 16
            Width = 513
            Height = 21
            TabOrder = 0
          end
          object btFolder: TButton
            Left = 576
            Top = 16
            Width = 25
            Height = 21
            Caption = '...'
            TabOrder = 1
            OnClick = btFolderClick
          end
          object edFilter: TEdit
            Left = 56
            Top = 48
            Width = 89
            Height = 21
            TabOrder = 2
            Text = '*.*;*'
          end
        end
        object GroupBox2: TGroupBox
          Left = 0
          Top = 80
          Width = 609
          Height = 89
          Caption = 'Scanoptions'
          TabOrder = 2
          object rbHyperspeed: TRadioButton
            Left = 8
            Top = 16
            Width = 113
            Height = 17
            Caption = 'hyperspeed (size)'
            TabOrder = 0
          end
          object rbFast: TRadioButton
            Left = 8
            Top = 32
            Width = 113
            Height = 17
            Caption = 'fast (adler32)'
            Checked = True
            TabOrder = 1
            TabStop = True
          end
          object rbSlow: TRadioButton
            Left = 8
            Top = 48
            Width = 113
            Height = 17
            Caption = 'slow (md5)'
            TabOrder = 2
          end
          object rbParanoid: TRadioButton
            Left = 8
            Top = 64
            Width = 161
            Height = 17
            Caption = 'paranoid (adler32 & md5)'
            TabOrder = 3
          end
        end
        object GroupBox3: TGroupBox
          Left = 0
          Top = 168
          Width = 609
          Height = 137
          Caption = 'Log'
          TabOrder = 3
          object lbLog: TLogBox
            Left = 8
            Top = 16
            Width = 593
            Height = 113
            ItemHeight = 13
            TabOrder = 0
          end
        end
      end
      object Remover: TTabSheet
        Caption = 'Output'
        ImageIndex = 1
        object lbFound: TCheckListBox
          Left = 0
          Top = 0
          Width = 609
          Height = 337
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
  end
  object dgBrowse: TBrowseDialog
    Left = 92
    Top = 344
  end
end
