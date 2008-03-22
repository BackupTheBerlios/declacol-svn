object Form1: TForm1
  Left = 359
  Top = 196
  Width = 362
  Height = 300
  Caption = 'SplitJoin | (c) 2008 Borg@Sven-of-Nine.de'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 353
    Height = 273
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Join'
      object gbInput: TGroupBox
        Left = 0
        Top = 0
        Width = 345
        Height = 153
        Caption = 'Input Files'
        TabOrder = 0
        object lbFiles: TCheckListBox
          Left = 8
          Top = 48
          Width = 329
          Height = 97
          ItemHeight = 13
          TabOrder = 0
        end
        object edJoinSource: TEdit
          Left = 8
          Top = 16
          Width = 313
          Height = 21
          TabOrder = 1
        end
        object btBrowseJoinSource: TButton
          Left = 320
          Top = 16
          Width = 17
          Height = 21
          Caption = '...'
          TabOrder = 2
          OnClick = btBrowseJoinSourceClick
        end
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 152
        Width = 345
        Height = 57
        Caption = 'Output File'
        TabOrder = 1
        object edJoinTarget: TEdit
          Left = 8
          Top = 16
          Width = 313
          Height = 21
          TabOrder = 0
        end
        object btBrowseJoinTarget: TButton
          Left = 320
          Top = 16
          Width = 17
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = btBrowseJoinTargetClick
        end
      end
      object btJoin: TButton
        Left = 0
        Top = 216
        Width = 75
        Height = 25
        Caption = 'Join'
        TabOrder = 2
        OnClick = btJoinClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Split'
      ImageIndex = 1
      object btSplit: TButton
        Left = 0
        Top = 216
        Width = 75
        Height = 25
        Caption = 'Split'
        TabOrder = 0
        OnClick = btSplitClick
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 345
        Height = 57
        Caption = 'Input File'
        TabOrder = 1
        object edSplitSource: TEdit
          Left = 8
          Top = 16
          Width = 313
          Height = 21
          TabOrder = 0
        end
        object btBrowseSplitSource: TButton
          Left = 320
          Top = 16
          Width = 17
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = btBrowseSplitSourceClick
        end
      end
      object GroupBox3: TGroupBox
        Left = 0
        Top = 56
        Width = 345
        Height = 89
        Caption = 'Options'
        TabOrder = 2
        object Label1: TLabel
          Left = 96
          Top = 20
          Width = 30
          Height = 13
          Caption = 'MByte'
        end
        object cbSize: TComboBox
          Left = 8
          Top = 16
          Width = 73
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          Text = '1.4'
          OnChange = cbSizeChange
          Items.Strings = (
            '1.44'
            '10'
            '100'
            '640'
            '1024')
        end
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 152
        Width = 345
        Height = 57
        Caption = 'Output File'
        TabOrder = 3
        object edSplitTarget: TEdit
          Left = 8
          Top = 16
          Width = 313
          Height = 21
          TabOrder = 0
        end
        object btBrowseSplitTarget: TButton
          Left = 320
          Top = 16
          Width = 17
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = btBrowseSplitTargetClick
        end
      end
    end
  end
  object dgOpen: TOpenDialog
    Left = 288
    Top = 240
  end
  object dgSave: TSaveDialog
    Left = 312
    Top = 240
  end
end
