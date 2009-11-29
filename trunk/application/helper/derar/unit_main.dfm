object fmMain: TfmMain
  Left = 282
  Top = 155
  Width = 498
  Height = 381
  Caption = 'fmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PCMain: TPageControl
    Left = 0
    Top = 0
    Width = 489
    Height = 353
    ActivePage = TSOptions
    TabOrder = 0
    object TSMain: TTabSheet
      Caption = 'Main'
      object GBArchives: TGroupBox
        Left = 0
        Top = 0
        Width = 481
        Height = 233
        Caption = 'Archives'
        TabOrder = 0
        object CLBFiles: TCheckListBox
          Left = 8
          Top = 16
          Width = 465
          Height = 209
          ItemHeight = 13
          Items.Strings = (
            'd:\temp\pwtest.rar'
            'd:\temp\test.rar')
          TabOrder = 0
        end
      end
      object GBMain: TGroupBox
        Left = 0
        Top = 272
        Width = 481
        Height = 49
        Caption = 'Actions'
        TabOrder = 1
        object btBreak: TButton
          Left = 8
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Stop'
          TabOrder = 1
          OnClick = btBreakClick
        end
        object btUnpack: TButton
          Left = 8
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Unpack'
          TabOrder = 0
          OnClick = btUnpackClick
        end
        object btClear: TButton
          Left = 400
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 2
          OnClick = btClearClick
        end
      end
      object PNProgress: TPanel
        Left = 0
        Top = 240
        Width = 481
        Height = 33
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 2
        object lbPW: TLabel
          Left = 384
          Top = 8
          Width = 89
          Height = 13
          AutoSize = False
          Caption = 'PW :'
        end
        object PBUnpack: TProgressBar
          Left = 8
          Top = 8
          Width = 369
          Height = 16
          TabOrder = 0
        end
      end
    end
    object TSOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      OnShow = TSOptionsShow
      object GBOptions: TGroupBox
        Left = 0
        Top = 0
        Width = 481
        Height = 321
        Caption = 'Options'
        TabOrder = 0
        object lbTargetDir: TLabel
          Left = 8
          Top = 16
          Width = 42
          Height = 13
          Caption = 'Targetdir'
        end
        object btBrowseTarget: TButton
          Left = 456
          Top = 32
          Width = 19
          Height = 21
          Caption = '...'
          TabOrder = 0
          OnClick = btBrowseTargetClick
        end
        object edTargetDir: TEdit
          Left = 8
          Top = 32
          Width = 449
          Height = 21
          TabOrder = 1
        end
        object CBSmartPW: TCheckBox
          Left = 8
          Top = 96
          Width = 249
          Height = 17
          Caption = 'smart passwords'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object CBSavePW: TCheckBox
          Left = 8
          Top = 72
          Width = 249
          Height = 17
          Caption = 'save passwords on close'
          TabOrder = 3
        end
      end
    end
    object TSPasswords: TTabSheet
      Caption = 'Passwords'
      ImageIndex = 2
      object GBPasswords: TGroupBox
        Left = 0
        Top = 0
        Width = 481
        Height = 317
        Caption = 'Password'
        TabOrder = 0
        object LBPasswords: TListBox
          Left = 8
          Top = 16
          Width = 465
          Height = 289
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
    object TSLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 3
      object GBLog: TGroupBox
        Left = 0
        Top = 0
        Width = 481
        Height = 317
        Caption = 'Log'
        TabOrder = 0
        object lbLog: TListBox
          Left = 8
          Top = 16
          Width = 465
          Height = 289
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
  end
  object UnRAR: TDFUnRar
    CanProgress = True
    Mode = DFRAR_EXTRACT
    OverrideEvent = OR_ALWAYS
    PromptForPass = False
    PromptForVolumn = False
    StopProcessing = False
    OnError = UnRARError
    OnProgress = UnRARProgress
    Left = 444
    Top = 304
  end
  object DGTarget: TSaveDialog
    OptionsEx = [ofExNoPlacesBar]
    Left = 444
    Top = 272
  end
end
