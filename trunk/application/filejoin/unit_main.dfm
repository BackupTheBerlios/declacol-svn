object Form1: TForm1
  Left = 309
  Top = 173
  Width = 355
  Height = 276
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object gbInput: TGroupBox
    Left = 0
    Top = 0
    Width = 345
    Height = 153
    Caption = 'Input Files'
    TabOrder = 0
    object edSource: TEdit
      Left = 8
      Top = 16
      Width = 313
      Height = 21
      TabOrder = 0
    end
    object btBrowseSource: TButton
      Left = 320
      Top = 16
      Width = 17
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btBrowseSourceClick
    end
    object lbFiles: TCheckListBox
      Left = 8
      Top = 48
      Width = 329
      Height = 97
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 160
    Width = 345
    Height = 57
    Caption = 'Output File'
    TabOrder = 1
    object edTarget: TEdit
      Left = 8
      Top = 16
      Width = 313
      Height = 21
      TabOrder = 0
    end
    object btBrowseTarget: TButton
      Left = 320
      Top = 16
      Width = 17
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btBrowseTargetClick
    end
  end
  object btJoin: TButton
    Left = 0
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Join'
    TabOrder = 2
    OnClick = btJoinClick
  end
  object dgOpen: TOpenDialog
    Left = 312
    Top = 216
  end
  object dgSave: TSaveDialog
    Left = 280
    Top = 216
  end
end
