object Form1: TForm1
  Left = 248
  Top = 130
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'DirSync 0.1'
  ClientHeight = 449
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 136
    Width = 433
    Height = 313
    Caption = 'log'
    TabOrder = 0
    object lbLog: TListBox
      Left = 8
      Top = 16
      Width = 409
      Height = 289
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 0
    Width = 433
    Height = 73
    Caption = 'path'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 18
      Width = 32
      Height = 13
      Caption = 'source'
    end
    object Label2: TLabel
      Left = 8
      Top = 42
      Width = 27
      Height = 13
      Caption = 'target'
    end
    object edsource: TEdit
      Left = 48
      Top = 16
      Width = 353
      Height = 21
      ReadOnly = True
      TabOrder = 0
      Text = 'd:\test\a'
    end
    object Button1: TButton
      Left = 400
      Top = 16
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object edtarget: TEdit
      Left = 48
      Top = 40
      Width = 353
      Height = 21
      ReadOnly = True
      TabOrder = 2
      Text = 'd:\test\b'
    end
    object Button2: TButton
      Left = 400
      Top = 40
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = Button2Click
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 80
    Width = 433
    Height = 49
    Caption = 'options'
    TabOrder = 2
    object Button3: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = Button3Click
    end
    object cblowercase: TCheckBox
      Left = 136
      Top = 10
      Width = 73
      Height = 17
      Caption = 'lowercase'
      TabOrder = 1
    end
    object cbtest: TCheckBox
      Left = 136
      Top = 28
      Width = 97
      Height = 17
      Caption = 'testrun'
      TabOrder = 2
    end
  end
end
