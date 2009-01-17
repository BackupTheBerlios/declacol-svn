object MyOwnDodger: TMyOwnDodger
  Left = 369
  Top = 243
  BorderStyle = bsToolWindow
  Caption = 'SpamDodger'
  ClientHeight = 86
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 56
    Width = 121
    Height = 25
    Caption = 'create email'
    TabOrder = 0
    OnClick = Button1Click
  end
  object edEMail: TEdit
    Left = 8
    Top = 8
    Width = 241
    Height = 21
    TabOrder = 1
    Text = 'edEMail'
  end
  object cbClipboard: TCheckBox
    Left = 8
    Top = 32
    Width = 121
    Height = 17
    Caption = 'copy to clipboard'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object cbBrowser: TCheckBox
    Left = 136
    Top = 32
    Width = 97
    Height = 17
    Caption = 'open in browser'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
