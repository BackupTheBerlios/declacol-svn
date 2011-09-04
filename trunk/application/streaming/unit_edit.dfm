object fmEditChannel: TfmEditChannel
  Left = 354
  Top = 298
  BorderStyle = bsToolWindow
  Caption = 'change'
  ClientHeight = 139
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 22
    Height = 13
    Caption = 'URL'
  end
  object edChannel: TEdit
    Left = 8
    Top = 24
    Width = 377
    Height = 21
    TabOrder = 0
  end
  object edURL: TEdit
    Left = 8
    Top = 72
    Width = 377
    Height = 21
    TabOrder = 1
  end
  object btOK: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = btOKClick
  end
  object btTest: TButton
    Left = 312
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 3
    OnClick = btTestClick
  end
end
