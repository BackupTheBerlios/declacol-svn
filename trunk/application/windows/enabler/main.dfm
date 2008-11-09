object Form1: TForm1
  Left = 381
  Top = 146
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'Enabler (c) 2008 Borg@Sven-of-Nine.de'
  ClientHeight = 453
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Enum Objects'
    TabOrder = 0
    OnClick = Button1Click
  end
  object TreeView1: TTreeView
    Left = 8
    Top = 40
    Width = 329
    Height = 401
    Indent = 19
    TabOrder = 1
  end
  object cbEnable: TCheckBox
    Left = 112
    Top = 8
    Width = 137
    Height = 17
    Caption = 'enable Objects'
    TabOrder = 2
  end
end
