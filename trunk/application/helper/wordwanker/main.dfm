object Form1: TForm1
  Left = 158
  Top = 251
  Width = 562
  Height = 115
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSizeToolWin
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object cbInput: TComboBox
    Left = 8
    Top = 8
    Width = 537
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'Die Krankenhauseinfahrt sollte '#252'berarbeitet und eloxiert werden.'
      'Nat'#252'rlich vorkommende Schleiereulen '#252'berleben beliebige Winter'
      'Solarzellen kann man nach verschiedenen Kriterien einordnen'
      
        'Die Kristallstruktur kann kristallin mono polykristallin oder am' +
        'orph sein'
      
        'Klaus Wucherer promovierte nach dem Studium der Elektrotechnik u' +
        'nd des Maschinenbaus in eben jenen ingenieurwissenschaftlichen D' +
        'isziplinen'
      
        'Eine Muskelfibrille besteht aus Hunderten hintereinandergeschalt' +
        'eten Baueinheiten mit gleichem inneren Aufbau'
      'Schloss Schwarzwasserstelz war eine Wasserburg'
      
        'Die Bezeichnung Konteradmiral geht auf die britische Flotte zur'#252 +
        'ck'
      
        'Lufthansa Regional ist eine Dachmarke f'#252'r f'#252'nf Regionalfluggesel' +
        'lschaften'
      
        'Die Publizit'#228'tspflicht auch Offenlegungspflicht ist die im Hande' +
        'sgesetzbuch geregelte Pflicht den kaufm'#228'nnischen Jahresabschluss' +
        ' im Elektronischen Bundesanzeiger zu ver'#246'ffentlichen.')
  end
  object edOutput: TEdit
    Left = 8
    Top = 32
    Width = 537
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'btGo'
    TabOrder = 2
    OnClick = Button1Click
  end
end
