object Form1: TForm1
  Left = 412
  Top = 237
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 291
  ClientWidth = 386
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
  object FlatPanel1: TFlatPanel
    Left = 0
    Top = 16
    Width = 385
    Height = 273
    ParentColor = True
    TabOrder = 0
    UseDockManager = True
    object FlatGroupBox2: TFlatGroupBox
      Left = 8
      Top = 8
      Width = 121
      Height = 121
      Caption = 'Command'
      TabOrder = 0
      object FlatButton3: TFlatButton
        Left = 8
        Top = 88
        Width = 105
        Height = 25
        Caption = 'Exit'
        TabOrder = 2
        OnClick = FlatButton3Click
      end
      object FlatButton2: TFlatButton
        Left = 8
        Top = 48
        Width = 105
        Height = 25
        Caption = 'Save as Array'
        TabOrder = 1
        OnClick = Button2Click
      end
      object FlatButton1: TFlatButton
        Left = 8
        Top = 16
        Width = 105
        Height = 25
        Caption = 'Load'
        TabOrder = 0
        OnClick = Button1Click
      end
    end
    object FlatGroupBox1: TFlatGroupBox
      Left = 8
      Top = 136
      Width = 369
      Height = 129
      Caption = 'Preview'
      TabOrder = 1
      object IM_Char: TImage
        Left = 8
        Top = 16
        Width = 353
        Height = 107
        Picture.Data = {
          07544269746D6170BE010000424DBE010000000000003E000000280000006000
          000020000000010001000000000080010000130B0000130B0000020000000200
          000000000000FFFFFF00FFFDFFFFFFFFFFFFFFFFFFFFFFFDFFFFFFFCE1938700
          FFFFFFF5FFFFFFFCC0930200FFFFFFF4FFFFFFFCCC93327FFFFFEFF63FFFFFFC
          CC93327FFFFFEF035FFFFFFCCC933201FFFFEF81BFFFFFFCCC933200FFFFE040
          8FFFFFFCCC93327CFFFFE21FEFFFFFFCCC93327CFFFFFA31B7FFFFFC0C903200
          FFFFF82197FFFFFE1C987301FFFFFF6087FFFFFFFFFFFFFFFFFF80404FFFFFFF
          FFFFFFFFFFFF40C07BFFFFFFFFFFFFFFFFFF43C071FFFFFFFFFFFFFFFFFF1211
          083FFFFFFFFFFFFFFFFF4000081FFFFFFFFFFFFFFFFF83803C9FFFFFFFFFFFFF
          FFFFE33FB1BFFFFFFFFFFFFFFFFFE333907FFFFFFFFFFFFFFFFFEB31A0980783
          E01387F8073FFA73AE380381C01303F0033FFD5E4FFFF319CFF333F3F33FFF9F
          9FFFF319CFF333F3F33FFF81DFFC0318C03333F3F300FFE3DFF80738C01333F3
          F300FFC3FFF9FF3CCF9333F3F33FFFC7FFF9FF3CCF9333F3F33FFFCFFFF8023C
          C01033F00300FFEFFFFC023C603873F80780FFEFFFFFFFFFFFFFFFFFFFFFFFE7
          FFFFFFFFFFFFFFFFFFFF}
        Stretch = True
      end
    end
    object FlatGroupBox3: TFlatGroupBox
      Left = 128
      Top = 8
      Width = 241
      Height = 121
      Caption = 'Options'
      TabOrder = 2
      object Label1: TLabel
        Left = 8
        Top = 18
        Width = 28
        Height = 13
        Caption = 'Width'
      end
      object Label2: TLabel
        Left = 8
        Top = 42
        Width = 31
        Height = 13
        Caption = 'Height'
      end
      object Label3: TLabel
        Left = 8
        Top = 66
        Width = 26
        Height = 13
        Caption = 'Level'
      end
      object SE_NewWidth: TFlatSpinEditInteger
        Left = 56
        Top = 16
        Width = 73
        Height = 20
        ColorFlat = clBtnFace
        AutoSize = False
        MaxValue = 256
        MinValue = 1
        ParentColor = True
        TabOrder = 0
        Value = 96
      end
      object SE_NewHeight: TFlatSpinEditInteger
        Left = 56
        Top = 40
        Width = 73
        Height = 20
        ColorFlat = clBtnFace
        AutoSize = False
        MaxValue = 256
        MinValue = 1
        ParentColor = True
        TabOrder = 1
        Value = 32
      end
      object FlatButton4: TFlatButton
        Left = 144
        Top = 16
        Width = 89
        Height = 25
        Caption = 'Resample'
        TabOrder = 2
        OnClick = FlatButton4Click
      end
      object SE_Level: TFlatSpinEditInteger
        Left = 56
        Top = 64
        Width = 73
        Height = 20
        ColorFlat = clBtnFace
        AutoSize = False
        MaxValue = 255
        MinValue = 0
        ParentColor = True
        TabOrder = 3
        Value = 128
      end
      object FlatButton5: TFlatButton
        Left = 144
        Top = 88
        Width = 89
        Height = 25
        Caption = 'Invert'
        TabOrder = 4
        OnClick = FlatButton5Click
      end
    end
  end
  object Title: TFlatTitlebar
    Left = 0
    Top = 0
    Width = 385
    Height = 19
    ActiveTextColor = clWhite
    InactiveTextColor = 8559266
    ActiveTitlebarColor1 = clBlack
    ActiveTitlebarColor2 = clBlue
    InactiveTitlebarColor1 = clBlack
    InactiveTitlebarColor2 = clGray
    Caption = 'Font-BMP2Array'
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Bitmap|*.bmp'
    Left = 24
    Top = 176
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Text|*.txt'
    Left = 24
    Top = 208
  end
end
