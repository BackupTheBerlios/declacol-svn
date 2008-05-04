object FM_Main: TFM_Main
  Left = 386
  Top = 212
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ProcList (c) 2005 Borg@Sven-of-Nine.de'
  ClientHeight = 497
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 433
    Height = 425
    Caption = 'active processes'
    TabOrder = 0
    object LB_Procs: TListBox
      Left = 8
      Top = 16
      Width = 417
      Height = 401
      ItemHeight = 13
      PopupMenu = PopupMenu
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 456
    Width = 433
    Height = 41
    Caption = 'cpu load'
    TabOrder = 1
    object GG_CPU: TGauge
      Left = 8
      Top = 16
      Width = 417
      Height = 17
      Progress = 0
    end
    object LB_NoCPU: TLabel
      Left = 8
      Top = 16
      Width = 417
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = 'cpu load not available'
      Visible = False
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 432
    Width = 433
    Height = 25
    TabOrder = 2
    object cb_windowed: TCheckBox
      Left = 8
      Top = 5
      Width = 169
      Height = 17
      Caption = 'hide nonwindowed processes'
      TabOrder = 0
    end
  end
  object TI_Refresh: TTimer
    OnTimer = TI_RefreshTimer
    Left = 16
    Top = 24
  end
  object PopupMenu: TPopupMenu
    Left = 48
    Top = 24
    object Kill1: TMenuItem
      Caption = 'Kill'
      OnClick = Kill1Click
    end
    object Throttle1: TMenuItem
      Caption = 'Throttle'
      object N51: TMenuItem
        Caption = '5%'
        RadioItem = True
        OnClick = N51Click
      end
      object N101: TMenuItem
        Caption = '10%'
        RadioItem = True
        OnClick = N51Click
      end
      object N201: TMenuItem
        Caption = '20%'
        RadioItem = True
        OnClick = N51Click
      end
      object N301: TMenuItem
        Caption = '30%'
        RadioItem = True
        OnClick = N51Click
      end
      object N401: TMenuItem
        Caption = '40%'
        RadioItem = True
        OnClick = N51Click
      end
      object N501: TMenuItem
        Caption = '50%'
        RadioItem = True
        OnClick = N51Click
      end
      object N601: TMenuItem
        Caption = '60%'
        RadioItem = True
        OnClick = N51Click
      end
      object N701: TMenuItem
        Caption = '70%'
        RadioItem = True
        OnClick = N51Click
      end
      object N801: TMenuItem
        Caption = '80%'
        RadioItem = True
        OnClick = N51Click
      end
      object N901: TMenuItem
        Caption = '90%'
        RadioItem = True
        OnClick = N51Click
      end
      object off1: TMenuItem
        Caption = 'off'
        Checked = True
        RadioItem = True
        OnClick = N51Click
      end
    end
  end
  object TI_CPU: TTimer
    Interval = 500
    OnTimer = TI_CPUTimer
    Left = 80
    Top = 24
  end
end
