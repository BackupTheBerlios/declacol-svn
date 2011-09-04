object Form1: TForm1
  Left = 398
  Top = 265
  BorderStyle = bsDialog
  Caption = 'VLC-TV 0.1'
  ClientHeight = 215
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object lbChannel: TListBox
    Left = 8
    Top = 8
    Width = 209
    Height = 201
    ItemHeight = 13
    PopupMenu = pmEdit
    TabOrder = 0
    OnClick = lbChannelClick
    OnDblClick = lbChannelDblClick
  end
  object btView: TButton
    Left = 224
    Top = 8
    Width = 75
    Height = 25
    Caption = 'View'
    Enabled = False
    TabOrder = 1
    OnClick = btViewClick
  end
  object pnBusy: TPanel
    Left = 8
    Top = 8
    Width = 289
    Height = 201
    Caption = 'Waiting for VLC'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Visible = False
  end
  object tiNoFreeze: TTimer
    Interval = 200
    OnTimer = tiNoFreezeTimer
    Left = 16
    Top = 16
  end
  object pmEdit: TPopupMenu
    Left = 48
    Top = 16
    object pmChannelAdd: TMenuItem
      Caption = 'Add'
      OnClick = pmChannelAddClick
    end
    object pmEditChannel: TMenuItem
      Caption = 'Edit'
      OnClick = pmEditChannelClick
    end
    object N1: TMenuItem
      Caption = '___________'
    end
    object pmRemoveChannel: TMenuItem
      Caption = 'Remove'
      OnClick = pmRemoveChannelClick
    end
  end
end
