object frmDemoMain: TfrmDemoMain
  Left = 0
  Top = 0
  Caption = 'Create Database'
  ClientHeight = 150
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object lblStatus: TLabel
    Left = 16
    Top = 120
    Width = 87
    Height = 15
    Caption = 'System: CLOSED'
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 0
    Width = 446
    Height = 114
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnCreateDatabase: TButton
      Left = 0
      Top = 8
      Width = 130
      Height = 30
      Caption = 'Create Database'
      TabOrder = 0
      OnClick = btnCreateDatabaseClick
    end
    object btnOpenSystem: TButton
      Left = 136
      Top = 8
      Width = 130
      Height = 30
      Caption = 'Open System'
      TabOrder = 1
      OnClick = btnOpenSystemClick
    end
    object btnCloseSystem: TButton
      Left = 272
      Top = 8
      Width = 130
      Height = 30
      Caption = 'Close System'
      TabOrder = 2
      OnClick = btnCloseSystemClick
    end
  end
end
