object frmStep1: TfrmStep1
  Left = 0
  Top = 0
  Caption = 'Bold Tutorial - Step 1: New Project'
  ClientHeight = 300
  ClientWidth = 500
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 4210752
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 120
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 20
      Top = 20
      Width = 218
      Height = 25
      Caption = 'Step 1: New Project Setup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -19
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object lblDescription: TLabel
      Left = 20
      Top = 55
      Width = 460
      Height = 45
      AutoSize = False
      Caption = 
        'This step demonstrates the minimal Bold setup: a BoldModel (your' +
        ' UML model), a BoldSystemHandle (the runtime object space), and ' +
        'SQLite persistence.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 120
    Width = 500
    Height = 180
    Align = alClient
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 1
    object lblConfigFile: TLabel
      Left = 20
      Top = 20
      Width = 460
      Height = 15
      AutoSize = False
      Caption = 'Config: (loading...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblDatabaseStatus: TLabel
      Left = 20
      Top = 45
      Width = 460
      Height = 15
      AutoSize = False
      Caption = 'Database: (checking...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblBoldStatus: TLabel
      Left = 20
      Top = 70
      Width = 460
      Height = 15
      AutoSize = False
      Caption = 'Bold System: (checking...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object btnOpenClose: TButton
      Left = 20
      Top = 110
      Width = 120
      Height = 30
      Action = InvoiceDataModule.BoldActivateSystemAction1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
end
