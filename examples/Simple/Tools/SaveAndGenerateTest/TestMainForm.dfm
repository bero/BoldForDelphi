object frmTestMain: TfrmTestMain
  Left = 0
  Top = 0
  Caption = 'Test Save and Generate All'
  ClientHeight = 450
  ClientWidth = 920
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 920
    Height = 59
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 303
      Top = 14
      Width = 375
      Height = 39
      Caption = 
        'This call the same logic as "Save and generate all files" in mod' +
        'eleditor. But this is possible to debug'
      WordWrap = True
    end
    object btnSaveAndGenerateAll: TButton
      Left = 16
      Top = 12
      Width = 180
      Height = 30
      Caption = 'Save and Generate All'
      TabOrder = 0
      OnClick = btnSaveAndGenerateAllClick
    end
    object btnModelEditor: TButton
      Left = 202
      Top = 14
      Width = 95
      Height = 30
      Caption = 'Change Model'
      TabOrder = 1
      OnClick = btnModelEditorClick
    end
  end
  object memoLog: TMemo
    Left = 0
    Top = 114
    Width = 920
    Height = 336
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 59
    Width = 920
    Height = 55
    Align = alTop
    TabOrder = 2
    object btnValidateModel: TButton
      Left = 16
      Top = 15
      Width = 120
      Height = 30
      Caption = '1. Validate Model'
      TabOrder = 0
      OnClick = btnValidateModelClick
    end
    object btnSaveModel: TButton
      Left = 150
      Top = 15
      Width = 120
      Height = 30
      Caption = '2. Save Model'
      TabOrder = 1
      OnClick = btnSaveModelClick
    end
    object btnGenerateCode: TButton
      Left = 284
      Top = 15
      Width = 120
      Height = 30
      Caption = '3. Generate Code'
      TabOrder = 2
      OnClick = btnGenerateCodeClick
    end
    object btnUpdateDfm: TButton
      Left = 418
      Top = 15
      Width = 120
      Height = 30
      Caption = '4. Update DFM'
      TabOrder = 3
      OnClick = btnUpdateDfmClick
    end
    object btnGenerateSQLScript: TButton
      Left = 552
      Top = 15
      Width = 130
      Height = 30
      Caption = '5. Generate SQL'
      TabOrder = 4
      OnClick = btnGenerateSQLScriptClick
    end
  end
end
