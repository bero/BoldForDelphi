object frmOclWorkbench: TfrmOclWorkbench
  Left = 0
  Top = 0
  Caption = 'Bold OCL Workbench'
  ClientHeight = 740
  ClientWidth = 1085
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object splLeft: TSplitter
    Left = 330
    Top = 220
    Width = 5
    Height = 355
    Color = clBtnFace
    ParentColor = False
  end
  object splMessages: TSplitter
    Left = 0
    Top = 575
    Width = 1085
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    Color = clBtnFace
    ParentColor = False
  end
  object splExpression: TSplitter
    Left = 0
    Top = 215
    Width = 1085
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Color = clBtnFace
    ParentColor = False
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 710
    Width = 1085
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object lblStatus: TLabel
      Left = 8
      Top = 7
      Width = 67
      Height = 15
      Caption = 'Starting up...'
    end
    object pnlSaveState: TPanel
      Left = 865
      Top = 0
      Width = 220
      Height = 30
      Align = alRight
      BevelOuter = bvNone
      Caption = 'Unsaved changes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
    end
    object pnlDbState: TPanel
      Left = 605
      Top = 0
      Width = 260
      Height = 30
      Align = alRight
      BevelOuter = bvNone
      Caption = 'Database'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 220
    Width = 330
    Height = 355
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object splContext: TSplitter
      Left = 0
      Top = 145
      Width = 330
      Height = 5
      Cursor = crVSplit
      Align = alBottom
      Color = clBtnFace
      ParentColor = False
    end
    object pnlSamplesHeader: TPanel
      Left = 0
      Top = 0
      Width = 330
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblSamples: TLabel
        Left = 8
        Top = 4
        Width = 178
        Height = 15
        Caption = 'Sample expressions - click to load'
      end
    end
    object pnlContext: TPanel
      Left = 0
      Top = 150
      Width = 330
      Height = 205
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object pnlContextHeader: TPanel
        Left = 0
        Top = 0
        Width = 330
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblContext: TLabel
          Left = 8
          Top = 4
          Width = 235
          Height = 15
          Caption = 'Selected person is the OCL variable "current"'
        end
      end
      object lbPeople: TBoldListBox
        Left = 0
        Top = 24
        Width = 330
        Height = 181
        Align = alClient
        Alignment = taLeftJustify
        BoldHandle = lhaPeople
        BoldProperties.InternalDrag = False
        BoldRowProperties.Expression = 'fullName'
        DragMode = dmAutomatic
        TabOrder = 1
      end
    end
    object tvSamples: TTreeView
      Left = 0
      Top = 24
      Width = 330
      Height = 121
      Align = alClient
      HideSelection = False
      Indent = 19
      ReadOnly = True
      TabOrder = 2
      OnChange = tvSamplesChange
    end
  end
  object pnlMessages: TPanel
    Left = 0
    Top = 580
    Width = 1085
    Height = 130
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object pnlMessagesHeader: TPanel
      Left = 0
      Top = 0
      Width = 1085
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblMessages: TLabel
        Left = 8
        Top = 4
        Width = 51
        Height = 15
        Caption = 'Messages'
      end
    end
    object memMessages: TMemo
      Left = 0
      Top = 24
      Width = 1085
      Height = 106
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object pnlExpression: TPanel
    Left = 0
    Top = 0
    Width = 1085
    Height = 215
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object pnlExpressionHeader: TPanel
      Left = 0
      Top = 0
      Width = 1085
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblExpression: TLabel
        Left = 8
        Top = 4
        Width = 239
        Height = 15
        Caption = 'OCL expression - context is the whole system'
      end
    end
    object pnlExpressionButtons: TPanel
      Left = 0
      Top = 171
      Width = 1085
      Height = 44
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnEvaluate: TButton
        Left = 8
        Top = 8
        Width = 110
        Height = 27
        Action = actEvaluate
        TabOrder = 0
      end
      object btnEditOcl: TButton
        Left = 126
        Top = 8
        Width = 180
        Height = 27
        Action = actEditOcl
        TabOrder = 1
      end
      object cbEvaluateInPS: TCheckBox
        Left = 322
        Top = 13
        Width = 250
        Height = 17
        Caption = 'Evaluate in PS (translate to SQL)'
        TabOrder = 2
      end
    end
    object pnlComment: TPanel
      Left = 0
      Top = 127
      Width = 1085
      Height = 44
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object lblComment: TLabel
        Left = 0
        Top = 0
        Width = 1085
        Height = 44
        Align = alClient
        Caption = ' '
        WordWrap = True
      end
    end
    object memExpression: TMemo
      Left = 0
      Top = 24
      Width = 1085
      Height = 103
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      Lines.Strings = (
        'Person.allInstances')
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 3
    end
  end
  object pcResult: TPageControl
    Left = 335
    Top = 220
    Width = 750
    Height = 355
    ActivePage = tsLive
    Align = alClient
    TabOrder = 4
    object tsValue: TTabSheet
      Caption = 'Result'
      object memValue: TMemo
        Left = 0
        Top = 0
        Width = 742
        Height = 325
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsLive: TTabSheet
      Caption = 'Live list'
      ImageIndex = 1
      object pnlLiveHeader: TPanel
        Left = 0
        Top = 0
        Width = 742
        Height = 73
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          742
          73)
        object lblLive: TLabel
          Left = 8
          Top = 8
          Width = 360
          Height = 15
          Caption = 
            'Evaluate a list-valued expression to put a list handle behind th' +
            'is grid.'
        end
        object lblDerivedLegend: TLabel
          Left = 8
          Top = 30
          Width = 521
          Height = 37
          Caption = 
            'A leading slash marks a derived member, the way UML writes one: ' +
            'it is computed from other members, not stored. The Messages pane' +
            ' shows how each one is derived.'
          WordWrap = True
        end
        object btnDescribeColumns: TButton
          Left = 523
          Top = 14
          Width = 200
          Height = 27
          Action = actDescribeColumns
          Anchors = [akTop, akRight]
          TabOrder = 0
        end
      end
      object grdResult: TBoldGrid
        Left = 0
        Top = 73
        Width = 742
        Height = 252
        AddNewAtEnd = False
        Align = alClient
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = lhaResult
        BoldProperties.InternalDrag = False
        Columns = <
          item
            BoldProperties.Expression = ''
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            LookUpProperties.Expression = ''
          end
          item
            BoldProperties.Expression = ''
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            LookUpProperties.Expression = ''
          end>
        DefaultRowHeight = 17
        EnableColAdjust = False
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -12
        TitleFont.Name = 'Segoe UI'
        TitleFont.Style = []
        ColWidths = (
          17
          64)
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 420
    Top = 300
    object miFile: TMenuItem
      Caption = '&File'
      object miOpen: TMenuItem
        Action = actOpenSystem
      end
      object miClose: TMenuItem
        Action = actCloseSystem
      end
      object miOpenSep: TMenuItem
        Caption = '-'
      end
      object miSeed: TMenuItem
        Action = actSeed
      end
      object miClear: TMenuItem
        Action = actClearData
      end
      object miSave: TMenuItem
        Action = actSave
      end
      object miFileSep: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Action = actExit
      end
    end
    object miDatabase: TMenuItem
      Caption = '&Database'
      object miEngineSep: TMenuItem
        Caption = '-'
      end
      object miCheckCurrent: TMenuItem
        Action = actCheckPreconditions
      end
    end
    object miTools: TMenuItem
      Caption = '&Tools'
      object miEditOcl: TMenuItem
        Action = actEditOcl
      end
      object miExplorer: TMenuItem
        Action = actExplorer
      end
      object miToolsSep: TMenuItem
        Caption = '-'
      end
      object miRtDebugger: TMenuItem
        Action = actRtDebugger
      end
    end
  end
  object Actions: TActionList
    OnUpdate = ActionsUpdate
    Left = 490
    Top = 300
    object actOpenSystem: TAction
      Caption = 'Open system'
      OnExecute = actOpenSystemExecute
    end
    object actCloseSystem: TAction
      Caption = 'Close system'
      OnExecute = actCloseSystemExecute
    end
    object actEvaluate: TAction
      Caption = 'Evaluate (F9)'
      ShortCut = 120
      OnExecute = actEvaluateExecute
    end
    object actEditOcl: TAction
      Caption = 'Edit in the Bold OCL editor'
      ShortCut = 113
      OnExecute = actEditOclExecute
    end
    object actExplorer: TAction
      Caption = 'OCL Explorer'
      OnExecute = actExplorerExecute
    end
    object actSeed: TAction
      Caption = 'Create sample data'
      OnExecute = actSeedExecute
    end
    object actClearData: TAction
      Caption = 'Clear all data'
      OnExecute = actClearDataExecute
    end
    object actCheckPreconditions: TAction
      Caption = 'Check preconditions for the current engine'
      OnExecute = actCheckPreconditionsExecute
    end
    object actSave: TAction
      Caption = 'Save to database'
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actExit: TAction
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
    object actRtDebugger: TAction
      Caption = 'Repair failing OCL at runtime'
      OnExecute = actRtDebuggerExecute
    end
    object actDescribeColumns: TAction
      Caption = 'Columns from result type'
      OnExecute = actDescribeColumnsExecute
    end
  end
  object lhaResult: TBoldListHandle
    StaticSystemHandle = DemoDataModule.BoldSystemHandle1
    RootHandle = DemoDataModule.BoldSystemHandle1
    Left = 560
    Top = 300
  end
  object lhaPeople: TBoldListHandle
    StaticSystemHandle = DemoDataModule.BoldSystemHandle1
    RootHandle = DemoDataModule.BoldSystemHandle1
    Expression = 'Person.allInstances->orderBy(lastName)'
    Left = 200
    Top = 620
  end
  object OclVariables: TBoldOclVariables
    Variables = <>
    Left = 270
    Top = 620
  end
end
