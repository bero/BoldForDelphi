object OclExplorerForm: TOclExplorerForm
  Left = 132
  Top = 107
  Caption = 'OCL Explorer'
  ClientHeight = 465
  ClientWidth = 969
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object Splitter5: TSplitter
    Left = 497
    Top = 0
    Width = 4
    Height = 465
    ExplicitHeight = 466
  end
  object Panel1: TPanel
    Left = 501
    Top = 0
    Width = 468
    Height = 465
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 472
    ExplicitHeight = 466
    object Splitter1: TSplitter
      Left = 1
      Top = 137
      Width = 470
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 470
      Height = 136
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object Panel3: TPanel
        Left = 5
        Top = 5
        Width = 460
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          456
          28)
        object List2EditOCLButton: TButton
          Left = 0
          Top = 0
          Width = 81
          Height = 25
          Action = EditList2Action
          TabOrder = 0
        end
        object cbEvaluateInPS2: TCheckBox
          Left = 356
          Top = 6
          Width = 97
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Evaluate in PS'
          TabOrder = 1
          OnClick = cbEvaluateInPS2Click
          ExplicitLeft = 360
        end
      end
      object List2OCLMemo: TMemo
        Left = 5
        Top = 33
        Width = 456
        Height = 98
        Align = alClient
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
        ExplicitWidth = 460
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 141
      Width = 466
      Height = 323
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 470
      ExplicitHeight = 324
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 470
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label2: TLabel
          Left = 4
          Top = 7
          Width = 25
          Height = 13
          Caption = 'List 2'
        end
      end
      object RightGrid: TBoldGrid
        Left = 0
        Top = 25
        Width = 495
        Height = 299
        Align = alClient
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = List2Handle
        BoldProperties.InternalDrag = False
        DragMode = dmAutomatic
        TabOrder = 1
        Columns = <
        item
          BoldProperties.Expression = 'boldId'
          Title.Caption = 'BoldId'
        end
        item
          BoldProperties.Expression = 'oclType'
          Title.Caption = 'Class'
        end
        item
          BoldProperties.Expression = ''
          Title.Caption = 'AsString'
        end>
      end
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 465
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 466
    object Splitter3: TSplitter
      Left = 1
      Top = 137
      Width = 495
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
    object Panel9: TPanel
      Left = 1
      Top = 1
      Width = 495
      Height = 136
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object Panel10: TPanel
        Left = 5
        Top = 5
        Width = 485
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          485
          28)
        object List1EditOCLButton: TButton
          Left = 0
          Top = 0
          Width = 81
          Height = 25
          Action = EditList1Action
          TabOrder = 0
        end
        object cbEvaluateInPS1: TCheckBox
          Left = 384
          Top = 6
          Width = 97
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Evaluate in PS'
          TabOrder = 1
          OnClick = cbEvaluateInPS1Click
        end
      end
      object List1OCLMemo: TMemo
        Left = 5
        Top = 33
        Width = 485
        Height = 98
        Align = alClient
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
      end
    end
    object Panel11: TPanel
      Left = 1
      Top = 141
      Width = 495
      Height = 323
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 324
      object Panel12: TPanel
        Left = 0
        Top = 0
        Width = 495
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 4
          Top = 7
          Width = 25
          Height = 13
          Caption = 'List 1'
        end
      end
      object LeftGrid: TBoldGrid
        Left = 0
        Top = 25
        Width = 495
        Height = 299
        Align = alClient
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = List1Handle
        BoldProperties.InternalDrag = False
        DragMode = dmAutomatic
        TabOrder = 1
        Columns = <
        item
          BoldProperties.Expression = ''
          BoldProperties.Renderer = brIndex
          Title.Caption = 'Index'
        end
        item
          BoldProperties.Expression = ''
          Title.Caption = 'Class'
        end
        item
          BoldProperties.Expression = ''
          BoldProperties.Renderer = brObjects
          Title.Caption = 'Objects'
        end
        item
          BoldProperties.Expression = ''
          BoldProperties.Renderer = brIds
          Title.Caption = 'Ids'
        end
        item
          BoldProperties.Expression = ''
          BoldProperties.Renderer = bfIsPersistent
          Title.Caption = 'Persistent'
        end
        item
          BoldProperties.Expression = ''
          BoldProperties.Renderer = brIsAbstract
          Title.Caption = 'Abstract'
        end
        item
          BoldProperties.Expression = ''
          BoldProperties.Renderer = brIsLinkClass
          Title.Caption = 'LinkClass'
        end
        item
          BoldProperties.Expression = ''
          BoldProperties.Renderer = bfClassState
          Title.Caption = 'Loaded'
        end>
      end
    end
  end
  object List1Handle: TBoldListHandle
    Left = 16
    Top = 104
  end
  object List2Handle: TBoldListHandle
    Variables = BoldVariableDefinition
    Left = 516
    Top = 104
  end
  object BoldVariableDefinition: TBoldOclVariables
    Variables = <
      item
        BoldHandle = List1Handle
        VariableName = 'list'
        UseListElement = True
      end>
    Left = 49
    Top = 105
  end
  object MainMenu1: TMainMenu
    Left = 88
    Top = 104
    object FileMenu: TMenuItem
      Caption = '&File'
      object EditList1OCL1: TMenuItem
        Action = EditList1Action
      end
      object EditList2OCL1: TMenuItem
        Action = EditList2Action
      end
      object UpdateDB1: TMenuItem
        Action = BoldUpdateDBAction1
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = CloseApplicationAction
      end
    end
    object ToolsMenu: TMenuItem
      Caption = '&Tools'
      object EditOCL1: TMenuItem
        Action = ShowDebuggerAction
      end
      object ShowOCLSummary1: TMenuItem
        Action = ShowOCLSyntaxSummary
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Action = AboutAction
      end
    end
  end
  object ActionList1: TActionList
    Left = 128
    Top = 104
    object CloseApplicationAction: TAction
      Caption = '&Close'
      OnExecute = CloseApplicationActionExecute
    end
    object AboutAction: TAction
      Caption = '&About'
    end
    object ShowDebuggerAction: TAction
      Caption = 'Show &Debugger'
      OnExecute = ShowDebuggerActionExecute
    end
    object EditList1Action: TAction
      Caption = 'Edit List &1 OCL'
      OnExecute = List1EditOCL
    end
    object EditList2Action: TAction
      Caption = 'Edit List &2 OCL'
      OnExecute = List2EditOCL
    end
    object ShowOCLSyntaxSummary: TAction
      Caption = 'Show &OCL Syntax Summary'
      OnExecute = ShowOCLSyntaxSummaryExecute
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      ShortCut = 16467
    end
  end
  object BoldPlaceableListSubscriber1: TBoldPlaceableListSubscriber
    BoldHandle = List1Handle
    BoldRowProperties.Expression = ''
    OnAfterMakeUptoDate = BoldPlaceableListSubscriber1AfterMakeUptoDate
    Left = 233
    Top = 73
  end
  object BoldPlaceableListSubscriber2: TBoldPlaceableListSubscriber
    BoldHandle = List1Handle
    BoldRowProperties.Expression = ''
    OnAfterMakeUptoDate = BoldPlaceableListSubscriber2AfterMakeUptoDate
    Left = 625
    Top = 73
  end
  object brObjects: TBoldAsStringRenderer
    OnSubscribe = brClassListSubscribe
    OnGetAsString = brObjectsGetAsString
    Left = 265
    Top = 237
  end
  object brIndex: TBoldAsStringRenderer
    OnGetAsString = brIndexGetAsString
    Left = 17
    Top = 237
  end
  object bfIsPersistent: TBoldAsStringRenderer
    OnGetAsString = bfIsPersistentGetAsString
    Left = 377
    Top = 237
  end
  object bfClassState: TBoldAsStringRenderer
    OnSubscribe = bfClassStateSubscribe
    OnGetAsString = bfClassStateGetAsString
    Left = 449
    Top = 237
  end
  object brIds: TBoldAsStringRenderer
    OnSubscribe = brClassListSubscribe
    OnGetAsString = brIdsGetAsString
    Left = 320
    Top = 240
  end
  object brIsAbstract: TBoldAsStringRenderer
    OnGetAsString = brIsAbstractGetAsString
    Left = 360
    Top = 144
  end
  object brIsLinkClass: TBoldAsStringRenderer
    OnGetAsString = brIsLinkClassGetAsString
    Left = 296
    Top = 144
  end
end
