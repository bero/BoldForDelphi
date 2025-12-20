object frmQueryDemo: TfrmQueryDemo
  Left = 2
  Top = 1
  Caption = 'Bold Master-Detail Demo'
  ClientHeight = 450
  ClientWidth = 757
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 17
    Caption = 'Projects'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 11561472
    Font.Height = -13
    Font.Name = 'Segoe UI Semibold'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 192
    Width = 153
    Height = 17
    Caption = 'Tasks for Selected Project'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 11561472
    Font.Height = -13
    Font.Name = 'Segoe UI Semibold'
    Font.Style = []
    ParentFont = False
  end
  object BoldLabel1: TBoldLabel
    Left = 70
    Top = 8
    Width = 226
    Height = 15
    BoldHandle = lhaProjects
    BoldProperties.Expression = #39'('#39' + Project.allinstances->size.asString + '#39')'#39
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 367
    Top = 190
    Width = 51
    Height = 17
    Caption = 'All Tasks'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 11561472
    Font.Height = -13
    Font.Name = 'Segoe UI Semibold'
    Font.Style = []
    ParentFont = False
  end
  object lblInfo: TLabel
    Left = 424
    Top = 29
    Width = 257
    Height = 104
    Caption = 
      'This demo shows Bold'#39's master-detail capabilities:'#13#10#13#10'  '#8226' Top-le' +
      'ft grid: All Projects'#13#10'  '#8226' Bottom-left grid: Tasks for the selec' +
      'ted project'#13#10'  '#8226' Bottom-right grid: All tasks in the system'#13#10#13#10'P' +
      'rojects own their tasks (composite aggregation).'#13#10'Deleting a pro' +
      'ject cascades to its tasks.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6316128
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object grdTasks: TBoldGrid
    Left = 8
    Top = 211
    Width = 353
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = lhaProjectTasks
    BoldProperties.InternalDrag = False
    Columns = <
      item
        BoldProperties.Expression = ''
        Color = 15790320
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'title'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Title'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'priority'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Priority'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'isCompleted'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Completed'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'dueDate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Due Date'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end>
    DefaultRowHeight = 22
    EnableColAdjust = False
    FixedColor = 15790320
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4210752
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = 4210752
    TitleFont.Height = -11
    TitleFont.Name = 'Segoe UI Semibold'
    TitleFont.Style = []
    ColWidths = (
      17
      80
      55
      70
      75)
  end
  object bnProjectTasks: TBoldNavigator
    Left = 8
    Top = 336
    Width = 156
    Height = 25
    BoldDeleteMode = dmRemoveFromList
    BoldHandle = lhaProjectTasks
    TabOrder = 1
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete task?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object btnSave: TButton
    Left = 543
    Top = 337
    Width = 85
    Height = 28
    Caption = 'Save Changes'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object Button1: TButton
    Left = 634
    Top = 337
    Width = 115
    Height = 28
    Action = BoldActivateSystemAction1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4210752
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 370
    Width = 757
    Height = 80
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhitesmoke
    ParentBackground = False
    TabOrder = 4
    object lblConfigFile: TLabel
      Left = 12
      Top = 10
      Width = 600
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
      Left = 12
      Top = 30
      Width = 600
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
      Left = 12
      Top = 51
      Width = 600
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
  end
  object bnProjects: TBoldNavigator
    Left = 8
    Top = 161
    Width = 162
    Height = 25
    BoldHandle = lhaProjects
    TabOrder = 5
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete project?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object btnClear: TButton
    Left = 176
    Top = 161
    Width = 100
    Height = 25
    Caption = 'Clear All Data'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4210943
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = btnClearClick
  end
  object grdProjects: TBoldGrid
    Left = 8
    Top = 30
    Width = 393
    Height = 125
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = lhaProjects
    BoldProperties.InternalDrag = False
    Columns = <
      item
        BoldProperties.Expression = ''
        Color = 15790320
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Name'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'description'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Description'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'startDate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Start Date'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'endDate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'End Date'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'budget'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Budget'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end>
    DefaultRowHeight = 22
    EnableColAdjust = False
    FixedColor = 15790320
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4210752
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = 4210752
    TitleFont.Height = -11
    TitleFont.Name = 'Segoe UI Semibold'
    TitleFont.Style = []
    ColWidths = (
      17
      80
      90
      70
      70
      60)
  end
  object BoldGrid1: TBoldGrid
    Left = 367
    Top = 211
    Width = 382
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = lhaTasks
    BoldProperties.InternalDrag = False
    Columns = <
      item
        BoldProperties.Expression = ''
        Color = 15790320
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'title'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Title'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'priority'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Priority'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'isCompleted'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Completed'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'dueDate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Due Date'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'project.name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Project'
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -12
        Title.Font.Name = 'Segoe UI'
        Title.Font.Style = []
        LookUpProperties.Expression = ''
      end>
    DefaultRowHeight = 22
    EnableColAdjust = False
    FixedColor = 15790320
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4210752
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = 4210752
    TitleFont.Height = -11
    TitleFont.Name = 'Segoe UI Semibold'
    TitleFont.Style = []
    ColWidths = (
      17
      80
      50
      70
      75
      70)
  end
  object bnTasks: TBoldNavigator
    Left = 367
    Top = 335
    Width = 156
    Height = 25
    BoldDeleteMode = dmDelete
    BoldHandle = lhaTasks
    TabOrder = 9
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete task?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object btnAdd: TButton
    Left = 282
    Top = 161
    Width = 47
    Height = 25
    Caption = 'Add'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4210943
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 335
    Top = 161
    Width = 47
    Height = 25
    Caption = 'Delete'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4210943
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    OnClick = btnDeleteClick
  end
  object lhaTasks: TBoldListHandle
    StaticSystemHandle = DemoDataModule.BoldSystemHandle1
    RootHandle = DemoDataModule.BoldSystemHandle1
    Expression = 'Task.allInstances'
    Left = 476
    Top = 256
  end
  object lhaProjects: TBoldListHandle
    StaticSystemHandle = DemoDataModule.BoldSystemHandle1
    RootHandle = DemoDataModule.BoldSystemHandle1
    Expression = 'Project.allInstances'
    Left = 220
    Top = 60
  end
  object ActionList1: TActionList
    Left = 244
    Top = 144
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'BoldActivateSystemAction1'
      OnSystemOpened = BoldActivateSystemAction1SystemOpened
      OnSystemClosed = BoldActivateSystemAction1SystemClosed
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
  end
  object lhaProjectTasks: TBoldListHandle
    StaticSystemHandle = DemoDataModule.BoldSystemHandle1
    RootHandle = lhaProjects
    Expression = 'tasks'
    Left = 196
    Top = 264
  end
end
