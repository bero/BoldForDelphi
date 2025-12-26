object MainForm: TMainForm
  Left = 2
  Top = 1
  Caption = 'Bold Master-Detail Demo'
  ClientHeight = 500
  ClientWidth = 800
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
  object Splitter1: TSplitter
    Left = 0
    Top = 185
    Width = 800
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Color = clSilver
    ParentColor = False
    ExplicitWidth = 757
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 185
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    OnResize = pnlTopResize
    DesignSize = (
      800
      185)
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
    object BoldLabel1: TBoldLabel
      Left = 70
      Top = 8
      Width = 234
      Height = 15
      BoldHandle = lhaProjects
      BoldProperties.Expression = '('#39'('#39' + Project.allinstances->size.asString + '#39')'#39')'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object grdProjects: TBoldGrid
      Left = 8
      Top = 30
      Width = 784
      Height = 110
      AddNewAtEnd = False
      Anchors = [akLeft, akTop, akRight, akBottom]
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
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = 4210752
      TitleFont.Height = -11
      TitleFont.Name = 'Segoe UI Semibold'
      TitleFont.Style = []
      ColWidths = (
        17
        120
        200
        80
        80
        80)
    end
    object bnProjects: TBoldNavigator
      Left = 8
      Top = 150
      Width = 162
      Height = 25
      Anchors = [akLeft, akBottom]
      BoldHandle = lhaProjects
      TabOrder = 1
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
      Top = 150
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Clear All Data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210943
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnClearClick
    end
    object btnAdd: TButton
      Left = 282
      Top = 150
      Width = 47
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210943
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 335
      Top = 150
      Width = 47
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210943
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnDeleteClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 190
    Width = 800
    Height = 230
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 395
      Top = 0
      Width = 5
      Height = 230
      Color = clSilver
      ParentColor = False
      ExplicitLeft = 400
      ExplicitHeight = 210
    end
    object pnlBottomLeft: TPanel
      Left = 0
      Top = 0
      Width = 395
      Height = 230
      Align = alLeft
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      OnResize = pnlBottomLeftResize
      DesignSize = (
        395
        230)
      object Label3: TLabel
        Left = 8
        Top = 5
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
      object grdTasks: TBoldGrid
        Left = 8
        Top = 25
        Width = 379
        Height = 165
        AddNewAtEnd = False
        Anchors = [akLeft, akTop, akRight, akBottom]
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
            Title.Caption = 'Done'
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
          120
          55
          50
          75)
      end
      object bnProjectTasks: TBoldNavigator
        Left = 8
        Top = 196
        Width = 156
        Height = 25
        Anchors = [akLeft, akBottom]
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
    end
    object pnlBottomRight: TPanel
      Left = 400
      Top = 0
      Width = 400
      Height = 230
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      OnResize = pnlBottomRightResize
      DesignSize = (
        400
        230)
      object Label2: TLabel
        Left = 8
        Top = 5
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
      object BoldGrid1: TBoldGrid
        Left = 8
        Top = 25
        Width = 384
        Height = 165
        AddNewAtEnd = False
        Anchors = [akLeft, akTop, akRight, akBottom]
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
            Title.Caption = 'Pri'
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
            Title.Caption = 'Done'
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
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = 4210752
        TitleFont.Height = -11
        TitleFont.Name = 'Segoe UI Semibold'
        TitleFont.Style = []
        ColWidths = (
          17
          100
          35
          50
          100)
      end
      object bnTasks: TBoldNavigator
        Left = 8
        Top = 196
        Width = 156
        Height = 25
        Anchors = [akLeft, akBottom]
        BoldDeleteMode = dmDelete
        BoldHandle = lhaTasks
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
        Left = 200
        Top = 196
        Width = 85
        Height = 28
        Anchors = [akRight, akBottom]
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
        Left = 291
        Top = 196
        Width = 100
        Height = 28
        Action = DemoDataModule.BoldActivateSystemAction1
        Anchors = [akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 4210752
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
    end
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 420
    Width = 800
    Height = 80
    Align = alBottom
    BevelOuter = bvNone
    Color = $00F5F5F5
    ParentBackground = False
    TabOrder = 1
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
    object btnDropDatabase: TButton
      Left = 688
      Top = 25
      Width = 100
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Drop Database'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnDropDatabaseClick
      ExplicitLeft = 684
    end
  end
  object lhaTasks: TBoldListHandle
    StaticSystemHandle = DemoDataModule.BoldSystemHandle1
    RootHandle = DemoDataModule.BoldSystemHandle1
    Expression = 'Task.allInstances'
    Left = 540
    Top = 300
  end
  object lhaProjects: TBoldListHandle
    StaticSystemHandle = DemoDataModule.BoldSystemHandle1
    RootHandle = DemoDataModule.BoldSystemHandle1
    Expression = 'Project.allInstances'
    Left = 220
    Top = 60
  end
  object lhaProjectTasks: TBoldListHandle
    StaticSystemHandle = DemoDataModule.BoldSystemHandle1
    RootHandle = lhaProjects
    Expression = 'tasks'
    Left = 196
    Top = 300
  end
end
