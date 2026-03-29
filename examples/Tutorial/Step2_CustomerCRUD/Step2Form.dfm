object frmStep2: TfrmStep2
  Left = 0
  Top = 0
  Caption = 'Bold Tutorial - Step 2: Customer CRUD'
  ClientHeight = 500
  ClientWidth = 600
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
    Width = 600
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 8
      Top = 8
      Width = 300
      Height = 20
      Caption = 'Step 2: Customer CRUD'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -15
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
  end
  object pnlGrid: TPanel
    Left = 0
    Top = 35
    Width = 600
    Height = 200
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      600
      200)
    object grdCustomers: TBoldGrid
      Left = 8
      Top = 5
      Width = 584
      Height = 155
      AddNewAtEnd = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = lhaCustomers
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
          BoldProperties.Expression = 'city'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'City'
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -12
          Title.Font.Name = 'Segoe UI'
          Title.Font.Style = []
          LookUpProperties.Expression = ''
        end
        item
          BoldProperties.Expression = 'phone'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Phone'
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -12
          Title.Font.Name = 'Segoe UI'
          Title.Font.Style = []
          LookUpProperties.Expression = ''
        end
        item
          BoldProperties.Expression = 'email'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Email'
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
        130
        100
        100
        150)
    end
    object bnCustomers: TBoldNavigator
      Left = 8
      Top = 168
      Width = 162
      Height = 25
      Anchors = [akLeft, akBottom]
      BoldHandle = lhaCustomers
      TabOrder = 1
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete customer?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
  end
  object pnlEdit: TPanel
    Left = 0
    Top = 235
    Width = 600
    Height = 180
    Align = alClient
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 2
    object lblName: TLabel
      Left = 20
      Top = 15
      Width = 60
      Height = 15
      Caption = 'Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblAddress: TLabel
      Left = 20
      Top = 45
      Width = 60
      Height = 15
      Caption = 'Address:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblCity: TLabel
      Left = 20
      Top = 75
      Width = 60
      Height = 15
      Caption = 'City:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblPhone: TLabel
      Left = 20
      Top = 105
      Width = 60
      Height = 15
      Caption = 'Phone:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblEmail: TLabel
      Left = 20
      Top = 135
      Width = 60
      Height = 15
      Caption = 'Email:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object edtName: TBoldEdit
      Left = 90
      Top = 12
      Width = 250
      Height = 23
      BoldHandle = lhaCustomers
      BoldProperties.Expression = 'name'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object edtAddress: TBoldEdit
      Left = 90
      Top = 42
      Width = 400
      Height = 23
      BoldHandle = lhaCustomers
      BoldProperties.Expression = 'address'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object edtCity: TBoldEdit
      Left = 90
      Top = 72
      Width = 250
      Height = 23
      BoldHandle = lhaCustomers
      BoldProperties.Expression = 'city'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object edtPhone: TBoldEdit
      Left = 90
      Top = 102
      Width = 200
      Height = 23
      BoldHandle = lhaCustomers
      BoldProperties.Expression = 'phone'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object edtEmail: TBoldEdit
      Left = 90
      Top = 132
      Width = 300
      Height = 23
      BoldHandle = lhaCustomers
      BoldProperties.Expression = 'email'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 415
    Width = 600
    Height = 85
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhitesmoke
    ParentBackground = False
    TabOrder = 3
    object lblStatus: TLabel
      Left = 12
      Top = 55
      Width = 400
      Height = 15
      AutoSize = False
      Caption = 'Bold System: (checking...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object btnSave: TButton
      Left = 12
      Top = 15
      Width = 100
      Height = 30
      Caption = 'Save Changes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnSaveClick
    end
    object btnOpenClose: TButton
      Left = 120
      Top = 15
      Width = 100
      Height = 30
      Action = InvoiceDataModule.BoldActivateSystemAction1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object lhaCustomers: TBoldListHandle
    StaticSystemHandle = InvoiceDataModule.BoldSystemHandle1
    RootHandle = InvoiceDataModule.BoldSystemHandle1
    Expression = 'Customer.allInstances'
    Left = 500
    Top = 100
  end
end
