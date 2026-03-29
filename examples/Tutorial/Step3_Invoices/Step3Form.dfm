object frmStep3: TfrmStep3
  Left = 0
  Top = 0
  Caption = 'Bold Tutorial - Step 3: Invoices and Associations'
  ClientHeight = 600
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
    Top = 165
    Width = 800
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Color = clSilver
    ParentColor = False
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 365
    Width = 800
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Color = clSilver
    ParentColor = False
  end
  object pnlCustomers: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 165
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      800
      165)
    object lblCustomers: TLabel
      Left = 8
      Top = 5
      Width = 65
      Height = 17
      Caption = 'Customers'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object grdCustomers: TBoldGrid
      Left = 8
      Top = 25
      Width = 784
      Height = 100
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
        150
        100
        100
        200)
    end
    object bnCustomers: TBoldNavigator
      Left = 8
      Top = 133
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
  object pnlInvoices: TPanel
    Left = 0
    Top = 170
    Width = 800
    Height = 195
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      800
      195)
    object lblInvoices: TLabel
      Left = 8
      Top = 5
      Width = 145
      Height = 17
      Caption = 'Invoices for Customer'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object grdInvoices: TBoldGrid
      Left = 8
      Top = 25
      Width = 784
      Height = 130
      AddNewAtEnd = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = lhaInvoices
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
          BoldProperties.Expression = 'invoiceNumber'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Invoice #'
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -12
          Title.Font.Name = 'Segoe UI'
          Title.Font.Style = []
          LookUpProperties.Expression = ''
        end
        item
          BoldProperties.Expression = 'invoiceDate'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Date'
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
          BoldProperties.Expression = 'isPaid'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Paid'
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -12
          Title.Font.Name = 'Segoe UI'
          Title.Font.Style = []
          LookUpProperties.Expression = ''
        end
        item
          BoldProperties.Expression = 'totalAmount'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Total'
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
        90
        90
        50
        90)
    end
    object bnInvoices: TBoldNavigator
      Left = 8
      Top = 163
      Width = 162
      Height = 25
      Anchors = [akLeft, akBottom]
      BoldHandle = lhaInvoices
      TabOrder = 1
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete invoice?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
  end
  object pnlItems: TPanel
    Left = 0
    Top = 370
    Width = 800
    Height = 170
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    DesignSize = (
      800
      170)
    object lblItems: TLabel
      Left = 8
      Top = 5
      Width = 108
      Height = 17
      Caption = 'Invoice Line Items'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object grdItems: TBoldGrid
      Left = 8
      Top = 25
      Width = 784
      Height = 100
      AddNewAtEnd = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = lhaItems
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
          BoldProperties.Expression = 'quantity'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Qty'
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -12
          Title.Font.Name = 'Segoe UI'
          Title.Font.Style = []
          LookUpProperties.Expression = ''
        end
        item
          BoldProperties.Expression = 'unitPrice'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Unit Price'
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -12
          Title.Font.Name = 'Segoe UI'
          Title.Font.Style = []
          LookUpProperties.Expression = ''
        end
        item
          BoldProperties.Expression = 'lineTotal'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Line Total'
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
        250
        60
        90
        90)
    end
    object bnItems: TBoldNavigator
      Left = 8
      Top = 133
      Width = 162
      Height = 25
      Anchors = [akLeft, akBottom]
      BoldHandle = lhaItems
      TabOrder = 1
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete item?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 540
    Width = 800
    Height = 60
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhitesmoke
    ParentBackground = False
    TabOrder = 3
    object lblStatus: TLabel
      Left = 240
      Top = 22
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
    Left = 700
    Top = 50
  end
  object lhaInvoices: TBoldListHandle
    StaticSystemHandle = InvoiceDataModule.BoldSystemHandle1
    RootHandle = lhaCustomers
    Expression = 'invoices'
    Left = 700
    Top = 220
  end
  object lhaItems: TBoldListHandle
    StaticSystemHandle = InvoiceDataModule.BoldSystemHandle1
    RootHandle = lhaInvoices
    Expression = 'items'
    Left = 700
    Top = 400
  end
end
