object frmStep4: TfrmStep4
  Left = 0
  Top = 0
  Caption = 'Bold Tutorial - Step 4: OCL Expressions'
  ClientHeight = 650
  ClientWidth = 900
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
  object Splitter2: TSplitter
    Left = 0
    Top = 35
    Width = 900
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Color = clSilver
    ParentColor = False
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 525
    Width = 900
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    Color = clSilver
    ParentColor = False
  end
  object Splitter1: TSplitter
    Left = 350
    Top = 190
    Width = 5
    Height = 335
    Color = clSilver
    ParentColor = False
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 8
      Top = 8
      Width = 161
      Height = 20
      Caption = 'Step 4: OCL Expressions'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -15
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object lblCustomerCount: TBoldLabel
      Left = 250
      Top = 10
      Width = 312
      Height = 15
      BoldHandle = lhaCustomers
      BoldProperties.Expression = '('#39'Total Customers: '#39' + Customer.allInstances->size.asString)'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 190
    Width = 350
    Height = 335
    Align = alLeft
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      350
      335)
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
    object lblOutstanding: TBoldLabel
      Left = 8
      Top = 280
      Width = 451
      Height = 15
      Anchors = [akLeft, akBottom]
      BoldHandle = lhaCustomers
      BoldProperties.Expression = 
        '('#39'Outstanding: '#39' + invoices->select(not isPaid)->collect(totalAm' +
        'ount)->sum.asString)'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object grdCustomers: TBoldGrid
      Left = 8
      Top = 25
      Width = 334
      Height = 245
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
        100)
    end
    object bnCustomers: TBoldNavigator
      Left = 8
      Top = 300
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
  object pnlRight: TPanel
    Left = 355
    Top = 190
    Width = 545
    Height = 335
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    DesignSize = (
      545
      335)
    object lblInvoices: TLabel
      Left = 8
      Top = 5
      Width = 133
      Height = 17
      Caption = 'Invoices for Customer'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object lblInvoiceCount: TBoldLabel
      Left = 170
      Top = 7
      Width = 176
      Height = 15
      BoldHandle = lhaCustomers
      BoldProperties.Expression = '('#39'('#39' + invoices->size.asString + '#39')'#39')'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblItems: TLabel
      Left = 8
      Top = 185
      Width = 109
      Height = 17
      Caption = 'Invoice Line Items'
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
      Width = 529
      Height = 120
      AddNewAtEnd = False
      Anchors = [akLeft, akTop, akRight]
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
        50
        90)
    end
    object bnInvoices: TBoldNavigator
      Left = 8
      Top = 153
      Width = 162
      Height = 25
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
    object grdItems: TBoldGrid
      Left = 8
      Top = 205
      Width = 529
      Height = 90
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
      TabOrder = 2
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = 4210752
      TitleFont.Height = -11
      TitleFont.Name = 'Segoe UI Semibold'
      TitleFont.Style = []
      ColWidths = (
        17
        200
        50
        80
        80)
    end
    object bnItems: TBoldNavigator
      Left = 8
      Top = 300
      Width = 162
      Height = 25
      Anchors = [akLeft, akBottom]
      BoldHandle = lhaItems
      TabOrder = 3
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
  object pnlUnpaid: TPanel
    Left = 0
    Top = 40
    Width = 900
    Height = 150
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    DesignSize = (
      900
      150)
    object lblUnpaid: TLabel
      Left = 8
      Top = 5
      Width = 182
      Height = 17
      Caption = 'All Unpaid Invoices (OCL filter)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object grdUnpaid: TBoldGrid
      Left = 8
      Top = 25
      Width = 884
      Height = 115
      AddNewAtEnd = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = lhaUnpaidInvoices
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
          BoldProperties.Expression = 'customer.name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Customer'
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
          BoldProperties.Expression = 'totalAmount'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Amount'
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
        80
        90
        100)
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 530
    Width = 900
    Height = 120
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhitesmoke
    ParentBackground = False
    TabOrder = 4
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
    Left = 50
    Top = 80
  end
  object lhaInvoices: TBoldListHandle
    StaticSystemHandle = InvoiceDataModule.BoldSystemHandle1
    RootHandle = lhaCustomers
    Expression = 'invoices'
    Left = 450
    Top = 80
  end
  object lhaItems: TBoldListHandle
    StaticSystemHandle = InvoiceDataModule.BoldSystemHandle1
    RootHandle = lhaInvoices
    Expression = 'items'
    Left = 450
    Top = 250
  end
  object lhaUnpaidInvoices: TBoldListHandle
    StaticSystemHandle = InvoiceDataModule.BoldSystemHandle1
    RootHandle = InvoiceDataModule.BoldSystemHandle1
    Expression = 'Invoice.allInstances->select(not isPaid)'
    Left = 450
    Top = 400
  end
end
