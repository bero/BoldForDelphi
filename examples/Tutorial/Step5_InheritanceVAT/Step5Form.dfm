object frmStep5: TfrmStep5
  Left = 0
  Top = 0
  Caption = 'Bold Tutorial - Step 5: Inheritance & VAT'
  ClientHeight = 700
  ClientWidth = 1000
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
  object SplitterLeft: TSplitter
    Left = 320
    Top = 35
    Width = 5
    Height = 365
    Color = clSilver
    ParentColor = False
  end
  object SplitterItems: TSplitter
    Left = 0
    Top = 400
    Width = 1000
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Color = clSilver
    ParentColor = False
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1000
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 8
      Top = 8
      Width = 250
      Height = 20
      Caption = 'Step 5: Inheritance & VAT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -15
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object lblCustomerCount: TBoldLabel
      Left = 280
      Top = 10
      Width = 400
      Height = 15
      BoldHandle = lhaCustomers
      BoldProperties.Expression = '('#39'Customers: '#39' + Customer.allInstances->size.asString + '#39'  (Companies: '#39' + Company.allInstances->size.asString + '#39', Persons: '#39' + PrivatePerson.allInstances->size.asString + '#39')'#39')'
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
    Top = 35
    Width = 320
    Height = 365
    Align = alLeft
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      320
      365)
    object lblCustomers: TLabel
      Left = 8
      Top = 5
      Width = 120
      Height = 17
      Caption = 'All Customers'
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
      Width = 304
      Height = 270
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
          BoldProperties.Expression = 'oclType.name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = [fsItalic]
          Title.Caption = 'Type'
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
        70
        120
        80)
    end
    object btnNewCompany: TButton
      Left = 8
      Top = 303
      Width = 95
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'New Company'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnNewCompanyClick
    end
    object btnNewPerson: TButton
      Left = 110
      Top = 303
      Width = 95
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'New Person'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210752
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnNewPersonClick
    end
    object btnDeleteCustomer: TButton
      Left = 212
      Top = 303
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btnDeleteCustomerClick
    end
  end
  object pnlVATRates: TPanel
    Left = 325
    Top = 35
    Width = 675
    Height = 160
    Align = alNone
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    DesignSize = (
      675
      160)
    object lblVATRates: TLabel
      Left = 8
      Top = 5
      Width = 65
      Height = 17
      Caption = 'VAT Rates'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 11561472
      Font.Height = -13
      Font.Name = 'Segoe UI Semibold'
      Font.Style = []
      ParentFont = False
    end
    object grdVATRates: TBoldGrid
      Left = 8
      Top = 25
      Width = 659
      Height = 95
      AddNewAtEnd = False
      Anchors = [akLeft, akTop, akRight]
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = lhaVATRates
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
          BoldProperties.Expression = 'percentage'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'Percentage'
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
        200
        80)
    end
    object bnVATRates: TBoldNavigator
      Left = 8
      Top = 128
      Width = 162
      Height = 25
      BoldHandle = lhaVATRates
      TabOrder = 1
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete VAT rate?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
  end
  object SplitterMiddle: TSplitter
    Left = 325
    Top = 195
    Width = 675
    Height = 5
    Cursor = crVSplit
    Align = alNone
    Anchors = [akLeft, akTop, akRight]
    Color = clSilver
    ParentColor = False
  end
  object pnlInvoices: TPanel
    Left = 325
    Top = 200
    Width = 675
    Height = 200
    Align = alNone
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    DesignSize = (
      675
      200)
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
      Width = 659
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
    Top = 405
    Width = 1000
    Height = 200
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 4
    DesignSize = (
      1000
      200)
    object lblItems: TLabel
      Left = 8
      Top = 5
      Width = 155
      Height = 17
      Caption = 'Invoice Items (with VAT)'
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
      Width = 984
      Height = 130
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
        end
        item
          BoldProperties.Expression = 'vatRate'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 4210752
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          Title.Caption = 'VAT Rate'
          Title.Font.Charset = DEFAULT_CHARSET
          Title.Font.Color = clWindowText
          Title.Font.Height = -12
          Title.Font.Name = 'Segoe UI'
          Title.Font.Style = []
          LookUpProperties.Expression = 'VATRate.allInstances'
        end
        item
          BoldProperties.Expression = 'vatAmount'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          Title.Caption = 'VAT Amount'
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
        200
        50
        80
        80
        120
        90)
    end
    object bnItems: TBoldNavigator
      Left = 8
      Top = 163
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
    Top = 605
    Width = 1000
    Height = 95
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhitesmoke
    ParentBackground = False
    TabOrder = 5
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
      Action = dmStep5.BoldActivateSystemAction1
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
    StaticSystemHandle = dmStep5.BoldSystemHandle1
    RootHandle = dmStep5.BoldSystemHandle1
    Expression = 'Customer.allInstances'
    Left = 50
    Top = 80
  end
  object lhaInvoices: TBoldListHandle
    StaticSystemHandle = dmStep5.BoldSystemHandle1
    RootHandle = lhaCustomers
    Expression = 'invoices'
    Left = 450
    Top = 250
  end
  object lhaItems: TBoldListHandle
    StaticSystemHandle = dmStep5.BoldSystemHandle1
    RootHandle = lhaInvoices
    Expression = 'items'
    Left = 450
    Top = 450
  end
  object lhaVATRates: TBoldListHandle
    StaticSystemHandle = dmStep5.BoldSystemHandle1
    RootHandle = dmStep5.BoldSystemHandle1
    Expression = 'VATRate.allInstances'
    Left = 650
    Top = 80
  end
  object lhaCompanies: TBoldListHandle
    StaticSystemHandle = dmStep5.BoldSystemHandle1
    RootHandle = dmStep5.BoldSystemHandle1
    Expression = 'Company.allInstances'
    Left = 50
    Top = 350
  end
end
