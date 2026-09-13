object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Bold ASP Demo Client'
  ClientHeight = 561
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 128
    Width = 41
    Height = 15
    Caption = 'Persons'
  end
  object Label2: TLabel
    Left = 16
    Top = 296
    Width = 109
    Height = 15
    Caption = 'Residential buildings'
  end
  object Label3: TLabel
    Left = 400
    Top = 128
    Width = 349
    Height = 15
    Caption = 
      'Charge rent runs on the server, so the building must be saved fi' +
      'rst.'
  end
  object Label5: TLabel
    Left = 408
    Top = 296
    Width = 40
    Height = 15
    Caption = 'Owners'
  end
  object Label6: TLabel
    Left = 592
    Top = 296
    Width = 50
    Height = 15
    Caption = 'Residents'
  end
  object Label7: TLabel
    Left = 16
    Top = 536
    Width = 300
    Height = 15
    Caption = 'Owners and residents follow the building selected above.'
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 8
    Width = 753
    Height = 105
    Caption = ' Server '
    TabOrder = 0
    object Label4: TLabel
      Left = 16
      Top = 28
      Width = 46
      Height = 15
      Caption = 'URL root'
    end
    object edtURLRoot: TEdit
      Left = 80
      Top = 25
      Width = 425
      Height = 23
      TabOrder = 0
      Text = 'http://localhost/ASPdemo/ASPserver.dll'
    end
    object btnConnect: TButton
      Left = 520
      Top = 24
      Width = 105
      Height = 25
      Caption = 'Connect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object cmdOpen: TButton
      Left = 80
      Top = 64
      Width = 105
      Height = 25
      Action = BoldActivateSystemAction1
      TabOrder = 2
    end
    object cmdSave: TButton
      Left = 200
      Top = 64
      Width = 105
      Height = 25
      Action = BoldUpdateDBAction1
      TabOrder = 3
    end
    object cmdServerSave: TButton
      Left = 320
      Top = 64
      Width = 121
      Height = 25
      Caption = 'Save on server'
      TabOrder = 4
      OnClick = cmdServerSaveClick
    end
    object btnChargeRent: TButton
      Left = 456
      Top = 64
      Width = 121
      Height = 25
      Caption = 'Charge rent'
      TabOrder = 5
      OnClick = btnChargeRentClick
    end
  end
  object grdPersons: TBoldGrid
    Left = 16
    Top = 149
    Width = 753
    Height = 133
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhPersons
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
        BoldProperties.Expression = 'FirstName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'First name'
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'LastName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Last name'
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'Assets'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'IsMarried'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Married'
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'Home.Address'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Home'
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
      64
      64
      64
      64
      64)
  end
  object BoldNavigator1: TBoldNavigator
    Left = 16
    Top = 264
    Width = 240
    Height = 25
    BoldHandle = blhPersons
    TabOrder = 2
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete "%1:s"?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object grdBuildings: TBoldGrid
    Left = 16
    Top = 317
    Width = 377
    Height = 176
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhBuildings
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
        BoldProperties.Expression = 'Address'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'ZipCode'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Zip'
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'TotalRent'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        Title.Caption = 'Total rent'
        LookUpProperties.Expression = ''
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      64
      64)
  end
  object BoldNavigator2: TBoldNavigator
    Left = 16
    Top = 499
    Width = 240
    Height = 25
    BoldHandle = blhBuildings
    TabOrder = 4
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete "%1:s"?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object lbxOwners: TBoldListBox
    Left = 408
    Top = 317
    Width = 169
    Height = 176
    Alignment = taLeftJustify
    BoldHandle = blhOwners
    BoldProperties.InternalDrag = False
    BoldRowProperties.Expression = 'FirstName'
    DragMode = dmAutomatic
    ItemHeight = 15
    TabOrder = 5
  end
  object lbxResidents: TBoldListBox
    Left = 592
    Top = 317
    Width = 177
    Height = 176
    Alignment = taLeftJustify
    BoldHandle = blhResidents
    BoldProperties.InternalDrag = False
    BoldRowProperties.Expression = 'FirstName'
    DragMode = dmAutomatic
    ItemHeight = 15
    TabOrder = 6
  end
  object blhPersons: TBoldListHandle
    StaticSystemHandle = dmClient.BoldSystemHandle1
    RootHandle = dmClient.BoldSystemHandle1
    Expression = 'Person.allInstances'
    Left = 680
    Top = 152
  end
  object blhBuildings: TBoldListHandle
    StaticSystemHandle = dmClient.BoldSystemHandle1
    RootHandle = dmClient.BoldSystemHandle1
    Expression = 'Residential_Building.allInstances'
    Left = 680
    Top = 200
  end
  object blhOwners: TBoldListHandle
    StaticSystemHandle = dmClient.BoldSystemHandle1
    RootHandle = blhBuildings
    Expression = 'Owners'
    Left = 680
    Top = 248
  end
  object blhResidents: TBoldListHandle
    StaticSystemHandle = dmClient.BoldSystemHandle1
    RootHandle = blhBuildings
    Expression = 'Residents'
    Left = 680
    Top = 296
  end
  object ActionList1: TActionList
    Left = 680
    Top = 344
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = dmClient.BoldSystemHandle1
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Save'
      ShortCut = 16467
      BoldSystemHandle = dmClient.BoldSystemHandle1
    end
  end
end
