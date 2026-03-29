{$INCLUDE bold.inc}
unit Step5Form;

{ ============================================================================
  Step5Form - Inheritance, Lookup Tables, and VAT Calculations
  ============================================================================

  This step teaches two fundamental OOP concepts in Bold:

  1. INHERITANCE  (Customer -> Company / PrivatePerson)
  2. LOOKUP/REFERENCE classes  (VATRate)

  NEW CONCEPTS IN THIS STEP
  -------------------------

  INHERITANCE IN BOLD
    In UML and Bold, a class can have subclasses. Customer is the base class
    with shared attributes (Name, Address, City, Country). Company adds
    VATNumber and VATExempt. PrivatePerson adds nothing extra.

    In the database, Bold uses Table-Per-Hierarchy (TPH): all customer types
    share one table, with a BOLD_TYPE column to distinguish them. This is
    efficient for queries (no joins needed) and lets you query
    Customer.allInstances to get both companies and private persons.

    Key OCL operations for inheritance:
      Customer.allInstances              -> all customers (both types)
      Company.allInstances               -> only companies
      PrivatePerson.allInstances         -> only private persons
      customer.oclIsTypeOf(Company)      -> true if this specific customer is a Company
      customer.oclAsType(Company)        -> cast to Company (to access VATNumber)

  ABSTRACT CLASSES
    Customer is marked abstract in the model, meaning you cannot create a
    Customer directly - only Company or PrivatePerson. The Bold navigator
    for Customer.allInstances won't have an Insert button; instead, we
    provide explicit "New Company" and "New Person" buttons.

  LOOKUP CLASSES (VATRate)
    VATRate is a simple reference table with Name and Percentage. Invoice
    items link to a VATRate via an association. This is the Bold equivalent
    of a foreign key to a lookup table.

    In the Items grid, the VATRate column uses a LookUpProperties expression
    to show a dropdown of all available VAT rates. The user picks one, and
    Bold automatically updates the VATAmount derived attribute.

  DERIVED ATTRIBUTE: VATAmount
    InvoiceItem.VATAmount = lineTotal * vatRate.percentage / 100
    This navigates the association (vatRate) to read the percentage, then
    calculates the VAT. When you change the VAT rate or the line total,
    VATAmount updates automatically.

  HANDLE CHAIN
  ------------
    BoldSystemHandle1
         |
         +--> lhaCustomers       (Expression='Customer.allInstances')
         |         |
         |         +--> lhaInvoices  (Expression='invoices')
         |                   |
         |                   +--> lhaItems  (Expression='items')
         |
         +--> lhaVATRates        (Expression='VATRate.allInstances')
         |
         +--> lhaCompanies       (Expression='Company.allInstances')

  UI LAYOUT
  ---------
    +----------------------------------------------------+
    | Step 5: Inheritance & VAT                          |
    +---------------------+------------------------------+
    | All Customers       | VAT Rates                    |
    | (Company + Person)  | [grid: Name | %]             |
    | [grid]              | [nav]                        |
    | [New Company]       +------------------------------+
    | [New Person]        | Invoices for Customer         |
    | [Delete]            | [grid]  [nav]                |
    +---------------------+------------------------------+
    | Invoice Items (with VAT)                           |
    | [grid: Desc | Qty | Price | Total | Rate | VAT]    |
    | [nav]                                              |
    +----------------------------------------------------+
    | [Save] [Open/Close]   Status: Active               |
    +----------------------------------------------------+
  ============================================================================ }

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Grids,
  Vcl.StdCtrls,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldGrid,
  BoldHandle,
  BoldHandles,
  BoldLabel,
  BoldListHandle,
  BoldNavigator,
  BoldNavigatorDefs,
  BoldRootedHandles,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle;

type
  TfrmStep5 = class(TForm)
    { TOP PANEL }
    pnlTop: TPanel;
    lblTitle: TLabel;
    lblCustomerCount: TBoldLabel;

    { LEFT PANEL - All Customers (polymorphic list) }
    pnlLeft: TPanel;
    lblCustomers: TLabel;
    grdCustomers: TBoldGrid;
    btnNewCompany: TButton;
    btnNewPerson: TButton;
    btnDeleteCustomer: TButton;

    { TOP RIGHT - VAT Rates }
    pnlVATRates: TPanel;
    lblVATRates: TLabel;
    grdVATRates: TBoldGrid;
    bnVATRates: TBoldNavigator;

    { MIDDLE RIGHT - Invoices }
    pnlInvoices: TPanel;
    lblInvoices: TLabel;
    grdInvoices: TBoldGrid;
    bnInvoices: TBoldNavigator;

    { BOTTOM - Items with VAT }
    pnlItems: TPanel;
    lblItems: TLabel;
    grdItems: TBoldGrid;
    bnItems: TBoldNavigator;

    { Splitters }
    SplitterLeft: TSplitter;
    SplitterMiddle: TSplitter;
    SplitterItems: TSplitter;

    { BOTTOM PANEL }
    pnlBottom: TPanel;
    btnSave: TButton;
    btnOpenClose: TButton;
    lblStatus: TLabel;

    { HANDLES }
    lhaCustomers: TBoldListHandle;
    lhaInvoices: TBoldListHandle;
    lhaItems: TBoldListHandle;
    lhaVATRates: TBoldListHandle;
    lhaCompanies: TBoldListHandle;

    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSaveClick(Sender: TObject);
    procedure btnNewCompanyClick(Sender: TObject);
    procedure btnNewPersonClick(Sender: TObject);
    procedure btnDeleteCustomerClick(Sender: TObject);
  private
    procedure HandleSystemOpened(Sender: TObject);
    procedure HandleSystemClosed(Sender: TObject);
    procedure UpdateStatus;
  end;

var
  frmStep5: TfrmStep5;

implementation

uses
  System.UITypes,
  Step5DataModule,
  InvoiceClasses;

{$R *.DFM}

{ FormCreate
  Opens the Bold system. Once active, all handles evaluate their OCL
  expressions and the grids populate. Customer.allInstances returns BOTH
  Company and PrivatePerson objects (polymorphic query). }

procedure TfrmStep5.FormCreate(Sender: TObject);
begin
  dmStep5.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
  dmStep5.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;
  dmStep5.OpenSystem;
end;

procedure TfrmStep5.HandleSystemOpened(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStep5.HandleSystemClosed(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStep5.UpdateStatus;
begin
  if csDestroying in ComponentState then
    Exit;
  if dmStep5.BoldSystemHandle1.Active then
  begin
    lblStatus.Caption := 'Bold System: Active';
    lblStatus.Font.Color := $00008800;
    btnSave.Enabled := True;
    btnNewCompany.Enabled := True;
    btnNewPerson.Enabled := True;
    btnDeleteCustomer.Enabled := True;
  end
  else
  begin
    lblStatus.Caption := 'Bold System: Inactive';
    lblStatus.Font.Color := $000066AA;
    btnSave.Enabled := False;
    btnNewCompany.Enabled := False;
    btnNewPerson.Enabled := False;
    btnDeleteCustomer.Enabled := False;
  end;
end;

{ btnNewCompanyClick
  Creates a new Company object. Because Customer is abstract in our model,
  we don't use the navigator's Insert button for the Customer list. Instead,
  we create the specific subclass type directly.

  BoldSystem.CreateNewObjectByExpressionName('Company') tells Bold to
  instantiate TCompany. The new object automatically appears in both the
  Customer.allInstances list AND the Company.allInstances list. }

procedure TfrmStep5.btnNewCompanyClick(Sender: TObject);
begin
  if dmStep5.BoldSystemHandle1.Active then
    dmStep5.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Company');
end;

{ btnNewPersonClick
  Same pattern, but creates a PrivatePerson instead. }

procedure TfrmStep5.btnNewPersonClick(Sender: TObject);
begin
  if dmStep5.BoldSystemHandle1.Active then
    dmStep5.BoldSystemHandle1.System.CreateNewObjectByExpressionName('PrivatePerson');
end;

{ btnDeleteCustomerClick
  Deletes the currently selected customer (whether Company or PrivatePerson).
  The lhaCustomers handle's CurrentBoldObject is typed as TCustomer, but the
  actual runtime object is either TCompany or TPrivatePerson. Delete works
  polymorphically — Bold removes the object regardless of its concrete type. }

procedure TfrmStep5.btnDeleteCustomerClick(Sender: TObject);
begin
  if dmStep5.BoldSystemHandle1.Active and
     Assigned(lhaCustomers.CurrentBoldObject) then
  begin
    if MessageDlg('Delete this customer?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      lhaCustomers.CurrentBoldObject.Delete;
  end;
end;

procedure TfrmStep5.btnSaveClick(Sender: TObject);
begin
  dmStep5.BoldSystemHandle1.UpdateDatabase;
end;

procedure TfrmStep5.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if dmStep5.BoldSystemHandle1.Active then
    if dmStep5.BoldSystemHandle1.System.DirtyObjects.Count > 0 then
      case MessageDlg('There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: dmStep5.BoldSystemHandle1.System.UpdateDatabase;
        mrNo: dmStep5.BoldSystemHandle1.System.Discard;
        mrCancel: CanClose := False;
      end;
end;

end.
