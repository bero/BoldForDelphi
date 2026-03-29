{$INCLUDE bold.inc}
unit Step4Form;

{ ============================================================================
  Step4Form - OCL Expressions, Computed Labels, and Filtered Lists
  ============================================================================

  This final step showcases Bold's most powerful feature: OCL (Object
  Constraint Language). OCL lets you write declarative expressions that
  Bold evaluates reactively - when underlying data changes, all dependent
  expressions automatically recalculate.

  NEW COMPONENTS IN THIS STEP
  ----------------------------

  TBoldLabel (lblCustomerCount, lblInvoiceCount, lblOutstanding)
    - Like a TLabel, but its Caption is driven by an OCL expression
    - Configured in DFM with BoldHandle + Expression properties
    - Updates AUTOMATICALLY when the data changes (no manual refresh)

    Examples from this form's DFM:
      lblCustomerCount:
        BoldHandle  = lhaCustomers
        Expression  = 'Customer.allInstances->size'
        --> Shows: "Customers: 5"

      lblInvoiceCount:
        BoldHandle  = lhaInvoices
        Expression  = 'invoices->size'
        --> Shows: "Invoices: 3" (for selected customer)

      lblOutstanding:
        BoldHandle  = lhaCustomers
        Expression  = 'invoices->select(not isPaid)->collect(totalAmount)->sum'
        --> Shows: "Outstanding: 1500.00" (sum of unpaid invoice totals
            for the selected customer)

  TBoldListHandle with OCL Filter (lhaUnpaidInvoices)
    - Expression = 'Invoice.allInstances->select(not isPaid)'
    - RootHandle = BoldSystemHandle1
    - This creates a FILTERED view: only unpaid invoices from ALL customers
    - The ->select() OCL operator filters the collection
    - When you mark an invoice as paid, it automatically disappears from
      this list (Bold's subscription system detects the change)

  OCL EXPRESSIONS USED IN THIS STEP
  -----------------------------------
  | Expression                                              | Meaning                              |
  |--------------------------------------------------------|--------------------------------------|
  | Customer.allInstances->size                             | Count of all customers               |
  | invoices->size                                          | Count of selected customer's invoices|
  | invoices->select(not isPaid)->collect(totalAmount)->sum  | Sum of unpaid invoice totals         |
  | Invoice.allInstances->select(not isPaid)                | All unpaid invoices (global filter)  |

  OCL OPERATOR REFERENCE
  ----------------------
  ->size        : Returns the number of elements in a collection
  ->select(expr): Filters collection, keeping elements where expr is true
  ->collect(attr): Extracts one attribute from each element (like SQL SELECT)
  ->sum         : Sums numeric values in a collection
  ->sortedBy(attr): Returns collection sorted by the given attribute
  not           : Boolean negation

  DERIVED ATTRIBUTES (configured in the UML model)
  ------------------------------------------------
  Two attributes in our model are "derived" - Bold calculates them from OCL:

    Invoice.TotalAmount = items.lineTotal->sum
      "Sum of all line totals for this invoice's items"

    InvoiceItem.LineTotal = quantity * unitPrice
      "This item's quantity times its unit price"

  These are defined in the model (DFM) with DelphiAttributeType = 'derived'
  and a derivation OCL expression. You never set them manually - Bold
  recalculates them whenever quantity, unitPrice, or the items collection
  changes.

  HANDLE CHAIN (same as Step 3, plus the unpaid filter)
  -----------------------------------------------------
    BoldSystemHandle1
         |
         +--> lhaCustomers     (Expression='Customer.allInstances')
         |         |
         |         +--> lhaInvoices  (Expression='invoices')
         |                   |
         |                   +--> lhaItems  (Expression='items')
         |
         +--> lhaUnpaidInvoices  (Expression='Invoice.allInstances->select(not isPaid)')

  UI LAYOUT
  ---------
    +-------------------------------------------------------+
    | Customers: 5          (TBoldLabel with ->size)         |
    +-----------+-----------+-----------+-------------------+
    | Customers | Invoices  | Items     | Unpaid Invoices   |
    |           | (3)       |           | (global filter)   |
    | [grid]    | [grid]    | [grid]    | [grid]            |
    | [nav]     | [nav]     | [nav]     |                   |
    |           | Outstand: |           |                   |
    |           | $1500.00  |           |                   |
    +-----------+-----------+-----------+-------------------+
    | [Save]  [Open/Close System]   Status: Active          |
    +-------------------------------------------------------+
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
  BoldHandle,           // Base handle class (used by TBoldLabel)
  BoldHandles,
  BoldLabel,            // TBoldLabel - OCL-driven label component
  BoldListHandle,
  BoldNavigator,
  BoldNavigatorDefs,
  BoldRootedHandles,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle;

type
  TfrmStep4 = class(TForm)
    { TOP PANEL - Title and global customer count }
    pnlTop: TPanel;
    lblTitle: TLabel;
    lblCustomerCount: TBoldLabel;  // OCL: 'Customer.allInstances->size'

    { LEFT PANEL - All Customers }
    pnlLeft: TPanel;
    lblCustomers: TLabel;
    grdCustomers: TBoldGrid;
    bnCustomers: TBoldNavigator;

    { MIDDLE PANEL - Invoices for selected Customer }
    pnlRight: TPanel;
    lblInvoices: TLabel;
    lblInvoiceCount: TBoldLabel;   // OCL: 'invoices->size'
    grdInvoices: TBoldGrid;
    bnInvoices: TBoldNavigator;
    lblOutstanding: TBoldLabel;    // OCL: 'invoices->select(not isPaid)->collect(totalAmount)->sum'

    { ITEMS PANEL - Items for selected Invoice }
    pnlItems: TPanel;
    lblItems: TLabel;
    grdItems: TBoldGrid;
    bnItems: TBoldNavigator;

    { Splitters for resizable panels }
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;

    { UNPAID PANEL - Global filtered list of all unpaid invoices }
    pnlUnpaid: TPanel;
    lblUnpaid: TLabel;
    grdUnpaid: TBoldGrid;         // Shows Invoice.allInstances->select(not isPaid)

    { BOTTOM PANEL - Actions and status }
    pnlBottom: TPanel;
    btnSave: TButton;
    btnOpenClose: TButton;
    lblStatus: TLabel;

    { HANDLE CHAIN + FILTER
      lhaCustomers:       Root, Expression='Customer.allInstances'
      lhaInvoices:        Rooted on lhaCustomers, Expression='invoices'
      lhaItems:           Rooted on lhaInvoices, Expression='items'
      lhaUnpaidInvoices:  Root, Expression='Invoice.allInstances->select(not isPaid)' }
    lhaCustomers: TBoldListHandle;
    lhaInvoices: TBoldListHandle;
    lhaItems: TBoldListHandle;
    lhaUnpaidInvoices: TBoldListHandle;

    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSaveClick(Sender: TObject);
  private
    procedure HandleSystemOpened(Sender: TObject);
    procedure HandleSystemClosed(Sender: TObject);
    procedure UpdateStatus;
  end;

var
  frmStep4: TfrmStep4;

implementation

uses
  System.UITypes,
  InvoiceDataModule;

{$R *.DFM}

{ FormCreate
  Standard Bold startup pattern. Once the system opens, all four list handles
  activate and the OCL expressions start evaluating. The BoldLabels
  immediately show computed values (counts, sums) and the grids populate. }

procedure TfrmStep4.FormCreate(Sender: TObject);
begin
  dmInvoice.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
  dmInvoice.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;
  dmInvoice.OpenSystem;
end;

procedure TfrmStep4.HandleSystemOpened(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStep4.HandleSystemClosed(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStep4.UpdateStatus;
begin
  if csDestroying in ComponentState then
    Exit;
  if dmInvoice.BoldSystemHandle1.Active then
  begin
    lblStatus.Caption := 'Bold System: Active';
    lblStatus.Font.Color := $00008800;
    btnSave.Enabled := True;
  end
  else
  begin
    lblStatus.Caption := 'Bold System: Inactive';
    lblStatus.Font.Color := $000066AA;
    btnSave.Enabled := False;
  end;
end;

{ btnSaveClick
  Persists ALL dirty objects to the database. This includes any new or
  modified Customers, Invoices, and InvoiceItems created during the session.
  Derived attributes (TotalAmount, LineTotal) are recalculated by Bold
  before saving - you never need to compute them yourself. }

procedure TfrmStep4.btnSaveClick(Sender: TObject);
begin
  dmInvoice.BoldSystemHandle1.UpdateDatabase;
end;

{ FormCloseQuery
  Standard Bold dirty-object check before closing.
  See Step1Form.pas for detailed explanation. }

procedure TfrmStep4.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if dmInvoice.BoldSystemHandle1.Active then
    if dmInvoice.BoldSystemHandle1.System.DirtyObjects.Count > 0 then
      case MessageDlg('There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: dmInvoice.BoldSystemHandle1.System.UpdateDatabase;
        mrNo: dmInvoice.BoldSystemHandle1.System.Discard;
        mrCancel: CanClose := False;
      end;
end;

end.
