{$INCLUDE bold.inc}
unit Step3Form;

{ ============================================================================
  Step3Form - Invoices and Associations (Master-Detail)
  ============================================================================

  This step introduces UML ASSOCIATIONS - the relationships between classes.
  Our model has two associations:

    Customer --1:*--> Invoice --1:*--> InvoiceItem
       "A customer has many invoices, each invoice has many items"

  HANDLE CHAINING (the key concept)
  ----------------------------------
  In Bold, you create master-detail views by "chaining" list handles:

    lhaCustomers (root handle)
      Expression = 'Customer.allInstances'
      RootHandle = BoldSystemHandle1
      --> Shows ALL customers

    lhaInvoices (chained to lhaCustomers)
      Expression = 'invoices'
      RootHandle = lhaCustomers         <-- rooted on the CUSTOMER handle
      --> Shows invoices for the SELECTED customer only

    lhaItems (chained to lhaInvoices)
      Expression = 'items'
      RootHandle = lhaInvoices          <-- rooted on the INVOICE handle
      --> Shows items for the SELECTED invoice only

  HOW IT WORKS
  ------------
  When lhaInvoices has RootHandle = lhaCustomers and Expression = 'invoices':
    1. Bold evaluates the OCL 'invoices' in the context of the current
       customer (the one selected in grdCustomers)
    2. This returns that customer's invoice collection
    3. When you select a different customer, the expression is re-evaluated
       and grdInvoices updates automatically

  This is REACTIVE - you don't write any event handlers or manual filtering.
  Bold's subscription system detects the selection change and cascades
  updates through the handle chain.

  The same principle applies to lhaItems: it evaluates 'items' in the
  context of whichever invoice is currently selected in grdInvoices.

  UI LAYOUT
  ---------
    +------------------+------------------+------------------+
    |   Customers      |   Invoices       |   Items          |
    |   (all)          |   (for selected  |   (for selected  |
    |                  |    customer)     |    invoice)      |
    |  [grid]          |  [grid]          |  [grid]          |
    |  [navigator]     |  [navigator]     |  [navigator]     |
    +------------------+------------------+------------------+
    | [Save]  [Open/Close System]   Status: Active           |
    +--------------------------------------------------------+

  CREATING RELATED OBJECTS
  ------------------------
  When you click "+" on bnInvoices (the invoice navigator), Bold creates
  a new Invoice AND automatically links it to the currently selected
  Customer. This works because lhaInvoices is rooted on lhaCustomers -
  Bold knows the new object belongs in that customer's invoice collection.
  Same for items: clicking "+" on bnItems creates an item linked to the
  selected invoice.
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
  BoldAbstractListHandle,  // Base class for list handles
  BoldCursorHandle,        // Adds "current item" cursor to a list
  BoldGrid,                // TBoldGrid - object grid display
  BoldHandles,             // System type info handle
  BoldListHandle,          // TBoldListHandle - OCL-driven object list
  BoldNavigator,           // TBoldNavigator - CRUD buttons
  BoldNavigatorDefs,       // Navigator button type definitions
  BoldRootedHandles,       // Base class for handles rooted on other handles
  BoldSubscription,        // Bold's subscription/notification system
  BoldSystem,              // TBoldSystem - the object space
  BoldSystemHandle;        // TBoldSystemHandle

type
  TfrmStep3 = class(TForm)
    { LEFT PANEL - All Customers }
    pnlCustomers: TPanel;
    lblCustomers: TLabel;
    grdCustomers: TBoldGrid;      // Displays Customer objects
    bnCustomers: TBoldNavigator;  // Add/Delete customers

    Splitter1: TSplitter;         // Resizable divider

    { MIDDLE PANEL - Invoices for selected Customer }
    pnlInvoices: TPanel;
    lblInvoices: TLabel;
    grdInvoices: TBoldGrid;      // Displays Invoice objects (filtered by customer)
    bnInvoices: TBoldNavigator;  // Add/Delete invoices (auto-linked to customer)

    Splitter2: TSplitter;         // Resizable divider

    { RIGHT PANEL - Items for selected Invoice }
    pnlItems: TPanel;
    lblItems: TLabel;
    grdItems: TBoldGrid;         // Displays InvoiceItem objects (filtered by invoice)
    bnItems: TBoldNavigator;     // Add/Delete items (auto-linked to invoice)

    { BOTTOM PANEL - Actions and status }
    pnlBottom: TPanel;
    btnSave: TButton;             // Persists all dirty objects to database
    btnOpenClose: TButton;        // Wired to BoldActivateSystemAction1
    lblStatus: TLabel;            // Shows active/inactive state

    { HANDLE CHAIN - the master-detail binding (configured in DFM)
      lhaCustomers: RootHandle=BoldSystemHandle1, Expression='Customer.allInstances'
      lhaInvoices:  RootHandle=lhaCustomers,      Expression='invoices'
      lhaItems:     RootHandle=lhaInvoices,        Expression='items' }
    lhaCustomers: TBoldListHandle;
    lhaInvoices: TBoldListHandle;
    lhaItems: TBoldListHandle;

    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSaveClick(Sender: TObject);
  private
    procedure HandleSystemOpened(Sender: TObject);
    procedure HandleSystemClosed(Sender: TObject);
    procedure UpdateStatus;
  end;

var
  frmStep3: TfrmStep3;

implementation

uses
  System.UITypes,
  InvoiceDataModule;

{$R *.DFM}

{ FormCreate
  Standard Bold startup pattern: hook events, then open the system.
  Once the system is active, the handle chain activates automatically
  and the grids populate with data from the database. }

procedure TfrmStep3.FormCreate(Sender: TObject);
begin
  dmInvoice.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
  dmInvoice.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;
  dmInvoice.OpenSystem;
end;

procedure TfrmStep3.HandleSystemOpened(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStep3.HandleSystemClosed(Sender: TObject);
begin
  UpdateStatus;
end;

{ UpdateStatus
  Updates the status label and Save button state. }

procedure TfrmStep3.UpdateStatus;
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
  Saves ALL dirty objects (customers, invoices, and items) to the database
  in a single transaction. See Step2Form.pas for detailed explanation. }

procedure TfrmStep3.btnSaveClick(Sender: TObject);
begin
  dmInvoice.BoldSystemHandle1.UpdateDatabase;
end;

{ FormCloseQuery
  Standard Bold pattern: check for dirty objects before closing.
  See Step1Form.pas for detailed explanation. }

procedure TfrmStep3.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
