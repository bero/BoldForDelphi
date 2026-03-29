{$INCLUDE bold.inc}
unit Step2Form;

{ ============================================================================
  Step2Form - Customer CRUD with Bold Data-Aware Components
  ============================================================================

  This step introduces Bold's data-aware UI components. If you've used
  Delphi's TDBGrid and TDBNavigator with datasets, Bold's components
  work similarly - but instead of database rows, they display objects.

  KEY COMPONENTS (configured in the DFM)
  ---------------------------------------

  TBoldListHandle (lhaCustomers)
    - The "data source" for Bold UI components
    - RootHandle = BoldSystemHandle1 (connects to the object space)
    - Expression = 'Customer.allInstances' (OCL query that returns all customers)
    - Think of it as: "give me a live list of all Customer objects"
    - When objects are added/deleted, the list updates automatically

  TBoldGrid (grdCustomers)
    - Like TDBGrid but for Bold objects
    - BoldHandle = lhaCustomers (which list to display)
    - Columns are configured with OCL expressions for each attribute:
      Column[0].Expression = 'name'
      Column[1].Expression = 'address'
      etc.
    - Columns auto-refresh when attribute values change

  TBoldNavigator (bnCustomers)
    - Like TDBNavigator but for Bold objects
    - BoldHandle = lhaCustomers
    - Provides Add (creates new Customer), Delete, and navigation buttons
    - When you click Add, Bold creates a new Customer object in memory
    - The object only persists to the database when you call UpdateDatabase

  TBoldEdit (edtName, edtAddress, etc.)
    - Like TDBEdit but for a single Bold attribute
    - BoldHandle = lhaCustomers (the list handle with a current selection)
    - Expression = 'name' (which attribute to edit)
    - Automatically reads/writes the current customer's attribute
    - Changes are immediate in memory (no need to "post" like with datasets)

  DATA FLOW
  ---------
  BoldSystemHandle1 --> lhaCustomers --> grdCustomers (grid display)
       (object space)   (OCL query)     bnCustomers  (add/delete/navigate)
                             |          edtName      (edit individual fields)
                             |          edtAddress
                             v          edtCity
                    'Customer.allInstances'  ... etc.

  PERSISTENCE
  -----------
  Bold objects live in memory. Changes are NOT automatically saved to the
  database. You must explicitly call UpdateDatabase to persist:
    - btnSave calls dmInvoice.BoldSystemHandle1.UpdateDatabase
    - This writes ALL dirty objects to the database in a single transaction
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
  BoldEdit,                // TBoldEdit - single-attribute editor
  BoldGrid,                // TBoldGrid - grid display for Bold objects
  BoldHandles,             // TBoldSystemTypeInfoHandle and related
  BoldListHandle,          // TBoldListHandle - OCL-driven object list
  BoldNavigator,           // TBoldNavigator - add/delete/navigate buttons
  BoldNavigatorDefs,       // Navigator button definitions
  BoldRootedHandles,       // Base class for handles with a root handle
  BoldSubscription,        // Bold's observer/subscription pattern
  BoldSystem,              // TBoldSystem
  BoldSystemHandle;        // TBoldSystemHandle

type
  TfrmStep2 = class(TForm)
    { Title panel }
    pnlTop: TPanel;
    lblTitle: TLabel;

    { Grid panel - shows all customers in a grid with navigation }
    pnlGrid: TPanel;
    grdCustomers: TBoldGrid;        // Displays Customer objects as rows
    bnCustomers: TBoldNavigator;    // Add/Delete/Navigate buttons for customers

    { Edit panel - detail view for editing individual customer fields }
    pnlEdit: TPanel;
    lblName: TLabel;
    lblAddress: TLabel;
    lblCity: TLabel;
    lblPhone: TLabel;
    lblEmail: TLabel;
    edtName: TBoldEdit;       // Edits currentCustomer.name
    edtAddress: TBoldEdit;    // Edits currentCustomer.address
    edtCity: TBoldEdit;       // Edits currentCustomer.city
    edtPhone: TBoldEdit;      // Edits currentCustomer.phone
    edtEmail: TBoldEdit;      // Edits currentCustomer.email

    { Bottom panel - Save button, Open/Close button, status }
    pnlBottom: TPanel;
    btnSave: TButton;
    btnOpenClose: TButton;    // Wired to BoldActivateSystemAction1 via DFM
    lblStatus: TLabel;

    { The list handle - the heart of Bold data binding.
      Configured in DFM: RootHandle=BoldSystemHandle1,
      Expression='Customer.allInstances' }
    lhaCustomers: TBoldListHandle;

    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSaveClick(Sender: TObject);
  private
    procedure HandleSystemOpened(Sender: TObject);
    procedure HandleSystemClosed(Sender: TObject);
    procedure UpdateStatus;
  end;

var
  frmStep2: TfrmStep2;

implementation

uses
  System.UITypes,
  InvoiceDataModule;

{$R *.DFM}

{ FormCreate
  Hooks up event handlers and opens the Bold system.
  See Step1Form.pas for detailed explanation of this pattern. }

procedure TfrmStep2.FormCreate(Sender: TObject);
begin
  dmInvoice.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
  dmInvoice.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;
  dmInvoice.OpenSystem;
end;

procedure TfrmStep2.HandleSystemOpened(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStep2.HandleSystemClosed(Sender: TObject);
begin
  UpdateStatus;
end;

{ UpdateStatus
  Shows the current system state and database path in the status bar.
  Also enables/disables the Save button based on whether the system is active
  (you can only save when connected to a database). }

procedure TfrmStep2.UpdateStatus;
begin
  if csDestroying in ComponentState then
    Exit;
  if dmInvoice.BoldSystemHandle1.Active then
  begin
    lblStatus.Caption := 'Bold System: Active | Database: ' + dmInvoice.DatabaseName;
    lblStatus.Font.Color := $00008800;  // Dark green = connected
    btnSave.Enabled := True;
  end
  else
  begin
    lblStatus.Caption := 'Bold System: Inactive';
    lblStatus.Font.Color := $000066AA;  // Amber = disconnected
    btnSave.Enabled := False;
  end;
end;

{ btnSaveClick
  Persists ALL dirty (modified) objects to the SQLite database.

  Bold uses a "unit of work" pattern: you make as many changes as you want
  in memory, then call UpdateDatabase once to save everything in a single
  transaction. This is more efficient and safer than saving after each change.

  What happens during UpdateDatabase:
    1. Bold identifies all dirty objects (new, modified, or deleted)
    2. Generates INSERT/UPDATE/DELETE SQL statements
    3. Executes them in a database transaction
    4. Marks all objects as clean (not dirty) }

procedure TfrmStep2.btnSaveClick(Sender: TObject);
begin
  dmInvoice.BoldSystemHandle1.UpdateDatabase;
end;

{ FormCloseQuery
  Standard Bold pattern: check for dirty objects before closing.
  See Step1Form.pas for detailed explanation. }

procedure TfrmStep2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
