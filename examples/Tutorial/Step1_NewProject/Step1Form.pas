{$INCLUDE bold.inc}
unit Step1Form;

{ ============================================================================
  Step1Form - Minimal Bold Application Form
  ============================================================================

  This is the simplest Bold form possible. It demonstrates:

    1. How to open the Bold system at startup (FormCreate)
    2. How to respond to system state changes (OnSystemOpened/OnSystemClosed)
    3. How to display system status information
    4. How to handle dirty objects on exit (FormCloseQuery)

  BOLD SYSTEM LIFECYCLE
  ---------------------
  The Bold system goes through these states:

    Inactive  --[Activate]-->  Active  --[Deactivate]-->  Inactive
                                  |
                          Objects live here.
                     You can create, query, modify,
                     and persist objects to the database.

  The Open/Close button is wired to BoldActivateSystemAction1 via the
  DFM's Action property. This action automatically:
    - Toggles the system between Active and Inactive
    - Updates its own caption ("Open System" / "Close System")
    - Fires OnSystemOpened / OnSystemClosed events

  DIRTY OBJECTS
  -------------
  In Bold, a "dirty" object is one that has been modified in memory but not
  yet saved to the database. FormCloseQuery checks for dirty objects and
  asks the user whether to save, discard, or cancel the close.
  ============================================================================ }

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.StdCtrls,
  BoldSubscription,    // Bold's observer pattern (subscription/notification)
  BoldSystem,          // TBoldSystem - the runtime object space
  BoldSystemHandle;    // TBoldSystemHandle - wrapper that manages TBoldSystem

type
  TfrmStep1 = class(TForm)
    { UI panels and labels - defined in the DFM }
    pnlTop: TPanel;           // Title area
    lblTitle: TLabel;          // "Step 1: New Project Setup"
    lblDescription: TLabel;    // Explanatory text
    pnlStatus: TPanel;        // Status information area
    lblConfigFile: TLabel;     // Shows which .ini file is being used
    lblDatabaseStatus: TLabel; // Shows database path and connection state
    lblBoldStatus: TLabel;     // Shows whether Bold system is active
    btnOpenClose: TButton;     // Wired to BoldActivateSystemAction1 via DFM
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure UpdateStatusLabels;
    { Event handlers for Bold system state changes.
      These are assigned to BoldActivateSystemAction1's events in FormCreate. }
    procedure HandleSystemOpened(Sender: TObject);
    procedure HandleSystemClosed(Sender: TObject);
  public
  end;

var
  frmStep1: TfrmStep1;

implementation

uses
  System.UITypes,      // For mrYes, mrNo, mrCancel constants
  InvoiceDataModule;   // Access to dmInvoice (the Bold infrastructure)

{$R *.DFM}

{ FormCreate
  Called once when the form is first created.

  We hook into the BoldActivateSystemAction's events to be notified when
  the Bold system opens or closes. Then we call OpenSystem to activate
  the system immediately on startup.

  WHY USE EVENTS INSTEAD OF CHECKING STATE?
  Bold is reactive - rather than polling "is the system active?", we
  subscribe to state-change events. This pattern scales well: multiple
  forms can each hook their own handlers to the same action. }

procedure TfrmStep1.FormCreate(Sender: TObject);
begin
  // Subscribe to system state changes so we can update the UI
  dmInvoice.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
  dmInvoice.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;
  // Activate the Bold system (creates DB if needed, loads model, connects)
  dmInvoice.OpenSystem;
end;

{ UpdateStatusLabels
  Refreshes all status labels to reflect the current system state.
  Called whenever the system opens or closes. }

procedure TfrmStep1.UpdateStatusLabels;
begin
  if csDestroying in ComponentState then
    Exit;

  if not Assigned(dmInvoice) then
    Exit;

  // Show which configuration file is being used
  lblConfigFile.Caption := 'Config: ' + ChangeFileExt(Application.ExeName, '.ini');

  if dmInvoice.BoldSystemHandle1.Active then
  begin
    // System is active - show the database path in green
    lblDatabaseStatus.Caption := 'Database: ' + dmInvoice.DatabaseName;
    lblDatabaseStatus.Font.Color := $00008800;  // Dark green
    lblBoldStatus.Caption := 'Bold System: Active';
    lblBoldStatus.Font.Color := $00008800;       // Dark green
  end
  else
  begin
    // System is inactive - show disconnected state in red/amber
    lblDatabaseStatus.Caption := 'Database: Not connected';
    lblDatabaseStatus.Font.Color := $000000CC;   // Dark red
    lblBoldStatus.Caption := 'Bold System: Inactive';
    lblBoldStatus.Font.Color := $000066AA;       // Amber
  end;
end;

{ HandleSystemOpened / HandleSystemClosed
  Event handlers fired by BoldActivateSystemAction1 when the system
  transitions between active and inactive states. }

procedure TfrmStep1.HandleSystemOpened(Sender: TObject);
begin
  UpdateStatusLabels;
end;

procedure TfrmStep1.HandleSystemClosed(Sender: TObject);
begin
  UpdateStatusLabels;
end;

{ FormCloseQuery
  Called when the user tries to close the form (Alt+F4, X button, etc.).

  DIRTY OBJECT HANDLING
  Bold tracks all in-memory modifications. DirtyObjects.Count > 0 means
  there are unsaved changes. We give the user three choices:
    - Yes    : Call UpdateDatabase to persist changes to SQLite
    - No     : Call Discard to revert objects to their last saved state
    - Cancel : Set CanClose := False to abort the form close

  This pattern appears in all tutorial steps - it's a Bold best practice
  to never lose unsaved work silently. }

procedure TfrmStep1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
