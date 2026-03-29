{$INCLUDE bold.inc}
unit InvoiceDataModule;

{ ============================================================================
  InvoiceDataModule - The Bold System Infrastructure
  ============================================================================

  This data module is the "engine room" of every Bold application. It wires
  together the three pillars that every Bold app needs:

    1. BoldModel1           - The UML model (your class diagram at runtime)
    2. BoldSystemHandle1    - The object space (in-memory object graph)
    3. Persistence layer    - How objects get saved to / loaded from a database

  ARCHITECTURE OVERVIEW
  ---------------------
  In Bold, you don't to write SQL or manually map objects to tables. Instead:

    UML Model  -->  BoldModel1 (loaded from DFM at design time)
                       |
                       v
                BoldSystemHandle1 (creates the runtime object space)
                       |
                       v
                PersistenceHandleDB --> DatabaseAdapter --> FDConnection
                       |
                       v
                   SQLite file on disk

  When you call BoldSystemHandle1.Active := True, Bold:
    - Reads the UML model to understand your classes and associations
    - Creates the runtime type system (RTTI for your domain model)
    - Connects to the database via the persistence chain
    - Loads objects on demand as you query them with OCL

  WHY A DATA MODULE?
  ------------------
  A TDataModule is a non-visual container for components. We put all Bold
  infrastructure here so that:
    - Multiple forms can share the same BoldSystem (object space)
    - The persistence setup is centralized in one place
    - Forms only need to reference this unit to access domain objects

  All tutorial steps (Step1 through Step4) share this single data module.
  ============================================================================ }

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.IOUtils,
  System.UITypes,
  System.Actions,
  System.StrUtils,
  Data.DB,
  Vcl.ActnList,
  Vcl.Dialogs,
  Vcl.Forms,

  // FireDAC - Embarcadero's database access framework.
  // We use it as the "transport layer" to talk to the SQLite database.
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,
  FireDAC.DApt,

  // FireDAC SQLite driver - must be in uses clause so the driver registers
  // itself at startup. Without this, FireDAC won't know how to talk to SQLite.
  FireDAC.Phys.SQLite,

  // Bold framework units - each serves a specific role:
  BoldAbstractDatabaseAdapter,       // Base class for database adapters
  BoldAbstractModel,                 // Base class for the UML model component
  BoldAbstractPersistenceHandleDB,   // Base class for DB persistence handles
  BoldActions,                       // TBoldActivateSystemAction (open/close toggle)
  BoldDatabaseAdapterFireDAC,        // Bridges Bold persistence to FireDAC
  BoldDefs,                          // Core Bold constants and types
  BoldHandle,                        // Base handle class
  BoldHandleAction,                  // Actions that work with Bold handles
  BoldHandles,                       // TBoldSystemTypeInfoHandle and others
  BoldModel,                         // TBoldModel - the UML model component
  BoldPersistenceHandle,             // Abstract persistence handle
  BoldPersistenceHandleDB,           // TBoldPersistenceHandleDB - DB persistence
  BoldSQLDatabaseConfig,             // Database engine configuration constants
  BoldSubscription,                  // Bold's observer/subscription pattern
  BoldSystem,                        // TBoldSystem - the runtime object space
  BoldSystemHandle;                  // TBoldSystemHandle - wraps TBoldSystem

type
  { TInvoiceDataModule
    -----------------
    Central data module that all tutorial forms depend on.

    Design-time components (created in the DFM):
      - BoldModel1                  : Contains the embedded UML model text
      - BoldSystemHandle1           : Manages the runtime object space
      - BoldSystemTypeInfoHandle1   : Provides type metadata to handles/grids
      - ActionList1                 : Standard Delphi action list
      - BoldActivateSystemAction1   : Toggle action to open/close the system

    Runtime components (created in code):
      - FFDConnection               : FireDAC connection to SQLite
      - FFireDACAdapter             : Bridges FireDAC to Bold's persistence
      - FPersistenceHandleDB        : Manages object-relational mapping }

  TInvoiceDataModule = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure BoldActivateSystemAction1Execute(Sender: TObject);
  private
    { Persistence chain components - created at runtime in SetupPersistence.
      We create these in code (not on the DFM) to keep the tutorial simple
      and to show exactly what each piece does. }
    FPersistenceHandleDB: TBoldPersistenceHandleDB;
    FFDConnection: TFDConnection;
    FFireDACAdapter: TBoldDatabaseAdapterFireDAC;

    function GetIniFileName: string;
    procedure SetupPersistence;
    procedure EnsureDatabaseExists;
  public
    { OpenSystem - Called by forms to activate Bold on startup }
    procedure OpenSystem;
    { DatabaseName - Returns the full path to the SQLite database file }
    function DatabaseName: string;
  end;

var
  { Global variable that Delphi's Application.CreateForm populates.
    All forms access the data module through this variable. }
  dmInvoice: TInvoiceDataModule;

implementation

{$R *.dfm}

{ GetIniFileName
  Returns the path to the .ini configuration file.
  Convention: the .ini file sits next to the .exe with the same base name.
  For example, Step1.exe reads Step1.ini. }

function TInvoiceDataModule.GetIniFileName: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

{ DatabaseName
  Reads the SQLite database file path from the .ini file.

  INI file format:
    [SQLite]
    Database=Step1.db

  If the path is relative, it's resolved relative to the .exe directory.
  If the INI key is missing, defaults to <ExeName>.db (e.g., Step1.db). }

function TInvoiceDataModule.DatabaseName: string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFileName);
  try
    Result := Ini.ReadString('SQLite', 'Database', ChangeFileExt(ExtractFileName(Application.ExeName), '.db'));
    // Convert relative paths to absolute (relative to the .exe directory)
    if not TPath.IsPathRooted(Result) then
      Result := TPath.Combine(ExtractFilePath(Application.ExeName), Result);
  finally
    Ini.Free;
  end;
end;

{ SetupPersistence
  Builds the persistence chain that connects Bold's object space to the
  SQLite database. This is called once during DataModuleCreate.

  The chain looks like this:

    BoldSystemHandle1
         |  .PersistenceHandle
         v
    FPersistenceHandleDB    (object-relational mapping engine)
         |  .DatabaseAdapter
         v
    FFireDACAdapter         (translates Bold SQL to FireDAC calls)
         |  .Connection
         v
    FFDConnection           (FireDAC connection to SQLite file)

  IMPORTANT: dbeInterbaseSQLDialect3 is used as the DatabaseEngine because
  SQLite's SQL dialect is closest to InterBase SQL Dialect 3 among Bold's
  supported engines. This tells Bold how to generate compatible SQL. }

procedure TInvoiceDataModule.SetupPersistence;
var
  DbFile: string;
begin
  DbFile := DatabaseName;

  // Step 1: Create the FireDAC connection to the SQLite file.
  // CreateDatabase=True tells SQLite to create the file if it doesn't exist.
  FFDConnection := TFDConnection.Create(Self);
  FFDConnection.LoginPrompt := False;
  FFDConnection.Params.DriverID := 'SQLite';
  FFDConnection.Params.Database := DbFile;
  FFDConnection.Params.Add('CreateDatabase=True');

  // Step 2: Create the Bold-to-FireDAC adapter.
  // This translates Bold's persistence requests into FireDAC database calls.
  FFireDACAdapter := TBoldDatabaseAdapterFireDAC.Create(Self);
  FFireDACAdapter.DatabaseEngine := dbeInterbaseSQLDialect3;
  FFireDACAdapter.Connection := FFDConnection;

  // Step 3: Create the persistence handle that performs object-relational mapping.
  // It needs the model (to know the class structure) and the adapter (to reach the DB).
  FPersistenceHandleDB := TBoldPersistenceHandleDB.Create(Self);
  FPersistenceHandleDB.BoldModel := BoldModel1;
  FPersistenceHandleDB.DatabaseAdapter := FFireDACAdapter;

  // Step 4: Connect the persistence handle to the system handle.
  // Now when BoldSystemHandle1 activates, it knows where to persist objects.
  BoldSystemHandle1.PersistenceHandle := FPersistenceHandleDB;
end;

{ EnsureDatabaseExists
  Checks whether Bold's schema tables exist in the SQLite database. If not,
  it creates them using Bold's CreateDataBaseSchema method, which generates
  all tables, columns, and indices based on the UML model.

  This is called just before activating the system, so the very first run
  of a tutorial step will automatically create a fresh database.

  NOTE: We use TableExists('BOLD_TYPE') instead of DatabaseInterface.DatabaseExists
  because SQLite doesn't have a "database exists" SQL query template. The
  BOLD_TYPE table is one of Bold's core system tables, so if it exists,
  the schema has already been created. The SQLite file itself is created
  automatically by FireDAC's CreateDatabase=True parameter. }

procedure TInvoiceDataModule.EnsureDatabaseExists;
begin
  try
    FFDConnection.Open;
    // Check if Bold's core system table exists — if not, the schema needs creation
    if not FPersistenceHandleDB.DatabaseInterface.TableExists('BOLD_TYPE') then
    begin
      // CreateDataBaseSchema generates CREATE TABLE statements from the UML model
      // and executes them against the connected database.
      FPersistenceHandleDB.CreateDataBaseSchema(True);  // True = ignore unknown tables
    end;
    FFDConnection.Close;
  except
    on E: Exception do
    begin
      MessageDlg('Database error: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

{ DataModuleCreate
  Called automatically when the data module is instantiated.
  Sets up the entire persistence chain so the system is ready to activate. }

procedure TInvoiceDataModule.DataModuleCreate(Sender: TObject);
begin
  SetupPersistence;
end;

{ DataModuleDestroy
  Clean shutdown: if the system is active and has unsaved changes,
  discard them (since we're shutting down) and deactivate the system.

  BoldDirty = True means there are in-memory objects that haven't been
  persisted to the database yet. Discard reverts them to their last
  saved state in database before we close the connection. }

procedure TInvoiceDataModule.DataModuleDestroy(Sender: TObject);
begin
  // 1. Deactivate the Bold system (flushes internal state).
  //    Note: Form event handlers (OnSystemClosed) may fire here, but the
  //    forms guard against this with 'if csDestroying in ComponentState'.
  if BoldSystemHandle1.Active then
  begin
    if BoldSystemHandle1.System.BoldDirty then
      BoldSystemHandle1.System.Discard;
    BoldSystemHandle1.Active := False;
  end;
  // 2. Disconnect the persistence chain before freeing components.
  //    This prevents dangling references during destruction.
  BoldSystemHandle1.PersistenceHandle := nil;
  // 3. Free persistence components in reverse creation order.
  //    Relying on owner-based destruction can free them in wrong order,
  //    causing leaked strings from FireDAC's internal caches.
  FreeAndNil(FPersistenceHandleDB);
  FreeAndNil(FFireDACAdapter);
  if FFDConnection.Connected then
    FFDConnection.Close;
  FreeAndNil(FFDConnection);
end;

{ BoldActivateSystemAction1Execute
  Handler for the TBoldActivateSystemAction. This action toggles the Bold
  system between Active and Inactive states.

  When OPENING (Active = False -> True):
    1. EnsureDatabaseExists creates schema tables on first run
    2. Setting Active := True loads the model, connects to DB, and
       makes the object space ready for queries and object creation

  When CLOSING (Active = True -> False):
    - If there are unsaved ("dirty") objects, ask the user what to do:
      * Yes    -> UpdateDatabase persists all dirty objects to the DB
      * No     -> Discard reverts objects to their last saved state
      * Cancel -> Abort the close, keep system active

  The button in the form is wired to this action via the DFM's Action
  property, so clicking it automatically calls this handler. The action
  also updates the button caption to "Open System" / "Close System". }

procedure TInvoiceDataModule.BoldActivateSystemAction1Execute(Sender: TObject);
begin
  if not BoldSystemHandle1.Active then
  begin
    EnsureDatabaseExists;
    BoldSystemHandle1.Active := True;
  end
  else
  begin
    if BoldSystemHandle1.System.BoldDirty then
    begin
      case MessageDlg('Save changes before closing?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: BoldSystemHandle1.System.UpdateDatabase;
        mrNo: BoldSystemHandle1.System.Discard;
        mrCancel: Exit;
      end;
    end;
    BoldSystemHandle1.Active := False;
  end;
end;

{ OpenSystem
  Convenience method called by tutorial forms in their FormCreate handler.
  It simply triggers the activate action, which opens the system. }

procedure TInvoiceDataModule.OpenSystem;
begin
  BoldActivateSystemAction1.Execute;
end;

end.
