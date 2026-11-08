unit maan_UndoRedoFireDAC;

{ The database for dmUndoRedo (maan_UndoRedoBase) on Delphi: FireDAC,
  configured from UnitTest.ini (or BOLD_TEST_ENGINE). Linking this unit
  (UnitTest.dpr) registers it as dmUndoRedo's connector.

  The two adapters and connections are streamed from maan_UndoRedoFireDAC.dfm,
  exactly as they used to be from maan_UndoRedo.dfm: the adapters are stored
  with DatabaseEngine = dbeSQLServer and their SQL settings, which the SQL
  Server runs keep (setting the same engine again does not reset them).

  The Delphi-only tests that use FireDAC directly reach these components
  through UndoRedoFireDAC. }

interface

uses
  System.SysUtils,
  System.Classes,
  {$IFDEF MSWINDOWS}
  Vcl.Forms,
  {$ENDIF}
  BoldDatabaseAdapterFireDAC,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Pool,
  FireDAC.Phys,
  {$IFDEF MSWINDOWS}
  FireDAC.VCLUI.Wait;
  {$ELSE}
  FireDAC.ConsoleUI.Wait; // no VCL off Windows
  {$ENDIF}

type
  TdmUndoRedoFireDAC = class(TDataModule)
    BoldDatabaseAdapterFireDAC1: TBoldDatabaseAdapterFireDAC;
    FDConnection1: TFDConnection;
    FDConnection2: TFDConnection;
    BoldDatabaseAdapterFireDAC2: TBoldDatabaseAdapterFireDAC;
  end;

{ The FireDAC components behind the current dmUndoRedo; nil when there is no
  dmUndoRedo. }
function UndoRedoFireDAC: TdmUndoRedoFireDAC;

implementation

{$R *.dfm}

uses
  BoldTestDatabaseConfig,
  maan_UndoRedoBase;

function UndoRedoFireDAC: TdmUndoRedoFireDAC;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(dmUndoRedo) then
    for i := 0 to dmUndoRedo.ComponentCount - 1 do
      if dmUndoRedo.Components[i] is TdmUndoRedoFireDAC then
        Exit(TdmUndoRedoFireDAC(dmUndoRedo.Components[i]));
end;

procedure ConnectFireDAC(DataModule: TdmUndoRedo);
var
  FireDACModule: TdmUndoRedoFireDAC;
begin
  {$IFDEF MSWINDOWS}
  if not Assigned(Application) then
    raise Exception.Create('Application is nil');
  Application.Initialize;
  {$ENDIF}

  // Create the test database first
  CreateTestDatabase;

  // Owned by dmUndoRedo, so it goes when dmUndoRedo is freed.
  FireDACModule := TdmUndoRedoFireDAC.Create(DataModule);
  DataModule.BoldPersistenceHandleDB1.DatabaseAdapter := FireDACModule.BoldDatabaseAdapterFireDAC1;
  DataModule.BoldPersistenceHandleDB2.DatabaseAdapter := FireDACModule.BoldDatabaseAdapterFireDAC2;

  // Configure database connections from INI file
  ConfigureConnection(FireDACModule.FDConnection1, FireDACModule.BoldDatabaseAdapterFireDAC1);
  ConfigureConnection(FireDACModule.FDConnection2, FireDACModule.BoldDatabaseAdapterFireDAC2);

  // Open the connection of the first system; EnsureDM creates the schema
  FireDACModule.FDConnection1.Open;
  if not FireDACModule.FDConnection1.Connected then
    raise Exception.Create('FDConnection1 failed to open');
end;

procedure PrepareSystem2(DataModule: TdmUndoRedo);
var
  FireDACModule: TdmUndoRedoFireDAC;
begin
  // For SQLite in-memory, both connections share the same database file
  // Copy connection params from Connection1 to Connection2
  FireDACModule := UndoRedoFireDAC;
  FireDACModule.FDConnection2.Params.Values['Database'] := FireDACModule.FDConnection1.Params.Values['Database'];
end;

// On SQLite a second in-memory database (the connection stays open, so the
// shared cache stays alive); on server engines a sibling of the test database.
function OpenSecondDatabase(DataModule: TdmUndoRedo): Boolean;
const
  cSQLiteDatabase = 'file:memdb_dbcopy?mode=memory&cache=shared';
var
  FireDACModule: TdmUndoRedoFireDAC;
  Database: string;
begin
  Result := SameText(GetTestDatabaseEngine, 'SQLite');
  if Result then
    Database := cSQLiteDatabase
  else
  begin
    Database := GetTestDatabaseName + '_DbCopy';
    CreateTestDatabase(Database);
  end;
  FireDACModule := UndoRedoFireDAC;
  FireDACModule.FDConnection2.Params.Values['Database'] := Database;
  FireDACModule.FDConnection2.Open;
end;

initialization
  RegisterUndoRedoDMConnector(ConnectFireDAC, PrepareSystem2);
  RegisterUndoRedoSecondDatabase(OpenSecondDatabase);

finalization
  // Runs before the finalization of FireDAC and BoldTestDatabaseConfig, which
  // this unit uses, so the database can still be dropped here.
  try
    FreeUndoRedoDM;
    DropTestDatabase;
  except
    // Ignore errors during cleanup
  end;

end.
