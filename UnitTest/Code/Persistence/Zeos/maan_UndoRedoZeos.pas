unit maan_UndoRedoZeos;

{ The database for dmUndoRedo (maan_UndoRedoBase) under Free Pascal: Zeos on
  the engine of the run (BoldTestZeos: SQLite in memory, or PostgreSQL), the
  counterpart of
  maan_UndoRedoFireDAC on Delphi. Linking this unit (FPC/UnitTestFPC.lpr)
  registers it as dmUndoRedo's connector. Both systems get their own
  connection to the same database. }

interface

implementation

uses
  ZConnection,
  BoldDatabaseAdapterZeos,
  BoldTestZeos,
  maan_UndoRedoBase;

procedure ConnectZeos(DataModule: TdmUndoRedo);
var
  Connection1, Connection2: TZConnection;
begin
  // Everything is owned by dmUndoRedo, so it goes when dmUndoRedo is freed.
  NewTestZeosKeepAlive(DataModule);
  Connection1 := NewTestZeosConnection(DataModule);
  Connection2 := NewTestZeosConnection(DataModule);
  DataModule.BoldPersistenceHandleDB1.DatabaseAdapter := NewTestZeosAdapter(DataModule, Connection1);
  DataModule.BoldPersistenceHandleDB2.DatabaseAdapter := NewTestZeosAdapter(DataModule, Connection2);
  // Open the connection of the first system; EnsureDM creates the schema
  Connection1.Connect;
end;

// On SQLite a second shared-cache in-memory database, kept alive by a
// connection that dmUndoRedo owns, so it goes when dmUndoRedo is freed. On
// PostgreSQL a sibling of the test database, <database>_dbcopy as on Delphi.
function OpenSecondDatabase(DataModule: TdmUndoRedo): Boolean;
const
  cSecondDatabase = 'file:dbcopy';
var
  SecondDatabase, Connection2: TZConnection;
begin
  SecondDatabase := NewTestZeosKeepAlive(DataModule, cSecondDatabase);
  Connection2 := (DataModule.BoldPersistenceHandleDB2.DatabaseAdapter as TBoldDatabaseAdapterZeos).Connection;
  if Connection2.Connected then
    Connection2.Disconnect;
  // The name the engine of the run knows the database by
  Connection2.Database := SecondDatabase.Database;
  Connection2.Connect;
  Result := not TestZeosEngineIsPostgres;
end;

initialization
  RegisterUndoRedoDMConnector(ConnectZeos, nil);
  RegisterUndoRedoSecondDatabase(OpenSecondDatabase);

end.
