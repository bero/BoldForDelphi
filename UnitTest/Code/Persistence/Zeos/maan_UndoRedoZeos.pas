unit maan_UndoRedoZeos;

{ The database for dmUndoRedo (maan_UndoRedoBase) under Free Pascal: Zeos with
  a shared-cache in-memory SQLite database (BoldTestZeos), the counterpart of
  maan_UndoRedoFireDAC on Delphi. Linking this unit (FPC/UnitTestFPC.lpr)
  registers it as dmUndoRedo's connector. Both systems get their own
  connection to the same database. }

interface

implementation

uses
  ZConnection,
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

initialization
  RegisterUndoRedoDMConnector(ConnectZeos, nil);

end.
