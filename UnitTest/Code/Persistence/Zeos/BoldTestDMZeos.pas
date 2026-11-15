unit BoldTestDMZeos;

{ The database for dmBoldTest under Free Pascal: Zeos on the engine of the run
  (BoldTestZeos: SQLite in memory, or PostgreSQL), the counterpart of
  BoldTestDMFireDAC on Delphi. Linking this unit (FPC/UnitTestFPC.lpr)
  registers it as dmBoldTest's connector.

  The connection and the adapter are created in code. On Delphi the adapter
  is streamed with SQL Server settings, but the SQLite path switches the
  engine to dbeGenericANSISQL92, which resets them, so both builds end up
  with the same configuration. }

interface

implementation

uses
  ZConnection,
  BoldTestZeos,
  dmBoldTest;

procedure ConnectZeos(DataModule: TBoldTestDM);
var
  Connection: TZConnection;
begin
  // Everything is owned by the test data module, so it goes when BoldTestDM is freed.
  NewTestZeosKeepAlive(DataModule);
  Connection := NewTestZeosConnection(DataModule);
  DataModule.BoldPersistenceHandleDB1.DatabaseAdapter := NewTestZeosAdapter(DataModule, Connection);
  WriteLn('BoldTestDM: Using ', TestZeosEngineDescription);
  Connection.Connect;
end;

initialization
  RegisterBoldTestDMConnector(ConnectZeos);

end.
