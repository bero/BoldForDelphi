unit BoldTestPersistence;

{ SQLite-based test infrastructure for Bold persistence integration tests.

  This unit provides helper utilities for database integration tests
  that use in-memory SQLite for fast, isolated testing.

  The main test file Test.BoldPersistence.pas contains the actual tests.
}

interface

uses
  FireDAC.Comp.Client,
  BoldDatabaseAdapterFireDAC,
  BoldDBInterfaces;

type
  { Helper record for creating in-memory SQLite test databases }
  TBoldTestDatabaseHelper = record
    class function CreateInMemorySQLiteConnection: TFDConnection; static;
    class function CreateAdapter(Connection: TFDConnection): TBoldDatabaseAdapterFireDAC; static;
  end;

implementation

uses
  FireDAC.Stan.Def,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef;

{ TBoldTestDatabaseHelper }

class function TBoldTestDatabaseHelper.CreateInMemorySQLiteConnection: TFDConnection;
begin
  Result := TFDConnection.Create(nil);
  Result.DriverName := 'SQLite';
  Result.Params.Values['Database'] := ':memory:';
  Result.LoginPrompt := False;
end;

class function TBoldTestDatabaseHelper.CreateAdapter(Connection: TFDConnection): TBoldDatabaseAdapterFireDAC;
begin
  Result := TBoldDatabaseAdapterFireDAC.Create(nil);
  Result.Connection := Connection;
end;

end.
