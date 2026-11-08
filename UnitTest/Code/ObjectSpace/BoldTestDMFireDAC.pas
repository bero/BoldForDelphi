unit BoldTestDMFireDAC;

{ The database for dmBoldTest on Delphi: FireDAC, SQLite shared-cache
  in-memory by default; UnitTest.ini (or BOLD_TEST_ENGINE) selects another
  engine. Linking this unit (UnitTest.dpr) registers it as dmBoldTest's
  connector.

  The adapter and the connection are streamed from BoldTestDMFireDAC.dfm,
  exactly as they used to be from dmBoldTest.dfm: the adapter there is stored
  with DatabaseEngine = dbeSQLServer and the SQL settings beside it. A
  streamed engine does not reset those settings, and setting the same engine
  again (the SQL Server path of ConfigureConnection) does not either, so the
  SQL Server runs keep them. An adapter created in code would get the engine
  defaults instead, which differ in 27 settings. }

interface

uses
  System.SysUtils,
  System.Classes,
  {$IFDEF MSWINDOWS}
  Vcl.Forms,
  {$ENDIF}
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterFireDAC,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Comp.Client,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef,
  {$IFDEF MSWINDOWS}
  FireDAC.VCLUI.Wait;
  {$ELSE}
  FireDAC.ConsoleUI.Wait; // no VCL off Windows
  {$ENDIF}

type
  TBoldTestDMFireDAC = class(TDataModule)
    BoldDatabaseAdapterFireDAC1: TBoldDatabaseAdapterFireDAC;
    FDConnection1: TFDConnection;
  end;

implementation

{$R *.dfm}

uses
  System.IniFiles,
  BoldSQLDatabaseConfig,
  BoldTestDatabaseConfig,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  dmBoldTest;

function TryGetIniFilePath(out APath: string): Boolean;
begin
  APath := ExtractFilePath(ParamStr(0)) + 'UnitTest.ini';
  if FileExists(APath) then
    Exit(True);
  APath := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'UnitTest.ini';
  Result := FileExists(APath);
end;

procedure ConfigureSQLite(Connection: TFDConnection; Adapter: TBoldDatabaseAdapterFireDAC);
begin
  // The SQLite database file of this test run (#120), no setup required. All
  // Bold internal connections (created via CreateAnotherDatabaseConnection)
  // open the same file.
  Connection.Close;
  Connection.Params.Clear;
  Connection.DriverName := 'SQLite';
  Connection.Params.Values['Database'] := GetTestSQLiteDatabase;
  Connection.LoginPrompt := False;
  Connection.Open;

  Adapter.DatabaseEngine := dbeGenericANSISQL92;
  with Adapter.SQLDatabaseConfig do
  begin
    ColumnTypeForText := 'TEXT';
    ColumnTypeForUnicodeText := 'TEXT';
    ColumnTypeForAnsiText := 'TEXT';
    ColumnTypeForInt64 := 'INTEGER';
  end;
end;

procedure ConfigureFromIni(const AIniPath: string; Connection: TFDConnection; Adapter: TBoldDatabaseAdapterFireDAC);
var
  Engine: string;
begin
  Engine := GetTestDatabaseEngine(AIniPath); // honours BOLD_TEST_ENGINE

  if SameText(Engine, 'SQLite') then
  begin
    WriteLn('BoldTestDM: Using SQLite, a temporary file (from ' + AIniPath + ')');
    ConfigureSQLite(Connection, Adapter);
  end
  else
  begin
    WriteLn('BoldTestDM: Using ' + Engine + ' (from ' + AIniPath + ')');
    BoldTestDatabaseConfig.CreateTestDatabase;
    BoldTestDatabaseConfig.ConfigureConnection(Connection, Adapter);
    Connection.Open;
  end;
end;

procedure ConnectFireDAC(DataModule: TBoldTestDM);
var
  FireDACModule: TBoldTestDMFireDAC;
  IniPath: string;
begin
  {$IFDEF MSWINDOWS}
  if not Assigned(Application) then
    Application.Initialize;
  {$ENDIF}

  // Owned by the test data module, so it goes when BoldTestDM is freed.
  FireDACModule := TBoldTestDMFireDAC.Create(DataModule);
  DataModule.BoldPersistenceHandleDB1.DatabaseAdapter := FireDACModule.BoldDatabaseAdapterFireDAC1;

  // Use UnitTest.ini if available, otherwise default to SQLite in-memory
  if TryGetIniFilePath(IniPath) then
    ConfigureFromIni(IniPath, FireDACModule.FDConnection1, FireDACModule.BoldDatabaseAdapterFireDAC1)
  else
  begin
    WriteLn('BoldTestDM: Using SQLite, a temporary file (no UnitTest.ini found)');
    ConfigureSQLite(FireDACModule.FDConnection1, FireDACModule.BoldDatabaseAdapterFireDAC1);
  end;
end;

initialization
  RegisterBoldTestDMConnector(ConnectFireDAC);

end.
