unit dmBoldTest;

{ Generic DataModule for Bold unit tests.
  Uses SQLite shared-cache in-memory by default.
  Override via UnitTest.ini to use SQL Server or other engines. }

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  BoldSystem,
  BoldSystemHandle,
  BoldModel,
  BoldHandles,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
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
  FireDAC.VCLUI.Wait;

type
  TBoldTestDM = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterFireDAC1: TBoldDatabaseAdapterFireDAC;
    FDConnection1: TFDConnection;
  public
    destructor Destroy; override;
  end;

var
  BoldTestDM: TBoldTestDM;

procedure EnsureBoldTestDM;
procedure CloseBoldTestDM;

implementation

{$R *.dfm}

uses
  System.IniFiles,
  BoldTestDatabaseConfig,
  BoldSQLDatabaseConfig,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef;

destructor TBoldTestDM.Destroy;
begin
  if BoldSystemHandle1.Active then
    BoldSystemHandle1.Active := False;
  inherited;
end;

function TryGetIniFilePath(out APath: string): Boolean;
begin
  APath := ExtractFilePath(ParamStr(0)) + 'UnitTest.ini';
  if FileExists(APath) then
    Exit(True);
  APath := ExtractFilePath(ParamStr(0)) + '..\UnitTest.ini';
  Result := FileExists(APath);
end;

procedure ConfigureSQLiteInMemory;
begin
  // SQLite shared-cache in-memory: fast, no setup required.
  // Named memory URI ensures all Bold internal connections (created via
  // CreateAnotherDatabaseConnection) share the same in-memory database.
  BoldTestDM.FDConnection1.Close;
  BoldTestDM.FDConnection1.Params.Clear;
  BoldTestDM.FDConnection1.DriverName := 'SQLite';
  BoldTestDM.FDConnection1.Params.Values['Database'] :=
    'file:memdb1?mode=memory&cache=shared';
  BoldTestDM.FDConnection1.LoginPrompt := False;
  BoldTestDM.FDConnection1.Open;

  BoldTestDM.BoldDatabaseAdapterFireDAC1.DatabaseEngine := dbeGenericANSISQL92;
  with BoldTestDM.BoldDatabaseAdapterFireDAC1.SQLDatabaseConfig do
  begin
    ColumnTypeForText := 'TEXT';
    ColumnTypeForUnicodeText := 'TEXT';
    ColumnTypeForAnsiText := 'TEXT';
    ColumnTypeForInt64 := 'INTEGER';
  end;
end;

procedure ConfigureFromIni(const AIniPath: string);
var
  Engine: string;
begin
  Engine := GetTestDatabaseEngine(AIniPath); // honours BOLD_TEST_ENGINE

  if SameText(Engine, 'SQLite') then
  begin
    WriteLn('BoldTestDM: Using SQLite in-memory (from ' + AIniPath + ')');
    ConfigureSQLiteInMemory;
  end
  else
  begin
    WriteLn('BoldTestDM: Using ' + Engine + ' (from ' + AIniPath + ')');
    BoldTestDatabaseConfig.CreateTestDatabase;
    BoldTestDatabaseConfig.ConfigureConnection(BoldTestDM.FDConnection1,
      BoldTestDM.BoldDatabaseAdapterFireDAC1);
    BoldTestDM.FDConnection1.Open;
  end;
end;

procedure EnsureBoldTestDM;
var
  IniPath: string;
begin
  if not Assigned(BoldTestDM) then
  begin
    if not Assigned(Application) then
      Application.Initialize;

    BoldTestDM := TBoldTestDM.Create(nil);

    // Use UnitTest.ini if available, otherwise default to SQLite in-memory
    if TryGetIniFilePath(IniPath) then
      ConfigureFromIni(IniPath)
    else
    begin
      WriteLn('BoldTestDM: Using SQLite in-memory (no UnitTest.ini found)');
      ConfigureSQLiteInMemory;
    end;

    BoldTestDM.BoldPersistenceHandleDB1.CreateDataBaseSchema;
    BoldTestDM.BoldSystemHandle1.Active := True;
  end;
end;

procedure CloseBoldTestDM;
begin
  if Assigned(BoldTestDM) then
  begin
    if BoldTestDM.BoldSystemHandle1.Active then
      BoldTestDM.BoldSystemHandle1.Active := False;
    FreeAndNil(BoldTestDM);
  end;
end;

end.
