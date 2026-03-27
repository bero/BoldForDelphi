unit dmBoldTest;

{ Generic DataModule for Bold unit tests }

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
procedure EnsureBoldTestDMSQLite;
procedure CloseBoldTestDM;

implementation

{$R *.dfm}

uses
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

procedure DropAndRecreateSchema;
begin
  // Drop existing database and recreate from scratch.
  // This is simpler and more reliable than ClearAllTables which requires
  // knowing all table names and their dependency order.
  DropTestDatabase;
  CreateTestDatabase;
  BoldTestDM.FDConnection1.Close;
  BoldTestDM.FDConnection1.Open;
  BoldTestDM.BoldPersistenceHandleDB1.CreateDataBaseSchema;
end;

procedure EnsureBoldTestDM;
begin
  if not Assigned(BoldTestDM) then
  begin
    if not Assigned(Application) then
      raise Exception.Create('Application is nil');
    Application.Initialize;

    // Create the test database first (IF NOT EXISTS)
    CreateTestDatabase;

    BoldTestDM := TBoldTestDM.Create(Application);
    if not Assigned(BoldTestDM) then
      raise Exception.Create('Failed to create BoldTestDM');

    // Configure database connection from INI file
    ConfigureConnection(BoldTestDM.FDConnection1,
                        BoldTestDM.BoldDatabaseAdapterFireDAC1);

    // Open connection
    BoldTestDM.FDConnection1.Open;
    if not BoldTestDM.FDConnection1.Connected then
      raise Exception.Create('FDConnection1 failed to open');

    // Create schema (Bold skips tables that already exist)
    BoldTestDM.BoldPersistenceHandleDB1.CreateDataBaseSchema;

    // Activate system
    BoldTestDM.BoldSystemHandle1.Active := True;
    if not Assigned(BoldTestDM.BoldSystemHandle1.System) then
      raise Exception.Create('BoldSystem failed to activate');
  end;
end;

procedure EnsureBoldTestDMSQLite;
begin
  if not Assigned(BoldTestDM) then
  begin
    BoldTestDM := TBoldTestDM.Create(nil);

    // Configure SQLite shared-cache in-memory database.
    // Named memory URI ensures all Bold internal connections (created via
    // CreateAnotherDatabaseConnection) share the same in-memory database.
    BoldTestDM.FDConnection1.Close;
    BoldTestDM.FDConnection1.Params.Clear;
    BoldTestDM.FDConnection1.DriverName := 'SQLite';
    BoldTestDM.FDConnection1.Params.Values['Database'] :=
      'file:memdb1?mode=memory&cache=shared';
    BoldTestDM.FDConnection1.LoginPrompt := False;
    BoldTestDM.FDConnection1.Open;

    // Adjust SQL config for SQLite compatibility
    BoldTestDM.BoldDatabaseAdapterFireDAC1.DatabaseEngine := dbeGenericANSISQL92;
    with BoldTestDM.BoldDatabaseAdapterFireDAC1.SQLDatabaseConfig do
    begin
      ColumnTypeForText := 'TEXT';
      ColumnTypeForUnicodeText := 'TEXT';
      ColumnTypeForAnsiText := 'TEXT';
      ColumnTypeForInt64 := 'INTEGER';
    end;

    // Create schema and activate
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
