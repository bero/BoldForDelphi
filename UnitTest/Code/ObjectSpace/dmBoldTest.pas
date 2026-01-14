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
  TdmBoldTest = class(TDataModule)
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
  dmBoldTest: TdmBoldTest;

procedure EnsureBoldTestDM;
procedure CloseBoldTestDM;
procedure ClearAllTables;

implementation

{$R *.dfm}

uses
  BoldTestDatabaseConfig;

destructor TdmBoldTest.Destroy;
begin
  if BoldSystemHandle1.Active then
    BoldSystemHandle1.Active := False;
  inherited;
end;

function SchemaExists(Connection: TFDConnection): Boolean;
var
  TableCount: Integer;
begin
  // Check if Bold_ID table exists (core Bold table)
  TableCount := Connection.ExecSQLScalar(
    'SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = ''BOLD_ID''');
  Result := TableCount > 0;
end;

procedure EnsureBoldTestDM;
var
  NeedSchema: Boolean;
begin
  if not Assigned(dmBoldTest) then
  begin
    if not Assigned(Application) then
      raise Exception.Create('Application is nil');
    Application.Initialize;

    // Create the test database first (IF NOT EXISTS)
    CreateTestDatabase;

    dmBoldTest := TdmBoldTest.Create(Application);
    if not Assigned(dmBoldTest) then
      raise Exception.Create('Failed to create dmBoldTest');

    // Configure database connection from INI file
    ConfigureConnection(dmBoldTest.FDConnection1,
                        dmBoldTest.BoldDatabaseAdapterFireDAC1);

    // Open connection
    dmBoldTest.FDConnection1.Open;
    if not dmBoldTest.FDConnection1.Connected then
      raise Exception.Create('FDConnection1 failed to open');

    // Only create schema if it doesn't exist (much faster on subsequent runs)
    NeedSchema := not SchemaExists(dmBoldTest.FDConnection1);
    if NeedSchema then
      dmBoldTest.BoldPersistenceHandleDB1.CreateDataBaseSchema
    else
      ClearAllTables; // Clear existing data instead

    // Activate system
    dmBoldTest.BoldSystemHandle1.Active := True;
    if not Assigned(dmBoldTest.BoldSystemHandle1.System) then
      raise Exception.Create('BoldSystem failed to activate');
  end;
end;

procedure CloseBoldTestDM;
begin
  if Assigned(dmBoldTest) then
  begin
    if dmBoldTest.BoldSystemHandle1.Active then
      dmBoldTest.BoldSystemHandle1.Active := False;
    FreeAndNil(dmBoldTest);
  end;
end;

procedure ClearAllTables;
const
  // Clear tables in order that respects foreign key dependencies
  // Child tables first, then parent tables, BOLD_ID last
  ClearSQL =
    'DELETE FROM TOPICBOOK;' +
    'DELETE FROM PARTPARTOF;' +
    'DELETE FROM LINKCLASS;' +
    'DELETE FROM CLASSWITHLINK;' +
    'DELETE FROM BOOK;' +
    'DELETE FROM TOPIC;' +
    'DELETE FROM ATRANSIENTCLASS;' +
    'DELETE FROM APERSISTENTCLASS;' +
    'DELETE FROM SOMECLASS;' +
    'DELETE FROM TESTMODELCLASSESROOT;' +
    'DELETE FROM BOLD_ID;';
begin
  if Assigned(dmBoldTest) and dmBoldTest.FDConnection1.Connected then
    dmBoldTest.FDConnection1.ExecSQL(ClearSQL);
end;

end.
