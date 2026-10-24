{$INCLUDE bold.inc}
unit dmInvoicing;

{ The Bold infrastructure of the Invoicing example: the model, the object
  space and SQLite persistence through FireDAC.

  The unit imports no VCL or FMX unit, so the same data module serves the
  console runner, the unit tests and the FMX client. Each program links its
  own FireDAC wait-cursor unit (FireDAC.ConsoleUI.Wait or FireDAC.FMXUI.Wait);
  FireDAC selects the matching provider from the application type.

  The model is not stored in the dfm. Call LoadModelFromFile or
  LoadModelFromResource before OpenSystem, so that Invoicing.bld stays the
  single source of the model for every project, including a future Lazarus
  one, and the generated InvoicingClasses unit can never disagree with a
  second copy of the model. }

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  Data.DB,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Comp.Client,
  FireDAC.DApt,
  BoldDefs,
  BoldAbstractModel,
  BoldDatabaseAdapterFireDAC,
  BoldHandles,
  BoldModel,
  BoldPersistenceHandleDB,
  BoldSQLDatabaseConfig,
  BoldSystem,
  BoldSystemHandle;

type
  TdmInvoicing = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FConnection: TFDConnection;
    FAdapter: TBoldDatabaseAdapterFireDAC;
    FPersistenceHandle: TBoldPersistenceHandleDB;
    FDatabase: string;
    FModelLoaded: Boolean;
    function GetBoldSystem: TBoldSystem;
    function GetIsOpen: Boolean;
    procedure ConfigureSQLite(const ADatabase: string);
    procedure EnsureSchema;
    procedure SetModel(AStrings: TStrings);
  public
    { Load Invoicing.bld from a file or from an RCDATA resource. One of them
      must be called before OpenSystem. }
    procedure LoadModelFromFile(const AFileName: string);
    procedure LoadModelFromResource(const AResourceName: string);
    { ADatabase is the FireDAC SQLite Database parameter, a file path. The
      schema is created when the database has no Bold tables yet.

      There is no in-memory option. FireDAC's SQLite does not interpret URI
      filenames, so file:x?mode=memory&cache=shared is opened as an ordinary
      file (on NTFS as an alternate data stream of a file named "file"), and a
      plain :memory: database is private to one connection and gone when it
      closes. The tests use a temporary file per test instead. }
    procedure OpenSystem(const ADatabase: string);
    procedure CloseSystem(ASaveChanges: Boolean);
    procedure Save;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property IsOpen: Boolean read GetIsOpen;
    property ModelLoaded: Boolean read FModelLoaded;
    property Database: string read FDatabase;
  end;

const
  cDefaultModelResource = 'InvoicingModel';

implementation

{$R *.dfm}

resourcestring
  sModelNotLoaded = 'Load the model with LoadModelFromFile or LoadModelFromResource before opening the system';
  sSystemAlreadyOpen = 'The system is already open on %s';
  sModelFileNotFound = 'Model file not found: %s';

const
  cSQLiteDriver = 'SQLite';
  cBoldTypeTable = 'BOLD_TYPE';

{ TdmInvoicing }

procedure TdmInvoicing.DataModuleCreate(Sender: TObject);
begin
  FConnection := TFDConnection.Create(Self);
  FAdapter := TBoldDatabaseAdapterFireDAC.Create(Self);
  FAdapter.Connection := FConnection;
  FPersistenceHandle := TBoldPersistenceHandleDB.Create(Self);
  FPersistenceHandle.BoldModel := BoldModel1;
  FPersistenceHandle.DatabaseAdapter := FAdapter;
  BoldSystemHandle1.PersistenceHandle := FPersistenceHandle;
end;

procedure TdmInvoicing.DataModuleDestroy(Sender: TObject);
begin
  CloseSystem(False);
  // Reverse creation order, so nothing is destroyed while still referenced.
  BoldSystemHandle1.PersistenceHandle := nil;
  FreeAndNil(FPersistenceHandle);
  FreeAndNil(FAdapter);
  FreeAndNil(FConnection);
end;

function TdmInvoicing.GetBoldSystem: TBoldSystem;
begin
  Result := BoldSystemHandle1.System;
end;

function TdmInvoicing.GetIsOpen: Boolean;
begin
  Result := BoldSystemHandle1.Active;
end;

procedure TdmInvoicing.SetModel(AStrings: TStrings);
begin
  BoldModel1.SetFromModelAsString(AStrings);
  FModelLoaded := True;
end;

procedure TdmInvoicing.LoadModelFromFile(const AFileName: string);
var
  Strings: TStringList;
begin
  if not FileExists(AFileName) then
    raise EBold.CreateFmt(sModelFileNotFound, [AFileName]);

  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(AFileName);
    SetModel(Strings);
  finally
    Strings.Free;
  end;
end;

procedure TdmInvoicing.LoadModelFromResource(const AResourceName: string);
var
  Stream: TResourceStream;
  Strings: TStringList;
begin
  Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  Strings := TStringList.Create;
  try
    Strings.LoadFromStream(Stream);
    SetModel(Strings);
  finally
    Strings.Free;
    Stream.Free;
  end;
end;

procedure TdmInvoicing.ConfigureSQLite(const ADatabase: string);
begin
  FConnection.Close;
  FConnection.Params.Clear;
  FConnection.DriverName := cSQLiteDriver;
  FConnection.Params.Values['Database'] := ADatabase;
  FConnection.LoginPrompt := False;
  // The same engine setup the Bold test suite runs on SQLite: generic SQL92
  // with SQLite's own column types for text and 64-bit integers.
  FAdapter.DatabaseEngine := dbeGenericANSISQL92;
  FAdapter.SQLDatabaseConfig.ColumnTypeForText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForUnicodeText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForAnsiText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForInt64 := 'INTEGER';
end;

procedure TdmInvoicing.EnsureSchema;
var
  TableNames: TStringList;
begin
  FConnection.Open;
  TableNames := TStringList.Create;
  try
    // Not TableExists: it caches the first answer for the lifetime of the
    // adapter, and a system reopened after creating the schema would then
    // try to create it again.
    FPersistenceHandle.DatabaseInterface.AllTableNames('*', True, TableNames);

    if TableNames.IndexOf(cBoldTypeTable) = -1 then
      FPersistenceHandle.CreateDataBaseSchema(True);
  finally
    TableNames.Free;
  end;
end;

procedure TdmInvoicing.OpenSystem(const ADatabase: string);
begin
  if not FModelLoaded then
    raise EBold.Create(sModelNotLoaded);

  if IsOpen then
    raise EBold.CreateFmt(sSystemAlreadyOpen, [FDatabase]);

  FDatabase := ADatabase;
  ConfigureSQLite(ADatabase);
  EnsureSchema;
  BoldSystemHandle1.Active := True;
end;

procedure TdmInvoicing.CloseSystem(ASaveChanges: Boolean);
begin
  if not IsOpen then
    Exit;

  if BoldSystem.BoldDirty then
  begin
    if ASaveChanges then
      BoldSystem.UpdateDatabase
    else
      BoldSystem.Discard;
  end;

  BoldSystemHandle1.Active := False;
  FConnection.Close;
end;

procedure TdmInvoicing.Save;
begin
  BoldSystem.UpdateDatabase;
end;

end.
