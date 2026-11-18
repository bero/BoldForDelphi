{$INCLUDE bold.inc}
unit dmMasterDetail;

{ The Bold infrastructure of the FMX Master-Detail demo: the model of
  examples\Shared\DemoModel.bld, the object space and SQLite persistence
  through FireDAC.

  The VCL demo's data module (examples\Shared\DemoDataModule.pas) holds a VCL
  action list and asks its questions with VCL dialogs, so this one is its
  framework-neutral counterpart for SQLite only: the unit imports no VCL or
  FMX unit, and the CLASSGROUP directive keeps the IDE designer from adding
  one when the data module is saved there. The program links its own FireDAC
  wait-cursor unit (FireDAC.FMXUI.Wait).

  The model is not stored in the dfm: LoadModelFromFile reads DemoModel.bld,
  the file the shared DemoClasses unit is generated from, so the two cannot
  disagree. }

interface

uses
  System.SysUtils,
  System.Classes,
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
  BoldSubscription, // declares the components' ancestor; the designer adds it on save otherwise
  BoldSystem,
  BoldSystemHandle;

type
  TdmMasterDetail = class(TDataModule)
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
  public
    { Reads DemoModel.bld; call it before OpenSystem }
    procedure LoadModelFromFile(const AFileName: string);
    { ADatabase is the SQLite file. The schema is created when the database
      has no Bold tables yet, so a missing file starts an empty database. }
    procedure OpenSystem(const ADatabase: string);
    procedure CloseSystem(ASaveChanges: Boolean);
    { Closes the system, discarding unsaved changes, and deletes the file }
    procedure DropDatabase;
    procedure Save;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property IsOpen: Boolean read GetIsOpen;
    property Database: string read FDatabase;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{$IFDEF POSIX}
uses
  Posix.Unistd; // SysUtils.DeleteFile is inlined from it (H2443 without)
{$ENDIF}

resourcestring
  sModelNotLoaded = 'Load the model with LoadModelFromFile before opening the system';
  sSystemAlreadyOpen = 'The system is already open on %s';
  sModelFileNotFound = 'Model file not found: %s';
  sDatabaseNotDeleted = 'The database file could not be deleted: %s';

const
  cSQLiteDriver = 'SQLite';
  cBoldTypeTable = 'BOLD_TYPE';

{ TdmMasterDetail }

procedure TdmMasterDetail.DataModuleCreate(Sender: TObject);
begin
  FConnection := TFDConnection.Create(Self);
  FAdapter := TBoldDatabaseAdapterFireDAC.Create(Self);
  FAdapter.Connection := FConnection;
  FPersistenceHandle := TBoldPersistenceHandleDB.Create(Self);
  FPersistenceHandle.BoldModel := BoldModel1;
  FPersistenceHandle.DatabaseAdapter := FAdapter;
  BoldSystemHandle1.PersistenceHandle := FPersistenceHandle;
end;

procedure TdmMasterDetail.DataModuleDestroy(Sender: TObject);
begin
  CloseSystem(False);
  // Reverse creation order, so nothing is destroyed while still referenced
  BoldSystemHandle1.PersistenceHandle := nil;
  FreeAndNil(FPersistenceHandle);
  FreeAndNil(FAdapter);
  FreeAndNil(FConnection);
end;

function TdmMasterDetail.GetBoldSystem: TBoldSystem;
begin
  Result := BoldSystemHandle1.System;
end;

function TdmMasterDetail.GetIsOpen: Boolean;
begin
  Result := BoldSystemHandle1.Active;
end;

procedure TdmMasterDetail.LoadModelFromFile(const AFileName: string);
var
  Strings: TStringList;
begin
  if not FileExists(AFileName) then
    raise EBold.CreateFmt(sModelFileNotFound, [AFileName]);
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(AFileName);
    BoldModel1.SetFromModelAsString(Strings);
    FModelLoaded := True;
  finally
    Strings.Free;
  end;
end;

procedure TdmMasterDetail.ConfigureSQLite(const ADatabase: string);
begin
  FConnection.Close;
  FConnection.Params.Clear;
  FConnection.DriverName := cSQLiteDriver;
  FConnection.Params.Values['Database'] := ADatabase;
  FConnection.LoginPrompt := False;
  // The setup the Bold test suite runs on SQLite: generic SQL92 with
  // SQLite's own column types for text and 64-bit integers
  FAdapter.DatabaseEngine := dbeGenericANSISQL92;
  FAdapter.SQLDatabaseConfig.ColumnTypeForText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForUnicodeText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForAnsiText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForInt64 := 'INTEGER';
end;

procedure TdmMasterDetail.EnsureSchema;
var
  TableNames: TStringList;
begin
  FConnection.Open;
  TableNames := TStringList.Create;
  try
    // Not TableExists: it caches its first answer for the lifetime of the
    // adapter, and a database dropped and opened again would then not get
    // its schema
    FPersistenceHandle.DatabaseInterface.AllTableNames('*', True, TableNames);
    if TableNames.IndexOf(cBoldTypeTable) = -1 then
      FPersistenceHandle.CreateDataBaseSchema(True);
  finally
    TableNames.Free;
  end;
end;

procedure TdmMasterDetail.OpenSystem(const ADatabase: string);
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

procedure TdmMasterDetail.CloseSystem(ASaveChanges: Boolean);
begin
  if IsOpen then
  begin
    if BoldSystem.BoldDirty then
    begin
      if ASaveChanges then
        BoldSystem.UpdateDatabase
      else
        BoldSystem.Discard;
    end;
    BoldSystemHandle1.Active := False;
  end;
  FConnection.Close;
end;

procedure TdmMasterDetail.DropDatabase;
begin
  CloseSystem(False);
  if FileExists(FDatabase) and not System.SysUtils.DeleteFile(FDatabase) then
    raise EBold.CreateFmt(sDatabaseNotDeleted, [FDatabase]);
end;

procedure TdmMasterDetail.Save;
begin
  BoldSystem.UpdateDatabase;
end;

end.
