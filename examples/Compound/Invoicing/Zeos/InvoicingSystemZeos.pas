{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit InvoicingSystemZeos;
{ The Bold infrastructure of the Invoicing example on a SQL database under
  Free Pascal: SQLite through Zeos and Bold's Zeos adapter
  (TBoldDatabaseAdapterZeos, in the Lazarus package BoldZeos). It is the twin
  of ../Data/dmInvoicing.pas, which does the same through FireDAC in Delphi,
  and has the calls of ../FPC/InvoicingSystemFPC.pas, which stores in an XML
  file and needs no database library. }

interface

uses
  SysUtils,
  Classes,
  ZConnection,
  BoldDefs,
  BoldHandles,
  BoldModel,
  BoldSystem,
  BoldSystemHandle,
  BoldPersistenceHandleDB,
  BoldDatabaseAdapterZeos;

type
  TInvoicingSystemZeos = class
  private
    FModel: TBoldModel;
    FTypeInfoHandle: TBoldSystemTypeInfoHandle;
    FConnection: TZConnection;
    FAdapter: TBoldDatabaseAdapterZeos;
    FPersistenceHandle: TBoldPersistenceHandleDB;
    FSystemHandle: TBoldSystemHandle;
    FModelLoaded: Boolean;
    FDatabase: string;
    function GetBoldSystem: TBoldSystem;
    function GetIsOpen: Boolean;
    procedure ConfigureSQLite(const ADatabase: string);
    procedure EnsureSchema;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadModelFromFile(const AFileName: string);
    { ADatabase is the SQLite database file. A file that does not exist yet is
      created, with Bold's schema for the model. }
    procedure OpenSystem(const ADatabase: string);
    procedure CloseSystem(ASaveChanges: Boolean);
    procedure Save;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property IsOpen: Boolean read GetIsOpen;
    property ModelLoaded: Boolean read FModelLoaded;
    property Database: string read FDatabase;
    property SystemHandle: TBoldSystemHandle read FSystemHandle;
  end;

implementation

uses
  BoldSQLDatabaseConfig;

resourcestring
  sModelNotLoaded = 'Load the model with LoadModelFromFile before opening the system';
  sSystemAlreadyOpen = 'The system is already open on %s';
  sModelFileNotFound = 'Model file not found: %s';

const
  cSQLiteProtocol = 'sqlite';
  cBoldTypeTable = 'BOLD_TYPE';

constructor TInvoicingSystemZeos.Create;
begin
  inherited Create;
  FModel := TBoldModel.Create(nil);
  FTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  FTypeInfoHandle.BoldModel := FModel;
  FConnection := TZConnection.Create(nil);
  FAdapter := TBoldDatabaseAdapterZeos.Create(nil);
  FAdapter.Connection := FConnection;
  FPersistenceHandle := TBoldPersistenceHandleDB.Create(nil);
  FPersistenceHandle.BoldModel := FModel;
  FPersistenceHandle.DatabaseAdapter := FAdapter;
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FTypeInfoHandle;
  FSystemHandle.PersistenceHandle := FPersistenceHandle;
end;

destructor TInvoicingSystemZeos.Destroy;
begin
  CloseSystem(False);
  // Reverse creation order, so nothing is destroyed while still referenced.
  FSystemHandle.PersistenceHandle := nil;
  FreeAndNil(FSystemHandle);
  FreeAndNil(FPersistenceHandle);
  FreeAndNil(FAdapter);
  FreeAndNil(FConnection);
  FreeAndNil(FTypeInfoHandle);
  FreeAndNil(FModel);
  inherited;
end;

function TInvoicingSystemZeos.GetBoldSystem: TBoldSystem;
begin
  Result := FSystemHandle.System;
end;

function TInvoicingSystemZeos.GetIsOpen: Boolean;
begin
  Result := FSystemHandle.Active;
end;

procedure TInvoicingSystemZeos.LoadModelFromFile(const AFileName: string);
var
  Strings: TStringList;
begin
  if not FileExists(AFileName) then
    raise EBold.CreateFmt(sModelFileNotFound, [AFileName]);
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(AFileName);
    FModel.SetFromModelAsString(Strings);
    FModelLoaded := True;
  finally
    Strings.Free;
  end;
end;

procedure TInvoicingSystemZeos.ConfigureSQLite(const ADatabase: string);
begin
  if FConnection.Connected then
    FConnection.Disconnect;
  FConnection.Protocol := cSQLiteProtocol;
  FConnection.Database := ADatabase;
  FConnection.LoginPrompt := False;
  // The same engine setup the Bold test suite runs on SQLite: generic SQL92
  // with SQLite's own column types for text and 64-bit integers.
  FAdapter.DatabaseEngine := dbeGenericANSISQL92;
  FAdapter.SQLDatabaseConfig.ColumnTypeForText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForUnicodeText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForAnsiText := 'TEXT';
  FAdapter.SQLDatabaseConfig.ColumnTypeForInt64 := 'INTEGER';
end;

procedure TInvoicingSystemZeos.EnsureSchema;
var
  TableNames: TStringList;
begin
  FConnection.Connect;
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

procedure TInvoicingSystemZeos.OpenSystem(const ADatabase: string);
begin
  if not FModelLoaded then
    raise EBold.Create(sModelNotLoaded);
  if IsOpen then
    raise EBold.CreateFmt(sSystemAlreadyOpen, [FDatabase]);
  FDatabase := ADatabase;
  ConfigureSQLite(ADatabase);
  EnsureSchema;
  FSystemHandle.Active := True;
end;

procedure TInvoicingSystemZeos.CloseSystem(ASaveChanges: Boolean);
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
  FSystemHandle.Active := False;
end;

procedure TInvoicingSystemZeos.Save;
begin
  BoldSystem.UpdateDatabase;
end;

end.
