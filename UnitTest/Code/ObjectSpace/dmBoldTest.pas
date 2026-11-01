unit dmBoldTest;

{ The data module with the BoldTestModel model and a persistent system, shared
  by the Delphi and the Free Pascal test builds.

  dmBoldTest.dfm holds only the Bold handles and the model. The database behind
  the persistence handle (connection and adapter) comes from the connector that
  the test runner links in:
    Delphi:      BoldTestDMFireDAC (UnitTest.dpr): FireDAC, SQLite shared-cache
                 in-memory by default, another engine through UnitTest.ini
    Free Pascal: BoldTestDMZeos (FPC/UnitTestFPC.lpr): Zeos, SQLite shared-cache
                 in-memory
  Each connector registers itself in its initialization. }

interface

uses
  Classes,
  BoldSystemHandle,
  BoldModel,
  BoldHandles,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB;

type
  TBoldTestDM = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
  public
    destructor Destroy; override;
  end;

  { Creates the connection and the database adapter, owned by DataModule,
    configures them and assigns the adapter to the persistence handle. }
  TBoldTestDMConnector = procedure(DataModule: TBoldTestDM);

var
  BoldTestDM: TBoldTestDM;

procedure RegisterBoldTestDMConnector(Connector: TBoldTestDMConnector);
procedure EnsureBoldTestDM;
procedure CloseBoldTestDM;

implementation

{$R *.dfm}

uses
  SysUtils,
  BoldTestModel; // links the generated classes, without which the system cannot activate

var
  RegisteredConnector: TBoldTestDMConnector = nil;

destructor TBoldTestDM.Destroy;
begin
  if BoldSystemHandle1.Active then
    BoldSystemHandle1.Active := False;
  inherited;
end;

procedure RegisterBoldTestDMConnector(Connector: TBoldTestDMConnector);
begin
  RegisteredConnector := Connector;
end;

procedure EnsureBoldTestDM;
begin
  if not Assigned(BoldTestDM) then
  begin
    if not Assigned(RegisteredConnector) then
      raise Exception.Create('dmBoldTest: no database connector is registered. The test runner must ' +
        'use BoldTestDMFireDAC (Delphi) or BoldTestDMZeos (Free Pascal).');
    BoldTestDM := TBoldTestDM.Create(nil);
    RegisteredConnector(BoldTestDM);
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
