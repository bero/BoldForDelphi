{ Global compiler directives }
{$include bold.inc}
unit BoldDatabaseAdapterZeos;

{ The Bold database adapter for ZeosLib 8.0 (Free Pascal / Lazarus).
  Delphi uses TBoldDatabaseAdapterFireDAC. }

interface

uses
  Classes,
  BoldAbstractDataBaseAdapter,
  BoldDBInterfaces,
  BoldZeosInterfaces,
  ZConnection;

type
  { TBoldDatabaseAdapterZeos }
  TBoldDatabaseAdapterZeos = class(TBoldAbstractDatabaseAdapter)
  private
    fBoldZeosConnection: TBoldZeosConnection;
    procedure SetConnection(const Value: TZConnection);
    function GetConnection: TZConnection;
  protected
    procedure ReleaseBoldDatabase; override;
    function GetDataBaseInterface: IBoldDatabase; override;
  public
    destructor Destroy; override;
    procedure CreateDatabase(DropExisting: Boolean = true); override;
    procedure DropDatabase; override;
    function DatabaseExists: Boolean; override;
  published
    property Connection: TZConnection read GetConnection write SetConnection;
    property DatabaseEngine;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs;

{ TBoldDatabaseAdapterZeos }

destructor TBoldDatabaseAdapterZeos.Destroy;
begin
  Changed;
  FreePublisher;
  FreeAndNil(fBoldZeosConnection);
  inherited;
end;

function TBoldDatabaseAdapterZeos.DatabaseExists: Boolean;
begin
  Result := DatabaseInterface.DatabaseExists;
end;

procedure TBoldDatabaseAdapterZeos.CreateDatabase(DropExisting: Boolean);
begin
  DatabaseInterface.CreateDatabase(DropExisting);
end;

procedure TBoldDatabaseAdapterZeos.DropDatabase;
begin
  DatabaseInterface.DropDatabase;
end;

function TBoldDatabaseAdapterZeos.GetConnection: TZConnection;
begin
  Result := InternalDatabase as TZConnection;
end;

procedure TBoldDatabaseAdapterZeos.SetConnection(const Value: TZConnection);
begin
  InternalDatabase := Value;
end;

function TBoldDatabaseAdapterZeos.GetDataBaseInterface: IBoldDatabase;
begin
  if not Assigned(Connection) then
    raise EBold.CreateFmt(sAdapterNotConnected, [ClassName, 'Zeos']);
  if not Assigned(fBoldZeosConnection) then
    fBoldZeosConnection := TBoldZeosConnection.Create(Connection, SQLDataBaseConfig);
  Result := fBoldZeosConnection;
end;

procedure TBoldDatabaseAdapterZeos.ReleaseBoldDatabase;
begin
  FreeAndNil(fBoldZeosConnection);
end;

end.
