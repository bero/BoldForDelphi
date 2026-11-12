{ Global compiler directives }
{$include bold.inc}
unit BoldZeosInterfaces;

{ Bold database interfaces on ZeosLib 8.0, for Free Pascal / Lazarus.

  Delphi uses FireDAC (BoldFireDACInterfaces); this unit follows that one
  closely. As on FireDAC, one query class serves both IBoldQuery and
  IBoldExecQuery.

  Zeos is compiled in mode Delphi (String = AnsiString) and Bold in mode
  DelphiUnicode, so text is handed to Zeos through its UnicodeString
  accessors (AsUnicodeString and friends), never through AsString.

  The connection's transaction isolation level is left as configured on the
  TZConnection: FireDAC switches between read committed and repeatable read
  per transaction, but Zeos rejects levels a driver does not support (SQLite
  supports only serializable). }

interface

{$M-}  // Reset RTTI state (BoldDBInterfaces enables {$M+} for interface mocking)

uses
  Classes,
  Db,
  SysUtils,

  ZConnection,
  ZDataset,
  ZDatasetParam,

  BoldSQLDatabaseConfig,
  BoldDBInterfaces,
  BoldDefs;

type
  { forward declarations }
  TBoldZeosParameter = class;
  TBoldZeosQuery = class;
  TBoldZeosTable = class;
  TBoldZeosConnection = class;

  TBoldZeosQueryClass = class of TBoldZeosQuery;

  { TBoldZeosParameter }
  TBoldZeosParameter = class(TBoldParameterWrapper, IBoldParameter)
  private
    fZParam: TZParam;
    function EmptyStringMarker: string;
    function CurrencyIsBoundAsScaledInteger: Boolean;
    procedure SetCurrencyValue(const Value: Currency);
    function GetAsVariant: Variant;
    procedure SetAsVariant(const NewValue: Variant);
    function GetName: string;
    procedure Clear;
    function GetDataType: TFieldType;
    procedure SetDataType(Value: TFieldType);
    function GetAsBCD: Currency;
    function GetAsBlob: TBoldBlobData;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsInt64: Int64;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetAsAnsiString: TBoldAnsiString;
    function GetAsWideString: WideString;
    function GetIsNull: Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsBlob(const Value: TBoldBlobData);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(Value: Integer);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsAnsiString(const Value: TBoldAnsiString);
    procedure SetAsWideString(const Value: WideString);
    procedure SetAsSmallInt(Value: Integer);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsWord(Value: Integer);
    procedure SetText(const Value: string);
    procedure AssignFieldValue(const source: IBoldField);
    procedure Assign(const source: IBoldParameter);
  public
    constructor Create(ZeosParameter: TZParam; DatasetWrapper: TBoldAbstractQueryWrapper);
    property ZParam: TZParam read fZParam;
  end;

  { TBoldZeosQuery }
  TBoldZeosQuery = class(TBoldBatchDataSetWrapper, IBoldQuery, IBoldExecQuery, IBoldParameterized)
  private
    fQuery: TZQuery;
    fParamsSnapshot: TParams;
    fScript: TObject; // TZSQLProcessor, created on the first batch
    fLastExecWasScript: Boolean;
    fParamCheck: Boolean;
    fParamsCleared: Boolean;
    fReadTransactionStarted: Boolean;
    fUseReadTransactions: Boolean;
    function IsMultiStatement: Boolean;
    procedure ExecScript;
    function GetQuery: TZQuery;
    function GetZeosConnection: TBoldZeosConnection;
    procedure AssignParams(SourceParams: TParams);
    function GetParamCount: Integer;
    function GetParam(i: Integer): IBoldParameter;
    function GetParamCheck: Boolean;
    procedure SetParamCheck(Value: Boolean);
    function GetRequestLiveQuery: Boolean;
    procedure SetRequestLiveQuery(NewValue: Boolean);
    procedure AssignSQL(SQL: TStrings);
    procedure ReparseClearedParams;
    function GetRecordCount: Integer;
    function GetUseReadTransactions: Boolean;
    procedure SetUseReadTransactions(Value: Boolean);
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
    procedure BeginStatement;
    procedure EndStatement;
    procedure FailStatement(E: Exception);
    property ZeosConnection: TBoldZeosConnection read GetZeosConnection;
  protected
    function ParamByName(const Value: string): IBoldParameter; override;
    function FindParam(const Value: string): IBoldParameter; override;
    function CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: Integer): IBoldParameter; override;
    function GetParams: TParams; override;
    function GetSqlText: string; override;
    function GetSQLStrings: TStrings; override;
    procedure AssignSQLText(const SQL: string); override;
    function GetRowsAffected: Integer;
    function GetDataSet: TDataSet; override;
    procedure Prepare;
    procedure ClearParams;
    procedure Open; override;
    procedure Close; override;
    procedure ExecSQL; override;
    function GetRecNo: Integer; override;
    property Query: TZQuery read GetQuery;
  public
    constructor Create(BoldZeosConnection: TBoldZeosConnection); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  { TBoldZeosTable }
  TBoldZeosTable = class(TBoldDatasetWrapper, IBoldTable)
  private
    fZTable: TZTable;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure CreateTable;
    procedure DeleteTable;
    function GetIndexDefs: TIndexDefs;
    procedure SetTableName(const NewName: string);
    function GetTableName: string;
    procedure SetExclusive(NewValue: Boolean);
    function GetExclusive: Boolean;
    function GetExists: Boolean;
  protected
    function GetDataSet: TDataSet; override;
    function ParamByName(const Value: string): IBoldParameter; override;
    function FindParam(const Value: string): IBoldParameter; override;
  public
    constructor Create(aZTable: TZTable; BoldZeosConnection: TBoldZeosConnection); reintroduce;
    destructor Destroy; override;
  end;

  { TBoldZeosConnection }
  TBoldZeosConnection = class(TBoldDatabaseWrapper, IBoldDataBase)
  private
    fZConnection: TZConnection;
    fOwnsConnection: Boolean;
    fCachedTable: TBoldZeosTable;
    fCachedQuery1: TBoldZeosQuery;
    fCachedQuery2: TBoldZeosQuery;
    fCachedExecQuery1: TBoldZeosQuery;
    fExecuteQueryCount: Integer;
    // True between StartTransaction and Commit/RollBack. Detects a transaction
    // silently destroyed by a connection reset (the server rolls it back and
    // every later statement would autocommit -> partial commit corruption).
    fExpectedTransaction: Boolean;
    function GetConnected: Boolean;
    function GetInTransaction: Boolean;
    function GetIsSQLBased: Boolean;
    procedure SetLogInPrompt(NewValue: Boolean);
    function GetLogInPrompt: Boolean;
    procedure SetKeepConnection(NewValue: Boolean);
    function GetKeepConnection: Boolean;
    procedure StartTransaction;
    procedure StartReadTransaction;
    procedure Commit;
    procedure RollBack;
    procedure Open;
    procedure Close;
    procedure Reconnect;
    procedure EnsureTransactionIntact;
    function SupportsTableCreation: Boolean;
    procedure ReleaseCachedObjects;
    function GetIsExecutingQuery: Boolean;
    procedure BeginExecuteQuery;
    procedure EndExecuteQuery;
    function CreateAnotherDatabaseConnection: IBoldDatabase;
    function GetImplementor: TObject;
    procedure ExecuteScript(const Script: string);
  protected
    procedure AllTableNames(Pattern: string; ShowSystemTables: Boolean; TableNameList: TStrings); override;
    function GetIndexDescriptions(const TableName: String): TBoldIndexDescriptionArray; override;
    function GetTable: IBoldTable; override;
    function GetQuery: IBoldQuery; override;
    function GetExecQuery: IBoldExecQuery; override;
    procedure ReleaseTable(var Table: IBoldTable); override;
    procedure ReleaseQuery(var Query: IBoldQuery); override;
    procedure ReleaseExecQuery(var Query: IBoldExecQuery); override;
  public
    constructor Create(aZConnection: TZConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
    destructor Destroy; override;
    procedure CreateDatabase(DropExisting: Boolean = true); override;
    procedure DropDatabase; override;
    function DatabaseExists: Boolean; override;
    function GetDatabaseError(const E: Exception; const sSQL: string = ''): EBoldDatabaseError;
    property ZConnection: TZConnection read fZConnection;
    property ExecuteQueryCount: Integer read fExecuteQueryCount;
  end;

var
  BoldZeosQueryClass: TBoldZeosQueryClass = TBoldZeosQuery;

implementation

uses
  Variants,
  StrUtils,

  ZExceptions,
  ZSqlProcessor,

  BoldUtils,
  BoldGuard;

type
  TStringsAccess = class(TStrings);
  TCollectionAccess = class(TCollection);

{ Case-insensitive match of Name against a file-name style Mask (* and ?), as
  MatchesMask does on FireDAC. Masks is a Lazarus unit, not part of the
  Free Pascal RTL. }
function MatchesTableMask(const Name, Mask: string): Boolean;

  function Match(n, m: Integer): Boolean;
  begin
    while m <= Length(Mask) do
    begin
      case Mask[m] of
        '*':
          begin
            while (m <= Length(Mask)) and (Mask[m] = '*') do
              inc(m);
            if m > Length(Mask) then
              Exit(True);
            while n <= Length(Name) do
            begin
              if Match(n, m) then
                Exit(True);
              inc(n);
            end;
            Exit(False);
          end;
        '?':
          if n > Length(Name) then
            Exit(False);
      else
        if (n > Length(Name)) or (UpCase(Name[n]) <> UpCase(Mask[m])) then
          Exit(False);
      end;
      inc(n);
      inc(m);
    end;
    Result := n > Length(Name);
  end;

begin
  Result := Match(1, 1);
end;

{ TBoldZeosParameter }

constructor TBoldZeosParameter.Create(ZeosParameter: TZParam; DatasetWrapper: TBoldAbstractQueryWrapper);
begin
  inherited Create(DatasetWrapper);
  fZParam := ZeosParameter;
end;

function TBoldZeosParameter.EmptyStringMarker: string;
begin
  Result := DatasetWrapper.DatabaseWrapper.SQLDataBaseConfig.EmptyStringMarker;
end;

{ Zeos binds a currency parameter for SQLite as its scaled 64-bit integer
  (120.5 as 1205000), which reads back right only from a column declared
  CURRENCY or MONEY. Bold's column for a currency is DOUBLE PRECISION, where
  that integer is stored as it is. }
function TBoldZeosParameter.CurrencyIsBoundAsScaledInteger: Boolean;
begin
  Result := (DatasetWrapper.DatabaseWrapper is TBoldZeosConnection) and
    StartsText('sqlite', TBoldZeosConnection(DatasetWrapper.DatabaseWrapper).fZConnection.Protocol);
end;

{ For SQLite the value goes as a Double, which the column stores as the same
  REAL that FireDAC's decimal text ends up as, so a database written under
  Delphi and one written under Free Pascal hold the same values. A Double
  carries the four decimals of a currency up to about 15 significant digits.
  Other protocols take the currency itself. }
procedure TBoldZeosParameter.SetCurrencyValue(const Value: Currency);
begin
  if CurrencyIsBoundAsScaledInteger then
    fZParam.AsDouble := Value
  else
    fZParam.AsCurrency := Value;
end;

procedure TBoldZeosParameter.Clear;
begin
  fZParam.Clear;
end;

function TBoldZeosParameter.GetName: string;
begin
  Result := fZParam.Name;
end;

function TBoldZeosParameter.GetDataType: TFieldType;
begin
  Result := fZParam.DataType;
  // SetAsBlob stores the bytes with AsBytes, which makes Zeos report
  // ftVarBytes. Bold has no byte-array parameters: it is a blob, and callers
  // such as the SQL log must see a blob type, or they read the bytes AsString
  // and Zeos raises.
  if Result in [ftBytes, ftVarBytes] then
    Result := ftBlob;
end;

procedure TBoldZeosParameter.SetDataType(Value: TFieldType);
begin
  fZParam.DataType := Value;
end;

function TBoldZeosParameter.GetAsVariant: Variant;
begin
  Result := fZParam.Value;
end;

procedure TBoldZeosParameter.SetAsVariant(const NewValue: Variant);
begin
  // A currency in a Variant would take the same path as AsCurrency
  if VarType(NewValue) = varCurrency then
    SetCurrencyValue(NewValue)
  else
    fZParam.Value := NewValue;
end;

function TBoldZeosParameter.GetIsNull: Boolean;
begin
  Result := fZParam.IsNull;
end;

function TBoldZeosParameter.GetAsBCD: Currency;
begin
  Result := fZParam.AsCurrency;
end;

function TBoldZeosParameter.GetAsBlob: TBoldBlobData;
var
  Bytes: TBytes;
begin
  Bytes := fZParam.AsBytes;
  SetLength(Result, Length(Bytes));
  if Length(Bytes) > 0 then
    Move(Bytes[0], Result[1], Length(Bytes));
end;

function TBoldZeosParameter.GetAsBoolean: Boolean;
begin
  Result := fZParam.AsBoolean;
end;

function TBoldZeosParameter.GetAsDateTime: TDateTime;
begin
  Result := fZParam.AsDateTime;
end;

function TBoldZeosParameter.GetAsCurrency: Currency;
begin
  Result := fZParam.AsCurrency;
end;

function TBoldZeosParameter.GetAsFloat: Double;
begin
  Result := fZParam.AsDouble;
end;

function TBoldZeosParameter.GetAsInteger: Integer;
begin
  Result := fZParam.AsInteger;
end;

function TBoldZeosParameter.GetAsInt64: Int64;
begin
  Result := fZParam.AsInt64;
end;

function TBoldZeosParameter.GetAsMemo: string;
begin
  Result := fZParam.AsUnicodeString;
end;

function TBoldZeosParameter.GetAsString: string;
begin
  Result := fZParam.AsUnicodeString;
  if Result = EmptyStringMarker then
    Result := '';
end;

function TBoldZeosParameter.GetAsAnsiString: TBoldAnsiString;
begin
  Result := fZParam.AsAnsiString;
end;

function TBoldZeosParameter.GetAsWideString: WideString;
begin
  Result := fZParam.AsUnicodeString;
  if Result = EmptyStringMarker then
    Result := '';
end;

procedure TBoldZeosParameter.SetAsBCD(const Value: Currency);
begin
  SetCurrencyValue(Value);
end;

procedure TBoldZeosParameter.SetAsBlob(const Value: TBoldBlobData);
var
  Bytes: TBytes;
begin
  if fZParam.DataType = ftUnknown then
    fZParam.DataType := ftBlob;
  if Value = '' then
    fZParam.Value := Null
  else
  begin
    SetLength(Bytes, Length(Value));
    Move(Value[1], Bytes[0], Length(Value));
    // AsBytes turns the Zeos parameter into ftVarBytes; GetDataType reports
    // that as ftBlob. Zeos 8.0.0's AsBlob would keep ftBlob, but under FPC its
    // single-value blob is corrupted and the parameter's destructor crashes.
    fZParam.AsBytes := Bytes;
  end;
end;

procedure TBoldZeosParameter.SetAsBoolean(Value: Boolean);
begin
  fZParam.AsBoolean := Value;
end;

procedure TBoldZeosParameter.SetAsCurrency(const Value: Currency);
begin
  SetCurrencyValue(Value);
end;

procedure TBoldZeosParameter.SetAsDate(const Value: TDateTime);
begin
  fZParam.AsDate := Value;
end;

procedure TBoldZeosParameter.SetAsDateTime(const Value: TDateTime);
begin
  fZParam.AsDateTime := Value;
end;

procedure TBoldZeosParameter.SetAsFloat(const Value: Double);
begin
  fZParam.AsDouble := Value;
end;

procedure TBoldZeosParameter.SetAsInteger(Value: Integer);
begin
  fZParam.AsInteger := Value;
end;

procedure TBoldZeosParameter.SetAsInt64(const Value: Int64);
begin
  fZParam.AsInt64 := Value;
end;

procedure TBoldZeosParameter.SetAsMemo(const Value: string);
begin
  fZParam.AsUnicodeMemo := Value;
end;

procedure TBoldZeosParameter.SetAsString(const Value: string);
begin
  if Value = '' then
    fZParam.AsUnicodeString := EmptyStringMarker
  else
    fZParam.AsUnicodeString := Value;
end;

procedure TBoldZeosParameter.SetAsAnsiString(const Value: TBoldAnsiString);
begin
  fZParam.AsAnsiString := Value;
end;

procedure TBoldZeosParameter.SetAsWideString(const Value: WideString);
begin
  if Value = '' then
    fZParam.AsUnicodeString := EmptyStringMarker
  else
    fZParam.AsUnicodeString := Value;
end;

procedure TBoldZeosParameter.SetAsSmallInt(Value: Integer);
begin
  fZParam.AsSmallInt := Value;
end;

procedure TBoldZeosParameter.SetAsTime(const Value: TDateTime);
begin
  fZParam.AsTime := Value;
end;

procedure TBoldZeosParameter.SetAsWord(Value: Integer);
begin
  fZParam.AsInteger := Value;
end;

procedure TBoldZeosParameter.SetText(const Value: string);
begin
  fZParam.Value := Value;
end;

procedure TBoldZeosParameter.Assign(const source: IBoldParameter);
begin
  // Through SetAsVariant, for a currency; see SetCurrencyValue
  SetAsVariant(source.AsVariant);
end;

procedure TBoldZeosParameter.AssignFieldValue(const source: IBoldField);
begin
  // Type and value only, as on FireDAC: Zeos binds by name, so the field's
  // name must not be copied onto the parameter.
  // A currency goes through SetCurrencyValue, which chooses the type too
  if VarType(source.AsVariant) = varCurrency then
    SetCurrencyValue(source.AsVariant)
  else
  begin
    fZParam.DataType := source.Field.DataType;
    fZParam.Value := source.AsVariant;
  end;
end;

{ TBoldZeosQuery }

constructor TBoldZeosQuery.Create(BoldZeosConnection: TBoldZeosConnection);
begin
  inherited Create(BoldZeosConnection);
  fUseReadTransactions := true;
  fParamCheck := true;
  fQuery := TZQuery.Create(nil);
  fQuery.Connection := BoldZeosConnection.ZConnection;
  fQuery.ParamCheck := true; // stays on, see SetParamCheck
end;

destructor TBoldZeosQuery.Destroy;
begin
  if fReadTransactionStarted then
    Close;
  FreeAndNil(fScript);
  FreeAndNil(fQuery);
  FreeAndNil(fParamsSnapshot);
  inherited;
end;

function TBoldZeosQuery.GetQuery: TZQuery;
begin
  Result := fQuery;
end;

function TBoldZeosQuery.GetZeosConnection: TBoldZeosConnection;
begin
  Result := DatabaseWrapper as TBoldZeosConnection;
end;

function TBoldZeosQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TBoldZeosQuery.GetParamCheck: Boolean;
begin
  Result := fParamCheck;
end;

procedure TBoldZeosQuery.SetParamCheck(Value: Boolean);
begin
  { Remembered, not passed on. With ParamCheck off, FireDAC leaves the
    parameters alone when the SQL changes; Zeos drops them all instead. Bold
    builds a batch with ParamCheck off: it creates the parameters, then
    finishes the SQL (EndUpdate) and only then switches ParamCheck on, which
    on Zeos would leave every parameter NULL. With ParamCheck on, Zeos
    rebuilds the parameters from the markers and keeps the values of those
    with the same name, which is what FireDAC's behaviour amounts to here.
    Quoted text is not scanned for markers. }
  fParamCheck := Value;
end;

function TBoldZeosQuery.GetParamCount: Integer;
begin
  Result := Query.Params.Count;
end;

function TBoldZeosQuery.GetParam(i: Integer): IBoldParameter;
begin
  Result := TBoldZeosParameter.Create(Query.Params[i], Self);
end;

function TBoldZeosQuery.GetParams: TParams;
var
  i: Integer;
begin
  { A detached snapshot, as on FireDAC (PSGetParams): Bold copies parameter
    values through IBoldParameterized/IBoldParameter, and only counts and
    brackets updates through this view. TZParams.AssignTo(TParams) fills only
    parameters that already exist in the target, so the copy is built here. }
  if not Assigned(fParamsSnapshot) then
    fParamsSnapshot := TParams.Create(nil);
  fParamsSnapshot.Clear;
  for i := 0 to Query.Params.Count - 1 do
    Query.Params[i].AssignTo(fParamsSnapshot.Add as TParam);
  Result := fParamsSnapshot;
end;

function TBoldZeosQuery.GetRecNo: Integer;
begin
  Result := Query.RecNo - 1;
end;

function TBoldZeosQuery.GetRecordCount: Integer;
begin
  Result := Query.RecordCount;
end;

function TBoldZeosQuery.GetRequestLiveQuery: Boolean;
begin
  Result := False;
end;

procedure TBoldZeosQuery.SetRequestLiveQuery(NewValue: Boolean);
begin
  // ignore, as on FireDAC
end;

function TBoldZeosQuery.GetRowsAffected: Integer;
begin
  // TZSQLProcessor does not report rows affected, so a batch reports -1
  if fLastExecWasScript then
    Result := -1
  else
    Result := Query.RowsAffected;
end;

function TBoldZeosQuery.GetSQLStrings: TStrings;
begin
  Result := Query.SQL;
end;

function TBoldZeosQuery.GetSqlText: string;
begin
  Result := Query.SQL.Text;
end;

function TBoldZeosQuery.GetUseReadTransactions: Boolean;
begin
  Result := fUseReadTransactions;
end;

procedure TBoldZeosQuery.SetUseReadTransactions(Value: Boolean);
begin
  fUseReadTransactions := Value;
end;

procedure TBoldZeosQuery.AssignParams(SourceParams: TParams);
var
  i: Integer;
  ZParam: TZParam;
begin
  Query.Params.Clear;
  if Assigned(SourceParams) then
    for i := 0 to SourceParams.Count - 1 do
    begin
      ZParam := Query.Params.CreateParam(SourceParams[i].DataType, SourceParams[i].Name, SourceParams[i].ParamType);
      ZParam.Value := SourceParams[i].Value;
    end;
end;

procedure TBoldZeosQuery.AssignSQL(SQL: TStrings);
begin
  ReparseClearedParams;
  Query.SQL.Assign(SQL);
end;

procedure TBoldZeosQuery.AssignSQLText(const SQL: string);
begin
  ReparseClearedParams;
  Query.SQL.Text := SQL;
end;

{ Zeos creates the parameters from the SQL's markers only when the SQL text
  changes. After ClearParams the next SQL must bring its parameters back even
  when it is the text the query already has, as on FireDAC (TBoldDbCopy sets
  the same INSERT again once per batch). Emptying the SQL first makes the
  assignment a change. Only when no parameter exists: a caller that created
  its own since ClearParams (the link mapper's id-list parameters) keeps them. }
procedure TBoldZeosQuery.ReparseClearedParams;
begin
  if fParamsCleared and (Query.Params.Count = 0) then
    Query.SQL.Clear;
  fParamsCleared := False;
end;

procedure TBoldZeosQuery.BeginExecuteQuery;
begin
  ZeosConnection.BeginExecuteQuery;
end;

procedure TBoldZeosQuery.EndExecuteQuery;
begin
  ZeosConnection.EndExecuteQuery;
end;

procedure TBoldZeosQuery.Clear;
begin
  { Close (not just clearing the SQL) so a read transaction this query
    started gets committed before the wrapper goes back to the cache. }
  Close;
  AssignSQLText('');
  ClearParams;
end;

procedure TBoldZeosQuery.ClearParams;
begin
  Query.Params.Clear;
  fParamsCleared := True; // see ReparseClearedParams
end;

procedure TBoldZeosQuery.Close;
begin
  inherited;
  if fReadTransactionStarted and ZeosConnection.GetInTransaction then
    ZeosConnection.Commit;
  fReadTransactionStarted := false;
end;

function TBoldZeosQuery.CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType; Size: Integer): IBoldParameter;
var
  ZParam: TZParam;
begin
  ZParam := Query.Params.CreateParam(FldType, ParamName, ptUnknown);
  ZParam.Value := Null;
  Result := TBoldZeosParameter.Create(ZParam, Self);
end;

function TBoldZeosQuery.FindParam(const Value: string): IBoldParameter;
var
  ZParam: TZParam;
begin
  Result := nil;
  ZParam := Query.Params.FindParam(Value);
  if Assigned(ZParam) then
    Result := TBoldZeosParameter.Create(ZParam, Self);
end;

function TBoldZeosQuery.ParamByName(const Value: string): IBoldParameter;
begin
  Result := TBoldZeosParameter.Create(Query.Params.ParamByName(Value), Self);
end;

procedure TBoldZeosQuery.Prepare;
begin
  Query.Prepare;
end;

procedure TBoldZeosQuery.BeginStatement;
begin
  ZeosConnection.EnsureTransactionIntact;
  if not ZeosConnection.GetInTransaction then
  begin
    if fUseReadTransactions then
      ZeosConnection.StartReadTransaction;
    fReadTransactionStarted := fUseReadTransactions;
  end;
  { When already in a transaction, fReadTransactionStarted is left as-is so a
    read transaction this query started stays owned and gets committed. }
end;

procedure TBoldZeosQuery.EndStatement;
begin
  if fReadTransactionStarted and ZeosConnection.GetInTransaction then
  begin
    ZeosConnection.Commit;
    fReadTransactionStarted := false;
  end;
end;

procedure TBoldZeosQuery.FailStatement(E: Exception);
begin
  if ZeosConnection.GetInTransaction then
    ZeosConnection.RollBack;
  fReadTransactionStarted := false;
  raise ZeosConnection.GetDatabaseError(E, Query.SQL.Text);
end;

procedure TBoldZeosQuery.ExecSQL;
begin
  if InBatch then
  begin
    BatchExecSQL;
    exit;
  end;
  BeginExecuteQuery;
  try
    BoldLogSQLWithParams(Query.SQL, self);
    try
      BeginStatement;
      if IsMultiStatement then
        ExecScript
      else
      begin
        Query.ExecSQL;
        fLastExecWasScript := False;
      end;
      EndStatement;
    except
      on E: Exception do
        FailStatement(E);
    end;
  finally
    EndExecuteQuery;
  end;
end;

{ True when the SQL holds more than one statement, as Bold's batches do
  (SQLDatabaseConfig.BatchQuerySeparator). SQL without a ';' is never parsed. }
function TBoldZeosQuery.IsMultiStatement: Boolean;
var
  Script: TZSQLProcessor;
begin
  Result := False;
  if Pos(';', Query.SQL.Text) = 0 then
    Exit;
  if not Assigned(fScript) then
  begin
    Script := TZSQLProcessor.Create(nil);
    Script.Connection := Query.Connection;
    fScript := Script;
  end;
  Script := TZSQLProcessor(fScript);
  Script.Script.Assign(Query.SQL);
  Script.Parse;
  Result := Script.StatementCount > 1;
end;

{ SQLite prepares only the first statement of a string, and TZQuery runs one
  statement. TZSQLProcessor runs them one by one and binds each statement's
  parameters by name, as FireDAC does for a batch. }
procedure TBoldZeosQuery.ExecScript;
var
  Script: TZSQLProcessor;
begin
  Script := TZSQLProcessor(fScript);
  Script.Params.Assign(Query.Params);
  Script.Execute;
  fLastExecWasScript := True;
end;

procedure TBoldZeosQuery.Open;
begin
  BeginExecuteQuery;
  try
    BoldLogSQLWithParams(Query.SQL, self);
    try
      BeginStatement;
      Query.ReadOnly := true;
      inherited;
    except
      on E: Exception do
        FailStatement(E);
    end;
  finally
    EndExecuteQuery;
  end;
end;

{ TBoldZeosTable }

constructor TBoldZeosTable.Create(aZTable: TZTable; BoldZeosConnection: TBoldZeosConnection);
begin
  inherited Create(BoldZeosConnection);
  fZTable := aZTable;
end;

destructor TBoldZeosTable.Destroy;
begin
  FreeAndNil(fZTable);
  inherited;
end;

function TBoldZeosTable.GetDataSet: TDataSet;
begin
  Result := fZTable;
end;

procedure TBoldZeosTable.CreateTable;
begin
  // Not used - SupportsTableCreation returns False, so Bold uses SQL generation instead
  raise EBold.CreateFmt('%s.CreateTable is not implemented', [ClassName]);
end;

procedure TBoldZeosTable.DeleteTable;
begin
  // Not used - SupportsTableCreation returns False, so Bold uses SQL generation instead
  raise EBold.CreateFmt('%s.DeleteTable is not implemented', [ClassName]);
end;

procedure TBoldZeosTable.AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string);
begin
  // Not used - SupportsTableCreation returns False, so Bold uses SQL generation instead
  raise EBold.CreateFmt('%s.AddIndex is not implemented', [ClassName]);
end;

function TBoldZeosTable.GetIndexDefs: TIndexDefs;
begin
  // Zeos datasets have no IndexDefs. Bold reads indexes through the engine's
  // IndexInfoTemplate query instead (TBoldDatabaseWrapper.GetIndexDescriptions).
  raise EBold.CreateFmt('%s.GetIndexDefs: Zeos datasets have no IndexDefs; set IndexInfoTemplate in the SQLDatabaseConfig', [ClassName]);
end;

function TBoldZeosTable.FindParam(const Value: string): IBoldParameter;
begin
  // Required by the base class but never called for IBoldTable
  Result := nil;
end;

function TBoldZeosTable.ParamByName(const Value: string): IBoldParameter;
begin
  // Required by the base class but never called for IBoldTable
  Result := nil;
end;

function TBoldZeosTable.GetExclusive: Boolean;
begin
  Result := False;
end;

procedure TBoldZeosTable.SetExclusive(NewValue: Boolean);
begin
end;

function TBoldZeosTable.GetExists: Boolean;
var
  AllTables: TStringList;
begin
  Result := False;
  if Assigned(fZTable.Connection) then
  begin
    AllTables := TStringList.Create;
    try
      AllTables.CaseSensitive := false;
      fZTable.Connection.GetTableNames('', AllTables);
      Result := AllTables.IndexOf(GetTableName) <> -1;
    finally
      AllTables.Free;
    end;
  end;
end;

function TBoldZeosTable.GetTableName: string;
begin
  Result := fZTable.TableName;
end;

procedure TBoldZeosTable.SetTableName(const NewName: string);
begin
  fZTable.TableName := NewName;
end;

{ TBoldZeosConnection }

constructor TBoldZeosConnection.Create(aZConnection: TZConnection; SQLDataBaseConfig: TBoldSQLDatabaseConfig);
begin
  inherited Create(SQLDataBaseConfig);
  fZConnection := aZConnection;
end;

destructor TBoldZeosConnection.Destroy;
begin
  ReleaseCachedObjects;
  if fOwnsConnection then
    FreeAndNil(fZConnection);
  inherited;
end;

function TBoldZeosConnection.GetImplementor: TObject;
begin
  Result := fZConnection;
end;

procedure TBoldZeosConnection.AllTableNames(Pattern: string; ShowSystemTables: Boolean; TableNameList: TStrings);
var
  AllTables: TStringList;
  i: Integer;
begin
  // Zeos returns user tables only; ShowSystemTables has no effect.
  AllTables := TStringList.Create;
  try
    fZConnection.GetTableNames('', AllTables);
    if Pattern = '' then
      TableNameList.Assign(AllTables)
    else
      for i := 0 to AllTables.Count - 1 do
        if MatchesTableMask(AllTables[i], Pattern) then
          TableNameList.Add(AllTables[i]);
  finally
    AllTables.Free;
  end;
end;

procedure TBoldZeosConnection.EnsureTransactionIntact;
begin
  if fExpectedTransaction and not fZConnection.InTransaction then
  begin
    fExpectedTransaction := false;
    raise EBoldDatabaseConnectionError.Create(BOLD_DATABASE_ERROR_TRANSACTION_LOST);
  end;
end;

function TBoldZeosConnection.GetInTransaction: Boolean;
begin
  Result := fZConnection.InTransaction;
end;

procedure TBoldZeosConnection.StartReadTransaction;
begin
  fZConnection.StartTransaction;
end;

procedure TBoldZeosConnection.StartTransaction;
begin
  fZConnection.StartTransaction;
  fExpectedTransaction := true;
end;

procedure TBoldZeosConnection.Commit;
begin
  fExpectedTransaction := false;
  fZConnection.Commit;
end;

procedure TBoldZeosConnection.RollBack;
begin
  fExpectedTransaction := false;
  // Tolerate a transaction that vanished with a connection reset: the server
  // has already rolled it back, there is nothing left to roll back here.
  if fZConnection.InTransaction then
    fZConnection.Rollback;
end;

function TBoldZeosConnection.GetIsExecutingQuery: Boolean;
begin
  Result := fExecuteQueryCount > 0;
end;

procedure TBoldZeosConnection.BeginExecuteQuery;
begin
  inc(fExecuteQueryCount);
end;

procedure TBoldZeosConnection.EndExecuteQuery;
begin
  dec(fExecuteQueryCount);
end;

function TBoldZeosConnection.GetIsSQLBased: Boolean;
begin
  Result := True;
end;

function TBoldZeosConnection.GetKeepConnection: Boolean;
begin
  Result := True;
end;

procedure TBoldZeosConnection.SetKeepConnection(NewValue: Boolean);
begin
end;

function TBoldZeosConnection.GetLogInPrompt: Boolean;
begin
  Result := fZConnection.LoginPrompt;
end;

procedure TBoldZeosConnection.SetLogInPrompt(NewValue: Boolean);
begin
  fZConnection.LoginPrompt := NewValue;
end;

function TBoldZeosConnection.GetConnected: Boolean;
begin
  Result := fZConnection.Connected;
end;

procedure TBoldZeosConnection.Open;
begin
  try
    fZConnection.Connect;
  except
    on E: Exception do
      raise GetDatabaseError(E);
  end;
end;

procedure TBoldZeosConnection.Close;
begin
  fExpectedTransaction := false;
  fZConnection.Disconnect;
end;

procedure TBoldZeosConnection.Reconnect;
begin
  fZConnection.Disconnect;
  fZConnection.Connect;
end;

function TBoldZeosConnection.SupportsTableCreation: Boolean;
begin
  Result := False;
end;

function TBoldZeosConnection.CreateAnotherDatabaseConnection: IBoldDatabase;
var
  Connection: TZConnection;
  NewDbConnection: TBoldZeosConnection;
begin
  Connection := TZConnection.Create(nil);
  Connection.Protocol := fZConnection.Protocol;
  Connection.HostName := fZConnection.HostName;
  Connection.Port := fZConnection.Port;
  Connection.Database := fZConnection.Database;
  Connection.User := fZConnection.User;
  Connection.Password := fZConnection.Password;
  Connection.LibraryLocation := fZConnection.LibraryLocation;
  Connection.Properties.Assign(fZConnection.Properties);
  Connection.LoginPrompt := fZConnection.LoginPrompt;
  Connection.TransactIsolationLevel := fZConnection.TransactIsolationLevel;
  NewDbConnection := TBoldZeosConnection.Create(Connection, SQLDatabaseConfig);
  NewDbConnection.fOwnsConnection := True;
  Result := NewDbConnection;
end;

function TBoldZeosConnection.GetDatabaseError(const E: Exception; const sSQL: string): EBoldDatabaseError;
var
  ErrorDetail: string;
begin
  ErrorDetail := '';
  if E is EZSQLException then
    ErrorDetail := Format('%s %d: %s', [E.ClassName, EZSQLException(E).ErrorCode, E.Message]);
  Result := InternalGetDatabaseError(bdetError, E,
    fZConnection.Protocol + '://' + fZConnection.HostName + '/' + fZConnection.Database,
    '', '', '', false, ErrorDetail);
end;

procedure TBoldZeosConnection.ExecuteScript(const Script: string);
var
  Processor: TZSQLProcessor;
begin
  Processor := TZSQLProcessor.Create(nil);
  try
    Processor.Connection := fZConnection;
    Processor.Script.Text := Script;
    Processor.Execute;
  finally
    Processor.Free;
  end;
end;

function TBoldZeosConnection.DatabaseExists: Boolean;
var
  Query: IBoldQuery;
  DatabaseName: string;
begin
  DatabaseName := fZConnection.Database;
  fZConnection.Disconnect;
  fZConnection.Database := ''; // connect to the server, not to the database
  Query := GetQuery;
  try
    Query.SQLText := SQLDataBaseConfig.GetDatabaseExistsQuery(DatabaseName);
    Query.Open;
    Result := not Query.Eof;
  finally
    ReleaseQuery(Query);
    fZConnection.Disconnect;
    fZConnection.Database := DatabaseName;
  end;
end;

procedure TBoldZeosConnection.CreateDatabase(DropExisting: Boolean);
var
  DatabaseName: string;
begin
  DatabaseName := LowerCase(fZConnection.Database);
  if DropExisting and DatabaseExists then
    DropDatabase;
  fZConnection.Database := ''; // connect to the server, not to the database
  try
    ExecuteScript(SQLDataBaseConfig.GetCreateDatabaseQuery(DatabaseName));
    fZConnection.Disconnect;
  finally
    fZConnection.Database := DatabaseName;
  end;
end;

procedure TBoldZeosConnection.DropDatabase;
var
  DatabaseName: string;
begin
  DatabaseName := LowerCase(fZConnection.Database);
  fZConnection.Database := ''; // connect to the server, not to the database
  try
    ExecuteScript(SQLDataBaseConfig.GetDropDatabaseQuery(DatabaseName));
    fZConnection.Disconnect;
  finally
    fZConnection.Database := DatabaseName;
  end;
end;

// Zeos datasets have no IndexDefs (TBoldZeosTable.GetIndexDefs), so without an
// IndexInfoTemplate the table path of the base class cannot read indexes. SQLite
// lists them in its PRAGMA functions (TBoldDatabaseWrapper.GetSQLiteIndexDescriptions).
function TBoldZeosConnection.GetIndexDescriptions(const TableName: String): TBoldIndexDescriptionArray;
begin
  if (SQLDatabaseConfig.IndexInfoTemplate = '') and StartsText('sqlite', fZConnection.Protocol) then
    Result := GetSQLiteIndexDescriptions(TableName)
  else
    Result := inherited GetIndexDescriptions(TableName);
end;

function TBoldZeosConnection.GetQuery: IBoldQuery;
begin
  if Assigned(fCachedQuery1) then
  begin
    Result := fCachedQuery1;
    fCachedQuery1 := nil;
  end
  else if Assigned(fCachedQuery2) then
  begin
    Result := fCachedQuery2;
    fCachedQuery2 := nil;
  end
  else
    Result := BoldZeosQueryClass.Create(Self);
end;

function TBoldZeosConnection.GetExecQuery: IBoldExecQuery;
begin
  if Assigned(fCachedExecQuery1) then
  begin
    Result := fCachedExecQuery1;
    fCachedExecQuery1 := nil;
  end
  else
    Result := BoldZeosQueryClass.Create(Self);
end;

function TBoldZeosConnection.GetTable: IBoldTable;
var
  ZTable: TZTable;
begin
  if Assigned(fCachedTable) then
  begin
    Result := fCachedTable;
    fCachedTable := nil;
  end
  else
  begin
    ZTable := TZTable.Create(nil);
    ZTable.Connection := fZConnection;
    Result := TBoldZeosTable.Create(ZTable, Self);
  end;
end;

{ Repairs the update counts of a query going back to the cache. Below zero no
  change notification ever fires again, so the cached query would silently
  execute stale SQL for its next user. }
procedure RepairUpdateCounts(Query: TBoldZeosQuery);
begin
  while BoldStringsUpdating(Query.SQLStrings) do
    Query.SQLStrings.EndUpdate;
  while TCollectionAccess(Query.Params).UpdateCount > 0 do
    Query.Params.EndUpdate;
  while TStringsAccess(Query.SQLStrings).UpdateCount < 0 do
    Query.SQLStrings.BeginUpdate;
  while TCollectionAccess(Query.Params).UpdateCount < 0 do
    Query.Params.BeginUpdate;
end;

procedure TBoldZeosConnection.ReleaseQuery(var Query: IBoldQuery);
var
  ZeosQuery: TBoldZeosQuery;
begin
  if Query.Implementor is TBoldZeosQuery then
  begin
    ZeosQuery := Query.Implementor as TBoldZeosQuery;
    ZeosQuery.Clear;
    RepairUpdateCounts(ZeosQuery);
    Query := nil;
    if not Assigned(fCachedQuery1) then
      fCachedQuery1 := ZeosQuery
    else if not Assigned(fCachedQuery2) then
      fCachedQuery2 := ZeosQuery
    else
      ZeosQuery.Free;
  end;
end;

procedure TBoldZeosConnection.ReleaseExecQuery(var Query: IBoldExecQuery);
var
  ZeosQuery: TBoldZeosQuery;
begin
  if Query.Implementor is TBoldZeosQuery then
  begin
    ZeosQuery := Query.Implementor as TBoldZeosQuery;
    if ZeosQuery.SQLStrings.Count <> 0 then
    begin
      ZeosQuery.SQLStrings.BeginUpdate;
      ZeosQuery.Clear;
    end;
    RepairUpdateCounts(ZeosQuery);
    Query := nil;
    if not Assigned(fCachedExecQuery1) then
      fCachedExecQuery1 := ZeosQuery
    else
      ZeosQuery.Free;
  end;
end;

procedure TBoldZeosConnection.ReleaseTable(var Table: IBoldTable);
var
  ZeosTable: TBoldZeosTable;
begin
  if Table.Implementor is TBoldZeosTable then
  begin
    ZeosTable := Table.Implementor as TBoldZeosTable;
    Table := nil;
    if not Assigned(fCachedTable) then
      fCachedTable := ZeosTable
    else
      ZeosTable.Free;
  end;
end;

procedure TBoldZeosConnection.ReleaseCachedObjects;
begin
  FreeAndNil(fCachedTable);
  FreeAndNil(fCachedQuery1);
  FreeAndNil(fCachedQuery2);
  FreeAndNil(fCachedExecQuery1);
end;

end.
