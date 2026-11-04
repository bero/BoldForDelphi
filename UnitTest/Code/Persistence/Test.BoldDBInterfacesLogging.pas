unit Test.BoldDBInterfacesLogging;

{ Tests for the SQL log written by BoldLogSQL.

  Statements bind their values as parameters, so the statement text carries
  placeholders (:ID1) rather than literals. A log line built from SQLText alone
  therefore shows which statement ran but not which values were sent.

  The queries are real ones from the runner's database (TBoldTestCaseDB:
  FireDAC under Delphi, Zeos under Free Pascal), with parameters parsed from
  the statement text. Nothing is executed: BoldLogSQL only reads the text and
  the parameters, so the tables in the statements need not exist. }

interface

uses
  Classes,
  BoldTestFramework,
  BoldLogHandler,
  BoldLogReceiverInterface,
  BoldDBInterfaces,
  BoldTestCaseDB;

type
  {$IFNDEF FPC}
  [Category('Persistence')]
  {$ENDIF}
  TTestBoldDBInterfacesLogging = class(TBoldTestCaseDB)
  private
    fLoggedLines: TStringList;
    fReceiver: IBoldLogReceiver;
    fSavedLogHandler: TBoldLogHandler;
    function LoggedText: string;
    function Database: IBoldDatabase;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure QueryLogIncludesParameterValues;
    procedure ExecQueryLogIncludesParameterValues;
    procedure NullParameterIsLoggedAsNull;
    procedure BlobParameterValueIsNotExpanded;
    procedure QueryWithoutParametersLogsStatementOnly;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  BoldDefs;

const
  cSelectById = 'SELECT BOLD_ID FROM BOLD_OBJECT WHERE BOLD_ID = :ID1';

type
  TCapturingLogReceiver = class(TBoldLogReceiver)
  private
    fTarget: TStrings;
  protected
    procedure Log(const s: string; LogType: TBoldLogType); override;
  public
    constructor Create(ATarget: TStrings);
  end;

constructor TCapturingLogReceiver.Create(ATarget: TStrings);
begin
  inherited Create;
  fTarget := ATarget;
end;

procedure TCapturingLogReceiver.Log(const s: string; LogType: TBoldLogType);
begin
  fTarget.Add(s);
end;

{ TTestBoldDBInterfacesLogging }

procedure TTestBoldDBInterfacesLogging.SetUp;
begin
  inherited; // the database and its system; their SQL is not captured
  fLoggedLines := TStringList.Create;
  fReceiver := TCapturingLogReceiver.Create(fLoggedLines);
  BoldLog.RegisterLogReceiver(fReceiver);
  fSavedLogHandler := BoldSQLLogHandler;
  BoldSQLLogHandler := BoldLog;
end;

procedure TTestBoldDBInterfacesLogging.TearDown;
begin
  BoldSQLLogHandler := fSavedLogHandler;
  BoldLog.UnregisterLogReceiver(fReceiver);
  fReceiver := nil;
  FreeAndNil(fLoggedLines);
  inherited;
end;

function TTestBoldDBInterfacesLogging.Database: IBoldDatabase;
begin
  Result := DatabaseAdapter.DatabaseInterface;
end;

function TTestBoldDBInterfacesLogging.LoggedText: string;
begin
  Result := fLoggedLines.Text;
end;

procedure TTestBoldDBInterfacesLogging.QueryLogIncludesParameterValues;
var
  Query: IBoldQuery;
begin
  Query := Database.GetQuery;
  try
    Query.AssignSQLText(cSelectById);
    Query.ParamByName('ID1').AsInteger := 4711;
    BoldLogSQL(Query);
  finally
    Database.ReleaseQuery(Query);
  end;

  Assert.Contains(LoggedText, ':ID1', 'The statement text must still be logged');
  Assert.Contains(LoggedText, '4711',
    'The bound parameter value must appear in the SQL log');
end;

procedure TTestBoldDBInterfacesLogging.ExecQueryLogIncludesParameterValues;
var
  Query: IBoldExecQuery;
begin
  Query := Database.GetExecQuery;
  try
    // The cached exec query keeps the ParamCheck of its last user: schema
    // creation turns it off, so set it before the text, as BoldDbCopy does.
    Query.ParamCheck := True;
    Query.AssignSQLText('UPDATE VEHICLE SET NAME = :NAME1');
    Query.ParamByName('NAME1').AsString := 'Volvo';
    BoldLogSQL(Query);
  finally
    Database.ReleaseExecQuery(Query);
  end;

  Assert.Contains(LoggedText, ':NAME1', 'The statement text must still be logged');
  Assert.Contains(LoggedText, 'Volvo',
    'The bound parameter value must appear in the SQL log');
end;

procedure TTestBoldDBInterfacesLogging.NullParameterIsLoggedAsNull;
var
  Query: IBoldQuery;
begin
  Query := Database.GetQuery;
  try
    Query.AssignSQLText(cSelectById); // ID1 is never given a value
    Assert.AreEqual(1, Query.ParamCount, 'precondition: the marker became a parameter');
    BoldLogSQL(Query);
  finally
    Database.ReleaseQuery(Query);
  end;

  Assert.Contains(LoggedText, 'NULL',
    'An unbound parameter must be distinguishable from an empty string');
end;

procedure TTestBoldDBInterfacesLogging.BlobParameterValueIsNotExpanded;
const
  cBlobContent = 'BLOB-CONTENT-MUST-NOT-REACH-THE-LOG';
var
  Query: IBoldExecQuery;
begin
  Query := Database.GetExecQuery;
  try
    Query.ParamCheck := True; // see ExecQueryLogIncludesParameterValues
    Query.AssignSQLText('UPDATE PICTURE SET DATA = :DATA1');
    Query.ParamByName('DATA1').AsBlob := cBlobContent;
    BoldLogSQL(Query);
  finally
    Database.ReleaseExecQuery(Query);
  end;

  Assert.Contains(LoggedText, 'DATA1', 'The parameter name must be logged');
  Assert.IsFalse(ContainsText(LoggedText, cBlobContent),
    'Blob payloads must not be expanded into the log');
end;

procedure TTestBoldDBInterfacesLogging.QueryWithoutParametersLogsStatementOnly;
var
  Query: IBoldQuery;
begin
  Query := Database.GetQuery;
  try
    Query.AssignSQLText('SELECT COUNT(*) FROM BOLD_OBJECT');
    BoldLogSQL(Query);
  finally
    Database.ReleaseQuery(Query);
  end;

  Assert.AreEqual('SELECT COUNT(*) FROM BOLD_OBJECT', Trim(LoggedText),
    'A statement without parameters must be logged unchanged');
end;

initialization
  BoldRegisterTest(TTestBoldDBInterfacesLogging);

end.
