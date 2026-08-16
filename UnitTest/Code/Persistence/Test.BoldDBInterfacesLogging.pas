unit Test.BoldDBInterfacesLogging;

{ Tests for the SQL log written by BoldLogSQL.

  Statements bind their values as parameters, so the statement text carries
  placeholders (:ID1) rather than literals. A log line built from SQLText alone
  therefore shows which statement ran but not which values were sent. }

interface

uses
  System.Classes,
  DUnitX.TestFramework,
  BoldLogHandler,
  BoldLogReceiverInterface;

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDBInterfacesLogging = class
  private
    fLoggedLines: TStringList;
    fReceiver: IBoldLogReceiver;
    fSavedLogHandler: TBoldLogHandler;
    function LoggedText: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [Category('Quick')]
    procedure QueryLogIncludesParameterValues;

    [Test]
    [Category('Quick')]
    procedure ExecQueryLogIncludesParameterValues;

    [Test]
    [Category('Quick')]
    procedure NullParameterIsLoggedAsNull;

    [Test]
    [Category('Quick')]
    procedure BlobParameterValueIsNotExpanded;

    [Test]
    [Category('Quick')]
    procedure QueryWithoutParametersLogsStatementOnly;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Rtti,
  Data.DB,
  Delphi.Mocks,
  BoldDefs,
  BoldDBInterfaces;

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

function MockParameter(const AName: string; ADataType: TFieldType;
  const AAsString: string; AIsNull: Boolean = False): TMock<IBoldParameter>;
begin
  Result := TMock<IBoldParameter>.Create;
  Result.Setup.WillReturn(AName).When.GetName;
  Result.Setup.WillReturn(AIsNull).When.GetIsNull;
  Result.Setup.WillReturn(TValue.From<TFieldType>(ADataType)).When.GetDataType;
  Result.Setup.WillReturn(AAsString).When.GetAsString;
end;

{ TTestBoldDBInterfacesLogging }

procedure TTestBoldDBInterfacesLogging.Setup;
begin
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
end;

function TTestBoldDBInterfacesLogging.LoggedText: string;
begin
  Result := fLoggedLines.Text;
end;

procedure TTestBoldDBInterfacesLogging.QueryLogIncludesParameterValues;
var
  Query: TMock<IBoldQuery>;
  Param: TMock<IBoldParameter>;
begin
  Param := MockParameter('ID1', ftInteger, '4711');

  Query := TMock<IBoldQuery>.Create;
  Query.Setup.WillReturn(cSelectById).When.GetSQLText;
  Query.Setup.WillReturn(1).When.GetParamCount;
  Query.Setup.WillReturn(TValue.From<IBoldParameter>(Param.Instance)).When.GetParam(0);

  BoldLogSQL(Query.Instance);

  Assert.Contains(LoggedText, ':ID1', 'The statement text must still be logged');
  Assert.Contains(LoggedText, '4711',
    'The bound parameter value must appear in the SQL log');
end;

procedure TTestBoldDBInterfacesLogging.ExecQueryLogIncludesParameterValues;
var
  Query: TMock<IBoldExecQuery>;
  Param: TMock<IBoldParameter>;
begin
  Param := MockParameter('NAME1', ftString, 'Volvo');

  Query := TMock<IBoldExecQuery>.Create;
  Query.Setup.WillReturn('UPDATE VEHICLE SET NAME = :NAME1').When.GetSQLText;
  Query.Setup.WillReturn(1).When.GetParamCount;
  Query.Setup.WillReturn(TValue.From<IBoldParameter>(Param.Instance)).When.GetParam(0);

  BoldLogSQL(Query.Instance);

  Assert.Contains(LoggedText, ':NAME1', 'The statement text must still be logged');
  Assert.Contains(LoggedText, 'Volvo',
    'The bound parameter value must appear in the SQL log');
end;

procedure TTestBoldDBInterfacesLogging.NullParameterIsLoggedAsNull;
var
  Query: TMock<IBoldQuery>;
  Param: TMock<IBoldParameter>;
begin
  Param := MockParameter('ID1', ftInteger, '', True);

  Query := TMock<IBoldQuery>.Create;
  Query.Setup.WillReturn(cSelectById).When.GetSQLText;
  Query.Setup.WillReturn(1).When.GetParamCount;
  Query.Setup.WillReturn(TValue.From<IBoldParameter>(Param.Instance)).When.GetParam(0);

  BoldLogSQL(Query.Instance);

  Assert.Contains(LoggedText, 'NULL',
    'An unbound parameter must be distinguishable from an empty string');
end;

procedure TTestBoldDBInterfacesLogging.BlobParameterValueIsNotExpanded;
const
  cBlobContent = 'BLOB-CONTENT-MUST-NOT-REACH-THE-LOG';
var
  Query: TMock<IBoldExecQuery>;
  Param: TMock<IBoldParameter>;
begin
  Param := MockParameter('DATA1', ftBlob, cBlobContent);

  Query := TMock<IBoldExecQuery>.Create;
  Query.Setup.WillReturn('UPDATE PICTURE SET DATA = :DATA1').When.GetSQLText;
  Query.Setup.WillReturn(1).When.GetParamCount;
  Query.Setup.WillReturn(TValue.From<IBoldParameter>(Param.Instance)).When.GetParam(0);

  BoldLogSQL(Query.Instance);

  Assert.Contains(LoggedText, 'DATA1', 'The parameter name must be logged');
  Assert.IsFalse(ContainsText(LoggedText, cBlobContent),
    'Blob payloads must not be expanded into the log');
end;

procedure TTestBoldDBInterfacesLogging.QueryWithoutParametersLogsStatementOnly;
var
  Query: TMock<IBoldQuery>;
begin
  Query := TMock<IBoldQuery>.Create;
  Query.Setup.WillReturn('SELECT COUNT(*) FROM BOLD_OBJECT').When.GetSQLText;
  Query.Setup.WillReturn(0).When.GetParamCount;

  BoldLogSQL(Query.Instance);

  Assert.AreEqual('SELECT COUNT(*) FROM BOLD_OBJECT', Trim(LoggedText),
    'A statement without parameters must be logged unchanged');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDBInterfacesLogging);

end.
