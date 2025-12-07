unit BoldTestCase;

{ DUnitX-compatible Bold Test Case Framework

  Usage notes:
  * All test cases must be subclasses of TBoldTestCase
  * Use [TestFixture] and [Test] attributes to mark test classes and methods
  * Override SetUp and TearDown for test initialization/cleanup
  * Use Assert class for test assertions (Assert.AreEqual, Assert.IsTrue, etc.)
  * Register test fixtures in initialization section using TDUnitX.RegisterTestFixture

  Migration from DUnit:
  * Replace 'TestFramework' with 'DUnitX.TestFramework' in uses
  * Replace Check* calls with Assert.* calls
  * Add [TestFixture] attribute to test classes
  * Add [Test] attribute to test methods
  * Remove Suit method - DUnitX uses RTTI to find [Test] methods
}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes;

type
  { Class types }
  TBoldTestCase = class;
  TBoldTestCaseClass = class of TBoldTestCase;

  { method types }
  TNotifyFault = procedure(TestCase: TBoldTestCase) of object;

  { enumerations }
  TResultCode = (etNotFound, etAllOK, etFailure, etError);

  /// <summary>
  /// Base class for Bold test cases using DUnitX.
  /// Provides common setup/teardown functionality for Bold framework testing.
  /// </summary>
  [TestFixture]
  TBoldTestCase = class
  private
    FComment: string;
    FTestResult: TResultCode;
    FTestMessage: string;
  protected
    function GetComment: string; virtual;
    procedure SetComment(const Value: string); virtual;
    function GetTestResult: TResultCode; virtual;
    function GetTestMessage: string; virtual;
    procedure SetTestMessage(const Value: string); virtual;

    // Override these in subclasses for custom setup/teardown
    procedure SetUp; virtual;
    procedure TearDown; virtual;
  public
    constructor Create; virtual;

    [Setup]
    procedure TestSetUp;
    [TearDown]
    procedure TestTearDown;

    property Comment: string read GetComment write SetComment;
    property TestResult: TResultCode read GetTestResult;
    property TestMessage: string read GetTestMessage write SetTestMessage;
  end;

  { TBoldTestSuite - Collection of test cases (for compatibility) }
  TBoldTestSuite = class
  private
    FTests: TList;
    FRuns: Integer;
    FErrors: Integer;
    FFailures: Integer;
    FNotFound: Integer;
    FOnFault: TNotifyFault;
    function GetProblems: Integer;
    function GetTestCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTest(Test: TBoldTestCase);
    procedure Clear;

    property Runs: Integer read FRuns;
    property Errors: Integer read FErrors;
    property Failures: Integer read FFailures;
    property NotFound: Integer read FNotFound;
    property ProblemCount: Integer read GetProblems;
    property TestCount: Integer read GetTestCount;
    property OnFault: TNotifyFault read FOnFault write FOnFault;
  end;

const
  ErrorMsg: array[TResultCode] of string = ('Not Found', 'OK', 'Failure', 'Error');

implementation

{ TBoldTestCase }

constructor TBoldTestCase.Create;
begin
  inherited Create;
  FTestResult := etAllOK;
  FComment := '';
  FTestMessage := '';
end;

procedure TBoldTestCase.SetUp;
begin
  // Override in descendants for custom setup
end;

procedure TBoldTestCase.TearDown;
begin
  // Override in descendants for custom teardown
end;

procedure TBoldTestCase.TestSetUp;
begin
  FTestResult := etAllOK;
  FTestMessage := '';
  SetUp;
end;

procedure TBoldTestCase.TestTearDown;
begin
  TearDown;
end;

function TBoldTestCase.GetComment: string;
begin
  Result := FComment;
end;

procedure TBoldTestCase.SetComment(const Value: string);
begin
  FComment := Value;
end;

function TBoldTestCase.GetTestResult: TResultCode;
begin
  Result := FTestResult;
end;

function TBoldTestCase.GetTestMessage: string;
begin
  Result := FTestMessage;
end;

procedure TBoldTestCase.SetTestMessage(const Value: string);
begin
  FTestMessage := Value;
end;

{ TBoldTestSuite }

constructor TBoldTestSuite.Create;
begin
  inherited Create;
  FTests := TList.Create;
  FRuns := 0;
  FErrors := 0;
  FFailures := 0;
  FNotFound := 0;
end;

destructor TBoldTestSuite.Destroy;
begin
  Clear;
  FTests.Free;
  inherited Destroy;
end;

procedure TBoldTestSuite.AddTest(Test: TBoldTestCase);
begin
  FTests.Add(Test);
end;

procedure TBoldTestSuite.Clear;
var
  I: Integer;
begin
  for I := 0 to FTests.Count - 1 do
    TObject(FTests[I]).Free;
  FTests.Clear;
end;

function TBoldTestSuite.GetProblems: Integer;
begin
  Result := FErrors + FFailures + FNotFound;
end;

function TBoldTestSuite.GetTestCount: Integer;
begin
  Result := FTests.Count;
end;

end.
