unit Test.BoldLogReceiverInterface;

interface

uses
  DUnitX.TestFramework,
  BoldLogReceiverInterface;

type
  /// <summary>
  /// Test fixture for BoldLogReceiverInterface - Log receiver base class
  /// </summary>
  [TestFixture]
  [Category('Common')]
  TTestBoldLogReceiverInterface = class
  public
    [Test]
    [Category('Quick')]
    procedure TestLogEnabledDefault;
    [Test]
    procedure TestSetLogEnabled;
    [Test]
    procedure TestLogTypeToString_Info;
    [Test]
    procedure TestLogTypeToString_Detail;
    [Test]
    procedure TestLogTypeToString_Warning;
    [Test]
    procedure TestLogTypeToString_Error;
    [Test]
    procedure TestLogTypeToString_Separator;
    [Test]
    procedure TestVirtualMethodsDoNotRaise;
    [Test]
    procedure TestInterfaceImplementation;
  end;

implementation

uses
  System.SysUtils,
  BoldDefs;

{ TTestBoldLogReceiverInterface }

procedure TTestBoldLogReceiverInterface.TestLogEnabledDefault;
var
  Receiver: TBoldLogReceiver;
begin
  Receiver := TBoldLogReceiver.Create;
  try
    // Default value should be False (uninitialized boolean)
    Assert.IsFalse(Receiver.LogEnabled, 'LogEnabled should default to False');
  finally
    Receiver.Free;
  end;
end;

procedure TTestBoldLogReceiverInterface.TestSetLogEnabled;
var
  Receiver: TBoldLogReceiver;
begin
  Receiver := TBoldLogReceiver.Create;
  try
    Receiver.LogEnabled := True;
    Assert.IsTrue(Receiver.LogEnabled, 'LogEnabled should be True');

    Receiver.LogEnabled := False;
    Assert.IsFalse(Receiver.LogEnabled, 'LogEnabled should be False');
  finally
    Receiver.Free;
  end;
end;

procedure TTestBoldLogReceiverInterface.TestLogTypeToString_Info;
var
  Receiver: TBoldLogReceiver;
begin
  Receiver := TBoldLogReceiver.Create;
  try
    Assert.AreEqual('Info', Receiver.LogTypeToString(ltInfo));
  finally
    Receiver.Free;
  end;
end;

procedure TTestBoldLogReceiverInterface.TestLogTypeToString_Detail;
var
  Receiver: TBoldLogReceiver;
begin
  Receiver := TBoldLogReceiver.Create;
  try
    Assert.AreEqual('Detail', Receiver.LogTypeToString(ltDetail));
  finally
    Receiver.Free;
  end;
end;

procedure TTestBoldLogReceiverInterface.TestLogTypeToString_Warning;
var
  Receiver: TBoldLogReceiver;
begin
  Receiver := TBoldLogReceiver.Create;
  try
    Assert.AreEqual('Warning', Receiver.LogTypeToString(ltWarning));
  finally
    Receiver.Free;
  end;
end;

procedure TTestBoldLogReceiverInterface.TestLogTypeToString_Error;
var
  Receiver: TBoldLogReceiver;
begin
  Receiver := TBoldLogReceiver.Create;
  try
    Assert.AreEqual('Error', Receiver.LogTypeToString(ltError));
  finally
    Receiver.Free;
  end;
end;

procedure TTestBoldLogReceiverInterface.TestLogTypeToString_Separator;
var
  Receiver: TBoldLogReceiver;
begin
  Receiver := TBoldLogReceiver.Create;
  try
    Assert.AreEqual(BOLDCRLF, Receiver.LogTypeToString(ltSeparator));
  finally
    Receiver.Free;
  end;
end;

procedure TTestBoldLogReceiverInterface.TestVirtualMethodsDoNotRaise;
var
  LogReceiver: IBoldLogReceiver;
begin
  // All virtual methods are no-ops and should not raise
  // Use interface to access protected methods
  LogReceiver := TBoldLogReceiver.Create;
  LogReceiver.Clear;
  LogReceiver.Hide;
  LogReceiver.Show;
  LogReceiver.Sync;
  LogReceiver.ProgressStep;
  LogReceiver.ProcessInterruption;
  LogReceiver.StartLog('TestSession');
  LogReceiver.EndLog;
  LogReceiver.Log('Test message', ltInfo);
  Assert.Pass('All virtual methods executed without raising');
end;

procedure TTestBoldLogReceiverInterface.TestInterfaceImplementation;
var
  Receiver: TBoldLogReceiver;
  LogReceiver: IBoldLogReceiver;
begin
  Receiver := TBoldLogReceiver.Create;
  // Get interface - this also tests that the class properly implements IBoldLogReceiver
  if Supports(Receiver, IBoldLogReceiver, LogReceiver) then
  begin
    LogReceiver.LogEnabled := True;
    Assert.IsTrue(LogReceiver.LogEnabled, 'Interface should work correctly');
    LogReceiver.ProgressMax := 100;
    LogReceiver.Progress := 50;
    LogReceiver.LogHeader := 'Test Header';
    LogReceiver.Log('Test', ltInfo);
    Assert.Pass('Interface implementation works');
  end
  else
    Assert.Fail('TBoldLogReceiver should implement IBoldLogReceiver');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldLogReceiverInterface);

end.
