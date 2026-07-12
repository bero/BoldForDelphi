unit Test.BoldDbValidator;

{ DUnitX tests for BoldDbValidator - remedy list thread safety (H8) }

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  DUnitX.TestFramework,
  BoldDbValidator;

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldDbValidator = class
  public
    [Test]
    [Category('Quick')]
    procedure TestAddRemedyThreadSafety;
  end;

implementation

type
  // CreateValidatorThread is abstract; the hammer test never spawns
  // validator threads, it drives AddRemedy from plain TThreads instead.
  TTestableDbValidator = class(TBoldDbValidator)
  protected
    function CreateValidatorThread: TBoldDbValidatorThread; override;
  end;

function TTestableDbValidator.CreateValidatorThread: TBoldDbValidatorThread;
begin
  result := nil;
end;

procedure TTestBoldDbValidator.TestAddRemedyThreadSafety;
const
  cThreads = 8;
  cAddsPerThread = 200000;
var
  Validator: TTestableDbValidator;
  Threads: array[0..cThreads - 1] of TThread;
  StartSignal: TEvent;
  i: Integer;
begin
  // Regression for H8 (0b52cef): up to ThreadCount validator threads add to
  // the shared remedy TList<String> with no synchronization. Concurrent Add
  // races lose entries or corrupt the list. With the lock in place the final
  // count is exact and deterministic. The start barrier is essential: without
  // it thread startup latency serializes the workloads and hides the race.
  Validator := TTestableDbValidator.Create(nil);
  StartSignal := TEvent.Create(nil, True, False, '');
  try
    for i := 0 to cThreads - 1 do
    begin
      Threads[i] := TThread.CreateAnonymousThread(
        procedure
        var
          j: Integer;
        begin
          StartSignal.WaitFor(INFINITE);
          for j := 1 to cAddsPerThread do
            Validator.AddRemedy('remedy');
        end);
      Threads[i].FreeOnTerminate := False;
    end;
    for i := 0 to cThreads - 1 do
      Threads[i].Start;
    StartSignal.SetEvent;  // release all threads at once
    for i := 0 to cThreads - 1 do
    begin
      Threads[i].WaitFor;
      Threads[i].Free;
    end;
    Assert.AreEqual(cThreads * cAddsPerThread, Validator.Remedy.Count,
      'concurrent AddRemedy must not lose entries');
  finally
    Validator.Free;
    StartSignal.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDbValidator);

end.
