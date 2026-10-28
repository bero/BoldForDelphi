unit Test.BoldClock;

interface

uses
  BoldTestFramework,
  BoldClock;

type
  TTestBoldClock = class(TBoldTestCase)
  private
    fClock: TBoldClock;
    fFireCount: Integer;
    fOtherFireCount: Integer;
    fFiredOnMainThread: Boolean;
    procedure CountFire;
    procedure CountOtherFire;
    function WaitForFires(ExpectedCount, TimeoutMs: Integer): Boolean;
    procedure PumpFor(Milliseconds: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAlarmFiresOnMainThreadAfterDueTime;
    procedure TestOverdueAlarmFiresOnNextCheckSynchronize;
    procedure TestAlarmFiresOnce;
    procedure TestCancelledAlarmDoesNotFire;
    procedure TestCancelAfterDueTimeDoesNotFire;
    procedure TestRescheduleReplacesDueTime;
    procedure TestAlarmsAreIndependent;
    procedure TestDestroyWithPendingAlarmDoesNotFire;
    procedure TestGlobalClockIsShared;
  end;

implementation

uses
  Classes,
  SysUtils,
  DateUtils;

const
  cFireTimeoutMs = 2000;
  cShortDelayMs = 30;
  cSettleMs = 150;

procedure TTestBoldClock.SetUp;
begin
  fClock := TBoldClock.Create;
  fFireCount := 0;
  fOtherFireCount := 0;
  fFiredOnMainThread := False;
end;

procedure TTestBoldClock.TearDown;
begin
  FreeAndNil(fClock);
end;

procedure TTestBoldClock.CountFire;
begin
  Inc(fFireCount);
  fFiredOnMainThread := TThread.CurrentThread.ThreadID = MainThreadID;
end;

procedure TTestBoldClock.CountOtherFire;
begin
  Inc(fOtherFireCount);
end;

function TTestBoldClock.WaitForFires(ExpectedCount, TimeoutMs: Integer): Boolean;
var
  StartTicks: Int64;
begin
  StartTicks := BoldTestTickCount;

  while (fFireCount < ExpectedCount) and ((BoldTestTickCount - StartTicks) < TimeoutMs) do
  begin
    CheckSynchronize(20);
  end;

  Result := fFireCount >= ExpectedCount;
end;

procedure TTestBoldClock.PumpFor(Milliseconds: Integer);
var
  StartTicks: Int64;
begin
  StartTicks := BoldTestTickCount;

  while (BoldTestTickCount - StartTicks) < Milliseconds do
  begin
    CheckSynchronize(20);
  end;
end;

procedure TTestBoldClock.TestAlarmFiresOnMainThreadAfterDueTime;
begin
  fClock.Schedule(IncMilliSecond(Now, cShortDelayMs), CountFire);
  Assert.AreEqual(1, fClock.Count);
  Assert.IsTrue(WaitForFires(1, cFireTimeoutMs), 'Alarm did not fire');
  Assert.IsTrue(fFiredOnMainThread, 'Alarm fired on another thread');
  Assert.AreEqual(0, fClock.Count, 'A fired alarm is no longer scheduled');
end;

procedure TTestBoldClock.TestOverdueAlarmFiresOnNextCheckSynchronize;
begin
  fClock.Schedule(IncSecond(Now, -1), CountFire);
  Assert.IsTrue(WaitForFires(1, cFireTimeoutMs), 'Overdue alarm did not fire');
end;

procedure TTestBoldClock.TestAlarmFiresOnce;
begin
  fClock.Schedule(Now, CountFire);
  Assert.IsTrue(WaitForFires(1, cFireTimeoutMs), 'Alarm did not fire');
  PumpFor(cSettleMs);
  Assert.AreEqual(1, fFireCount, 'A one-shot alarm fired again');
end;

procedure TTestBoldClock.TestCancelledAlarmDoesNotFire;
begin
  fClock.Schedule(IncMilliSecond(Now, cShortDelayMs), CountFire);
  fClock.Cancel(CountFire);
  Assert.AreEqual(0, fClock.Count);
  PumpFor(cSettleMs);
  Assert.AreEqual(0, fFireCount, 'Cancelled alarm fired');
end;

procedure TTestBoldClock.TestCancelAfterDueTimeDoesNotFire;
begin
  // The alarm is due, and the clock thread has had time to hand it to the
  // main thread, but the main thread has not run it yet when Cancel is called
  fClock.Schedule(IncSecond(Now, -1), CountFire);
  Sleep(cSettleMs);
  fClock.Cancel(CountFire);
  PumpFor(cSettleMs);
  Assert.AreEqual(0, fFireCount, 'Alarm cancelled after its due time fired');
end;

procedure TTestBoldClock.TestRescheduleReplacesDueTime;
begin
  fClock.Schedule(IncSecond(Now, 60), CountFire);
  fClock.Schedule(IncMilliSecond(Now, cShortDelayMs), CountFire);
  Assert.AreEqual(1, fClock.Count, 'The same event is scheduled once');
  Assert.IsTrue(WaitForFires(1, cFireTimeoutMs), 'Rescheduled alarm did not fire at the new time');
end;

procedure TTestBoldClock.TestAlarmsAreIndependent;
begin
  fClock.Schedule(IncMilliSecond(Now, cShortDelayMs), CountFire);
  fClock.Schedule(IncSecond(Now, 60), CountOtherFire);
  Assert.IsTrue(WaitForFires(1, cFireTimeoutMs), 'The earlier alarm did not fire');
  PumpFor(cSettleMs);
  Assert.AreEqual(0, fOtherFireCount, 'The later alarm fired early');
  Assert.AreEqual(1, fClock.Count, 'The later alarm is still scheduled');
end;

procedure TTestBoldClock.TestDestroyWithPendingAlarmDoesNotFire;
begin
  fClock.Schedule(IncSecond(Now, -1), CountFire);
  FreeAndNil(fClock);
  PumpFor(cSettleMs);
  Assert.AreEqual(0, fFireCount, 'Alarm of a destroyed clock fired');
end;

procedure TTestBoldClock.TestGlobalClockIsShared;
begin
  Assert.IsNotNull(BoldSharedClock);
  Assert.AreSame(BoldSharedClock, BoldSharedClock);
end;

initialization
  BoldRegisterTest(TTestBoldClock);

end.
