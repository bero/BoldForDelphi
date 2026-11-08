unit Test.BoldUnloaderHandle;

{ TBoldUnloaderHandle drives its TBoldUnloader with a periodic tick. The tick
  must arrive in any application, also one without a window handle or a
  message loop, such as a console program or a service on Linux. These tests
  pump CheckSynchronize themselves, as such a program does. The unloader asks
  OnMayStart on every tick while it is active, so the tests count those
  requests and refuse each one, which keeps the unloader from scanning. }

interface

uses
  BoldTestFramework,
  BoldUnloaderHandle,
  jehoBoldTestDataModule;

type
  {$IFNDEF FPC}
  [Category('Handles')]
  {$ENDIF}
  TTestBoldUnloaderHandle = class(TBoldTestCase)
  private
    fDataModule: TjehodmBoldTest;
    fHandle: TBoldUnloaderHandle;
    fStartRequests: Integer;
    procedure RefuseStart(var aStart: Boolean);
    procedure CreateHandle(Active: Boolean);
    function PumpUntilStartRequests(Count: Integer): Boolean;
    procedure PumpFor(Milliseconds: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTickIntervalDefaultAndChange;
    procedure TestTicksWithoutMessageLoop;
    procedure TestInactiveHandleDoesNotTick;
    procedure TestDeactivatedSystemStopsTicking;
    procedure TestDestroyCancelsTick;
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldClock;

const
  cTestTickMs = 20;
  cTimeoutMs = 2000;
  cQuietMs = 200;

procedure TTestBoldUnloaderHandle.SetUp;
begin
  fStartRequests := 0;
  fDataModule := TjehodmBoldTest.Create(nil);
  fDataModule.BoldSystemHandle1.Active := True;
end;

procedure TTestBoldUnloaderHandle.TearDown;
begin
  FreeAndNil(fHandle);
  FreeAndNil(fDataModule);
end;

procedure TTestBoldUnloaderHandle.RefuseStart(var aStart: Boolean);
begin
  Inc(fStartRequests);
  aStart := False;
end;

procedure TTestBoldUnloaderHandle.CreateHandle(Active: Boolean);
begin
  fHandle := TBoldUnloaderHandle.Create(nil);
  fHandle.Active := Active;
  fHandle.TickInterval := cTestTickMs;
  fHandle.OnMayStart := RefuseStart;
  fHandle.StaticSystemHandle := fDataModule.BoldSystemHandle1;
  // Activating the unloader runs one Tick at once (TBoldUnloader.SetActive),
  // so an active handle has asked OnMayStart once before any alarm fired.
  // The tests count only the ticks that come from the clock.
  fStartRequests := 0;
end;

function TTestBoldUnloaderHandle.PumpUntilStartRequests(Count: Integer): Boolean;
var
  StartTicks: Int64;
begin
  StartTicks := BoldTestTickCount;
  while (fStartRequests < Count) and ((BoldTestTickCount - StartTicks) < cTimeoutMs) do
    CheckSynchronize(10);
  Result := fStartRequests >= Count;
end;

procedure TTestBoldUnloaderHandle.PumpFor(Milliseconds: Integer);
var
  StartTicks: Int64;
begin
  StartTicks := BoldTestTickCount;
  while (BoldTestTickCount - StartTicks) < Milliseconds do
    CheckSynchronize(10);
end;

procedure TTestBoldUnloaderHandle.TestTickIntervalDefaultAndChange;
begin
  fHandle := TBoldUnloaderHandle.Create(nil);
  Assert.AreEqual(cTickInterval, fHandle.TickInterval, 'default TickInterval');
  fHandle.TickInterval := 1234;
  Assert.AreEqual(1234, fHandle.TickInterval, 'TickInterval after a change');
end;

procedure TTestBoldUnloaderHandle.TestTicksWithoutMessageLoop;
begin
  CreateHandle(True);
  // The handle holds one alarm at a time, so the pump stops at exactly two
  // ticks; the second shows that each tick schedules the next one
  PumpUntilStartRequests(2);
  Assert.AreEqual(2, fStartRequests,
    'An active handle on an active system must tick repeatedly from CheckSynchronize (OnMayStart requests in 2 s)');
end;

procedure TTestBoldUnloaderHandle.TestInactiveHandleDoesNotTick;
begin
  CreateHandle(False);
  PumpFor(cQuietMs);
  Assert.AreEqual(0, fStartRequests, 'An inactive handle must not tick');
end;

procedure TTestBoldUnloaderHandle.TestDeactivatedSystemStopsTicking;
begin
  CreateHandle(True);
  Assert.IsTrue(PumpUntilStartRequests(1), 'The handle never ticked');
  fDataModule.BoldSystemHandle1.Active := False;
  fStartRequests := 0;
  PumpFor(cQuietMs);
  Assert.AreEqual(0, fStartRequests, 'The handle must not tick after its system was deactivated');
end;

procedure TTestBoldUnloaderHandle.TestDestroyCancelsTick;
var
  AlarmsBefore: Integer;
  RequestsBefore: Integer;
begin
  CreateHandle(True);
  Assert.IsTrue(PumpUntilStartRequests(1), 'The handle never ticked');
  // The tick has rescheduled itself, so the handle holds one alarm now
  AlarmsBefore := BoldSharedClock.Count;
  FreeAndNil(fHandle);
  Assert.AreEqual(AlarmsBefore - 1, BoldSharedClock.Count, 'Destroy must cancel the scheduled tick');
  RequestsBefore := fStartRequests;
  PumpFor(cQuietMs);
  Assert.AreEqual(RequestsBefore, fStartRequests, 'No tick may arrive after Destroy');
end;

initialization
  BoldRegisterTest(TTestBoldUnloaderHandle);

end.
