
{ Global compiler directives }
{$include bold.inc}
unit BoldClock;

interface

uses
  Classes,
  SyncObjs,
  Generics.Collections,
  BoldDefs;

type
  TBoldClock = class;

  { Internal to TBoldClock: one scheduled event. Public only because the
    clock's list needs the type. }
  TBoldClockAlarm = class
  private
    fClock: TBoldClock;
    fDueAt: TDateTime;
    fEvent: TBoldJustNotifyEvent;
    fQueued: Boolean;
    procedure Fire;
  public
    constructor Create(Clock: TBoldClock; DueAt: TDateTime; Event: TBoldJustNotifyEvent);
    property DueAt: TDateTime read fDueAt write fDueAt;
    property Event: TBoldJustNotifyEvent read fEvent;
    property Queued: Boolean read fQueued write fQueued;
  end;

  { Fires one-shot alarms on the main thread without a window handle or a
    message loop. A background thread hands each due alarm to the main thread
    with TThread.Queue, so the event runs on the next CheckSynchronize: VCL
    and FMX call that from their idle loops, a console application calls it
    itself. An event that wants to run again schedules itself again. }
  TBoldClock = class
  private
    fAlarms: TList<TBoldClockAlarm>;
    fLock: TCriticalSection;
    fWakeUp: TEvent;
    fThread: TThread;
    // FindAlarm, Dequeue and Remove expect the caller to hold fLock
    function FindAlarm(Event: TBoldJustNotifyEvent): TBoldClockAlarm;
    procedure Dequeue(Alarm: TBoldClockAlarm);
    procedure Remove(Alarm: TBoldClockAlarm);
    procedure EnsureThread;
    function QueueDueAlarms: Cardinal;
    procedure Deliver(Alarm: TBoldClockAlarm);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { Calls Event on the main thread once the wall clock has passed DueAt.
      Scheduling an event that is already scheduled replaces its due time. }
    procedure Schedule(DueAt: TDateTime; Event: TBoldJustNotifyEvent);
    procedure Cancel(Event: TBoldJustNotifyEvent);
    property Count: Integer read GetCount;
  end;

{ The clock shared by the application. nil once the unit has been finalized. }
function BoldSharedClock: TBoldClock;

implementation

uses
  SysUtils,
  DateUtils,
  Math;

type
  TBoldClockThread = class(TThread)
  private
    fClock: TBoldClock;
  protected
    procedure Execute; override;
  public
    constructor Create(Clock: TBoldClock);
  end;

const
  // Longest sleep of the clock thread: bounds how late an alarm fires after
  // the system clock jumps, for example on resume from sleep
  cMaxWaitMs = 60 * 1000;

var
  G_BoldClock: TBoldClock;
  G_BoldClockFinalized: Boolean;

function BoldSharedClock: TBoldClock;
begin
  if not Assigned(G_BoldClock) and not G_BoldClockFinalized then
  begin
    G_BoldClock := TBoldClock.Create;
  end;

  Result := G_BoldClock;
end;

function SameEvent(const Left, Right: TBoldJustNotifyEvent): Boolean;
begin
  Result := (TMethod(Left).Code = TMethod(Right).Code) and (TMethod(Left).Data = TMethod(Right).Data);
end;

{ TBoldClockAlarm }

constructor TBoldClockAlarm.Create(Clock: TBoldClock; DueAt: TDateTime; Event: TBoldJustNotifyEvent);
begin
  inherited Create;
  fClock := Clock;
  fDueAt := DueAt;
  fEvent := Event;
end;

procedure TBoldClockAlarm.Fire;
begin
  // Deliver frees this alarm, so nothing may follow the call
  fClock.Deliver(Self);
end;

{ TBoldClockThread }

constructor TBoldClockThread.Create(Clock: TBoldClock);
begin
  inherited Create(True);
  fClock := Clock;
end;

procedure TBoldClockThread.Execute;
begin
  while not Terminated do
  begin
    fClock.fWakeUp.WaitFor(fClock.QueueDueAlarms);
  end;
end;

{ TBoldClock }

constructor TBoldClock.Create;
begin
  inherited Create;
  fAlarms := TList<TBoldClockAlarm>.Create;
  fLock := TCriticalSection.Create;
  fWakeUp := TEvent.Create(nil, False, False, '');
end;

destructor TBoldClock.Destroy;
begin
  if Assigned(fThread) then
  begin
    fThread.Terminate;
    fWakeUp.SetEvent;
    fThread.WaitFor;
    FreeAndNil(fThread);
  end;

  if Assigned(fAlarms) then
  begin
    fLock.Enter;
    try
      while fAlarms.Count > 0 do
      begin
        Remove(fAlarms.Last);
      end;
    finally
      fLock.Leave;
    end;
  end;

  FreeAndNil(fAlarms);
  FreeAndNil(fWakeUp);
  FreeAndNil(fLock);
  inherited;
end;

procedure TBoldClock.Schedule(DueAt: TDateTime; Event: TBoldJustNotifyEvent);
var
  oAlarm: TBoldClockAlarm;
begin
  fLock.Enter;
  try
    oAlarm := FindAlarm(Event);

    if Assigned(oAlarm) then
    begin
      Dequeue(oAlarm);
      oAlarm.DueAt := DueAt;
    end
    else
    begin
      fAlarms.Add(TBoldClockAlarm.Create(Self, DueAt, Event));
    end;

    EnsureThread;
  finally
    fLock.Leave;
  end;

  fWakeUp.SetEvent;
end;

procedure TBoldClock.Cancel(Event: TBoldJustNotifyEvent);
var
  oAlarm: TBoldClockAlarm;
begin
  fLock.Enter;
  try
    oAlarm := FindAlarm(Event);

    if Assigned(oAlarm) then
    begin
      Remove(oAlarm);
    end;
  finally
    fLock.Leave;
  end;
end;

function TBoldClock.GetCount: Integer;
begin
  fLock.Enter;
  try
    Result := fAlarms.Count;
  finally
    fLock.Leave;
  end;
end;

function TBoldClock.FindAlarm(Event: TBoldJustNotifyEvent): TBoldClockAlarm;
var
  oAlarm: TBoldClockAlarm;
begin
  for oAlarm in fAlarms do
  begin
    if SameEvent(oAlarm.Event, Event) then
    begin
      Exit(oAlarm);
    end;
  end;

  Result := nil;
end;

procedure TBoldClock.Dequeue(Alarm: TBoldClockAlarm);
begin
  if Alarm.Queued then
  begin
    TThread.RemoveQueuedEvents(Alarm.Fire);
    Alarm.Queued := False;
  end;
end;

procedure TBoldClock.Remove(Alarm: TBoldClockAlarm);
begin
  Dequeue(Alarm);
  fAlarms.Remove(Alarm);
  Alarm.Free;
end;

procedure TBoldClock.EnsureThread;
begin
  if not Assigned(fThread) then
  begin
    fThread := TBoldClockThread.Create(Self);
    fThread.Start;
  end;
end;

// Runs on the clock thread. Hands every due alarm to the main thread and
// returns how long to sleep before the next alarm is due.
function TBoldClock.QueueDueAlarms: Cardinal;
var
  dNow: TDateTime;
  dNextDue: TDateTime;
  oAlarm: TBoldClockAlarm;
begin
  fLock.Enter;
  try
    dNow := Now;
    dNextDue := 0;

    for oAlarm in fAlarms do
    begin
      if oAlarm.Queued then
      begin
        Continue;
      end;

      if oAlarm.DueAt <= dNow then
      begin
        oAlarm.Queued := True;
        TThread.Queue(nil, oAlarm.Fire);
      end
      else if (dNextDue = 0) or (oAlarm.DueAt < dNextDue) then
      begin
        dNextDue := oAlarm.DueAt;
      end;
    end;
  finally
    fLock.Leave;
  end;

  if dNextDue = 0 then
  begin
    Result := cMaxWaitMs;
  end
  else
  begin
    Result := EnsureRange(MilliSecondsBetween(dNextDue, dNow) + 1, 1, cMaxWaitMs);
  end;
end;

// Runs on the main thread, from CheckSynchronize
procedure TBoldClock.Deliver(Alarm: TBoldClockAlarm);
var
  DueEvent: TBoldJustNotifyEvent;
begin
  fLock.Enter;
  try
    DueEvent := Alarm.Event;
    fAlarms.Remove(Alarm);
    Alarm.Free;
  finally
    fLock.Leave;
  end;

  DueEvent;
end;

initialization

finalization
  G_BoldClockFinalized := True;
  FreeAndNil(G_BoldClock);

end.
