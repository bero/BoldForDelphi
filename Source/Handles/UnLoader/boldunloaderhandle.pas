/////////////////////////////////////////////////////////


unit BoldUnloaderHandle;

interface

uses
  BoldDefs,
  BoldBase,
  BoldUnloader,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle,
  BoldHandles,
  Classes;

const
  cTickInterval = 60 * 1000; // 1 minute
  cScanPerTick = 100; // milliseconds, max time to spend scaning
  cMinAgeForUnload = 5; // unit is the TickInterval so (cTickInterval * cMinAgeForUnload) = 5 minutes

type
  { Forward declaration of classes }
  TBoldUnloaderHandle = class;

  {$IFNDEF FPC}[ComponentPlatformsAttribute (pidWin32 or pidWin64)]{$ENDIF}
  TBoldUnloaderHandle = class(TBoldSystemExtensionComponent)
  private
    fActive: Boolean;
    fUnloader: TBoldUnloader;
    fTickInterval: Integer;
    fTickScheduled: Boolean;
    function GetUnloadDelayedFetch: boolean;
    function GetMinAgeForUnload: integer;
    function GetScanPerTick: integer;
    procedure SetActive(const Value: boolean);
    procedure SetUnloadDelayedFetch(const Value: boolean);
    procedure SetMinAgeForUnload(const Value: integer);
    procedure SetScanPerTick(const Value: integer);
    function GetOnMayStart: TBoldMayUnloadStartEvent;
    procedure SetOnMayStart(const Value: TBoldMayUnloadStartEvent);
    function GetOnReportUnload: TBoldReportUnloadEvent;
    procedure SetOnReportUnload(const Value: TBoldReportUnloadEvent);
    function GetUnloadFromCurrentClassList: boolean;
    procedure SetUnloadFromCurrentClassList(const Value: boolean);
    property Unloader: TBoldUnloader read fUnloader;
    procedure Tick;
    procedure ScheduleTick;
    procedure CancelTick;
    procedure PropagateToUnloder;
    function GetTickInterval: integer;
    procedure SetTickInterval(const Value: integer);
    function GetOnMayInvalidate: TBoldInvalidateMemberEvent;
    function GetOnMayUnload: TBoldUnloadObjectEvent;
    procedure SetOnMayInvalidate(const Value: TBoldInvalidateMemberEvent);
    procedure SetOnMayUnload(const Value: TBoldUnloadObjectEvent);
  protected
    procedure StaticBoldTypeChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TickInterval: integer read GetTickInterval write SetTickInterval default cTickInterval;
    property ScanPerTick: integer read GetScanPerTick write SetScanPerTick default cScanPerTick;
    property MinAgeForUnload: integer read GetMinAgeForUnload write SetMinAgeForUnload default cMinAgeForUnload;
    property UnloadDelayedFetch: boolean read GetUnloadDelayedFetch write SetUnloadDelayedFetch default false;
    property UnloadFromCurrentClassList: boolean read GetUnloadFromCurrentClassList write SetUnloadFromCurrentClassList default false;
    property Active: boolean read fActive write SetActive default true;
    property OnMayInvalidate: TBoldInvalidateMemberEvent read GetOnMayInvalidate write SetOnMayInvalidate;
    property OnMayUnload: TBoldUnloadObjectEvent read GetOnMayUnload write SetOnMayUnload;
    property OnMayStart: TBoldMayUnloadStartEvent read GetOnMayStart write SetOnMayStart;
    property OnReportUnload: TBoldReportUnloadEvent read GetOnReportUnload write SetOnReportUnload;        
  end;


implementation

uses
  SysUtils,
  DateUtils,
  BoldClock,
  BoldUtils;
const
  breHandleDestroyed = 42;
  breSystemChanged = 43;

{ TBoldUnloaderHandle }

constructor TBoldUnloaderHandle.Create(AOwner: TComponent);
begin
  inherited;
  fUnLoader := TBoldUnloader.Create;
  TickInterval := cTickInterval;
  ScanPertick := cScanPertick;
  MinAgeForUnload := cMinAgeForUnload;
  Active := True;
end;

destructor TBoldUnloaderHandle.Destroy;
begin
  CancelTick;
  FreeAndNil(fUnLoader);
  inherited;
end;

function TBoldUnloaderHandle.GetUnloadDelayedFetch: boolean;
begin
  Result := Unloader.UnloadDelayedFetch;
end;

function TBoldUnloaderHandle.GetUnloadFromCurrentClassList: boolean;
begin
  result := Unloader.UnloadFromCurrentClassList;
end;

function TBoldUnloaderHandle.GetMinAgeForUnload: integer;
begin
  Result := Unloader.MinAgeForUnload;
end;

function TBoldUnloaderHandle.GetScanPerTick: integer;
begin
  Result := Unloader.ScanPerTick;
end;

function TBoldUnloaderHandle.GetTickInterval: integer;
begin
  Result := fTickInterval;
end;

procedure TBoldUnloaderHandle.PropagateToUnloder;
begin
  if Active then
  begin
    if Assigned(StaticSystemHandle) then
      fUnloader.BoldSystem := StaticSystemHandle.System
    else
     fUnloader.BoldSystem := nil;
    fUnloader.Active := Assigned(fUnloader.BoldSystem);
  end
  else
    fUnLoader.Active := False;
  ScheduleTick;
end;

procedure TBoldUnloaderHandle.SetActive(const Value: boolean);
begin
  if (fActive <> Value) then
  begin
    fActive := Value;
    PropagateToUnloder;
  end;
end;

procedure TBoldUnloaderHandle.SetUnloadDelayedFetch(
  const Value: boolean);
begin
  Unloader.UnloadDelayedFetch := Value;
end;

procedure TBoldUnloaderHandle.SetUnloadFromCurrentClassList(
  const Value: boolean);
begin
  Unloader.UnloadFromCurrentClassList := Value;
end;

procedure TBoldUnloaderHandle.StaticBoldTypeChanged;
begin
  inherited;
  PropagateToUnloder;
end;

procedure TBoldUnloaderHandle.SetMinAgeForUnload(
  const Value: integer);
begin
  Unloader.MinAgeForUnload := Value;
end;

procedure TBoldUnloaderHandle.SetScanPerTick(const Value: integer);
begin
  Unloader.ScanPerTick := Value;
end;

procedure TBoldUnloaderHandle.SetTickInterval(const Value: integer);
begin
  fTickInterval := Value;
  ScheduleTick;
end;

// The tick comes from BoldSharedClock, not from a VCL TTimer, so it arrives in
// every application that calls CheckSynchronize: VCL and FMX from their idle
// loops, a console program or service itself. The handle schedules a tick only
// while its unloader is active.
procedure TBoldUnloaderHandle.ScheduleTick;
var
  Clock: TBoldClock;
begin
  Clock := BoldSharedClock; // nil once BoldClock is finalized
  if Assigned(fUnloader) and fUnloader.Active and (fTickInterval > 0) and Assigned(Clock) then
  begin
    Clock.Schedule(IncMilliSecond(Now, fTickInterval), Tick);
    fTickScheduled := True;
  end
  else
    CancelTick;
end;

procedure TBoldUnloaderHandle.CancelTick;
var
  Clock: TBoldClock;
begin
  if fTickScheduled then
  begin
    fTickScheduled := False;
    Clock := BoldSharedClock;
    if Assigned(Clock) then
      Clock.Cancel(Tick);
  end;
end;

procedure TBoldUnloaderHandle.Tick;
begin
  // The alarm has fired. Schedule the next one first, so that an exception in
  // the unloader does not end the ticking, as it did not with the timer.
  fTickScheduled := False;
  ScheduleTick;
  UnLoader.Tick;
end;

function TBoldUnloaderHandle.GetOnMayInvalidate: TBoldInvalidateMemberEvent;
begin
  Result := Unloader.OnMayInvalidate;
end;

function TBoldUnloaderHandle.GetOnMayStart: TBoldMayUnloadStartEvent;
begin
  Result := Unloader.OnMayStart;
end;

function TBoldUnloaderHandle.GetOnMayUnload: TBoldUnloadObjectEvent;
begin
  Result := Unloader.OnMayUnload;
end;

function TBoldUnloaderHandle.GetOnReportUnload: TBoldReportUnloadEvent;
begin
  result := Unloader.OnReportUnload;
end;

procedure TBoldUnloaderHandle.SetOnMayInvalidate(
  const Value: TBoldInvalidateMemberEvent);
begin
  Unloader.OnMayInvalidate := Value;
end;

procedure TBoldUnloaderHandle.SetOnMayStart(const Value: TBoldMayUnloadStartEvent);
begin
  Unloader.OnMayStart := Value;
end;

procedure TBoldUnloaderHandle.SetOnMayUnload(
  const Value: TBoldUnloadObjectEvent);
begin
  Unloader.OnMayUnload := Value;
end;

procedure TBoldUnloaderHandle.SetOnReportUnload(
  const Value: TBoldReportUnloadEvent);
begin
  Unloader.OnReportUnload := Value;
end;

end.

