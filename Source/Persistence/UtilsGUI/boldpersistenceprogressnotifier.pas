
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceProgressNotifier;

{ TBoldPersistenceProgressNotifier shows the progress of fetches and updates in
  VCL controls: a progress bar, a label or a control's caption, and an
  animation. It lives apart from BoldPersistenceNotifier, so that the
  persistence core imports no VCL unit; a VCL form that holds this component
  uses both units. }

interface

uses
  Classes,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  BoldSubscription,
  BoldPersistenceNotifier;

type
  { TBoldPersistenceProgressNotifier }
  {$IFNDEF FPC}[ComponentPlatformsAttribute (pidWin32 or pidWin64)]{$ENDIF}
  TBoldPersistenceProgressNotifier = class(TBoldAbstractPersistenceNotifier)
  private
    { Private declarations }
    fLastUpdate: TDateTime;
    FAnimation: TAnimate;
    fAnimationTimer: TTimer;
    FWinControl: TWinControl;
    fMsgLabel: TLabel;
    FProgressBar: TProgressBar;
    fMsgFetchObjects: String;
    fMsgRetrieveIds: String;
    fMsgUpdateDatabase: String;
    procedure AnimationTurnOff(Sender: TObject);
    procedure SetProgressBar(const Value: TProgressBar);
    procedure SetAnimation(const Value: TAnimate);
    procedure SetWinControl(const Value: TWinControl);
    procedure SetMsgLabel(const Value: TLabel);
    function GetAnimationInterval: integer;
    procedure SetAnimationInterval(const Value: integer);
    function GetAnimationTimer: TTimer;
  protected
    { Protected declarations }
    procedure EndEvent;
    procedure StartEvent(Count: integer);
    procedure StepProgress;
    procedure StepAnimation;
    procedure SetMessage(const s: String);
    procedure UpdateProgressBar;
    procedure EndFetch(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure EndUpdate(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure EndFetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure StartUpdate(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure StartFetch(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure UpdateObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure DeleteObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure CreateObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure FetchObject(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure FetchMember(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure StartFetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure FetchID(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property AnimationTimer: TTimer read GetAnimationTimer;    
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
  published
    { Published declarations }
    property Animation: TAnimate read FAnimation write SetAnimation;
    property WinControl: TWinControl read FWinControl write SetWinControl;
    property MsgLabel: TLabel read fMsgLabel write SetMsgLabel;
    property ProgressBar: TProgressBar read FProgressBar write SetProgressBar;
    property AnimationInterval: integer read GetAnimationInterval write SetAnimationInterval;
    property PersistenceHandle;
    property MsgFetchObjects: string read fMsgFetchObjects write fMsgFetchObjects;
    property MsgRetrieveIds: string read fMsgRetrieveIds write fMsgRetrieveIds;
    property MsgUpdateDatabase: string read fMsgUpdateDatabase write fMsgUpdateDatabase;
    property OnProgressStart;
    property OnProgressEnd;
    property OnAlertExcessiveFetch;
  end;

implementation

uses
  SysUtils,
  BoldId;

const
  c100ms = 1/24/60/60/10;

{ TBoldPersistenceProgressNotifier }

procedure TBoldPersistenceProgressNotifier.AnimationTurnOff(Sender: TObject);
begin
  if assigned(Animation) then
  begin
    Animation.Stop;
    AnimationTimer.Enabled := false;
  end;
end;

constructor TBoldPersistenceProgressNotifier.create(owner: TComponent);
begin
  inherited;
  fMsgFetchObjects := 'Loading %d objects';
  fMsgRetrieveIds := 'Loading IDs';
  fMsgUpdateDatabase := 'Saving %d objects';
end;

procedure TBoldPersistenceProgressNotifier.CreateObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  inherited;
  StepProgress;
end;

procedure TBoldPersistenceProgressNotifier.DeleteObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  inherited;
  StepProgress;
end;

procedure TBoldPersistenceProgressNotifier.EndEvent;
begin
  if assigned(ProgressBar) then
  begin
    ProgressBar.Visible := false;
    ProgressBar.Max := 0;
    UpdateProgressBar;
  end;
  if assigned(Animation) then
  begin
    AnimationTimer.Enabled := true;
    Animation.Refresh;
  end;
  SetMessage('');
end;

procedure TBoldPersistenceProgressNotifier.EndFetch(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  EndEvent;
end;

procedure TBoldPersistenceProgressNotifier.EndFetchID(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  EndEvent;
end;

procedure TBoldPersistenceProgressNotifier.EndUpdate(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  EndEvent;
end;

procedure TBoldPersistenceProgressNotifier.FetchID(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
end;

procedure TBoldPersistenceProgressNotifier.FetchMember(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  StepAnimation;
end;

procedure TBoldPersistenceProgressNotifier.FetchObject(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  StepProgress;
end;

function TBoldPersistenceProgressNotifier.GetAnimationInterval: integer;
begin
  result := AnimationTimer.Interval;
end;

function TBoldPersistenceProgressNotifier.GetAnimationTimer: TTimer;
begin
  if not Assigned(fAnimationTimer) then
  begin
    fAnimationTimer := TTimer.Create(self);
    fAnimationTimer.OnTimer := AnimationTurnOff;
    fAnimationTimer.Interval := 100;
  end;
  result := fAnimationTimer;
end;

{ Handle removing of non-bold components }
procedure TBoldPersistenceProgressNotifier.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = ProgressBar then
      ProgressBar := nil;
    if AComponent = Animation then
      Animation := nil;
    if AComponent = WinControl then
      WinControl := nil;
    if AComponent = MsgLabel then
      MsgLabel := nil;
  end;
end;

procedure TBoldPersistenceProgressNotifier.SetAnimation(
  const Value: TAnimate);
begin
  FAnimation := Value;
  if Assigned(fAnimation) then
    fAnimation.FreeNotification(Self);
end;

procedure TBoldPersistenceProgressNotifier.SetAnimationInterval(
  const Value: integer);
begin
  AnimationTimer.Interval := Value;
end;

type
  TExposedWinControl = class(TWinControl);

procedure TBoldPersistenceProgressNotifier.SetMessage(const s: String);
begin
  if assigned(WinControl) then
  begin
    TExposedWinControl(WinControl).Caption := s;
    WinControl.Refresh;
  end;
  if assigned(MsgLabel) then
  begin
    MsgLabel.Caption := s;
    MsgLabel.Refresh;
  end;
end;

procedure TBoldPersistenceProgressNotifier.SetMsgLabel(
  const Value: TLabel);
begin
  fMsgLabel := Value;
  if Assigned(fMsgLabel) then
    fMsgLabel.FreeNotification(Self);
end;

procedure TBoldPersistenceProgressNotifier.SetProgressBar(
  const Value: TProgressBar);
begin
  FProgressBar := Value;
  if Assigned(fProgressBar) then
  begin
    fProgressBar.FreeNotification(self);
    FProgressBar.Step := 1;
  end;
end;

procedure TBoldPersistenceProgressNotifier.SetWinControl(
  const Value: TWinControl);
begin
  FWinControl := Value;
  if Assigned(fWinControl) then
    fWinControl.FreeNotification(Self);
end;

procedure TBoldPersistenceProgressNotifier.StartEvent(Count: integer);
begin
  if Assigned(ProgressBar) then
  begin
    ProgressBar.Visible := true;
    ProgressBar.Position := 0;
    ProgressBar.Max := Count;
    UpdateProgressBar;
  end;
  if Assigned(Animation) then
  begin
    AnimationTimer.Enabled := False;
    if not Animation.Active and (Animation.FrameCount > 0) then
      Animation.Play(0, Animation.FrameCount - 1, 0);
    Animation.Refresh;
  end;
end;

procedure TBoldPersistenceProgressNotifier.StartFetch(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  StartEvent((Args[0].VObject as TBoldObjectIdList).Count);
  SetMessage(MsgFetchObjects);
end;

procedure TBoldPersistenceProgressNotifier.StartFetchID(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  inherited;
  StepAnimation;
  SetMessage(MsgRetrieveIDs);
end;

procedure TBoldPersistenceProgressNotifier.StartUpdate(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  inherited;
  StartEvent((Args[0].VObject as TBoldObjectIdList).Count);
  SetMessage(MsgUpdateDatabase);
end;

procedure TBoldPersistenceProgressNotifier.StepAnimation;
begin
  if assigned(Animation) then
  begin
    AnimationTimer.Enabled := false;
    if (Animation.FrameCount > 0) then
      Animation.Play(0, Animation.FrameCount - 1, 0);
    Animation.Refresh;
  end;
end;

procedure TBoldPersistenceProgressNotifier.StepProgress;
begin
  if assigned(ProgressBar) then
  begin
    ProgressBar.StepIt;
    UpdateProgressBar;
  end;
end;

procedure TBoldPersistenceProgressNotifier.UpdateObject(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  inherited;
  StepProgress;
end;

procedure TBoldPersistenceProgressNotifier.UpdateProgressBar;
begin
  if now - fLastUpdate > c100ms then
  begin
    ProgressBar.Refresh;
    fLastUpdate := now;
  end;
end;

end.
