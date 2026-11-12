
{ Global compiler directives }
{$include bold.inc}
unit BoldEnvironmentLCL;

{ The Bold environment of a Lazarus (LCL) application, the twin of
  BoldEnvironmentVCL (#117). Name it in one uses clause of an LCL application:
  its initialization registers the environment. Bold's display queue is then
  drained from the LCL's idle handling, and AskUser, QueryUser and InformUser
  show LCL dialogs.

  The queue is drained when the application is idle. BoldEnvironmentVCL drains
  it before every message, so also inside a loop that calls
  Application.ProcessMessages; the LCL has no hook for that. Code that keeps
  the user interface alive with ProcessMessages during a long operation calls
  TBoldQueueable.DisplayAll itself to bring Bold-aware displays up to date.

  The unit builds with Free Pascal only and is in the Lazarus package BoldLCL
  (packages/Lazarus/BoldLCL.lpk). }

interface

implementation

uses
  SysUtils,
  Classes,
  Controls,
  Dialogs,
  Forms,

  BoldCoreConsts,
  BoldDefs,
  BoldQueue,
  BoldEnvironment;

type
  { forward declarations }
  TBoldLCLQueue = class;
  TBoldLCLEnvironmentConfiguration = class;

  { TBoldLCLQueue }
  TBoldLCLQueue = class(TBoldQueue)
  private
    fBoldQueueActiveNesting: Integer;
    fIdleHandlerAdded: Boolean;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure RemoveIdleHandler;
  protected
    function GetIsActive: boolean; override;
  public
    destructor Destroy; override;
    procedure ActivateDisplayQueue; override;
    procedure DeActivateDisplayQueue; override;
  end;

  { TBoldLCLEnvironmentConfiguration }
  TBoldLCLEnvironmentConfiguration = class(TBoldEnvironmentConfiguration)
  private
    procedure WakeUp(Data: PtrInt);
  protected
    function GetName: string; override;
    function GetRootComponent: TComponent; override;
    function GetQueueClass: TBoldQueueClass; override;
  public
    procedure HandleDesigntimeException(Sender: TObject); override;
    procedure UpdateDesigner(Sender: TObject); override;
    function IsFormOrDataModule(Sender: TObject): Boolean; override;
    function AskUser(const Text: string): Boolean; override;
    function QueryUser(const Title, Query: string): TBoldQueryResult; override;
    procedure InformUser(const Text: string); override;
    procedure ProcessMessages; override;
    procedure BringToFront; override;
    procedure TriggerQueueMechanism; override;
    procedure FocusMainForm; override;
  end;

{ TBoldLCLEnvironmentConfiguration }

function TBoldLCLEnvironmentConfiguration.AskUser(const Text: string): Boolean;
begin
  Result := MessageDlg(Text, mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

function TBoldLCLEnvironmentConfiguration.QueryUser(const Title, Query: string): TBoldQueryResult;
begin
  // Closing the dialog without an answer counts as No
  case MessageDlg(Title, Query, mtConfirmation, [mbYes, mbYesToAll, mbNo, mbNoToAll], 0) of
    mrYes: Result := qrYes;
    mrYesToAll: Result := qrYesAll;
    mrNoToAll: Result := qrNoAll;
  else
    Result := qrNo;
  end;
end;

procedure TBoldLCLEnvironmentConfiguration.InformUser(const Text: string);
begin
  ShowMessage(Text);
end;

procedure TBoldLCLEnvironmentConfiguration.BringToFront;
begin
  Application.BringToFront;
end;

procedure TBoldLCLEnvironmentConfiguration.FocusMainForm;
begin
  if Assigned(Application) and Assigned(Application.MainForm) then
    Application.MainForm.SetFocus;
end;

function TBoldLCLEnvironmentConfiguration.GetName: string;
begin
  Result := 'LCL';
end;

function TBoldLCLEnvironmentConfiguration.GetQueueClass: TBoldQueueClass;
begin
  Result := TBoldLCLQueue;
end;

function TBoldLCLEnvironmentConfiguration.GetRootComponent: TComponent;
begin
  Result := Application;
end;

procedure TBoldLCLEnvironmentConfiguration.HandleDesigntimeException(Sender: TObject);
begin
  Application.HandleException(Sender);
end;

function TBoldLCLEnvironmentConfiguration.IsFormOrDataModule(Sender: TObject): Boolean;
begin
  Result := (Sender is TCustomForm) or (Sender is TDataModule);
end;

procedure TBoldLCLEnvironmentConfiguration.ProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure TBoldLCLEnvironmentConfiguration.TriggerQueueMechanism;
begin
  // An asynchronous call wakes the message loop, also from another thread, and
  // the loop runs the idle handlers, the queue's among them, after it
  Application.QueueAsyncCall(WakeUp, 0);
end;

procedure TBoldLCLEnvironmentConfiguration.WakeUp(Data: PtrInt);
begin
  // Nothing: arriving is the point
end;

procedure TBoldLCLEnvironmentConfiguration.UpdateDesigner(Sender: TObject);
var
  ParentForm: TCustomForm;
  Owner: TComponent;
begin
  // Don't call inherited
  if not RunningInIDE then
    raise EBold.CreateFmt(sNotRunningInIDE, ['UpdateDesigner']); // do not localize
  ParentForm := nil;
  Owner := nil;
  if (Sender is TControl) then
    ParentForm := GetParentForm(TControl(Sender))
  else
  begin
    if Sender is TComponent then
      Owner := TComponent(Sender).Owner;
    if Assigned(Owner) then
    begin
      if (Owner is TCustomFrame) then
        ParentForm := GetParentForm(TCustomFrame(Owner))
      else if (Owner is TCustomForm) then
        ParentForm := TCustomForm(Owner)
      else if (Owner is TDataModule) and Assigned(Owner.Owner) and (Owner.Owner is TCustomForm) then
        ParentForm := TCustomForm(Owner.Owner);
    end;
  end;
  if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
    ParentForm.Designer.Modified;
end;

{ TBoldLCLQueue }

procedure TBoldLCLQueue.ApplicationIdle(Sender: TObject; var Done: Boolean);
var
  PreviousCount: integer;
  AllDisplayed: Boolean;
begin
  // Done is left alone unless there is more to display: the application's
  // other idle handlers share it
  if IsActive and not Empty then
  try
    AllDisplayed := True;
    PerformPreDisplayQueue;
    repeat
      PreviousCount := DisplayCount;
      case DisplayMode of
        dmDisplayOne: AllDisplayed := not DisplayOne;
        dmDisplayAll: AllDisplayed := not DisplayAll;
        else
          raise EBoldInternal.CreateFmt(sUnknownDisplayMode, [classname]);
      end;
    until (DisplayCount = 0) or (DisplayCount = PreviousCount); // stop if count did not change in last loop run
    if AllDisplayed then
      PerformPostDisplayQueue
    else
      Done := False;
  except
    Application.HandleException(nil);
  end;
end;

procedure TBoldLCLQueue.ActivateDisplayQueue;
begin
  inherited;
  if fBoldQueueActiveNesting > 0 then
    Dec(fBoldQueueActiveNesting);
  if (fBoldQueueActiveNesting = 0) and not fIdleHandlerAdded then
  begin
    Application.AddOnIdleHandler(ApplicationIdle, False);
    fIdleHandlerAdded := True;
  end;
end;

procedure TBoldLCLQueue.DeActivateDisplayQueue;
begin
  inherited;
  Inc(fBoldQueueActiveNesting);
  RemoveIdleHandler;
end;

destructor TBoldLCLQueue.Destroy;
begin
  RemoveIdleHandler;
  inherited;
end;

function TBoldLCLQueue.GetIsActive: boolean;
begin
  Result := fBoldQueueActiveNesting = 0;
end;

procedure TBoldLCLQueue.RemoveIdleHandler;
begin
  if fIdleHandlerAdded and Assigned(Application) then
    Application.RemoveOnIdleHandler(ApplicationIdle);
  fIdleHandlerAdded := False;
end;

initialization
  BoldRegisterFrameworkEnvironment(TBoldLCLEnvironmentConfiguration);

finalization
  // The queue has an idle handler in Application, and a wake-up call may be
  // waiting there: remove both while the LCL units used here are still
  // initialized; BoldEnvironment is finalized later
  if BoldInternalFrameworkConfiguration is TBoldLCLEnvironmentConfiguration then
  begin
    if Assigned(Application) then
      Application.RemoveAsyncCalls(BoldInternalFrameworkConfiguration);
    TBoldLCLEnvironmentConfiguration(BoldInternalFrameworkConfiguration).FinalizeQueue;
  end;

end.
