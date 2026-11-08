unit Test.BoldExternalObjectSpaceEventHandler;

{ TBoldExternalObjectSpaceEventHandler applies to the local object space the
  events that other clients send, through the COM propagator on Windows or the
  UDP broadcaster. Only its PropagatorHandle property belongs to the COM
  propagator, so that property exists only on Windows; the handler itself, and
  TExpressionSubscriber that listens to it, compile and work on every
  platform. These tests put messages in the handler's queue, as a listener
  does, on the transient jeho model: no database and no propagator. }

interface

uses
  SysUtils,
  BoldTestFramework,
  BoldSubscription,
  BoldSystem,
  BoldThreadSafeQueue,
  BoldExternalObjectSpaceEventHandler,
  jehoBoldTestDataModule;

type
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldExternalObjectSpaceEventHandler = class(TBoldTestCase)
  private
    fDataModule: TjehodmBoldTest;
    fQueue: TBoldThreadSafeStringQueue;
    fHandler: TBoldExternalObjectSpaceEventHandler;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fEvents: string;
    fChangedClass: TBoldObjectList;
    fLostLocks: string;
    procedure AppendToEvents(const Name: string);
    procedure RecordEvent(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure ClassChanged(TheClass: TBoldObjectList);
    procedure LockLost(LockName: String);
    procedure LockLostRaises(LockName: String);
    procedure FailedMessage(AMessage: String; AException: Exception);
    procedure ConnectActiveSystem;
    procedure Deliver(const aMessage: string);
    function DeliverRaisesEBold(const aMessage: string): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMessageNeedsSystemHandle;
    procedure TestMessageNeedsActiveSystem;
    procedure TestClassChangedMessage;
    procedure TestLockLostMessage;
    procedure TestFreedSystemHandleIsReleased;
    procedure TestExpressionSubscriberListens;
    procedure TestExpressionSubscriberIgnoresOtherMember;
    procedure TestFailedEventWithoutHandlerRaises;
    procedure TestFailedEventGoesToOnFailedMessage;
  end;

implementation

uses
  BoldDefs,
  BoldObjectSpaceExternalEvents,
  BoldExpressionSubscriber;

function ClassChangedMessage(const aClassName: string): string;
begin
  Result := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, aClassName, '', '', nil);
end;

function LockLostMessage(const aLockName: string): string;
begin
  Result := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsLockLost, '', '', aLockName, nil);
end;

procedure TTestBoldExternalObjectSpaceEventHandler.SetUp;
begin
  fEvents := '';
  fLostLocks := '';
  fChangedClass := nil;
  fDataModule := TjehodmBoldTest.Create(nil);
  fQueue := TBoldThreadSafeStringQueue.Create('Test.BoldExternalObjectSpaceEventHandler');
  // No owner: the handler then learns that its system handle is gone only
  // through its subscription, not through TComponent's notification
  fHandler := TBoldExternalObjectSpaceEventHandler.Create(nil);
  fHandler.Queue := fQueue;
  fHandler.OnFailedMessage := FailedMessage;
  fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(RecordEvent);
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TearDown;
begin
  FreeAndNil(fSubscriber);
  FreeAndNil(fHandler);
  FreeAndNil(fQueue);
  FreeAndNil(fDataModule);
end;

procedure TTestBoldExternalObjectSpaceEventHandler.AppendToEvents(const Name: string);
begin
  if fEvents <> '' then
    fEvents := fEvents + ',';
  fEvents := fEvents + Name;
end;

procedure TTestBoldExternalObjectSpaceEventHandler.RecordEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
var
  Name: string;
begin
  if OriginalEvent = boeClassChanged then
    Name := 'ClassChanged'
  else
    Name := 'OtherEvent';
  if (Length(Args) > 0) and (Args[0].VType = vtUnicodeString) then
    Name := Name + ':' + string(Args[0].VUnicodeString);
  AppendToEvents(Name);
end;

procedure TTestBoldExternalObjectSpaceEventHandler.ClassChanged(TheClass: TBoldObjectList);
begin
  fChangedClass := TheClass;
end;

procedure TTestBoldExternalObjectSpaceEventHandler.LockLost(LockName: String);
begin
  if fLostLocks <> '' then
    fLostLocks := fLostLocks + ',';
  fLostLocks := fLostLocks + LockName;
end;

procedure TTestBoldExternalObjectSpaceEventHandler.LockLostRaises(LockName: String);
begin
  LockLost(LockName);
  if LockName = 'Lock1' then
    raise EBold.CreateFmt('OnLockLost failed for %s', [LockName]);
end;

procedure TTestBoldExternalObjectSpaceEventHandler.FailedMessage(AMessage: String; AException: Exception);
begin
  // Makes a failing message visible in the assertions instead of hiding it
  AppendToEvents('Failed:' + AMessage + ':' + string(AException.Message));
end;

procedure TTestBoldExternalObjectSpaceEventHandler.ConnectActiveSystem;
begin
  fDataModule.BoldSystemHandle1.Active := True;
  fHandler.BoldSystemHandle := fDataModule.BoldSystemHandle1;
end;

procedure TTestBoldExternalObjectSpaceEventHandler.Deliver(const aMessage: string);
begin
  fQueue.Enqueue(aMessage);
  fHandler.DequeueAll;
end;

function TTestBoldExternalObjectSpaceEventHandler.DeliverRaisesEBold(const aMessage: string): Boolean;
begin
  Result := False;
  try
    Deliver(aMessage);
  except
    on EBold do
      Result := True;
  end;
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestMessageNeedsSystemHandle;
begin
  Assert.IsTrue(DeliverRaisesEBold(ClassChangedMessage('ClassA')),
    'A handler without a system handle refuses the message');
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestMessageNeedsActiveSystem;
begin
  fDataModule.BoldSystemHandle1.Active := False;
  fHandler.BoldSystemHandle := fDataModule.BoldSystemHandle1;
  Assert.IsTrue(DeliverRaisesEBold(ClassChangedMessage('ClassA')),
    'A handler whose system is not active refuses the message');
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestClassChangedMessage;
begin
  ConnectActiveSystem;
  fHandler.OnClassChanged := ClassChanged;
  fHandler.AddSubscription(fSubscriber, boeClassChanged);
  Deliver(ClassChangedMessage('ClassA'));
  Assert.AreSame(fDataModule.BoldSystemHandle1.System.ClassByExpressionName['ClassA'], fChangedClass,
    'OnClassChanged gets the class list of ClassA');
  Assert.AreEqual('ClassChanged:ClassA', fEvents, 'Subscribers get boeClassChanged with the class name');
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestLockLostMessage;
begin
  ConnectActiveSystem;
  fHandler.OnLockLost := LockLost;
  Deliver(LockLostMessage('Lock1'));
  Assert.AreEqual('Lock1', fLostLocks, 'OnLockLost gets the name of the lost lock');
  Assert.AreEqual('', fEvents, 'No message failed');
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestFreedSystemHandleIsReleased;
begin
  fHandler.BoldSystemHandle := fDataModule.BoldSystemHandle1;
  FreeAndNil(fDataModule);
  Assert.IsNull(fHandler.BoldSystemHandle, 'The handler drops the reference to a freed system handle');
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestExpressionSubscriberListens;
var
  Subscriber: TExpressionSubscriber;
begin
  Assert.IsFalse(fHandler.HasSubscribers, 'Precondition: nobody listens to the handler');
  Subscriber := TExpressionSubscriber.Create(nil);
  try
    Subscriber.BoldExternalObjectSpaceEventHandler := fHandler;
    Assert.AreSame(fHandler, Subscriber.BoldExternalObjectSpaceEventHandler);
    Assert.IsTrue(fHandler.HasSubscribers, 'The expression subscriber listens to the handler');
  finally
    Subscriber.Free;
  end;
  Assert.IsFalse(fHandler.HasSubscribers, 'A freed expression subscriber no longer listens');
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestExpressionSubscriberIgnoresOtherMember;
var
  Subscriber: TExpressionSubscriber;
  Raised: string;
begin
  ConnectActiveSystem;
  Subscriber := TExpressionSubscriber.Create(nil);
  try
    Subscriber.StaticSystemHandle := fDataModule.BoldSystemHandle1;
    Subscriber.RootHandle := fDataModule.BoldSystemHandle1;
    Subscriber.EvaluateInPS := True;
    Subscriber.Expression := 'ClassA.allInstances->select(aString = ''x'')';
    Subscriber.BoldExternalObjectSpaceEventHandler := fHandler;
    Raised := '';
    try
      // Another client changed ClassA.aInteger, which the expression does not use
      fHandler.SendExtendedEvent(fHandler, boeMemberChanged, ['ClassA', 'aInteger', nil, nil]);
    except
      on E: Exception do
        Raised := string(E.ClassName) + ': ' + string(E.Message);
    end;
    Assert.AreEqual('', Raised, 'A change of a member the expression does not use is ignored');
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestFailedEventWithoutHandlerRaises;
var
  Raised: string;
begin
  ConnectActiveSystem;
  fHandler.OnFailedMessage := nil;
  fHandler.OnLockLost := LockLostRaises;
  Raised := '';
  try
    Deliver(LockLostMessage('Lock1'));
  except
    on E: Exception do
      Raised := string(E.ClassName) + ': ' + string(E.Message);
  end;
  Assert.AreEqual('EBold: OnLockLost failed for Lock1', Raised,
    'Without OnFailedMessage the error of a failing event reaches the caller');
end;

procedure TTestBoldExternalObjectSpaceEventHandler.TestFailedEventGoesToOnFailedMessage;
begin
  ConnectActiveSystem;
  fHandler.OnLockLost := LockLostRaises;
  // One message with two events: the first one fails, the second one is still handled
  Deliver(LockLostMessage('Lock1') + ',' + LockLostMessage('Lock2'));
  Assert.AreEqual('Failed:L:Lock1:OnLockLost failed for Lock1', fEvents,
    'OnFailedMessage gets the failing event and its error');
  Assert.AreEqual('Lock1,Lock2', fLostLocks, 'The event after the failing one is handled too');
end;

initialization
  BoldRegisterTest(TTestBoldExternalObjectSpaceEventHandler);

end.
