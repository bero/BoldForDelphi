unit Test.BoldPersistenceHandle;

{ TBoldPersistenceHandle passes the events of its persistence controller on to
  the subscribers that AddPersistenceSubscription registers: the start and the
  end of each fetch and update, and each object on the way.
  TBoldPersistenceNotifier is built on it. These tests subscribe on the
  SQLite-backed BoldTestDM and record the events in the order they arrive. }

interface

uses
  BoldTestFramework,
  BoldSubscription;

type
  {$IFNDEF FPC}
  [Category('Persistence')]
  {$ENDIF}
  TTestBoldPersistenceHandleEvents = class(TBoldTestCase)
  private
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    fEvents: string;
    procedure RecordEvent(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSubscriberReceivesUpdateEvents;
    procedure TestSubscriberReceivesFetchEvents;
    procedure TestSubscriberReceivesOnlyRequestedEvents;
  end;

{ The name of a persistence event (bpe*), for readable event logs }
function BoldPersistenceEventName(Event: TBoldEvent): string;

{ Appends Name to the comma-separated Log }
procedure AppendEvent(var Log: string; const Name: string);

implementation

uses
  SysUtils,
  BoldSystem,
  dmBoldTest,
  BoldTestModel;

function BoldPersistenceEventName(Event: TBoldEvent): string;
begin
  case Event of
    bpeStartFetch: Result := 'StartFetch';
    bpeEndFetch: Result := 'EndFetch';
    bpeStartUpdate: Result := 'StartUpdate';
    bpeEndUpdate: Result := 'EndUpdate';
    bpeFetchObject: Result := 'FetchObject';
    bpeFetchMember: Result := 'FetchMember';
    bpeUpdateObject: Result := 'UpdateObject';
    bpeDeleteObject: Result := 'DeleteObject';
    bpeCreateObject: Result := 'CreateObject';
    bpeStartFetchID: Result := 'StartFetchID';
    bpeEndFetchID: Result := 'EndFetchID';
    bpeFetchId: Result := 'FetchID';
  else
    Result := 'Other';
  end;
end;

procedure AppendEvent(var Log: string; const Name: string);
begin
  if Log <> '' then
    Log := Log + ',';
  Log := Log + Name;
end;

procedure TTestBoldPersistenceHandleEvents.SetUp;
begin
  inherited;
  EnsureBoldTestDM;
  BoldTestDM.BoldSystemHandle1.System.Discard;
  fEvents := '';
  fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(RecordEvent);
end;

procedure TTestBoldPersistenceHandleEvents.TearDown;
begin
  FreeAndNil(fSubscriber);
  CloseBoldTestDM;
  inherited;
end;

procedure TTestBoldPersistenceHandleEvents.RecordEvent(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  AppendEvent(fEvents, BoldPersistenceEventName(OriginalEvent));
end;

procedure TTestBoldPersistenceHandleEvents.TestSubscriberReceivesUpdateEvents;
var
  System: TBoldSystem;
begin
  System := BoldTestDM.BoldSystemHandle1.System;
  TBook.Create(System).Title := 'Book 1';
  TBook.Create(System).Title := 'Book 2';
  BoldTestDM.BoldPersistenceHandleDB1.AddPersistenceSubscription(fSubscriber);
  System.UpdateDatabase;
  Assert.AreEqual('StartUpdate,CreateObject,CreateObject,EndUpdate', fEvents,
    'events of an update of two new objects');
end;

procedure TTestBoldPersistenceHandleEvents.TestSubscriberReceivesFetchEvents;
var
  System: TBoldSystem;
  Book1, Book2: TBoldObjectLocator;
  BookList: TBoldObjectList;
begin
  System := BoldTestDM.BoldSystemHandle1.System;
  Book1 := TBook.Create(System).BoldObjectLocator;
  Book2 := TBook.Create(System).BoldObjectLocator;
  System.UpdateDatabase;
  BookList := System.ClassByExpressionName['Book'];
  BookList.Invalidate;
  Book1.UnloadBoldObject;
  Book2.UnloadBoldObject;
  BoldTestDM.BoldPersistenceHandleDB1.AddPersistenceSubscription(fSubscriber);
  BookList.EnsureContentsCurrent;
  BookList.EnsureObjects;
  Assert.AreEqual('StartFetchID,FetchID,FetchID,EndFetchID,StartFetch,FetchObject,FetchObject,EndFetch', fEvents,
    'events of fetching the ids, then the objects of two books');
end;

procedure TTestBoldPersistenceHandleEvents.TestSubscriberReceivesOnlyRequestedEvents;
var
  System: TBoldSystem;
begin
  System := BoldTestDM.BoldSystemHandle1.System;
  TBook.Create(System).Title := 'Book 1';
  BoldTestDM.BoldPersistenceHandleDB1.AddPersistenceSubscription(fSubscriber, [bpeStartUpdate, bpeEndUpdate]);
  System.UpdateDatabase;
  Assert.AreEqual('StartUpdate,EndUpdate', fEvents, 'only the events asked for');
end;

initialization
  BoldRegisterTest(TTestBoldPersistenceHandleEvents);

end.
