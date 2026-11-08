unit Test.BoldPersistenceNotifier;

{ TBoldPersistenceNotifier reports the events of a persistence handle: the
  start and end of a fetch or an update, each object on the way, and the
  progress brackets around them. It belongs to the persistence core, so it
  must work in any program without a GUI framework (the VCL progress notifier
  lives in BoldPersistenceProgressNotifier). These tests run it on the
  SQLite-backed BoldTestDM and record what it reports. }

interface

uses
  BoldTestFramework,
  BoldSubscription,
  BoldPersistenceNotifier;

type
  {$IFNDEF FPC}
  [Category('Persistence')]
  {$ENDIF}
  TTestBoldPersistenceNotifier = class(TBoldTestCase)
  private
    fNotifier: TBoldPersistenceNotifier;
    fEvents: string;
    procedure RecordEvent(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure RecordProgressStart(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure RecordProgressEnd(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure ConnectNotifier;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUpdateReportsProgressAndEachObject;
    procedure TestFetchReportsProgressAndEachObject;
    procedure TestNoEventsAfterPersistenceHandleCleared;
  end;

implementation

uses
  SysUtils,
  BoldSystem,
  dmBoldTest,
  BoldTestModel,
  Test.BoldPersistenceHandle; // BoldPersistenceEventName, AppendEvent

procedure TTestBoldPersistenceNotifier.SetUp;
begin
  inherited;
  EnsureBoldTestDM;
  BoldTestDM.BoldSystemHandle1.System.Discard;
  fEvents := '';
  fNotifier := TBoldPersistenceNotifier.Create(nil);
  fNotifier.OnStartFetch := RecordEvent;
  fNotifier.OnEndFetch := RecordEvent;
  fNotifier.OnStartUpdate := RecordEvent;
  fNotifier.OnEndUpdate := RecordEvent;
  fNotifier.OnFetchObject := RecordEvent;
  fNotifier.OnFetchMember := RecordEvent;
  fNotifier.OnUpdateObject := RecordEvent;
  fNotifier.OnDeleteObject := RecordEvent;
  fNotifier.OnCreateObject := RecordEvent;
  fNotifier.OnStartFetchID := RecordEvent;
  fNotifier.OnEndFetchID := RecordEvent;
  fNotifier.OnFetchID := RecordEvent;
  fNotifier.OnProgressStart := RecordProgressStart;
  fNotifier.OnProgressEnd := RecordProgressEnd;
end;

procedure TTestBoldPersistenceNotifier.TearDown;
begin
  FreeAndNil(fNotifier);
  CloseBoldTestDM;
  inherited;
end;

procedure TTestBoldPersistenceNotifier.RecordEvent(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  AppendEvent(fEvents, BoldPersistenceEventName(OriginalEvent));
end;

procedure TTestBoldPersistenceNotifier.RecordProgressStart(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  AppendEvent(fEvents, 'ProgressStart');
end;

procedure TTestBoldPersistenceNotifier.RecordProgressEnd(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  AppendEvent(fEvents, 'ProgressEnd');
end;

procedure TTestBoldPersistenceNotifier.ConnectNotifier;
begin
  fNotifier.PersistenceHandle := BoldTestDM.BoldPersistenceHandleDB1;
end;

procedure TTestBoldPersistenceNotifier.TestUpdateReportsProgressAndEachObject;
var
  System: TBoldSystem;
begin
  System := BoldTestDM.BoldSystemHandle1.System;
  TBook.Create(System).Title := 'Book 1';
  TBook.Create(System).Title := 'Book 2';
  ConnectNotifier;
  System.UpdateDatabase;
  Assert.AreEqual('ProgressStart,StartUpdate,CreateObject,CreateObject,EndUpdate,ProgressEnd', fEvents,
    'events of an update of two new objects');
end;

procedure TTestBoldPersistenceNotifier.TestFetchReportsProgressAndEachObject;
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
  ConnectNotifier;
  BookList.EnsureContentsCurrent;
  BookList.EnsureObjects;
  Assert.AreEqual('ProgressStart,StartFetchID,FetchID,FetchID,EndFetchID,ProgressEnd,' +
    'ProgressStart,StartFetch,FetchObject,FetchObject,EndFetch,ProgressEnd', fEvents,
    'events of fetching the ids, then the objects of two books');
end;

procedure TTestBoldPersistenceNotifier.TestNoEventsAfterPersistenceHandleCleared;
var
  System: TBoldSystem;
begin
  System := BoldTestDM.BoldSystemHandle1.System;
  ConnectNotifier;
  fNotifier.PersistenceHandle := nil;
  TBook.Create(System).Title := 'Book 1';
  System.UpdateDatabase;
  Assert.AreEqual('', fEvents, 'a notifier without a persistence handle reports nothing');
end;

initialization
  BoldRegisterTest(TTestBoldPersistenceNotifier);

end.
