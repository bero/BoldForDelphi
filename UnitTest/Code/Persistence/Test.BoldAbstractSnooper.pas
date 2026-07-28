unit Test.BoldAbstractSnooper;

{ DUnitX tests for BoldAbstractSnooper - OSS class-event generation (H11) }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldDefs,
  BoldMeta,
  BoldAbstractSnooper,
  Test.BoldAttributes;  // declares TjehodmBoldTest, the standard test datamodule

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldAbstractSnooper = class
  private
    FDataModule: TjehodmBoldTest;
    function FindClass(AModel: TMoldModel; const AName: string): TMoldClass;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    [Category('Quick')]
    procedure TestClassEventsCoverSuperclassChain;
    [Test]
    [Category('Quick')]
    procedure TestExcessMemberSlotsAreNotIndexedIntoModelMembers;
  end;

implementation

uses
  BoldId,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldFreeStandingValues,
  BoldDefaultStreamNames,
  BoldObjectSpaceExternalEvents;

type
  // AddEvent/AddClassEvents are protected; this collector exposes them for
  // the test without touching production visibility.
  TEventCollectingSnooper = class(TBoldAbstractSnooper)
  private
    fCollected: TStringList;
  protected
    procedure AddEvent(const AEvent: string); override;
  public
    constructor Create(MoldModel: TMoldModel); override;
    destructor Destroy; override;
    procedure TransmitEvents(const ClientID: TBoldClientID); override;
    procedure CallAddClassEvents(TopSortedIndex: integer);
    procedure CallNonEmbeddedStateOfObjectChanged(const Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass);
    property Collected: TStringList read fCollected;
  end;

constructor TEventCollectingSnooper.Create(MoldModel: TMoldModel);
begin
  inherited Create(MoldModel);
  fCollected := TStringList.Create;
end;

destructor TEventCollectingSnooper.Destroy;
begin
  FreeAndNil(fCollected);
  inherited;
end;

procedure TEventCollectingSnooper.AddEvent(const AEvent: string);
begin
  fCollected.Add(AEvent);
end;

procedure TEventCollectingSnooper.TransmitEvents(const ClientID: TBoldClientID);
begin
  // not needed for these tests
end;

procedure TEventCollectingSnooper.CallAddClassEvents(TopSortedIndex: integer);
begin
  AddClassEvents(TopSortedIndex);
end;

procedure TEventCollectingSnooper.CallNonEmbeddedStateOfObjectChanged(const Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass);
begin
  NonEmbeddedStateOfObjectChanged(Object_Content, NewObject_Content, MoldClass);
end;

{ TTestBoldAbstractSnooper }

procedure TTestBoldAbstractSnooper.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldAbstractSnooper.TearDown;
begin
  FreeAndNil(FDataModule);
end;

function TTestBoldAbstractSnooper.FindClass(AModel: TMoldModel; const AName: string): TMoldClass;
var
  i: integer;
begin
  result := nil;
  for i := 0 to AModel.Classes.Count - 1 do
    if SameText(AModel.Classes[i].ExpandedExpressionName, AName) then
      exit(AModel.Classes[i]);
  Assert.Fail('class ' + AName + ' not found in test model');
end;

procedure TTestBoldAbstractSnooper.TestClassEventsCoverSuperclassChain;
var
  Snooper: TEventCollectingSnooper;
  MoldModel: TMoldModel;
  ClassB, ClassA: TMoldClass;
  EventB, EventA: string;

  function CountOf(const s: string): integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to Snooper.Collected.Count - 1 do
      if Snooper.Collected[i] = s then
        inc(result);
  end;

begin
  // Regression for H11: a change to ClassB must emit bsClassChanged for
  // ClassB AND its superclasses - clients watching a ClassA list contain the
  // changed ClassB instance too. The merged code emitted only the leaf event
  // while flagging the whole chain "already sent", so superclass watchers
  // never refreshed and a later direct ClassA change in the same batch was
  // dropped by the early-exit.
  MoldModel := FDataModule.BoldModel1.MoldModel;
  ClassB := FindClass(MoldModel, 'ClassB');
  ClassA := FindClass(MoldModel, 'ClassA');
  Assert.AreSame(TObject(ClassA), TObject(ClassB.SuperClass), 'model sanity: ClassB inherits ClassA');

  EventB := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, ClassB.ExpandedExpressionName, '', '', nil);
  EventA := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, ClassA.ExpandedExpressionName, '', '', nil);

  Snooper := TEventCollectingSnooper.Create(MoldModel);
  try
    Snooper.CallAddClassEvents(ClassB.TopSortedIndex);
    Assert.AreEqual(1, CountOf(EventB), 'leaf class event must be emitted');
    Assert.AreEqual(1, CountOf(EventA), 'superclass event must be emitted for the whole chain');

    // A direct superclass change later in the same batch: already covered by
    // the chain emission above - must stay at exactly one event (no drop, no dupe).
    Snooper.CallAddClassEvents(ClassA.TopSortedIndex);
    Assert.AreEqual(1, CountOf(EventA), 'superclass event neither dropped nor duplicated');
  finally
    Snooper.Free;
  end;
end;

procedure TTestBoldAbstractSnooper.TestExcessMemberSlotsAreNotIndexedIntoModelMembers;
var
  Snooper: TEventCollectingSnooper;
  MoldModel: TMoldModel;
  ClassA: TMoldClass;
  ValueSpace: TBoldFreeStandingValueSpace;
  Contents: IBoldObjectContents;
  ExcessValue: IBoldValue;
  Id: TBoldObjectId;
  ModelMemberCount: integer;
begin
  // Object contents can carry more member slots than the model class declares -
  // e.g. old values kept for an id that was re-issued to a class with fewer
  // members. The member loop sized itself from the contents but indexed
  // AllBoldMembers, so the surplus slot read past the end of the member list.
  // In production that raised only AFTER the update had committed: the caller
  // saw a failed save for data that was already written, and the whole batch of
  // OSS events was lost.
  MoldModel := FDataModule.BoldModel1.MoldModel;
  ClassA := FindClass(MoldModel, 'ClassA');
  ModelMemberCount := ClassA.AllBoldMembers.Count;

  ValueSpace := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassIDandInternalId(1, ClassA.TopSortedIndex, true);
    try
      Contents := ValueSpace.GetEnsuredObjectContentsByObjectId(Id);
      Assert.IsNotNull(Contents, 'contents for the test id must exist');

      // One slot beyond what the model class knows about.
      ExcessValue := Contents.EnsureMemberAndGetValueByIndex(ModelMemberCount, BoldContentName_Integer);
      Assert.IsNotNull(ExcessValue, 'excess member slot must be materialised');
      Assert.AreEqual(ModelMemberCount + 1, Contents.MemberCount, 'contents must carry one slot more than the model class');

      Snooper := TEventCollectingSnooper.Create(MoldModel);
      try
        try
          Snooper.CallNonEmbeddedStateOfObjectChanged(Contents, nil, ClassA);
        except
          on E: Exception do
            Assert.Fail(Format('surplus member slot must be ignored, not indexed into AllBoldMembers - got %s: %s',
              [E.ClassName, E.Message]));
        end;
        // ClassA is attribute-only (the model has no associations), so there is
        // no embedded single link to report on any of the real member slots.
        Assert.AreEqual(0, Snooper.Collected.Count, 'no link events expected for an attribute-only class');
      finally
        Snooper.Free;
      end;
    finally
      ExcessValue := nil;
      Contents := nil;
      Id.Free;
    end;
  finally
    ValueSpace.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldAbstractSnooper);

end.
