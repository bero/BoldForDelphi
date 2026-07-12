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
  end;

implementation

uses
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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldAbstractSnooper);

end.
