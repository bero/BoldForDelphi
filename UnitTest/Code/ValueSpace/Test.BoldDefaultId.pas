unit Test.BoldDefaultId;

interface

uses
  Classes,
  DUnitX.TestFramework,
  BoldBase,
  BoldDefaultId,
  BoldId,
  BoldDefs;

type
  [TestFixture]
  TTestBoldDefaultId = class
  public
    // TBoldDefaultId tests
    [Test]
    procedure TestDefaultIdCreate;
    [Test]
    procedure TestDefaultIdAsInteger;
    [Test]
    procedure TestDefaultIdGetAsString;
    [Test]
    procedure TestDefaultIdGetHash;
    [Test]
    procedure TestDefaultIdGetStreamName;
    [Test]
    procedure TestDefaultIdGetIsEqual_SameValue;
    [Test]
    procedure TestDefaultIdGetIsEqual_DifferentValue;
    [Test]
    procedure TestDefaultIdGetIsEqual_Nil;
    [Test]
    procedure TestDefaultIdGetIsEqual_TimestampedWithMaxTimestamp;
    [Test]
    procedure TestDefaultIdGetIsEqual_DifferentClass;
    [Test]
    procedure TestDefaultIdCloneWithClassId;
    [Test]
    procedure TestDefaultIdCloneWithTimeStamp_MaxTimestamp;
    [Test]
    procedure TestDefaultIdCloneWithTimeStamp_SpecificTimestamp;
    [Test]
    procedure TestDefaultIdCloneWithClassIdAndTimeStamp_MaxTimestamp;
    [Test]
    procedure TestDefaultIdCloneWithClassIdAndTimeStamp_SpecificTimestamp;

    // TBoldTimestampedDefaultId tests
    [Test]
    procedure TestTimestampedIdCreateWithTimeAndClassId;
    [Test]
    procedure TestTimestampedIdGetTimeStamp;
    [Test]
    procedure TestTimestampedIdGetHash;
    [Test]
    procedure TestTimestampedIdGetStreamName;
    [Test]
    procedure TestTimestampedIdGetIsEqual_SameValue;
    [Test]
    procedure TestTimestampedIdGetIsEqual_DifferentTimestamp;
    [Test]
    procedure TestTimestampedIdGetIsEqual_Nil;
    [Test]
    procedure TestTimestampedIdGetIsEqual_DefaultIdWithMaxTimestamp;
    [Test]
    procedure TestTimestampedIdGetIsEqual_DifferentClass;
    [Test]
    procedure TestTimestampedIdCloneWithClassId;
  end;

  [TestFixture]
  TTestBoldDefaultIdXMLStreaming = class
  private
    FDataModule: TDataModule;
    // The only places that know the XML back end (MSXML today). A port to
    // another back end replaces these two and keeps the id-list tests.
    function WriteToXML(const ARootName, AAccessor: string; AObject: TBoldInterfacedObject): string;
    function ReadFromXML(const AXML, ARootName, AAccessor: string): TObject;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestXMLRoundTrip_DefaultId;
    [Test]
    procedure TestXMLRoundTrip_TimestampedDefaultId;
    [Test]
    procedure TestXMLRoundTrip_ObjectIdList;
    [Test]
    procedure TestXMLRoundTrip_MemberIdList;
  end;

implementation

uses
  SysUtils,
  BoldStreams,
  BoldDefaultStreamNames,
  Bold_MSXML_TLB,
  BoldXMLStreaming,
  BoldDefaultXMLStreaming,
  Test.BoldAttributes;

{ TTestBoldDefaultId }

procedure TTestBoldDefaultId.TestDefaultIdCreate;
var
  Id: TBoldDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Assert.IsNotNull(Id);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdAsInteger;
var
  Id: TBoldDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 42;
    Assert.AreEqual(42, Id.AsInteger);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdGetAsString;
var
  Id: TBoldDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 12345;
    Assert.AreEqual('12345', Id.AsString);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdGetHash;
var
  Id: TBoldDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 100;
    // Hash = AsInteger + BOLDMAXTIMESTAMP (use Cardinal cast to avoid overflow)
    Assert.AreEqual(Cardinal(100) + Cardinal(BOLDMAXTIMESTAMP), Id.Hash);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdGetStreamName;
var
  Id: TBoldDefaultId;
  Streamable: IBoldStreamable;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Streamable := Id as IBoldStreamable;
    Assert.AreEqual(BOLDDEFAULTIDNAME, Streamable.StreamName);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdGetIsEqual_SameValue;
var
  Id1, Id2: TBoldDefaultId;
begin
  Id1 := TBoldDefaultId.CreateWithClassId(1, False);
  Id2 := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id1.AsInteger := 42;
    Id2.AsInteger := 42;
    Assert.IsTrue(Id1.IsEqual[Id2]);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdGetIsEqual_DifferentValue;
var
  Id1, Id2: TBoldDefaultId;
begin
  Id1 := TBoldDefaultId.CreateWithClassId(1, False);
  Id2 := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id1.AsInteger := 42;
    Id2.AsInteger := 99;
    Assert.IsFalse(Id1.IsEqual[Id2]);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdGetIsEqual_Nil;
var
  Id: TBoldDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 42;
    Assert.IsFalse(Id.IsEqual[nil]);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdGetIsEqual_TimestampedWithMaxTimestamp;
var
  Id1: TBoldDefaultId;
  Id2: TBoldTimestampedDefaultId;
begin
  Id1 := TBoldDefaultId.CreateWithClassId(1, False);
  Id2 := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(BOLDMAXTIMESTAMP, 1, False);
  try
    Id1.AsInteger := 42;
    Id2.AsInteger := 42;
    // DefaultId equals TimestampedDefaultId when timestamp is BOLDMAXTIMESTAMP
    Assert.IsTrue(Id1.IsEqual[Id2]);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdGetIsEqual_DifferentClass;
var
  Id1: TBoldDefaultId;
  Id2: TBoldInternalObjectId;
begin
  Id1 := TBoldDefaultId.CreateWithClassId(1, False);
  Id2 := TBoldInternalObjectId.CreateWithClassId(1, False);
  try
    Id1.AsInteger := 42;
    Assert.IsFalse(Id1.IsEqual[Id2]);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdCloneWithClassId;
var
  Id, Cloned: TBoldDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 42;
    Cloned := TBoldDefaultId(Id.CloneWithClassId(2, True));
    try
      Assert.AreEqual(42, Cloned.AsInteger);
      Assert.AreEqual(2, Cloned.TopSortedIndex);
      Assert.IsTrue(Cloned.TopSortedIndexExact);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdCloneWithTimeStamp_MaxTimestamp;
var
  Id, Cloned: TBoldDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 42;
    Cloned := Id.CloneWithTimeStamp(BOLDMAXTIMESTAMP);
    try
      Assert.AreEqual(42, Cloned.AsInteger);
      Assert.IsTrue(Cloned is TBoldDefaultId);
      Assert.IsFalse(Cloned is TBoldTimestampedDefaultId);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdCloneWithTimeStamp_SpecificTimestamp;
var
  Id: TBoldDefaultId;
  Cloned: TBoldTimestampedDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 42;
    Cloned := TBoldTimestampedDefaultId(Id.CloneWithTimeStamp(12345));
    try
      Assert.AreEqual(42, Cloned.AsInteger);
      Assert.IsTrue(Cloned is TBoldTimestampedDefaultId);
      Assert.AreEqual(TBoldTimestampType(12345), Cloned.TimeStamp);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdCloneWithClassIdAndTimeStamp_MaxTimestamp;
var
  Id, Cloned: TBoldDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 42;
    Cloned := Id.CloneWithClassIdAndTimeStamp(2, True, BOLDMAXTIMESTAMP);
    try
      Assert.AreEqual(42, Cloned.AsInteger);
      Assert.IsTrue(Cloned is TBoldDefaultId);
      Assert.IsFalse(Cloned is TBoldTimestampedDefaultId);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestDefaultIdCloneWithClassIdAndTimeStamp_SpecificTimestamp;
var
  Id: TBoldDefaultId;
  Cloned: TBoldTimestampedDefaultId;
begin
  Id := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id.AsInteger := 42;
    Cloned := TBoldTimestampedDefaultId(Id.CloneWithClassIdAndTimeStamp(2, True, 99999));
    try
      Assert.AreEqual(42, Cloned.AsInteger);
      Assert.IsTrue(Cloned is TBoldTimestampedDefaultId);
      Assert.AreEqual(TBoldTimestampType(99999), Cloned.TimeStamp);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdCreateWithTimeAndClassId;
var
  Id: TBoldTimestampedDefaultId;
begin
  Id := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(12345, 1, False);
  try
    Assert.IsNotNull(Id);
    Assert.AreEqual(TBoldTimestampType(12345), Id.TimeStamp);
    Assert.AreEqual(1, Id.TopSortedIndex);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdGetTimeStamp;
var
  Id: TBoldTimestampedDefaultId;
begin
  Id := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(99999, 1, False);
  try
    Assert.AreEqual(TBoldTimestampType(99999), Id.TimeStamp);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdGetHash;
var
  Id: TBoldTimestampedDefaultId;
begin
  Id := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(100, 1, False);
  try
    Id.AsInteger := 50;
    // Hash = AsInteger + TimeStamp
    Assert.AreEqual(Cardinal(50 + 100), Id.Hash);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdGetStreamName;
var
  Id: TBoldTimestampedDefaultId;
  Streamable: IBoldStreamable;
begin
  Id := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(100, 1, False);
  try
    Streamable := Id as IBoldStreamable;
    Assert.AreEqual(BOLDTIMESTAMPEDDEFAULTIDNAME, Streamable.StreamName);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdGetIsEqual_SameValue;
var
  Id1, Id2: TBoldTimestampedDefaultId;
begin
  Id1 := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(100, 1, False);
  Id2 := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(100, 1, False);
  try
    Id1.AsInteger := 42;
    Id2.AsInteger := 42;
    Assert.IsTrue(Id1.IsEqual[Id2]);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdGetIsEqual_DifferentTimestamp;
var
  Id1, Id2: TBoldTimestampedDefaultId;
begin
  Id1 := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(100, 1, False);
  Id2 := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(200, 1, False);
  try
    Id1.AsInteger := 42;
    Id2.AsInteger := 42;
    Assert.IsFalse(Id1.IsEqual[Id2]);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdGetIsEqual_Nil;
var
  Id: TBoldTimestampedDefaultId;
begin
  Id := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(100, 1, False);
  try
    Assert.IsFalse(Id.IsEqual[nil]);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdGetIsEqual_DefaultIdWithMaxTimestamp;
var
  Id1: TBoldTimestampedDefaultId;
  Id2: TBoldDefaultId;
begin
  Id1 := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(BOLDMAXTIMESTAMP, 1, False);
  Id2 := TBoldDefaultId.CreateWithClassId(1, False);
  try
    Id1.AsInteger := 42;
    Id2.AsInteger := 42;
    // TimestampedDefaultId equals DefaultId when timestamp is BOLDMAXTIMESTAMP
    Assert.IsTrue(Id1.IsEqual[Id2]);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdGetIsEqual_DifferentClass;
var
  Id1: TBoldTimestampedDefaultId;
  Id2: TBoldInternalObjectId;
begin
  Id1 := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(100, 1, False);
  Id2 := TBoldInternalObjectId.CreateWithClassId(1, False);
  try
    Assert.IsFalse(Id1.IsEqual[Id2]);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldDefaultId.TestTimestampedIdCloneWithClassId;
var
  Id: TBoldTimestampedDefaultId;
  Cloned: TBoldTimestampedDefaultId;
begin
  Id := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(12345, 1, False);
  try
    Id.AsInteger := 42;
    Cloned := TBoldTimestampedDefaultId(Id.CloneWithClassId(2, True));
    try
      Assert.AreEqual(42, Cloned.AsInteger);
      Assert.AreEqual(TBoldTimestampType(12345), Cloned.TimeStamp);
      Assert.AreEqual(2, Cloned.TopSortedIndex);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

{ TTestBoldDefaultIdXMLStreaming }

procedure TTestBoldDefaultIdXMLStreaming.Setup;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldDefaultIdXMLStreaming.TearDown;
begin
  FreeAndNil(FDataModule);
end;

procedure TTestBoldDefaultIdXMLStreaming.TestXMLRoundTrip_DefaultId;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  anId, ReadId: TBoldDefaultId;
begin
  anId := TBoldDefaultId.CreateWithClassID(1, True);
  try
    anId.AsInteger := 42;

    // Write to XML
    aDoc := TDomDocument.Create(nil);
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'DefaultIdTest');
      aNode.WriteSubNodeObject('Id', '', anId);
      anXML := aNode.XMLDomElement.ownerDocument.xml;
      aNode.Free;
      aDoc.Free;

      // Read back from XML
      aDoc := TDomDocument.Create(nil);
      aDoc.async := False;
      aDoc.loadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'DefaultIdTest');
      try
        ReadId := aNode.ReadSubNodeObject('Id', '') as TBoldDefaultId;
        try
          Assert.AreEqual(42, ReadId.AsInteger, 'AsInteger mismatch');
          Assert.AreEqual(1, ReadId.TopSortedIndex, 'TopSortedIndex mismatch');
          Assert.IsTrue(ReadId.TopSortedIndexExact, 'TopSortedIndexExact mismatch');
          Assert.IsTrue(anId.IsEqual[ReadId], 'IDs should be equal');
        finally
          ReadId.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    anId.Free;
  end;
end;

procedure TTestBoldDefaultIdXMLStreaming.TestXMLRoundTrip_TimestampedDefaultId;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  anId: TBoldTimestampedDefaultId;
  ReadId: TBoldTimestampedDefaultId;
begin
  anId := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(13, 1, True);
  try
    anId.AsInteger := 99;

    // Write to XML
    aDoc := TDomDocument.Create(nil);
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
    try
      aNode := aMgr.NewRootNode(aDoc, 'TimestampedIdTest');
      aNode.WriteSubNodeObject('Id', '', anId);
      anXML := aNode.XMLDomElement.ownerDocument.xml;
      aNode.Free;
      aDoc.Free;

      // Read back from XML
      aDoc := TDomDocument.Create(nil);
      aDoc.async := False;
      aDoc.loadXML(anXML);
      aNode := aMgr.GetRootNode(aDoc, 'TimestampedIdTest');
      try
        ReadId := aNode.ReadSubNodeObject('Id', '') as TBoldTimestampedDefaultId;
        try
          Assert.AreEqual(99, ReadId.AsInteger, 'AsInteger mismatch');
          Assert.AreEqual(1, ReadId.TopSortedIndex, 'TopSortedIndex mismatch');
          Assert.IsTrue(ReadId.TopSortedIndexExact, 'TopSortedIndexExact mismatch');
          Assert.AreEqual(TBoldTimestampType(13), ReadId.TimeStamp, 'TimeStamp mismatch');
          Assert.IsTrue(anId.IsEqual[ReadId], 'IDs should be equal');
        finally
          ReadId.Free;
        end;
      finally
        aNode.Free;
      end;
    finally
      aDoc.Free;
      aMgr.Free;
    end;
  finally
    anId.Free;
  end;
end;

function TTestBoldDefaultIdXMLStreaming.WriteToXML(const ARootName, AAccessor: string;
  AObject: TBoldInterfacedObject): string;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
begin
  aDoc := TDomDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
  try
    aNode := aMgr.NewRootNode(aDoc, ARootName);
    try
      aNode.WriteSubNodeObject(AAccessor, '', AObject);
      Result := aNode.XMLDomElement.ownerDocument.xml;
    finally
      aNode.Free;
    end;
  finally
    aDoc.Free;
    aMgr.Free;
  end;
end;

function TTestBoldDefaultIdXMLStreaming.ReadFromXML(const AXML, ARootName, AAccessor: string): TObject;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
begin
  aDoc := TDomDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    TjehodmBoldTest(FDataModule).BoldModel1.MoldModel);
  try
    aDoc.async := False;
    aDoc.loadXML(AXML);
    aNode := aMgr.GetRootNode(aDoc, ARootName);
    try
      Result := aNode.ReadSubNodeObject(AAccessor, '');
    finally
      aNode.Free;
    end;
  finally
    aDoc.Free;
    aMgr.Free;
  end;
end;

// Three ids that differ in value, class index, exactness and id class, in an
// order that is not sorted, so a reader that drops, duplicates or reorders ids,
// or loses the id class, fails here.
procedure TTestBoldDefaultIdXMLStreaming.TestXMLRoundTrip_ObjectIdList;
var
  List, ReadList: TBoldObjectIdList;
  anId: TBoldDefaultId;
  i: Integer;
begin
  List := TBoldObjectIdList.Create;
  try
    anId := TBoldDefaultId.CreateWithClassID(1, True);
    anId.AsInteger := 42;
    List.AddAndAdopt(anId);
    anId := TBoldDefaultId.CreateWithClassID(2, False);
    anId.AsInteger := 7;
    List.AddAndAdopt(anId);
    anId := TBoldTimestampedDefaultId.CreateWithTimeAndClassId(13, 1, True);
    anId.AsInteger := 99;
    List.AddAndAdopt(anId);

    ReadList := ReadFromXML(WriteToXML('IdListTest', 'Ids', List), 'IdListTest', 'Ids') as TBoldObjectIdList;
    try
      Assert.AreEqual(3, ReadList.Count, 'Count');
      for i := 0 to List.Count - 1 do
      begin
        Assert.AreEqual(List[i].ClassName, ReadList[i].ClassName, Format('Id class of id %d', [i]));
        Assert.AreEqual((List[i] as TBoldDefaultId).AsInteger, (ReadList[i] as TBoldDefaultId).AsInteger,
          Format('Value of id %d', [i]));
        Assert.AreEqual(List[i].TopSortedIndex, ReadList[i].TopSortedIndex, Format('Class index of id %d', [i]));
        Assert.IsTrue(List[i].TopSortedIndexExact = ReadList[i].TopSortedIndexExact, Format('Exactness of id %d', [i]));
      end;
      Assert.AreEqual(13, (ReadList[2] as TBoldTimestampedDefaultId).TimeStamp, 'TimeStamp of id 2');
    finally
      ReadList.Free;
    end;
  finally
    List.Free;
  end;
end;

// Two member ids in descending order, one of them index 0.
procedure TTestBoldDefaultIdXMLStreaming.TestXMLRoundTrip_MemberIdList;
var
  List, ReadList: TBoldMemberIdList;
begin
  List := TBoldMemberIdList.Create;
  try
    // The list owns its entries.
    List.Add(TBoldMemberId.Create(5));
    List.Add(TBoldMemberId.Create(0));

    ReadList := ReadFromXML(WriteToXML('MemberIdListTest', 'Ids', List), 'MemberIdListTest', 'Ids') as TBoldMemberIdList;
    try
      Assert.AreEqual(2, ReadList.Count, 'Count');
      Assert.AreEqual(5, ReadList[0].MemberIndex, 'Member index of id 0');
      Assert.AreEqual(0, ReadList[1].MemberIndex, 'Member index of id 1');
      Assert.IsTrue(ReadList.IsEqual(List), 'IsEqual');
    finally
      ReadList.Free;
    end;
  finally
    List.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDefaultId);
  TDUnitX.RegisterTestFixture(TTestBoldDefaultIdXMLStreaming);

end.
