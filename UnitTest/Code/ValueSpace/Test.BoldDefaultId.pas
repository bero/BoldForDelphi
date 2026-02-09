unit Test.BoldDefaultId;

interface

uses
  Classes,
  DUnitX.TestFramework,
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
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestXMLRoundTrip_DefaultId;
    [Test]
    procedure TestXMLRoundTrip_TimestampedDefaultId;
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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDefaultId);
  TDUnitX.RegisterTestFixture(TTestBoldDefaultIdXMLStreaming);

end.
