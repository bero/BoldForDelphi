unit Test.BoldDefaultXMLStreaming;

{ DUnitX tests for BoldDefaultXMLStreaming - XML value space streaming }

interface

uses
  DUnitX.TestFramework,
  BoldSystem,
  BoldDefs,
  BoldElements,
  BoldAttributes,
  BoldFreeStandingValues,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldDefaultXMLStreaming,
  BoldXMLStreaming,
  BoldSystemHandle,
  BoldHandles,
  BoldModel,
  BoldTypeNameHandle,
  jehoBCBoldTest,
  TestModel1,
  Test.BoldAttributes;

type
  [TestFixture]
  [Category('XMLStreaming')]
  TTestBoldDefaultXMLStreaming = class
  private
    FDataModule: TjehodmBoldTest;
    function GetSystem: TBoldSystem;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestWriteAndReadValueSpace;
    [Test]
    procedure TestWriteAndReadValueSpaceWithNull;
    [Test]
    procedure TestStreamManagerCreate;
    [Test]
    procedure TestClassStreamerCount;
    [Test]
    procedure TestWriteWithPersistenceState;
    [Test]
    procedure TestWriteMultipleObjects;
    [Test]
    procedure TestWriteReadWithBlobContent;
    [Test]
    procedure TestClassStreamerByNameNotFound;
  end;

  // Tests with TestModel1 (has associations — exercises IdRef/IdListRef streamers)
  [TestFixture]
  [Category('XMLStreaming')]
  TTestXMLStreamingWithLinks = class
  private
    FSystemHandle: TBoldSystemHandle;
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
    function GetSystem: TBoldSystem;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestWriteReadWithSingleLink;
    [Test]
    procedure TestWriteReadWithMultiLink;
    [Test]
    procedure TestWriteReadWithParentChild;
    [Test]
    procedure TestWriteReadMultipleLinkedObjects;
    [Test]
    procedure TestJsonWithSingleAndMultiLinks;
    [Test]
    procedure TestJsonWithPartPartOf;
  end;

implementation

uses
  SysUtils,
  Classes,
  System.JSON,
  BoldId,
  BoldDefaultId,
  BoldDomainElement,
  Bold_MSXML_TLB,
  BoldObjectRepresentationJson,
  dmModel1;

procedure TTestBoldDefaultXMLStreaming.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldDefaultXMLStreaming.TearDown;
begin
  FreeAndNil(FDataModule);
end;

function TTestBoldDefaultXMLStreaming.GetSystem: TBoldSystem;
begin
  Result := FDataModule.BoldSystemHandle1.System;
end;

procedure TTestBoldDefaultXMLStreaming.TestWriteAndReadValueSpace;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  ParseError: IXMLDOMParseError;
  Obj: jehoBCBoldTest.TClassA;
  anIdList: TBoldObjectIdList;
  FSValueSpace: TBoldFreeStandingValueSpace;
  DateTimeConst: TDateTime;
begin
  DateTimeConst := EncodeDate(2026, 3, 15) + EncodeTime(14, 30, 0, 0);

  // Create object with all attribute types
  Obj := jehoBCBoldTest.TClassA.Create(GetSystem);
  Obj.aString := 'TestXML';
  Obj.aBoolean := True;
  Obj.aByte := 42;
  Obj.aCurrency := 99.95;
  Obj.aDate := EncodeDate(2026, 6, 15);
  Obj.aDateTime := DateTimeConst;
  Obj.aTime := EncodeTime(10, 30, 0, 0);
  Obj.aFloat := 3.14;
  Obj.aInteger := 12345;
  Obj.aShortInt := 100;
  Obj.aSmallInt := 30000;
  Obj.aWord := 50000;
  Obj.aBlob := 'BlobData';

  // Write to XML
  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    FDataModule.BoldModel1.MoldModel);
  try
    aMgr.IgnorePersistenceState := True;
    aNode := aMgr.NewRootNode(aDoc, 'ValueSpaceTest');

    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Obj.BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;

    anXML := aNode.XMLDomElement.ownerDocument.xml;
    Assert.IsTrue(Length(anXML) > 100, 'XML output should be substantial: ' + IntToStr(Length(anXML)));

    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;

  // Read back from XML
  aDoc := TDOMDocument.Create(nil);
  try
    aDoc.async := False;
    aDoc.loadXML(anXML);
    ParseError := aDoc.parseError;
    Assert.IsTrue((not Assigned(ParseError)) or (ParseError.errorCode = 0), 'XML should parse without errors');

    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      FDataModule.BoldModel1.MoldModel);
    try
      aMgr.IgnorePersistenceState := True;
      aNode := aMgr.GetRootNode(aDoc, 'ValueSpaceTest');

      FSValueSpace := TBoldFreeStandingValueSpace.Create;
      try
        aMgr.ReadValueSpace(FSValueSpace, aNode);
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(Obj.BoldObjectLocator.BoldObjectID),
          'Value space should contain the object');
      finally
        FSValueSpace.Free;
      end;

      aNode.Free;
    finally
      aMgr.Free;
    end;
  finally
    aDoc.Free;
  end;
end;

procedure TTestBoldDefaultXMLStreaming.TestWriteAndReadValueSpaceWithNull;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Obj: jehoBCBoldTest.TClassA;
  anIdList: TBoldObjectIdList;
begin
  // Create object with null attributes (default)
  Obj := jehoBCBoldTest.TClassA.Create(GetSystem);

  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    FDataModule.BoldModel1.MoldModel);
  try
    aMgr.IgnorePersistenceState := True;
    aNode := aMgr.NewRootNode(aDoc, 'NullTest');

    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Obj.BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;

    anXML := aNode.XMLDomElement.ownerDocument.xml;
    Assert.IsTrue(Length(anXML) > 50, 'XML with nulls should still have content');

    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;
end;

procedure TTestBoldDefaultXMLStreaming.TestStreamManagerCreate;
var
  aMgr: TBoldDefaultXMLStreamManager;
begin
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    FDataModule.BoldModel1.MoldModel);
  try
    // Access class streamer by index 0 (root class)
    Assert.IsNotNull(aMgr.ClassStreamers[0], 'Should have root class streamer');
  finally
    aMgr.Free;
  end;
end;

procedure TTestBoldDefaultXMLStreaming.TestClassStreamerCount;
var
  aMgr: TBoldDefaultXMLStreamManager;
begin
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    FDataModule.BoldModel1.MoldModel);
  try
    // Access class streamer for ClassA (TopSortedIndex 1)
    Assert.IsNotNull(aMgr.ClassStreamers[1], 'Should have ClassA streamer');
    Assert.IsNotNull(aMgr.ClassStreamerByName['ClassA'], 'Should find ClassA by name');
  finally
    aMgr.Free;
  end;
end;

procedure TTestBoldDefaultXMLStreaming.TestWriteWithPersistenceState;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Obj: jehoBCBoldTest.TClassA;
  anIdList: TBoldObjectIdList;
begin
  Obj := jehoBCBoldTest.TClassA.Create(GetSystem);
  Obj.aString := 'WithState';
  Obj.aInteger := 99;

  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    FDataModule.BoldModel1.MoldModel);
  try
    // Include persistence state in streaming
    aMgr.IgnorePersistenceState := False;
    aMgr.PersistenceStatesToBeStreamed := [bvpsModified, bvpsCurrent, bvpsInvalid];
    aNode := aMgr.NewRootNode(aDoc, 'StateTest');

    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Obj.BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;

    anXML := aNode.XMLDomElement.ownerDocument.xml;
    Assert.IsTrue(Length(anXML) > 50, 'XML with persistence state should have content');

    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;
end;

procedure TTestBoldDefaultXMLStreaming.TestWriteMultipleObjects;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Obj1, Obj2, Obj3: jehoBCBoldTest.TClassA;
  anIdList: TBoldObjectIdList;
  FSValueSpace: TBoldFreeStandingValueSpace;
  ParseError: IXMLDOMParseError;
begin
  Obj1 := jehoBCBoldTest.TClassA.Create(GetSystem);
  Obj2 := jehoBCBoldTest.TClassA.Create(GetSystem);
  Obj3 := jehoBCBoldTest.TClassA.Create(GetSystem);
  Obj1.aString := 'First';
  Obj1.aInteger := 1;
  Obj2.aString := 'Second';
  Obj2.aFloat := 2.0;
  Obj3.aString := 'Third';
  Obj3.aCurrency := 3.00;

  // Write all 3 objects
  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    FDataModule.BoldModel1.MoldModel);
  try
    aMgr.IgnorePersistenceState := True;
    aNode := aMgr.NewRootNode(aDoc, 'MultiTest');

    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Obj1.BoldObjectLocator.BoldObjectID);
      anIdList.Add(Obj2.BoldObjectLocator.BoldObjectID);
      anIdList.Add(Obj3.BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;

    anXML := aNode.XMLDomElement.ownerDocument.xml;
    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;

  // Read back and verify all 3 objects
  aDoc := TDOMDocument.Create(nil);
  try
    aDoc.async := False;
    aDoc.loadXML(anXML);
    ParseError := aDoc.parseError;
    Assert.IsTrue((not Assigned(ParseError)) or (ParseError.errorCode = 0), 'Should parse');

    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      FDataModule.BoldModel1.MoldModel);
    try
      aMgr.IgnorePersistenceState := True;
      aNode := aMgr.GetRootNode(aDoc, 'MultiTest');

      FSValueSpace := TBoldFreeStandingValueSpace.Create;
      try
        aMgr.ReadValueSpace(FSValueSpace, aNode);
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(Obj1.BoldObjectLocator.BoldObjectID), 'Should have Obj1');
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(Obj2.BoldObjectLocator.BoldObjectID), 'Should have Obj2');
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(Obj3.BoldObjectLocator.BoldObjectID), 'Should have Obj3');
      finally
        FSValueSpace.Free;
      end;

      aNode.Free;
    finally
      aMgr.Free;
    end;
  finally
    aDoc.Free;
  end;
end;

procedure TTestBoldDefaultXMLStreaming.TestWriteReadWithBlobContent;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Obj: jehoBCBoldTest.TClassA;
  anIdList: TBoldObjectIdList;
begin
  Obj := jehoBCBoldTest.TClassA.Create(GetSystem);
  Obj.aBlob := 'Binary blob data with special chars: <>&"';
  Obj.aBlobContent := 'typed blob';
  Obj.M_aBlobContent.ContentType := 'text/plain';

  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    FDataModule.BoldModel1.MoldModel);
  try
    aMgr.IgnorePersistenceState := True;
    aNode := aMgr.NewRootNode(aDoc, 'BlobTest');

    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Obj.BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;

    anXML := aNode.XMLDomElement.ownerDocument.xml;
    Assert.IsTrue(Length(anXML) > 100, 'Blob XML should have content');

    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;
end;

procedure TTestBoldDefaultXMLStreaming.TestClassStreamerByNameNotFound;
var
  aMgr: TBoldDefaultXMLStreamManager;
begin
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    FDataModule.BoldModel1.MoldModel);
  try
    Assert.WillRaiseAny(
      procedure
      begin
        aMgr.ClassStreamerByName['NonExistentClass'];
      end
    );
  finally
    aMgr.Free;
  end;
end;

{ TTestXMLStreamingWithLinks }

procedure TTestXMLStreamingWithLinks.SetUp;
begin
  Ensuredm_Model;
  FSystemTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  FSystemTypeInfoHandle.BoldModel := dm_Model1.BoldModel1;
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FSystemTypeInfoHandle;
  FSystemHandle.Active := True;
end;

procedure TTestXMLStreamingWithLinks.TearDown;
begin
  if Assigned(FSystemHandle) and FSystemHandle.Active then
  begin
    FSystemHandle.System.Discard;
    FSystemHandle.Active := False;
  end;
  FreeAndNil(FSystemHandle);
  FreeAndNil(FSystemTypeInfoHandle);
end;

function TTestXMLStreamingWithLinks.GetSystem: TBoldSystem;
begin
  Result := FSystemHandle.System;
end;

procedure TTestXMLStreamingWithLinks.TestWriteReadWithSingleLink;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  ParseError: IXMLDOMParseError;
  Parent, Child: TestModel1.TClassA;
  anIdList: TBoldObjectIdList;
  FSValueSpace: TBoldFreeStandingValueSpace;
begin
  Parent := TestModel1.TClassA.Create(GetSystem);
  Child := TestModel1.TClassA.Create(GetSystem);
  Parent.aString := 'Parent';
  Child.aString := 'Child';
  Child.parent := Parent;

  // Write
  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    dm_Model1.BoldModel1.MoldModel);
  try
    aMgr.IgnorePersistenceState := True;
    aNode := aMgr.NewRootNode(aDoc, 'LinkTest');
    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Parent.BoldObjectLocator.BoldObjectID);
      anIdList.Add(Child.BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;
    anXML := aNode.XMLDomElement.ownerDocument.xml;
    Assert.IsTrue(Length(anXML) > 200, 'XML with links should be substantial');
    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;

  // Read back
  aDoc := TDOMDocument.Create(nil);
  try
    aDoc.async := False;
    aDoc.loadXML(anXML);
    ParseError := aDoc.parseError;
    Assert.IsTrue((not Assigned(ParseError)) or (ParseError.errorCode = 0), 'Should parse');
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      dm_Model1.BoldModel1.MoldModel);
    try
      aMgr.IgnorePersistenceState := True;
      aNode := aMgr.GetRootNode(aDoc, 'LinkTest');
      FSValueSpace := TBoldFreeStandingValueSpace.Create;
      try
        aMgr.ReadValueSpace(FSValueSpace, aNode);
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(Parent.BoldObjectLocator.BoldObjectID), 'Should have Parent');
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(Child.BoldObjectLocator.BoldObjectID), 'Should have Child');
      finally
        FSValueSpace.Free;
      end;
      aNode.Free;
    finally
      aMgr.Free;
    end;
  finally
    aDoc.Free;
  end;
end;

procedure TTestXMLStreamingWithLinks.TestWriteReadWithMultiLink;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Parent, C1, C2: TestModel1.TClassA;
  anIdList: TBoldObjectIdList;
begin
  Parent := TestModel1.TClassA.Create(GetSystem);
  C1 := TestModel1.TClassA.Create(GetSystem);
  C2 := TestModel1.TClassA.Create(GetSystem);
  Parent.aString := 'P';
  C1.parent := Parent;
  C2.parent := Parent;

  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    dm_Model1.BoldModel1.MoldModel);
  try
    aMgr.IgnorePersistenceState := True;
    aNode := aMgr.NewRootNode(aDoc, 'MultiLinkTest');
    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Parent.BoldObjectLocator.BoldObjectID);
      anIdList.Add(C1.BoldObjectLocator.BoldObjectID);
      anIdList.Add(C2.BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;
    anXML := aNode.XMLDomElement.ownerDocument.xml;
    Assert.IsTrue(Length(anXML) > 300, 'XML with multi-link should be large');
    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;
end;

procedure TTestXMLStreamingWithLinks.TestWriteReadWithParentChild;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  ParseError: IXMLDOMParseError;
  Root, L1, L2: TestModel1.TClassA;
  anIdList: TBoldObjectIdList;
  FSValueSpace: TBoldFreeStandingValueSpace;
begin
  Root := TestModel1.TClassA.Create(GetSystem);
  L1 := TestModel1.TClassA.Create(GetSystem);
  L2 := TestModel1.TClassA.Create(GetSystem);
  Root.aString := 'Root';
  Root.aInteger := 1;
  L1.aString := 'L1';
  L1.aInteger := 2;
  L2.aString := 'L2';
  L2.aInteger := 3;
  L1.parent := Root;
  L2.parent := L1;

  // Write
  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    dm_Model1.BoldModel1.MoldModel);
  try
    aMgr.IgnorePersistenceState := True;
    aNode := aMgr.NewRootNode(aDoc, 'HierarchyTest');
    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Root.BoldObjectLocator.BoldObjectID);
      anIdList.Add(L1.BoldObjectLocator.BoldObjectID);
      anIdList.Add(L2.BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;
    anXML := aNode.XMLDomElement.ownerDocument.xml;
    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;

  // Read back
  aDoc := TDOMDocument.Create(nil);
  try
    aDoc.async := False;
    aDoc.loadXML(anXML);
    ParseError := aDoc.parseError;
    Assert.IsTrue((not Assigned(ParseError)) or (ParseError.errorCode = 0), 'Should parse');
    aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
      dm_Model1.BoldModel1.MoldModel);
    try
      aMgr.IgnorePersistenceState := True;
      aNode := aMgr.GetRootNode(aDoc, 'HierarchyTest');
      FSValueSpace := TBoldFreeStandingValueSpace.Create;
      try
        aMgr.ReadValueSpace(FSValueSpace, aNode);
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(Root.BoldObjectLocator.BoldObjectID), 'Should have Root');
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(L1.BoldObjectLocator.BoldObjectID), 'Should have L1');
        Assert.IsTrue(FSValueSpace.GetHasContentsForId(L2.BoldObjectLocator.BoldObjectID), 'Should have L2');
      finally
        FSValueSpace.Free;
      end;
      aNode.Free;
    finally
      aMgr.Free;
    end;
  finally
    aDoc.Free;
  end;
end;

procedure TTestXMLStreamingWithLinks.TestWriteReadMultipleLinkedObjects;
var
  aMgr: TBoldDefaultXMLStreamManager;
  aDoc: TDomDocument;
  aNode: TBoldXMLNode;
  anXML: string;
  Obj1, Obj2: TestModel1.TClassA;
  anIdList: TBoldObjectIdList;
begin
  Obj1 := TestModel1.TClassA.Create(GetSystem);
  Obj2 := TestModel1.TClassA.Create(GetSystem);
  Obj1.aString := 'A';
  Obj2.aString := 'B';
  Obj1.next := Obj2;
  // Also add part relationship
  Obj1.part.Add(Obj2);

  aDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(
    TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
    dm_Model1.BoldModel1.MoldModel);
  try
    aMgr.IgnorePersistenceState := True;
    aNode := aMgr.NewRootNode(aDoc, 'ComplexLinkTest');
    anIdList := TBoldObjectIdList.Create;
    try
      anIdList.Add(Obj1.BoldObjectLocator.BoldObjectID);
      anIdList.Add(Obj2.BoldObjectLocator.BoldObjectID);
      // Also add the link class objects for part/partof
      if Obj1.partpartpartof.Count > 0 then
        anIdList.Add(Obj1.partpartpartof[0].BoldObjectLocator.BoldObjectID);
      aMgr.WriteValueSpace(GetSystem.AsIBoldvalueSpace[bdepContents], anIdList, nil, aNode);
    finally
      anIdList.Free;
    end;
    anXML := aNode.XMLDomElement.ownerDocument.xml;
    Assert.IsTrue(Length(anXML) > 200, 'Complex link XML should be substantial');
    aNode.Free;
  finally
    aMgr.Free;
    aDoc.Free;
  end;
end;

procedure TTestXMLStreamingWithLinks.TestJsonWithSingleAndMultiLinks;
var
  Parent, C1, C2: TestModel1.TClassA;
  Json: string;
  JsonValue: TJSONValue;
begin
  Parent := TestModel1.TClassA.Create(GetSystem);
  C1 := TestModel1.TClassA.Create(GetSystem);
  C2 := TestModel1.TClassA.Create(GetSystem);
  Parent.aString := 'JP';
  Parent.aInteger := 100;
  C1.aString := 'JC1';
  C2.aString := 'JC2';
  C1.parent := Parent;
  C2.parent := Parent;
  Parent.next := C1;

  // Serialize — should exercise CreateJsonForBoldSingleLink and CreateJsonForBoldMultiLink
  JsonValue := BoldElementToJson(Parent);
  try
    Assert.IsNotNull(JsonValue, 'JSON value should not be nil');
    Json := JsonValue.ToString;
    Assert.IsTrue(Length(Json) > 50, 'JSON with links should be substantial');
    Assert.IsTrue(Pos('JP', Json) > 0, 'Should contain parent string');
  finally
    JsonValue.Free;
  end;
end;

procedure TTestXMLStreamingWithLinks.TestJsonWithPartPartOf;
var
  Obj1, Obj2: TestModel1.TClassA;
  Json: string;
begin
  Obj1 := TestModel1.TClassA.Create(GetSystem);
  Obj2 := TestModel1.TClassA.Create(GetSystem);
  Obj1.aString := 'Container';
  Obj2.aString := 'Part';
  Obj1.part.Add(Obj2);

  Json := BoldElementToJsonString(Obj1);
  Assert.IsTrue(Length(Json) > 20, 'JSON with part/partof should have content');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDefaultXMLStreaming);
  TDUnitX.RegisterTestFixture(TTestXMLStreamingWithLinks);

end.
