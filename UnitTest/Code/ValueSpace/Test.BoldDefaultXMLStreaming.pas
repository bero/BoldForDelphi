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
  jehoBCBoldTest,
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

implementation

uses
  SysUtils,
  Classes,
  BoldId,
  BoldDefaultId,
  BoldDomainElement,
  Bold_MSXML_TLB;

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

initialization
  TDUnitX.RegisterTestFixture(TTestBoldDefaultXMLStreaming);

end.
