unit Test.BoldXMLStreaming;

{ TBoldXMLDocument and TBoldXMLNode on whichever XML library the build uses:
  fcl-xml under Free Pascal, MSXML for Delphi on Windows, Delphi's Xml.xmldom
  (OmniXML) for Delphi on other platforms or with BOLD_XMLDOM defined. The same
  tests run on every library, so they pin down what all of them must do: write
  values, serialise the document, parse it back and read the same values. }

interface

uses
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('XMLStreaming')]
  {$ENDIF}
  TTestBoldXMLStreaming = class(TBoldTestCase)
  published
    procedure TestValuesSurviveRoundTrip;
    procedure TestNonAsciiTextSurvivesRoundTrip;
    procedure TestNullSurvivesRoundTrip;
    procedure TestMissingSubNodeIsNil;
    procedure TestNewNodeIsEmpty;
    procedure TestChildNodesComeInDocumentOrder;
    procedure TestSubNodesByTagNameFindsDescendants;
    procedure TestRootXMLHasNoDeclaration;
    procedure TestMalformedXMLRaisesWithReason;
    procedure TestMalformedXMLDoesNotLeak;
    procedure TestTextWithoutRootElementRaises;
  end;

implementation

uses
  SysUtils,
  Contnrs,
  BoldDefs,
  BoldCoreConsts,
  BoldXMLStreaming;

const
  cRoot = 'Root';
  // What AllocatedNow counts: the Free Pascal heap counts bytes, the Delphi
  // memory manager blocks.
  cAllocationUnit = {$IFDEF FPC}'bytes'{$ELSE}'blocks'{$ENDIF};

function AllocatedNow: Int64;
begin
  {$IFDEF FPC}
  Result := BoldTestAllocatedBytes;
  {$ELSE}
  Result := BoldTestAllocatedBlocks;
  {$ENDIF}
end;

// Serialises Source's document and parses the text into a new document, as a
// receiver of the XML would.
function Reparse(Source: TBoldXMLDocument): TBoldXMLDocument;
begin
  Result := TBoldXMLDocument.Create;
  try
    Result.LoadXML(Source.RootXML);
  except
    Result.Free;
    raise;
  end;
end;

function NewManager: TBoldXMLStreamManager;
begin
  Result := TBoldXMLStreamManager.Create(TBoldXMLStreamerRegistry.MainStreamerRegistry);
end;

procedure TTestBoldXMLStreaming.TestValuesSurviveRoundTrip;
const
  cText = 'a < b & c > "d" ''e''';
var
  Manager: TBoldXMLStreamManager;
  Written, Read: TBoldXMLDocument;
  Node: TBoldXMLNode;
  Moment: TDateTime;
begin
  Moment := EncodeDate(2026, 11, 7) + EncodeTime(21, 8, 30, 123);
  Manager := NewManager;
  Written := TBoldXMLDocument.Create;
  Read := nil;
  try
    Node := Manager.NewRootNode(Written, cRoot);
    try
      Node.WriteSubNodeString('Text', cText);
      Node.WriteSubNodeInteger('Integer', -42);
      Node.WriteSubNodeBoolean('True', True);
      Node.WriteSubNodeBoolean('False', False);
      Node.WriteSubNodeFloat('Float', 3.25);
      Node.WriteSubNodeDateTime('Moment', Moment);
      Node.WriteSubNodeData('Plain', 'plain data');
      Node.WriteSubNodeData('Binary', 'x' + #1#2 + 'y');
    finally
      Node.Free;
    end;
    Read := Reparse(Written);
    Node := Manager.GetRootNode(Read, cRoot);
    try
      Assert.AreEqual(cText, Node.ReadSubNodeString('Text'));
      Assert.AreEqual(-42, Node.ReadSubNodeInteger('Integer'));
      Assert.IsTrue(Node.ReadSubNodeBoolean('True'), 'True');
      Assert.IsFalse(Node.ReadSubNodeBoolean('False'), 'False');
      Assert.AreEqual(3.25, Node.ReadSubNodeFloat('Float'), 0, 'Float');
      Assert.AreEqual(Moment, Node.ReadSubNodeDateTime('Moment'), 'Moment');
      Assert.AreEqual('plain data', string(Node.ReadSubNodeData('Plain')));
      Assert.AreEqual('x' + #1#2 + 'y', string(Node.ReadSubNodeData('Binary')));
    finally
      Node.Free;
    end;
  finally
    Read.Free;
    Written.Free;
    Manager.Free;
  end;
end;

procedure TTestBoldXMLStreaming.TestNonAsciiTextSurvivesRoundTrip;
const
  // a-ring, a-umlaut, o-umlaut and the euro sign, as character codes so that
  // the encoding of this source file cannot matter
  cText = 'x' + #$00E5#$00E4#$00F6#$20AC;
var
  Manager: TBoldXMLStreamManager;
  Written, Read: TBoldXMLDocument;
  Node: TBoldXMLNode;
begin
  Manager := NewManager;
  Written := TBoldXMLDocument.Create;
  Read := nil;
  try
    Node := Manager.NewRootNode(Written, cRoot);
    try
      Node.WriteSubNodeString('Text', cText);
    finally
      Node.Free;
    end;
    Read := Reparse(Written);
    Node := Manager.GetRootNode(Read, cRoot);
    try
      Assert.AreEqual(cText, Node.ReadSubNodeString('Text'));
    finally
      Node.Free;
    end;
  finally
    Read.Free;
    Written.Free;
    Manager.Free;
  end;
end;

procedure TTestBoldXMLStreaming.TestNullSurvivesRoundTrip;
var
  Manager: TBoldXMLStreamManager;
  Written, Read: TBoldXMLDocument;
  Node, SubNode: TBoldXMLNode;
begin
  Manager := NewManager;
  Written := TBoldXMLDocument.Create;
  Read := nil;
  try
    Node := Manager.NewRootNode(Written, cRoot);
    try
      SubNode := Node.NewSubNode('Null');
      SubNode.SetToNull;
      SubNode.Free;
      Node.WriteSubNodeString('NotNull', 'value');
    finally
      Node.Free;
    end;
    Read := Reparse(Written);
    Node := Manager.GetRootNode(Read, cRoot);
    try
      SubNode := Node.GetSubNode('Null');
      try
        Assert.IsTrue(SubNode.IsNull, 'Null');
      finally
        SubNode.Free;
      end;
      SubNode := Node.GetSubNode('NotNull');
      try
        Assert.IsFalse(SubNode.IsNull, 'NotNull');
      finally
        SubNode.Free;
      end;
    finally
      Node.Free;
    end;
  finally
    Read.Free;
    Written.Free;
    Manager.Free;
  end;
end;

procedure TTestBoldXMLStreaming.TestMissingSubNodeIsNil;
var
  Manager: TBoldXMLStreamManager;
  Document: TBoldXMLDocument;
  Node: TBoldXMLNode;
begin
  Manager := NewManager;
  Document := TBoldXMLDocument.Create;
  try
    Node := Manager.NewRootNode(Document, cRoot);
    try
      Node.WriteSubNodeString('Present', 'value');
      Assert.IsNull(Node.GetSubNode('Missing'));
    finally
      Node.Free;
    end;
  finally
    Document.Free;
    Manager.Free;
  end;
end;

procedure TTestBoldXMLStreaming.TestNewNodeIsEmpty;
var
  Manager: TBoldXMLStreamManager;
  Document: TBoldXMLDocument;
  Node, SubNode: TBoldXMLNode;
begin
  Manager := NewManager;
  Document := TBoldXMLDocument.Create;
  try
    Node := Manager.NewRootNode(Document, cRoot);
    try
      SubNode := Node.NewSubNode('Sub');
      try
        Assert.IsTrue(SubNode.IsEmpty, 'new node');
        SubNode.WriteString('text');
        Assert.IsFalse(SubNode.IsEmpty, 'node with text');
      finally
        SubNode.Free;
      end;
    finally
      Node.Free;
    end;
  finally
    Document.Free;
    Manager.Free;
  end;
end;

procedure TTestBoldXMLStreaming.TestChildNodesComeInDocumentOrder;
var
  Manager: TBoldXMLStreamManager;
  Written, Read: TBoldXMLDocument;
  Node: TBoldXMLNode;
  Children: TObjectList;
begin
  Manager := NewManager;
  Written := TBoldXMLDocument.Create;
  Read := nil;
  try
    Node := Manager.NewRootNode(Written, cRoot);
    try
      Node.WriteSubNodeString('A', '1');
      Node.WriteSubNodeString('B', '2');
      Node.WriteSubNodeString('C', '3');
    finally
      Node.Free;
    end;
    Read := Reparse(Written);
    Node := Manager.GetRootNode(Read, cRoot);
    try
      Children := Node.NewChildNodes;
      try
        Assert.AreEqual(3, Integer(Children.Count));
        Assert.AreEqual('A', TBoldXMLNode(Children[0]).Accessor);
        Assert.AreEqual('B', TBoldXMLNode(Children[1]).Accessor);
        Assert.AreEqual('C', TBoldXMLNode(Children[2]).Accessor);
        Assert.AreEqual('2', TBoldXMLNode(Children[1]).ReadString);
      finally
        Children.Free;
      end;
    finally
      Node.Free;
    end;
  finally
    Read.Free;
    Written.Free;
    Manager.Free;
  end;
end;

procedure TTestBoldXMLStreaming.TestSubNodesByTagNameFindsDescendants;
var
  Manager: TBoldXMLStreamManager;
  Written, Read: TBoldXMLDocument;
  Node, SubNode, SubSubNode: TBoldXMLNode;
  Found: TObjectList;
begin
  Manager := NewManager;
  Written := TBoldXMLDocument.Create;
  Read := nil;
  try
    Node := Manager.NewRootNode(Written, cRoot);
    try
      Node.WriteSubNodeString('id', 'first');
      SubNode := Node.NewSubNode('Nested');
      try
        SubSubNode := SubNode.NewSubNode('Deeper');
        try
          SubSubNode.WriteSubNodeString('id', 'second');
        finally
          SubSubNode.Free;
        end;
      finally
        SubNode.Free;
      end;
    finally
      Node.Free;
    end;
    Read := Reparse(Written);
    Node := Manager.GetRootNode(Read, cRoot);
    try
      Found := Node.NewSubNodesByTagName('id');
      try
        Assert.AreEqual(2, Integer(Found.Count));
        Assert.AreEqual('first', TBoldXMLNode(Found[0]).ReadString);
        Assert.AreEqual('second', TBoldXMLNode(Found[1]).ReadString);
      finally
        Found.Free;
      end;
    finally
      Node.Free;
    end;
  finally
    Read.Free;
    Written.Free;
    Manager.Free;
  end;
end;

procedure TTestBoldXMLStreaming.TestRootXMLHasNoDeclaration;
var
  Manager: TBoldXMLStreamManager;
  Document: TBoldXMLDocument;
  Node: TBoldXMLNode;
begin
  Manager := NewManager;
  Document := TBoldXMLDocument.Create;
  try
    Node := Manager.NewRootNode(Document, cRoot);
    Node.Free;
    Assert.AreEqual('<' + cRoot, Copy(Document.RootXML, 1, Length(cRoot) + 1), Document.RootXML);
  finally
    Document.Free;
    Manager.Free;
  end;
end;

procedure TTestBoldXMLStreaming.TestMalformedXMLRaisesWithReason;
var
  Document: TBoldXMLDocument;
  ErrorText: string;
begin
  Document := TBoldXMLDocument.Create;
  try
    ErrorText := '';
    try
      Document.LoadXML('<Root><Unclosed></Root>');
    except
      on E: EBold do
        ErrorText := E.Message;
    end;
    // The message starts with the general text and goes on with the parser's
    // reason, so that whoever reads it can find the fault in the XML.
    Assert.AreEqual(sXMLParseError, Copy(ErrorText, 1, Length(sXMLParseError)), ErrorText);
    Assert.IsTrue(Length(ErrorText) > Length(sXMLParseError) + 2, 'no reason in: ' + ErrorText);
  finally
    Document.Free;
  end;
end;

// A well-formed document has exactly one root element. OmniXML loads plain
// text without complaint and leaves the document without one.
procedure TTestBoldXMLStreaming.TestTextWithoutRootElementRaises;
var
  Document: TBoldXMLDocument;
  ErrorText: string;
begin
  Document := TBoldXMLDocument.Create;
  try
    ErrorText := '';
    try
      Document.LoadXML('not xml');
    except
      on E: EBold do
        ErrorText := E.Message;
    end;
    Assert.AreEqual(sXMLParseError, Copy(ErrorText, 1, Length(sXMLParseError)), 'not raised: ' + ErrorText);
    Assert.IsTrue(Length(ErrorText) > Length(sXMLParseError) + 2, 'no reason in: ' + ErrorText);
  finally
    Document.Free;
  end;
end;

// A document that fails to parse must not leave memory behind. Under Free
// Pascal, fcl-xml hands the partly read document to the caller when it raises.
procedure TTestBoldXMLStreaming.TestMalformedXMLDoesNotLeak;
var
  Document: TBoldXMLDocument;
  i: Integer;
  Before, After: Int64;

  procedure LoadMalformed;
  begin
    try
      Document.LoadXML('<Root><Unclosed></Root>');
    except
      on EBold do
        ; // the parse error is the expected outcome
    end;
  end;

begin
  Document := TBoldXMLDocument.Create;
  try
    for i := 1 to 5 do
      LoadMalformed; // warm up one-time allocations
    Before := AllocatedNow;
    for i := 1 to 20 do
      LoadMalformed;
    After := AllocatedNow;
    Assert.IsTrue(After = Before, Format('20 failed loads left %d %s', [After - Before, cAllocationUnit]));
  finally
    Document.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldXMLStreaming);

end.
