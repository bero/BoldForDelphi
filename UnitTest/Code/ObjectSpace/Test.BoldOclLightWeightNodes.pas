unit Test.BoldOclLightWeightNodes;

interface

uses
  DUnitX.TestFramework,
  BoldOclLightWeightNodes;

type
  TTestOLWTrackingVisitor = class(TBoldOLWNodeVisitor)
  private
    fVisitedMethods: string;
  protected
    procedure VisitTBoldOLWNode(N: TBoldOLWNode); override;
    procedure VisitTBoldOLWListCoercion(N: TBoldOLWListCoercion); override;
    procedure VisitTBoldOLWOperation(N: TBoldOLWOperation); override;
    procedure VisitTBoldOLWIteration(N: TBoldOLWIteration); override;
    procedure VisitTBoldOLWMember(N: TBoldOLWMember); override;
    procedure VisitTBoldOLWLiteral(N: TBoldOLWLiteral); override;
    procedure VisitTBoldOLWStrLiteral(N: TBoldOLWStrLiteral); override;
    procedure VisitTBoldOLWDateLiteral(N: TBoldOLWDateLiteral); override;
    procedure VisitTBoldOLWTimeLiteral(N: TBoldOLWTimeLiteral); override;
    procedure VisitTBoldOLWFloatLiteral(N: TBoldOLWFloatLiteral); override;
    procedure VisitTBoldOLWEnumLiteral(N: TBoldOLWEnumLiteral); override;
    procedure VisitTBoldOLWIntLiteral(N: TBoldOLWIntLiteral); override;
    procedure VisitTBoldOLWVariableBinding(N: TBoldOLWVariableBinding); override;
    procedure VisitTBoldOLWVariableReference(N: TBoldOLWVariableReference); override;
    procedure VisitTBoldOLWTypeNode(N: TBoldOLWTypeNode); override;
  public
    function WasVisited(const MethodName: string): Boolean;
    property VisitedMethods: string read fVisitedMethods;
  end;

  [TestFixture]
  TTestBoldOclLightWeightNodes = class
  public
    [Test]
    [Category('Quick')]
    procedure TestNodeCreation_AllTypes;
    [Test]
    [Category('Quick')]
    procedure TestAcceptVisitor_AllNodeTypes;
    [Test]
    [Category('Quick')]
    procedure TestVisitorBaseStubs_AllCalled;
    [Test]
    [Category('Quick')]
    procedure TestVisitorBaseStub_VisitNode_Direct;
    [Test]
    [Category('Quick')]
    procedure TestGetStreamName_AllTypes;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_AddAndCount;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_IndexOf;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_GetItem;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_PutItem;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_TraverseList;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_DestroyOwned;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_DestroyNonOwned;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_GetStreamName;
    [Test]
    [Category('Quick')]
    procedure TestOperation_DestroyFreesArgs;
    [Test]
    [Category('Quick')]
    procedure TestIteration_DestroyFreesLoopVar;
    [Test]
    [Category('Quick')]
    procedure TestMember_CreateAndDestroy;
    [Test]
    [Category('Quick')]
    procedure TestListCoercion_DestroyFreesChild;
    [Test]
    [Category('Quick')]
    procedure TestOclCondition_CreateAndDestroy;
    [Test]
    [Category('Quick')]
    procedure TestVariableBinding_AddRef_FirstCall;
    [Test]
    [Category('Quick')]
    procedure TestVariableBinding_AddRef_LoopVar_MultipleRefs;
    [Test]
    [Category('Quick')]
    procedure TestVariableBinding_AddRef_NonLoopVar_Raises;
    [Test]
    [Category('Quick')]
    procedure TestVariableBinding_Properties;
    [Test]
    [Category('Quick')]
    procedure TestEnumLiteral_IntValue;
    [Test]
    [Category('Quick')]
    procedure TestDateLiteral_DateValue;
    [Test]
    [Category('Quick')]
    procedure TestTimeLiteral_TimeValue;
  end;

implementation

uses
  SysUtils,
  Variants,
  BoldDefs,
  BoldStreams;

function GetNodeStreamName(Node: TBoldOLWNode): string;
var
  S: IBoldStreamable;
begin
  S := Node as IBoldStreamable;
  Result := S.GetStreamName;
end;

function GetListStreamName(List: TBoldOLWNodeList): string;
var
  S: IBoldStreamable;
begin
  S := List as IBoldStreamable;
  Result := S.GetStreamName;
end;

{ TTestOLWTrackingVisitor }

procedure TTestOLWTrackingVisitor.VisitTBoldOLWNode(N: TBoldOLWNode);
begin
  fVisitedMethods := fVisitedMethods + 'Node,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWListCoercion(N: TBoldOLWListCoercion);
begin
  fVisitedMethods := fVisitedMethods + 'ListCoercion,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWOperation(N: TBoldOLWOperation);
begin
  fVisitedMethods := fVisitedMethods + 'Operation,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWIteration(N: TBoldOLWIteration);
begin
  fVisitedMethods := fVisitedMethods + 'Iteration,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWMember(N: TBoldOLWMember);
begin
  fVisitedMethods := fVisitedMethods + 'Member,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWLiteral(N: TBoldOLWLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'Literal,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWStrLiteral(N: TBoldOLWStrLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'StrLiteral,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWDateLiteral(N: TBoldOLWDateLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'DateLiteral,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWTimeLiteral(N: TBoldOLWTimeLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'TimeLiteral,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWFloatLiteral(N: TBoldOLWFloatLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'FloatLiteral,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWEnumLiteral(N: TBoldOLWEnumLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'EnumLiteral,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWIntLiteral(N: TBoldOLWIntLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'IntLiteral,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWVariableBinding(N: TBoldOLWVariableBinding);
begin
  fVisitedMethods := fVisitedMethods + 'VariableBinding,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWVariableReference(N: TBoldOLWVariableReference);
begin
  fVisitedMethods := fVisitedMethods + 'VariableReference,';
end;

procedure TTestOLWTrackingVisitor.VisitTBoldOLWTypeNode(N: TBoldOLWTypeNode);
begin
  fVisitedMethods := fVisitedMethods + 'TypeNode,';
end;

function TTestOLWTrackingVisitor.WasVisited(const MethodName: string): Boolean;
begin
  Result := Pos(MethodName + ',', fVisitedMethods) > 0;
end;

{ TTestBoldOclLightWeightNodes }

procedure TTestBoldOclLightWeightNodes.TestNodeCreation_AllTypes;
var
  TypeNode: TBoldOLWTypeNode;
  Operation: TBoldOLWOperation;
  Binding: TBoldOLWVariableBinding;
  Iteration: TBoldOLWIteration;
  Member: TBoldOLWMember;
  VarRef: TBoldOLWVariableReference;
  StrLit: TBoldOLWStrLiteral;
  IntLit: TBoldOLWIntLiteral;
  FloatLit: TBoldOLWFloatLiteral;
  EnumLit: TBoldOLWEnumLiteral;
  DateLit: TBoldOLWDateLiteral;
  TimeLit: TBoldOLWTimeLiteral;
  ListCoercion: TBoldOLWListCoercion;
  ChildNode: TBoldOLWIntLiteral;
  MemberOf: TBoldOLWIntLiteral;
begin
  // TBoldOLWTypeNode
  TypeNode := TBoldOLWTypeNode.Create(10, 'Person', 5);
  try
    Assert.AreEqual(10, TypeNode.Position);
    Assert.AreEqual('Person', TypeNode.TypeName);
    Assert.AreEqual(5, TypeNode.TopSortedIndex);
  finally
    TypeNode.Free;
  end;

  // TBoldOLWOperation
  Operation := TBoldOLWOperation.Create(20, 'includes');
  try
    Assert.AreEqual(20, Operation.Position);
    Assert.AreEqual('includes', Operation.OperationName);
    Assert.IsNotNull(Operation.Args, 'Args list should be created');
    Assert.AreEqual(0, Operation.Args.Count);
  finally
    Operation.Free;
  end;

  // TBoldOLWVariableBinding
  Binding := TBoldOLWVariableBinding.Create(30, 'self', 0);
  try
    Assert.AreEqual(30, Binding.Position);
    Assert.AreEqual('self', Binding.VariableName);
    Assert.AreEqual(0, Binding.TopSortedIndex);
  finally
    Binding.Free;
  end;

  // TBoldOLWIteration (owns LoopVar)
  Binding := TBoldOLWVariableBinding.Create(35, 'e', 1);
  Iteration := TBoldOLWIteration.Create(40, 'select', Binding);
  try
    Assert.AreEqual(40, Iteration.Position);
    Assert.AreEqual('select', Iteration.OperationName);
    Assert.AreSame(Binding, Iteration.LoopVar);
    Assert.IsNotNull(Iteration.Args, 'Iteration.Args should be created');
  finally
    Iteration.Free; // frees Binding too
  end;

  // TBoldOLWMember (owns MemberOf and Qualifier)
  MemberOf := TBoldOLWIntLiteral.Create(0, 0);
  Member := TBoldOLWMember.Create(50, 'name', 3, MemberOf, True);
  try
    Assert.AreEqual(50, Member.Position);
    Assert.AreEqual('name', Member.MemberName);
    Assert.AreEqual(3, Member.MemberIndex);
    Assert.AreSame(MemberOf, Member.MemberOf);
    Assert.IsNotNull(Member.Qualifier, 'Qualifier should be created');
    Assert.IsTrue(Member.IsBoolean);
  finally
    Member.Free; // frees MemberOf and Qualifier
  end;

  // TBoldOLWVariableReference (does NOT own binding)
  Binding := TBoldOLWVariableBinding.Create(55, 'x', 2);
  try
    VarRef := TBoldOLWVariableReference.Create(60, Binding);
    try
      Assert.AreEqual(60, VarRef.Position);
      Assert.AreSame(Binding, VarRef.VariableBinding);
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
  end;

  // TBoldOLWStrLiteral
  StrLit := TBoldOLWStrLiteral.Create(70, 'hello');
  try
    Assert.AreEqual(70, StrLit.Position);
    Assert.AreEqual('hello', StrLit.StrValue);
  finally
    StrLit.Free;
  end;

  // TBoldOLWIntLiteral
  IntLit := TBoldOLWIntLiteral.Create(80, 42);
  try
    Assert.AreEqual(80, IntLit.Position);
    Assert.AreEqual(42, IntLit.IntValue);
  finally
    IntLit.Free;
  end;

  // TBoldOLWFloatLiteral
  FloatLit := TBoldOLWFloatLiteral.Create(90, 3.14);
  try
    Assert.AreEqual(90, FloatLit.Position);
    Assert.AreEqual(3.14, FloatLit.FloatValue, 0.001);
  finally
    FloatLit.Free;
  end;

  // TBoldOLWEnumLiteral
  EnumLit := TBoldOLWEnumLiteral.Create(100, 'Red');
  try
    Assert.AreEqual(100, EnumLit.Position);
    Assert.AreEqual('Red', EnumLit.Name);
  finally
    EnumLit.Free;
  end;

  // TBoldOLWDateLiteral
  DateLit := TBoldOLWDateLiteral.Create(110, EncodeDate(2025, 6, 15));
  try
    Assert.AreEqual(110, DateLit.Position);
    Assert.AreEqual(EncodeDate(2025, 6, 15), DateLit.DateValue);
  finally
    DateLit.Free;
  end;

  // TBoldOLWTimeLiteral
  TimeLit := TBoldOLWTimeLiteral.Create(120, EncodeTime(14, 30, 0, 0));
  try
    Assert.AreEqual(120, TimeLit.Position);
    Assert.AreEqual(EncodeTime(14, 30, 0, 0), TimeLit.TimeValue);
  finally
    TimeLit.Free;
  end;

  // TBoldOLWListCoercion (owns Child)
  ChildNode := TBoldOLWIntLiteral.Create(125, 99);
  ListCoercion := TBoldOLWListCoercion.Create(130, ChildNode);
  try
    Assert.AreEqual(130, ListCoercion.Position);
    Assert.AreSame(ChildNode, ListCoercion.Child);
  finally
    ListCoercion.Free; // frees ChildNode
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestAcceptVisitor_AllNodeTypes;
var
  Visitor: TTestOLWTrackingVisitor;
  TypeNode: TBoldOLWTypeNode;
  Operation: TBoldOLWOperation;
  Iteration: TBoldOLWIteration;
  Member: TBoldOLWMember;
  Binding: TBoldOLWVariableBinding;
  VarRef: TBoldOLWVariableReference;
  StrLit: TBoldOLWStrLiteral;
  IntLit: TBoldOLWIntLiteral;
  FloatLit: TBoldOLWFloatLiteral;
  EnumLit: TBoldOLWEnumLiteral;
  DateLit: TBoldOLWDateLiteral;
  TimeLit: TBoldOLWTimeLiteral;
  ListCoercion: TBoldOLWListCoercion;
begin
  // TypeNode: calls inherited (Node) + VisitTypeNode
  Visitor := TTestOLWTrackingVisitor.Create;
  TypeNode := TBoldOLWTypeNode.Create(0, 'T', 0);
  try
    TypeNode.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'TypeNode should call inherited Node');
    Assert.IsTrue(Visitor.WasVisited('TypeNode'), 'TypeNode should call VisitTypeNode');
  finally
    TypeNode.Free;
    Visitor.Free;
  end;

  // Operation: calls inherited (Node) + VisitOperation
  Visitor := TTestOLWTrackingVisitor.Create;
  Operation := TBoldOLWOperation.Create(0, 'op');
  try
    Operation.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'Operation should call inherited Node');
    Assert.IsTrue(Visitor.WasVisited('Operation'), 'Operation should call VisitOperation');
  finally
    Operation.Free;
    Visitor.Free;
  end;

  // Iteration: calls inherited chain (Node + Operation) + VisitIteration
  Visitor := TTestOLWTrackingVisitor.Create;
  Iteration := TBoldOLWIteration.Create(0, 'select', nil);
  try
    Iteration.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'Iteration should call Node');
    Assert.IsTrue(Visitor.WasVisited('Operation'), 'Iteration should call Operation');
    Assert.IsTrue(Visitor.WasVisited('Iteration'), 'Iteration should call VisitIteration');
  finally
    Iteration.Free;
    Visitor.Free;
  end;

  // Member: calls inherited (Node) + VisitMember
  Visitor := TTestOLWTrackingVisitor.Create;
  Member := TBoldOLWMember.Create(0, 'm', 0, nil, False);
  try
    Member.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'Member should call Node');
    Assert.IsTrue(Visitor.WasVisited('Member'), 'Member should call VisitMember');
  finally
    Member.Free;
    Visitor.Free;
  end;

  // VariableBinding: calls inherited (Node) + VisitVariableBinding
  Visitor := TTestOLWTrackingVisitor.Create;
  Binding := TBoldOLWVariableBinding.Create(0, 'v', 0);
  try
    Binding.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'Binding should call Node');
    Assert.IsTrue(Visitor.WasVisited('VariableBinding'), 'Binding should call VisitVariableBinding');
  finally
    Binding.Free;
    Visitor.Free;
  end;

  // VariableReference: calls inherited (Node) + VisitVariableReference
  Visitor := TTestOLWTrackingVisitor.Create;
  Binding := TBoldOLWVariableBinding.Create(0, 'v', 0);
  try
    VarRef := TBoldOLWVariableReference.Create(0, Binding);
    try
      VarRef.AcceptVisitor(Visitor);
      Assert.IsTrue(Visitor.WasVisited('Node'), 'VarRef should call Node');
      Assert.IsTrue(Visitor.WasVisited('VariableReference'), 'VarRef should call VisitVariableReference');
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
    Visitor.Free;
  end;

  // StrLiteral: calls inherited chain (Node + Literal) + VisitStrLiteral
  Visitor := TTestOLWTrackingVisitor.Create;
  StrLit := TBoldOLWStrLiteral.Create(0, 's');
  try
    StrLit.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'StrLit should call Node');
    Assert.IsTrue(Visitor.WasVisited('Literal'), 'StrLit should call Literal');
    Assert.IsTrue(Visitor.WasVisited('StrLiteral'), 'StrLit should call VisitStrLiteral');
  finally
    StrLit.Free;
    Visitor.Free;
  end;

  // IntLiteral: calls inherited chain (Node + Literal) + VisitIntLiteral
  Visitor := TTestOLWTrackingVisitor.Create;
  IntLit := TBoldOLWIntLiteral.Create(0, 0);
  try
    IntLit.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'IntLit should call Node');
    Assert.IsTrue(Visitor.WasVisited('Literal'), 'IntLit should call Literal');
    Assert.IsTrue(Visitor.WasVisited('IntLiteral'), 'IntLit should call VisitIntLiteral');
  finally
    IntLit.Free;
    Visitor.Free;
  end;

  // FloatLiteral
  Visitor := TTestOLWTrackingVisitor.Create;
  FloatLit := TBoldOLWFloatLiteral.Create(0, 0);
  try
    FloatLit.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'FloatLit should call Node');
    Assert.IsTrue(Visitor.WasVisited('Literal'), 'FloatLit should call Literal');
    Assert.IsTrue(Visitor.WasVisited('FloatLiteral'), 'FloatLit should call VisitFloatLiteral');
  finally
    FloatLit.Free;
    Visitor.Free;
  end;

  // EnumLiteral
  Visitor := TTestOLWTrackingVisitor.Create;
  EnumLit := TBoldOLWEnumLiteral.Create(0, 'E');
  try
    EnumLit.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'EnumLit should call Node');
    Assert.IsTrue(Visitor.WasVisited('Literal'), 'EnumLit should call Literal');
    Assert.IsTrue(Visitor.WasVisited('EnumLiteral'), 'EnumLit should call VisitEnumLiteral');
  finally
    EnumLit.Free;
    Visitor.Free;
  end;

  // DateLiteral
  Visitor := TTestOLWTrackingVisitor.Create;
  DateLit := TBoldOLWDateLiteral.Create(0, 0);
  try
    DateLit.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'DateLit should call Node');
    Assert.IsTrue(Visitor.WasVisited('Literal'), 'DateLit should call Literal');
    Assert.IsTrue(Visitor.WasVisited('DateLiteral'), 'DateLit should call VisitDateLiteral');
  finally
    DateLit.Free;
    Visitor.Free;
  end;

  // TimeLiteral
  Visitor := TTestOLWTrackingVisitor.Create;
  TimeLit := TBoldOLWTimeLiteral.Create(0, 0);
  try
    TimeLit.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'TimeLit should call Node');
    Assert.IsTrue(Visitor.WasVisited('Literal'), 'TimeLit should call Literal');
    Assert.IsTrue(Visitor.WasVisited('TimeLiteral'), 'TimeLit should call VisitTimeLiteral');
  finally
    TimeLit.Free;
    Visitor.Free;
  end;

  // ListCoercion
  Visitor := TTestOLWTrackingVisitor.Create;
  ListCoercion := TBoldOLWListCoercion.Create(0, nil);
  try
    ListCoercion.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'ListCoercion should call Node');
    Assert.IsTrue(Visitor.WasVisited('ListCoercion'), 'ListCoercion should call VisitListCoercion');
  finally
    ListCoercion.Free;
    Visitor.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestVisitorBaseStubs_AllCalled;
var
  BaseVisitor: TBoldOLWNodeVisitor;
  TypeNode: TBoldOLWTypeNode;
  Operation: TBoldOLWOperation;
  Iteration: TBoldOLWIteration;
  Member: TBoldOLWMember;
  Binding: TBoldOLWVariableBinding;
  VarRef: TBoldOLWVariableReference;
  StrLit: TBoldOLWStrLiteral;
  IntLit: TBoldOLWIntLiteral;
  FloatLit: TBoldOLWFloatLiteral;
  EnumLit: TBoldOLWEnumLiteral;
  DateLit: TBoldOLWDateLiteral;
  TimeLit: TBoldOLWTimeLiteral;
  ListCoercion: TBoldOLWListCoercion;
begin
  // Calling AcceptVisitor on all node types with the base visitor
  // exercises all empty VisitXxx stubs without raising exceptions
  BaseVisitor := TBoldOLWNodeVisitor.Create;
  try
    TypeNode := TBoldOLWTypeNode.Create(0, 'T', 0);
    try TypeNode.AcceptVisitor(BaseVisitor); finally TypeNode.Free; end;

    Operation := TBoldOLWOperation.Create(0, 'op');
    try Operation.AcceptVisitor(BaseVisitor); finally Operation.Free; end;

    Iteration := TBoldOLWIteration.Create(0, 'it', nil);
    try Iteration.AcceptVisitor(BaseVisitor); finally Iteration.Free; end;

    Member := TBoldOLWMember.Create(0, 'm', 0, nil, False);
    try Member.AcceptVisitor(BaseVisitor); finally Member.Free; end;

    Binding := TBoldOLWVariableBinding.Create(0, 'v', 0);
    try
      Binding.AcceptVisitor(BaseVisitor);
      VarRef := TBoldOLWVariableReference.Create(0, Binding);
      try VarRef.AcceptVisitor(BaseVisitor); finally VarRef.Free; end;
    finally
      Binding.Free;
    end;

    StrLit := TBoldOLWStrLiteral.Create(0, 's');
    try StrLit.AcceptVisitor(BaseVisitor); finally StrLit.Free; end;

    IntLit := TBoldOLWIntLiteral.Create(0, 0);
    try IntLit.AcceptVisitor(BaseVisitor); finally IntLit.Free; end;

    FloatLit := TBoldOLWFloatLiteral.Create(0, 0);
    try FloatLit.AcceptVisitor(BaseVisitor); finally FloatLit.Free; end;

    EnumLit := TBoldOLWEnumLiteral.Create(0, 'E');
    try EnumLit.AcceptVisitor(BaseVisitor); finally EnumLit.Free; end;

    DateLit := TBoldOLWDateLiteral.Create(0, 0);
    try DateLit.AcceptVisitor(BaseVisitor); finally DateLit.Free; end;

    TimeLit := TBoldOLWTimeLiteral.Create(0, 0);
    try TimeLit.AcceptVisitor(BaseVisitor); finally TimeLit.Free; end;

    ListCoercion := TBoldOLWListCoercion.Create(0, nil);
    try ListCoercion.AcceptVisitor(BaseVisitor); finally ListCoercion.Free; end;

    Assert.Pass('All base visitor stubs executed without exception');
  finally
    BaseVisitor.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestVisitorBaseStub_VisitNode_Direct;
var
  BaseVisitor: TBoldOLWNodeVisitor;
  IntLit: TBoldOLWIntLiteral;
begin
  // TBoldOLWNode.AcceptVisitor calls VisitTBoldOLWNode directly
  // Test via a concrete node type that calls inherited
  BaseVisitor := TBoldOLWNodeVisitor.Create;
  IntLit := TBoldOLWIntLiteral.Create(5, 99);
  try
    IntLit.AcceptVisitor(BaseVisitor);
    Assert.Pass('VisitTBoldOLWNode base stub executed');
  finally
    IntLit.Free;
    BaseVisitor.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestGetStreamName_AllTypes;
var
  TypeNode: TBoldOLWTypeNode;
  Operation: TBoldOLWOperation;
  Iteration: TBoldOLWIteration;
  Member: TBoldOLWMember;
  Binding: TBoldOLWVariableBinding;
  VarRef: TBoldOLWVariableReference;
  StrLit: TBoldOLWStrLiteral;
  IntLit: TBoldOLWIntLiteral;
  FloatLit: TBoldOLWFloatLiteral;
  EnumLit: TBoldOLWEnumLiteral;
  DateLit: TBoldOLWDateLiteral;
  TimeLit: TBoldOLWTimeLiteral;
  ListCoercion: TBoldOLWListCoercion;
  NodeList: TBoldOLWNodeList;
  Condition: TBoldOclCondition;
begin
  TypeNode := TBoldOLWTypeNode.Create(0, 'T', 0);
  try Assert.AreEqual('OLWTypeNode', GetNodeStreamName(TypeNode)); finally TypeNode.Free; end;

  Operation := TBoldOLWOperation.Create(0, 'op');
  try Assert.AreEqual('OLWOperation', GetNodeStreamName(Operation)); finally Operation.Free; end;

  Iteration := TBoldOLWIteration.Create(0, 'it', nil);
  try Assert.AreEqual('OLWIteration', GetNodeStreamName(Iteration)); finally Iteration.Free; end;

  Member := TBoldOLWMember.Create(0, 'm', 0, nil, False);
  try Assert.AreEqual('OLWMember', GetNodeStreamName(Member)); finally Member.Free; end;

  Binding := TBoldOLWVariableBinding.Create(0, 'v', 0);
  try Assert.AreEqual('OLWVariableBinding', GetNodeStreamName(Binding)); finally Binding.Free; end;

  Binding := TBoldOLWVariableBinding.Create(0, 'v', 0);
  try
    VarRef := TBoldOLWVariableReference.Create(0, Binding);
    try Assert.AreEqual('OLWVariableReference', GetNodeStreamName(VarRef)); finally VarRef.Free; end;
  finally
    Binding.Free;
  end;

  StrLit := TBoldOLWStrLiteral.Create(0, 's');
  try Assert.AreEqual('OLWStrLiteral', GetNodeStreamName(StrLit)); finally StrLit.Free; end;

  IntLit := TBoldOLWIntLiteral.Create(0, 0);
  try Assert.AreEqual('OLWIntLiteral', GetNodeStreamName(IntLit)); finally IntLit.Free; end;

  FloatLit := TBoldOLWFloatLiteral.Create(0, 0);
  try Assert.AreEqual('OLWFloatLiteral', GetNodeStreamName(FloatLit)); finally FloatLit.Free; end;

  EnumLit := TBoldOLWEnumLiteral.Create(0, 'E');
  try Assert.AreEqual('OLWEnumLiteral', GetNodeStreamName(EnumLit)); finally EnumLit.Free; end;

  DateLit := TBoldOLWDateLiteral.Create(0, 0);
  try Assert.AreEqual('OLWDateLiteral', GetNodeStreamName(DateLit)); finally DateLit.Free; end;

  TimeLit := TBoldOLWTimeLiteral.Create(0, 0);
  try Assert.AreEqual('OLWTimeLiteral', GetNodeStreamName(TimeLit)); finally TimeLit.Free; end;

  ListCoercion := TBoldOLWListCoercion.Create(0, nil);
  try Assert.AreEqual('OLWListCoercion', GetNodeStreamName(ListCoercion)); finally ListCoercion.Free; end;

  NodeList := TBoldOLWNodeList.Create;
  try Assert.AreEqual('OLWNodeList', GetListStreamName(NodeList)); finally NodeList.Free; end;

  Condition := TBoldOclCondition.Create;
  try Assert.AreEqual('OCLCondition', Condition.GetStreamName); finally Condition.Free; end;
end;

procedure TTestBoldOclLightWeightNodes.TestNodeList_AddAndCount;
var
  List: TBoldOLWNodeList;
begin
  List := TBoldOLWNodeList.Create;
  try
    Assert.AreEqual(0, List.Count);
    List.Add(TBoldOLWIntLiteral.Create(0, 1));
    Assert.AreEqual(1, List.Count);
    List.Add(TBoldOLWIntLiteral.Create(0, 2));
    Assert.AreEqual(2, List.Count);
    List.Add(TBoldOLWIntLiteral.Create(0, 3));
    Assert.AreEqual(3, List.Count);
  finally
    List.Free; // OwnsObjects=true, frees items
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestNodeList_IndexOf;
var
  List: TBoldOLWNodeList;
  Node1, Node2: TBoldOLWIntLiteral;
  NotInList: TBoldOLWIntLiteral;
begin
  List := TBoldOLWNodeList.Create;
  Node1 := TBoldOLWIntLiteral.Create(0, 10);
  Node2 := TBoldOLWIntLiteral.Create(0, 20);
  NotInList := TBoldOLWIntLiteral.Create(0, 30);
  try
    List.Add(Node1);
    List.Add(Node2);
    Assert.AreEqual(0, List.IndexOf[Node1]);
    Assert.AreEqual(1, List.IndexOf[Node2]);
    Assert.AreEqual(-1, List.IndexOf[NotInList]);
  finally
    List.Free;
    NotInList.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestNodeList_GetItem;
var
  List: TBoldOLWNodeList;
  Node1, Node2: TBoldOLWIntLiteral;
begin
  List := TBoldOLWNodeList.Create;
  Node1 := TBoldOLWIntLiteral.Create(0, 10);
  Node2 := TBoldOLWIntLiteral.Create(0, 20);
  try
    List.Add(Node1);
    List.Add(Node2);
    Assert.AreSame(Node1, List[0]);
    Assert.AreSame(Node2, List[1]);
  finally
    List.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestNodeList_PutItem;
var
  List: TBoldOLWNodeList;
  Node1, Replacement: TBoldOLWIntLiteral;
begin
  List := TBoldOLWNodeList.Create;
  List.OwnsObjects := False;
  Node1 := TBoldOLWIntLiteral.Create(0, 10);
  Replacement := TBoldOLWIntLiteral.Create(0, 99);
  try
    List.Add(Node1);
    Assert.AreSame(Node1, List[0]);
    List[0] := Replacement;
    Assert.AreSame(Replacement, List[0]);
  finally
    List.Free;
    Node1.Free;
    Replacement.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestNodeList_TraverseList;
var
  List: TBoldOLWNodeList;
  Visitor: TTestOLWTrackingVisitor;
begin
  List := TBoldOLWNodeList.Create;
  Visitor := TTestOLWTrackingVisitor.Create;
  try
    List.Add(TBoldOLWIntLiteral.Create(0, 1));
    List.Add(TBoldOLWStrLiteral.Create(0, 'x'));
    List.Add(TBoldOLWFloatLiteral.Create(0, 1.5));
    List.TraverseList(Visitor);
    Assert.IsTrue(Visitor.WasVisited('IntLiteral'), 'TraverseList should visit IntLiteral');
    Assert.IsTrue(Visitor.WasVisited('StrLiteral'), 'TraverseList should visit StrLiteral');
    Assert.IsTrue(Visitor.WasVisited('FloatLiteral'), 'TraverseList should visit FloatLiteral');
  finally
    List.Free;
    Visitor.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestNodeList_DestroyOwned;
var
  List: TBoldOLWNodeList;
begin
  List := TBoldOLWNodeList.Create;
  Assert.IsTrue(List.OwnsObjects, 'Default OwnsObjects should be True');
  List.Add(TBoldOLWIntLiteral.Create(0, 1));
  List.Add(TBoldOLWIntLiteral.Create(0, 2));
  List.Add(TBoldOLWIntLiteral.Create(0, 3));
  // Destroy with OwnsObjects=true should free all items without AV
  List.Free;
  Assert.Pass('Destroy with OwnsObjects=true completed');
end;

procedure TTestBoldOclLightWeightNodes.TestNodeList_DestroyNonOwned;
var
  List: TBoldOLWNodeList;
  Node1, Node2: TBoldOLWIntLiteral;
begin
  List := TBoldOLWNodeList.Create;
  List.OwnsObjects := False;
  Node1 := TBoldOLWIntLiteral.Create(0, 1);
  Node2 := TBoldOLWIntLiteral.Create(0, 2);
  try
    List.Add(Node1);
    List.Add(Node2);
    List.Free; // should NOT free items
    // Items should still be accessible (not freed)
    Assert.AreEqual(1, Node1.IntValue);
    Assert.AreEqual(2, Node2.IntValue);
  finally
    Node1.Free;
    Node2.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestNodeList_GetStreamName;
var
  List: TBoldOLWNodeList;
begin
  List := TBoldOLWNodeList.Create;
  try
    Assert.AreEqual('OLWNodeList', GetListStreamName(List));
  finally
    List.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestOperation_DestroyFreesArgs;
var
  Operation: TBoldOLWOperation;
begin
  Operation := TBoldOLWOperation.Create(0, 'test');
  Assert.IsNotNull(Operation.Args, 'Args should be created');
  Operation.Args.Add(TBoldOLWIntLiteral.Create(0, 1));
  Operation.Args.Add(TBoldOLWIntLiteral.Create(0, 2));
  // Destroy should free Args list and its owned items
  Operation.Free;
  Assert.Pass('Operation.Destroy freed Args without error');
end;

procedure TTestBoldOclLightWeightNodes.TestIteration_DestroyFreesLoopVar;
var
  Iteration: TBoldOLWIteration;
  Binding: TBoldOLWVariableBinding;
begin
  Binding := TBoldOLWVariableBinding.Create(0, 'i', 0);
  Iteration := TBoldOLWIteration.Create(0, 'collect', Binding);
  Assert.AreSame(Binding, Iteration.LoopVar);
  // Destroy should free LoopVar
  Iteration.Free;
  Assert.Pass('Iteration.Destroy freed LoopVar without error');
end;

procedure TTestBoldOclLightWeightNodes.TestMember_CreateAndDestroy;
var
  Member: TBoldOLWMember;
  MemberOf: TBoldOLWIntLiteral;
begin
  MemberOf := TBoldOLWIntLiteral.Create(0, 0);
  Member := TBoldOLWMember.Create(5, 'attr', 7, MemberOf, True);
  Assert.AreEqual('attr', Member.MemberName);
  Assert.AreEqual(7, Member.MemberIndex);
  Assert.AreSame(MemberOf, Member.MemberOf);
  Assert.IsNotNull(Member.Qualifier, 'Qualifier should be created');
  Assert.IsTrue(Member.IsBoolean);
  // Destroy should free MemberOf and Qualifier
  Member.Free;
  Assert.Pass('Member.Destroy freed MemberOf and Qualifier without error');
end;

procedure TTestBoldOclLightWeightNodes.TestListCoercion_DestroyFreesChild;
var
  ListCoercion: TBoldOLWListCoercion;
  Child: TBoldOLWIntLiteral;
begin
  Child := TBoldOLWIntLiteral.Create(0, 42);
  ListCoercion := TBoldOLWListCoercion.Create(0, Child);
  Assert.AreSame(Child, ListCoercion.Child);
  // Destroy should free Child
  ListCoercion.Free;
  Assert.Pass('ListCoercion.Destroy freed Child without error');
end;

procedure TTestBoldOclLightWeightNodes.TestOclCondition_CreateAndDestroy;
var
  Condition: TBoldOclCondition;
begin
  Condition := TBoldOclCondition.Create;
  try
    Assert.IsNotNull(Condition.Context, 'Context should be created');
    Assert.IsNotNull(Condition.Env, 'Env should be created');
    Assert.AreEqual('OCLCondition', Condition.GetStreamName);

    // Set properties
    Condition.OclExpr := 'self.name';
    Assert.AreEqual('self.name', Condition.OclExpr);

    // Set RootNode (owned by Condition)
    Condition.RootNode := TBoldOLWIntLiteral.Create(0, 1);
    Assert.IsNotNull(Condition.RootNode);
  finally
    Condition.Free; // should free Context, Env, and RootNode
  end;
  Assert.Pass('OclCondition create/destroy completed');
end;

procedure TTestBoldOclLightWeightNodes.TestVariableBinding_AddRef_FirstCall;
var
  Binding: TBoldOLWVariableBinding;
begin
  Binding := TBoldOLWVariableBinding.Create(0, 'x', 0);
  try
    // First AddRef should succeed (refcount goes from 0 to 1)
    Binding.AddRef;
    Assert.Pass('First AddRef succeeded');
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestVariableBinding_AddRef_LoopVar_MultipleRefs;
var
  Binding: TBoldOLWVariableBinding;
begin
  Binding := TBoldOLWVariableBinding.Create(0, 'i', 0);
  try
    Binding.IsLoopVar := True;
    Binding.AddRef;
    Binding.AddRef; // second ref allowed for loop vars
    Binding.AddRef; // third ref allowed for loop vars
    Assert.Pass('Multiple AddRef calls succeeded for loop var');
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestVariableBinding_AddRef_NonLoopVar_Raises;
var
  Binding: TBoldOLWVariableBinding;
begin
  Binding := TBoldOLWVariableBinding.Create(0, 'ext', 0);
  try
    Binding.AddRef; // first call OK
    Assert.WillRaise(
      procedure
      begin
        Binding.AddRef; // second call on non-loopvar should raise
      end,
      EBold
    );
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestVariableBinding_Properties;
var
  Binding: TBoldOLWVariableBinding;
begin
  Binding := TBoldOLWVariableBinding.Create(10, 'myVar', 3);
  try
    Assert.AreEqual('myVar', Binding.VariableName);
    Assert.AreEqual(3, Binding.TopSortedIndex);
    Assert.AreEqual(10, Binding.Position);

    // IsLoopVar defaults to False
    Assert.IsFalse(Binding.IsLoopVar);
    Binding.IsLoopVar := True;
    Assert.IsTrue(Binding.IsLoopVar);

    // ExternalVarValue
    Binding.ExternalVarValue := 42;
    Assert.AreEqual(42, Integer(Binding.ExternalVarValue));
    Binding.ExternalVarValue := 'test';
    Assert.AreEqual('test', string(Binding.ExternalVarValue));
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestEnumLiteral_IntValue;
var
  EnumLit: TBoldOLWEnumLiteral;
begin
  EnumLit := TBoldOLWEnumLiteral.Create(0, 'Green');
  try
    Assert.AreEqual('Green', EnumLit.Name);
    EnumLit.IntValue := 2;
    Assert.AreEqual(2, EnumLit.IntValue);
    EnumLit.IntValue := 0;
    Assert.AreEqual(0, EnumLit.IntValue);
  finally
    EnumLit.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestDateLiteral_DateValue;
var
  DateLit: TBoldOLWDateLiteral;
  TestDate: TDateTime;
begin
  TestDate := EncodeDate(2025, 12, 25);
  DateLit := TBoldOLWDateLiteral.Create(0, TestDate);
  try
    Assert.AreEqual(TestDate, DateLit.DateValue);
    DateLit.DateValue := EncodeDate(2026, 1, 1);
    Assert.AreEqual(EncodeDate(2026, 1, 1), DateLit.DateValue);
  finally
    DateLit.Free;
  end;
end;

procedure TTestBoldOclLightWeightNodes.TestTimeLiteral_TimeValue;
var
  TimeLit: TBoldOLWTimeLiteral;
  TestTime: TDateTime;
begin
  TestTime := EncodeTime(10, 30, 45, 0);
  TimeLit := TBoldOLWTimeLiteral.Create(0, TestTime);
  try
    Assert.AreEqual(TestTime, TimeLit.TimeValue);
    TimeLit.TimeValue := EncodeTime(23, 59, 59, 0);
    Assert.AreEqual(EncodeTime(23, 59, 59, 0), TimeLit.TimeValue);
  finally
    TimeLit.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldOclLightWeightNodes);

end.
