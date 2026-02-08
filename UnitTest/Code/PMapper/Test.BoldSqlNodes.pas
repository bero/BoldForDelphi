unit Test.BoldSqlNodes;

interface

uses
  DUnitX.TestFramework,
  BoldSqlNodes;

type
  TTestSqlTrackingVisitor = class(TBoldSqlNodeVisitor)
  private
    fVisitedMethods: string;
  protected
    procedure VisitTBoldSqlNode(N: TBoldSqlNode); override;
    procedure VisitTBoldSqlListCoercion(N: TBoldSqlListCoercion); override;
    procedure VisitTBoldSqlOperation(N: TBoldSqlOperation); override;
    procedure VisitTBoldSqlIteration(N: TBoldSqlIteration); override;
    procedure VisitTBoldSqlMember(N: TBoldSqlMember); override;
    procedure VisitTBoldSqlLiteral(N: TBoldSqlLiteral); override;
    procedure VisitTBoldSqlStrLiteral(N: TBoldSqlStrLiteral); override;
    procedure VisitTBoldSqlFloatLiteral(N: TBoldSqlFloatLiteral); override;
    procedure VisitTBoldSqlEnumLiteral(N: TBoldSqlEnumLiteral); override;
    procedure VisitTBoldSqlIntLiteral(N: TBoldSqlIntLiteral); override;
    procedure VisitTBoldSqlDateLiteral(N: TBoldSqlDateLiteral); override;
    procedure VisitTBoldSqlTimeLiteral(N: TBoldSqlTimeLiteral); override;
    procedure VisitTBoldSqlVariableBinding(N: TBoldSqlVariableBinding); override;
    procedure VisitTBoldSqlVariableReference(N: TBoldSqlVariableReference); override;
    procedure VisitTBoldSqlTypeNode(N: TBoldSqlTypeNode); override;
  public
    function WasVisited(const MethodName: string): Boolean;
    property VisitedMethods: string read fVisitedMethods;
  end;

  TTestSqlSymbol = class(TBoldSqlSymbol)
  private
    fName: string;
  protected
    function GetName: String; override;
  public
    constructor Create(const AName: string);
  end;

  [TestFixture]
  TTestBoldSqlNodes = class
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
    procedure TestLiteral_AsString_AllTypes;
    [Test]
    [Category('Quick')]
    procedure TestNodeList_AddAndCount;
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
    procedure TestNodeList_DestroyFreesItems;
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
    procedure TestVariableBinding_AddRef;
    [Test]
    [Category('Quick')]
    procedure TestVariableBinding_AddRef_NonLoopVar_Raises;
    [Test]
    [Category('Quick')]
    procedure TestVariableBinding_DecRef;
    [Test]
    [Category('Quick')]
    procedure TestVariableBinding_DecRef_ZeroRefCount_Raises;
    [Test]
    [Category('Quick')]
    procedure TestVariableBinding_Properties;
    [Test]
    [Category('Quick')]
    procedure TestVariableReference_IsExternalVariable;
    [Test]
    [Category('Quick')]
    procedure TestVariableReference_SetObjectMapper_Raises;
    [Test]
    [Category('Quick')]
    procedure TestVariableReference_SetQuery_Raises;
    [Test]
    [Category('Quick')]
    procedure TestVariableReference_GetHasQuery;
    [Test]
    [Category('Quick')]
    procedure TestNode_HasQuery_HasObjectMapper;
    [Test]
    [Category('Quick')]
    procedure TestNode_RelinquishWCF_NilCase;
    [Test]
    [Category('Quick')]
    procedure TestNode_RelinquishWCF_WithWCFAssigned;
    [Test]
    [Category('Quick')]
    procedure TestVariableReference_RelinquishWCF;
    [Test]
    [Category('Quick')]
    procedure TestMember_QueryOfMemberOfIsEnclosing;
    [Test]
    [Category('Quick')]
    procedure TestSymbol_Methods;
    [Test]
    [Category('Quick')]
    procedure TestWCFVariable_CreateAndDestroy;
  end;

implementation

uses
  SysUtils,
  Variants,
  BoldDefs,
  BoldId;

{ TTestSqlTrackingVisitor }

procedure TTestSqlTrackingVisitor.VisitTBoldSqlNode(N: TBoldSqlNode);
begin
  fVisitedMethods := fVisitedMethods + 'Node,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlListCoercion(N: TBoldSqlListCoercion);
begin
  fVisitedMethods := fVisitedMethods + 'ListCoercion,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlOperation(N: TBoldSqlOperation);
begin
  fVisitedMethods := fVisitedMethods + 'Operation,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlIteration(N: TBoldSqlIteration);
begin
  fVisitedMethods := fVisitedMethods + 'Iteration,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlMember(N: TBoldSqlMember);
begin
  fVisitedMethods := fVisitedMethods + 'Member,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlLiteral(N: TBoldSqlLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'Literal,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlStrLiteral(N: TBoldSqlStrLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'StrLiteral,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlFloatLiteral(N: TBoldSqlFloatLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'FloatLiteral,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlEnumLiteral(N: TBoldSqlEnumLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'EnumLiteral,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlIntLiteral(N: TBoldSqlIntLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'IntLiteral,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlDateLiteral(N: TBoldSqlDateLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'DateLiteral,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlTimeLiteral(N: TBoldSqlTimeLiteral);
begin
  fVisitedMethods := fVisitedMethods + 'TimeLiteral,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlVariableBinding(N: TBoldSqlVariableBinding);
begin
  fVisitedMethods := fVisitedMethods + 'VariableBinding,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlVariableReference(N: TBoldSqlVariableReference);
begin
  fVisitedMethods := fVisitedMethods + 'VariableReference,';
end;

procedure TTestSqlTrackingVisitor.VisitTBoldSqlTypeNode(N: TBoldSqlTypeNode);
begin
  fVisitedMethods := fVisitedMethods + 'TypeNode,';
end;

function TTestSqlTrackingVisitor.WasVisited(const MethodName: string): Boolean;
begin
  Result := Pos(MethodName + ',', fVisitedMethods) > 0;
end;

{ TTestSqlSymbol }

constructor TTestSqlSymbol.Create(const AName: string);
begin
  inherited Create;
  fName := AName;
end;

function TTestSqlSymbol.GetName: String;
begin
  Result := fName;
end;

{ TTestBoldSqlNodes }

procedure TTestBoldSqlNodes.TestNodeCreation_AllTypes;
var
  Node: TBoldSqlNode;
  TypeNode: TBoldSqlTypeNode;
  Operation: TBoldSqlOperation;
  Binding: TBoldSqlVariableBinding;
  Iteration: TBoldSqlIteration;
  Member: TBoldSqlMember;
  VarRef: TBoldSqlVariableReference;
  StrLit: TBoldSqlStrLiteral;
  IntLit: TBoldSqlIntLiteral;
  FloatLit: TBoldSqlFloatLiteral;
  EnumLit: TBoldSqlEnumLiteral;
  DateLit: TBoldSqlDateLiteral;
  TimeLit: TBoldSqlTimeLiteral;
  ListCoercion: TBoldSqlListCoercion;
  ChildNode: TBoldSqlIntLiteral;
  MemberOf: TBoldSqlIntLiteral;
begin
  // TBoldSqlNode
  Node := TBoldSqlNode.Create(1);
  try
    Assert.AreEqual(1, Node.Position);
  finally
    Node.Free;
  end;

  // TBoldSqlTypeNode
  TypeNode := TBoldSqlTypeNode.Create(10, 'Person', 5);
  try
    Assert.AreEqual(10, TypeNode.Position);
    Assert.AreEqual('Person', TypeNode.TypeName);
    Assert.AreEqual(5, TypeNode.TopSortedIndex);
  finally
    TypeNode.Free;
  end;

  // TBoldSqlOperation
  Operation := TBoldSqlOperation.Create(20, 'includes');
  try
    Assert.AreEqual(20, Operation.Position);
    Assert.AreEqual('includes', Operation.OperationName);
    Assert.IsNotNull(Operation.Args, 'Args list should be created');
    Assert.AreEqual(0, Operation.Args.Count);
  finally
    Operation.Free;
  end;

  // TBoldSqlVariableBinding
  Binding := TBoldSqlVariableBinding.Create(30, 'self', 0);
  try
    Assert.AreEqual(30, Binding.Position);
    Assert.AreEqual('self', Binding.VariableName);
    Assert.AreEqual(0, Binding.TopSortedIndex);
  finally
    Binding.Free;
  end;

  // TBoldSqlIteration (owns LoopVar)
  Binding := TBoldSqlVariableBinding.Create(35, 'e', 1);
  Iteration := TBoldSqlIteration.Create(40, 'select', Binding);
  try
    Assert.AreEqual(40, Iteration.Position);
    Assert.AreEqual('select', Iteration.OperationName);
    Assert.AreSame(Binding, Iteration.LoopVar);
    Assert.IsNotNull(Iteration.Args, 'Iteration.Args should be created');
  finally
    Iteration.Free; // frees Binding too
  end;

  // TBoldSqlMember (owns MemberOf and Qualifier)
  MemberOf := TBoldSqlIntLiteral.Create(0, 0);
  Member := TBoldSqlMember.Create(50, 'name', 3, MemberOf, True);
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

  // TBoldSqlVariableReference (does NOT own binding)
  Binding := TBoldSqlVariableBinding.Create(55, 'x', 2);
  try
    VarRef := TBoldSqlVariableReference.Create(60, Binding);
    try
      Assert.AreEqual(60, VarRef.Position);
      Assert.AreSame(Binding, VarRef.VariableBinding);
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
  end;

  // TBoldSqlStrLiteral
  StrLit := TBoldSqlStrLiteral.Create(70, 'hello');
  try
    Assert.AreEqual(70, StrLit.Position);
    Assert.AreEqual('hello', StrLit.StrValue);
  finally
    StrLit.Free;
  end;

  // TBoldSqlIntLiteral
  IntLit := TBoldSqlIntLiteral.Create(80, 42);
  try
    Assert.AreEqual(80, IntLit.Position);
    Assert.AreEqual(42, IntLit.IntValue);
  finally
    IntLit.Free;
  end;

  // TBoldSqlFloatLiteral
  FloatLit := TBoldSqlFloatLiteral.Create(90, 3.14);
  try
    Assert.AreEqual(90, FloatLit.Position);
    Assert.AreEqual(3.14, FloatLit.FloatValue, 0.001);
  finally
    FloatLit.Free;
  end;

  // TBoldSqlEnumLiteral
  EnumLit := TBoldSqlEnumLiteral.Create(100, 7, 'Red');
  try
    Assert.AreEqual(100, EnumLit.Position);
    Assert.AreEqual('Red', EnumLit.Name);
    Assert.AreEqual(7, EnumLit.IntValue);
  finally
    EnumLit.Free;
  end;

  // TBoldSqlDateLiteral
  DateLit := TBoldSqlDateLiteral.Create(110, EncodeDate(2025, 6, 15));
  try
    Assert.AreEqual(110, DateLit.Position);
    Assert.AreEqual(EncodeDate(2025, 6, 15), DateLit.DateValue);
  finally
    DateLit.Free;
  end;

  // TBoldSqlTimeLiteral
  TimeLit := TBoldSqlTimeLiteral.Create(120, EncodeTime(14, 30, 0, 0));
  try
    Assert.AreEqual(120, TimeLit.Position);
    Assert.AreEqual(EncodeTime(14, 30, 0, 0), TimeLit.TimeValue);
  finally
    TimeLit.Free;
  end;

  // TBoldSqlListCoercion (owns Child)
  ChildNode := TBoldSqlIntLiteral.Create(125, 99);
  ListCoercion := TBoldSqlListCoercion.Create(130, ChildNode);
  try
    Assert.AreEqual(130, ListCoercion.Position);
    Assert.AreSame(ChildNode, ListCoercion.Child);
  finally
    ListCoercion.Free; // frees ChildNode
  end;
end;

procedure TTestBoldSqlNodes.TestAcceptVisitor_AllNodeTypes;
var
  Visitor: TTestSqlTrackingVisitor;
  TypeNode: TBoldSqlTypeNode;
  Operation: TBoldSqlOperation;
  Iteration: TBoldSqlIteration;
  Member: TBoldSqlMember;
  Binding: TBoldSqlVariableBinding;
  VarRef: TBoldSqlVariableReference;
  StrLit: TBoldSqlStrLiteral;
  IntLit: TBoldSqlIntLiteral;
  FloatLit: TBoldSqlFloatLiteral;
  EnumLit: TBoldSqlEnumLiteral;
  DateLit: TBoldSqlDateLiteral;
  TimeLit: TBoldSqlTimeLiteral;
  ListCoercion: TBoldSqlListCoercion;
begin
  // TypeNode: calls inherited (Node) + VisitTypeNode
  Visitor := TTestSqlTrackingVisitor.Create;
  TypeNode := TBoldSqlTypeNode.Create(0, 'T', 0);
  try
    TypeNode.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'TypeNode should call inherited Node');
    Assert.IsTrue(Visitor.WasVisited('TypeNode'), 'TypeNode should call VisitTypeNode');
  finally
    TypeNode.Free;
    Visitor.Free;
  end;

  // Operation: calls inherited (Node) + VisitOperation
  Visitor := TTestSqlTrackingVisitor.Create;
  Operation := TBoldSqlOperation.Create(0, 'op');
  try
    Operation.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'Operation should call inherited Node');
    Assert.IsTrue(Visitor.WasVisited('Operation'), 'Operation should call VisitOperation');
  finally
    Operation.Free;
    Visitor.Free;
  end;

  // Iteration: calls inherited chain (Node + Operation) + VisitIteration
  Visitor := TTestSqlTrackingVisitor.Create;
  Iteration := TBoldSqlIteration.Create(0, 'select', nil);
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
  Visitor := TTestSqlTrackingVisitor.Create;
  Member := TBoldSqlMember.Create(0, 'm', 0, nil, False);
  try
    Member.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'Member should call Node');
    Assert.IsTrue(Visitor.WasVisited('Member'), 'Member should call VisitMember');
  finally
    Member.Free;
    Visitor.Free;
  end;

  // VariableBinding: calls inherited (Node) + VisitVariableBinding
  Visitor := TTestSqlTrackingVisitor.Create;
  Binding := TBoldSqlVariableBinding.Create(0, 'v', 0);
  try
    Binding.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'Binding should call Node');
    Assert.IsTrue(Visitor.WasVisited('VariableBinding'), 'Binding should call VisitVariableBinding');
  finally
    Binding.Free;
    Visitor.Free;
  end;

  // VariableReference: calls inherited (Node) + VisitVariableReference
  Visitor := TTestSqlTrackingVisitor.Create;
  Binding := TBoldSqlVariableBinding.Create(0, 'v', 0);
  try
    VarRef := TBoldSqlVariableReference.Create(0, Binding);
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

  // StrLiteral: calls VisitStrLiteral
  Visitor := TTestSqlTrackingVisitor.Create;
  StrLit := TBoldSqlStrLiteral.Create(0, 's');
  try
    StrLit.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('StrLiteral'), 'StrLit should call VisitStrLiteral');
  finally
    StrLit.Free;
    Visitor.Free;
  end;

  // IntLiteral: calls inherited chain (Node + Literal) + VisitIntLiteral
  Visitor := TTestSqlTrackingVisitor.Create;
  IntLit := TBoldSqlIntLiteral.Create(0, 0);
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
  Visitor := TTestSqlTrackingVisitor.Create;
  FloatLit := TBoldSqlFloatLiteral.Create(0, 0);
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
  Visitor := TTestSqlTrackingVisitor.Create;
  EnumLit := TBoldSqlEnumLiteral.Create(0, 0, 'E');
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
  Visitor := TTestSqlTrackingVisitor.Create;
  DateLit := TBoldSqlDateLiteral.Create(0, 0);
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
  Visitor := TTestSqlTrackingVisitor.Create;
  TimeLit := TBoldSqlTimeLiteral.Create(0, 0);
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
  Visitor := TTestSqlTrackingVisitor.Create;
  ListCoercion := TBoldSqlListCoercion.Create(0, nil);
  try
    ListCoercion.AcceptVisitor(Visitor);
    Assert.IsTrue(Visitor.WasVisited('Node'), 'ListCoercion should call Node');
    Assert.IsTrue(Visitor.WasVisited('ListCoercion'), 'ListCoercion should call VisitListCoercion');
  finally
    ListCoercion.Free;
    Visitor.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVisitorBaseStubs_AllCalled;
var
  BaseVisitor: TBoldSqlNodeVisitor;
  TypeNode: TBoldSqlTypeNode;
  Operation: TBoldSqlOperation;
  Iteration: TBoldSqlIteration;
  Member: TBoldSqlMember;
  Binding: TBoldSqlVariableBinding;
  VarRef: TBoldSqlVariableReference;
  StrLit: TBoldSqlStrLiteral;
  IntLit: TBoldSqlIntLiteral;
  FloatLit: TBoldSqlFloatLiteral;
  EnumLit: TBoldSqlEnumLiteral;
  DateLit: TBoldSqlDateLiteral;
  TimeLit: TBoldSqlTimeLiteral;
  ListCoercion: TBoldSqlListCoercion;
begin
  BaseVisitor := TBoldSqlNodeVisitor.Create;
  try
    TypeNode := TBoldSqlTypeNode.Create(0, 'T', 0);
    try TypeNode.AcceptVisitor(BaseVisitor); finally TypeNode.Free; end;

    Operation := TBoldSqlOperation.Create(0, 'op');
    try Operation.AcceptVisitor(BaseVisitor); finally Operation.Free; end;

    Iteration := TBoldSqlIteration.Create(0, 'it', nil);
    try Iteration.AcceptVisitor(BaseVisitor); finally Iteration.Free; end;

    Member := TBoldSqlMember.Create(0, 'm', 0, nil, False);
    try Member.AcceptVisitor(BaseVisitor); finally Member.Free; end;

    Binding := TBoldSqlVariableBinding.Create(0, 'v', 0);
    try
      Binding.AcceptVisitor(BaseVisitor);
      VarRef := TBoldSqlVariableReference.Create(0, Binding);
      try VarRef.AcceptVisitor(BaseVisitor); finally VarRef.Free; end;
    finally
      Binding.Free;
    end;

    StrLit := TBoldSqlStrLiteral.Create(0, 's');
    try StrLit.AcceptVisitor(BaseVisitor); finally StrLit.Free; end;

    IntLit := TBoldSqlIntLiteral.Create(0, 0);
    try IntLit.AcceptVisitor(BaseVisitor); finally IntLit.Free; end;

    FloatLit := TBoldSqlFloatLiteral.Create(0, 0);
    try FloatLit.AcceptVisitor(BaseVisitor); finally FloatLit.Free; end;

    EnumLit := TBoldSqlEnumLiteral.Create(0, 0, 'E');
    try EnumLit.AcceptVisitor(BaseVisitor); finally EnumLit.Free; end;

    DateLit := TBoldSqlDateLiteral.Create(0, 0);
    try DateLit.AcceptVisitor(BaseVisitor); finally DateLit.Free; end;

    TimeLit := TBoldSqlTimeLiteral.Create(0, 0);
    try TimeLit.AcceptVisitor(BaseVisitor); finally TimeLit.Free; end;

    ListCoercion := TBoldSqlListCoercion.Create(0, nil);
    try ListCoercion.AcceptVisitor(BaseVisitor); finally ListCoercion.Free; end;

    Assert.Pass('All base visitor stubs executed without exception');
  finally
    BaseVisitor.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestLiteral_AsString_AllTypes;
var
  StrLit: TBoldSqlStrLiteral;
  IntLit: TBoldSqlIntLiteral;
  FloatLit: TBoldSqlFloatLiteral;
  EnumLit: TBoldSqlEnumLiteral;
  DateLit: TBoldSqlDateLiteral;
  TimeLit: TBoldSqlTimeLiteral;
begin
  // StrLiteral.AsString returns the StrValue directly
  StrLit := TBoldSqlStrLiteral.Create(0, 'hello world');
  try
    Assert.AreEqual('hello world', StrLit.AsString);
  finally
    StrLit.Free;
  end;

  // IntLiteral.AsString returns IntToStr(IntValue)
  IntLit := TBoldSqlIntLiteral.Create(0, 42);
  try
    Assert.AreEqual('42', IntLit.AsString);
  finally
    IntLit.Free;
  end;

  IntLit := TBoldSqlIntLiteral.Create(0, -7);
  try
    Assert.AreEqual('-7', IntLit.AsString);
  finally
    IntLit.Free;
  end;

  // FloatLiteral.AsString returns format('%e', [FloatValue])
  FloatLit := TBoldSqlFloatLiteral.Create(0, 3.14);
  try
    Assert.IsNotEmpty(FloatLit.AsString, 'FloatLiteral.AsString should not be empty');
    // Format('%e', [3.14]) produces scientific notation like '3.14000000000000E+000'
    Assert.StartsWith('3', FloatLit.AsString, 'Should start with 3');
  finally
    FloatLit.Free;
  end;

  // EnumLiteral.AsString returns the Name
  EnumLit := TBoldSqlEnumLiteral.Create(0, 5, 'Active');
  try
    Assert.AreEqual('Active', EnumLit.AsString);
  finally
    EnumLit.Free;
  end;

  // DateLiteral.AsString returns DateToStr(DateValue)
  DateLit := TBoldSqlDateLiteral.Create(0, EncodeDate(2025, 6, 15));
  try
    Assert.IsNotEmpty(DateLit.AsString, 'DateLiteral.AsString should not be empty');
    Assert.AreEqual(DateToStr(EncodeDate(2025, 6, 15)), DateLit.AsString);
  finally
    DateLit.Free;
  end;

  // TimeLiteral.AsString returns TimeToStr(TimeValue)
  TimeLit := TBoldSqlTimeLiteral.Create(0, EncodeTime(14, 30, 0, 0));
  try
    Assert.IsNotEmpty(TimeLit.AsString, 'TimeLiteral.AsString should not be empty');
    Assert.AreEqual(TimeToStr(EncodeTime(14, 30, 0, 0)), TimeLit.AsString);
  finally
    TimeLit.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestNodeList_AddAndCount;
var
  List: TBoldSqlNodeList;
begin
  List := TBoldSqlNodeList.Create;
  try
    Assert.AreEqual(0, List.Count);
    List.Add(TBoldSqlIntLiteral.Create(0, 1));
    Assert.AreEqual(1, List.Count);
    List.Add(TBoldSqlIntLiteral.Create(0, 2));
    Assert.AreEqual(2, List.Count);
    List.Add(TBoldSqlIntLiteral.Create(0, 3));
    Assert.AreEqual(3, List.Count);
  finally
    List.Free; // Destroy frees all items
  end;
end;

procedure TTestBoldSqlNodes.TestNodeList_GetItem;
var
  List: TBoldSqlNodeList;
  Node1, Node2: TBoldSqlIntLiteral;
begin
  List := TBoldSqlNodeList.Create;
  Node1 := TBoldSqlIntLiteral.Create(0, 10);
  Node2 := TBoldSqlIntLiteral.Create(0, 20);
  try
    List.Add(Node1);
    List.Add(Node2);
    Assert.AreSame(Node1, List[0]);
    Assert.AreSame(Node2, List[1]);
  finally
    List.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestNodeList_PutItem;
var
  List: TBoldSqlNodeList;
  Node1: TBoldSqlIntLiteral;
  Replacement: TBoldSqlIntLiteral;
begin
  // TBoldSqlNodeList always owns items (no toggle), so we must be careful:
  // We add Node1, then replace it via PutItem. The old Node1 won't be freed
  // by PutItem (TList.Put doesn't free), so we free it manually.
  List := TBoldSqlNodeList.Create;
  Node1 := TBoldSqlIntLiteral.Create(0, 10);
  Replacement := TBoldSqlIntLiteral.Create(0, 99);
  try
    List.Add(Node1);
    Assert.AreSame(Node1, List[0]);
    List[0] := Replacement;
    Assert.AreSame(Replacement, List[0]);
  finally
    Node1.Free; // manually free since PutItem doesn't free the old item
    List.Free;  // Destroy frees Replacement via Items[0]
  end;
end;

procedure TTestBoldSqlNodes.TestNodeList_TraverseList;
var
  List: TBoldSqlNodeList;
  Visitor: TTestSqlTrackingVisitor;
begin
  List := TBoldSqlNodeList.Create;
  Visitor := TTestSqlTrackingVisitor.Create;
  try
    List.Add(TBoldSqlIntLiteral.Create(0, 1));
    List.Add(TBoldSqlStrLiteral.Create(0, 'x'));
    List.Add(TBoldSqlFloatLiteral.Create(0, 1.5));
    List.TraverseList(Visitor);
    Assert.IsTrue(Visitor.WasVisited('IntLiteral'), 'TraverseList should visit IntLiteral');
    Assert.IsTrue(Visitor.WasVisited('StrLiteral'), 'TraverseList should visit StrLiteral');
    Assert.IsTrue(Visitor.WasVisited('FloatLiteral'), 'TraverseList should visit FloatLiteral');
  finally
    List.Free;
    Visitor.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestNodeList_DestroyFreesItems;
var
  List: TBoldSqlNodeList;
begin
  List := TBoldSqlNodeList.Create;
  List.Add(TBoldSqlIntLiteral.Create(0, 1));
  List.Add(TBoldSqlIntLiteral.Create(0, 2));
  List.Add(TBoldSqlIntLiteral.Create(0, 3));
  // Destroy always frees all items (no OwnsObjects toggle on TBoldSqlNodeList)
  List.Free;
  Assert.Pass('Destroy freed all items without error');
end;

procedure TTestBoldSqlNodes.TestOperation_DestroyFreesArgs;
var
  Operation: TBoldSqlOperation;
begin
  Operation := TBoldSqlOperation.Create(0, 'test');
  Assert.IsNotNull(Operation.Args, 'Args should be created');
  Operation.Args.Add(TBoldSqlIntLiteral.Create(0, 1));
  Operation.Args.Add(TBoldSqlIntLiteral.Create(0, 2));
  // Destroy should free Args list and its owned items
  Operation.Free;
  Assert.Pass('Operation.Destroy freed Args without error');
end;

procedure TTestBoldSqlNodes.TestIteration_DestroyFreesLoopVar;
var
  Iteration: TBoldSqlIteration;
  Binding: TBoldSqlVariableBinding;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'i', 0);
  Iteration := TBoldSqlIteration.Create(0, 'collect', Binding);
  Assert.AreSame(Binding, Iteration.LoopVar);
  // Destroy should free LoopVar
  Iteration.Free;
  Assert.Pass('Iteration.Destroy freed LoopVar without error');
end;

procedure TTestBoldSqlNodes.TestMember_CreateAndDestroy;
var
  Member: TBoldSqlMember;
  MemberOf: TBoldSqlIntLiteral;
begin
  MemberOf := TBoldSqlIntLiteral.Create(0, 0);
  Member := TBoldSqlMember.Create(5, 'attr', 7, MemberOf, True);
  Assert.AreEqual('attr', Member.MemberName);
  Assert.AreEqual(7, Member.MemberIndex);
  Assert.AreSame(MemberOf, Member.MemberOf);
  Assert.IsNotNull(Member.Qualifier, 'Qualifier should be created');
  Assert.IsTrue(Member.IsBoolean);
  // Destroy should free MemberOf and Qualifier
  Member.Free;
  Assert.Pass('Member.Destroy freed MemberOf and Qualifier without error');
end;

procedure TTestBoldSqlNodes.TestListCoercion_DestroyFreesChild;
var
  ListCoercion: TBoldSqlListCoercion;
  Child: TBoldSqlIntLiteral;
begin
  Child := TBoldSqlIntLiteral.Create(0, 42);
  ListCoercion := TBoldSqlListCoercion.Create(0, Child);
  Assert.AreSame(Child, ListCoercion.Child);
  // Destroy should free Child
  ListCoercion.Free;
  Assert.Pass('ListCoercion.Destroy freed Child without error');
end;

procedure TTestBoldSqlNodes.TestVariableBinding_AddRef;
var
  Binding: TBoldSqlVariableBinding;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'x', 0);
  try
    // First AddRef should succeed (refcount goes from 0 to 1)
    Binding.AddRef;
    Assert.AreEqual(1, Binding.RefCount);

    // For loop vars, multiple AddRef calls are allowed
    Binding.IsLoopVar := True;
    Binding.AddRef;
    Assert.AreEqual(2, Binding.RefCount);
    Binding.AddRef;
    Assert.AreEqual(3, Binding.RefCount);
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVariableBinding_AddRef_NonLoopVar_Raises;
var
  Binding: TBoldSqlVariableBinding;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'ext', 0);
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

procedure TTestBoldSqlNodes.TestVariableBinding_DecRef;
var
  Binding: TBoldSqlVariableBinding;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'x', 0);
  try
    Binding.AddRef;
    Assert.AreEqual(1, Binding.RefCount);
    Binding.DecRef;
    Assert.AreEqual(0, Binding.RefCount);
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVariableBinding_DecRef_ZeroRefCount_Raises;
var
  Binding: TBoldSqlVariableBinding;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'x', 0);
  try
    // RefCount starts at 0, so DecRef should raise
    Assert.WillRaise(
      procedure
      begin
        Binding.DecRef;
      end,
      EBold
    );
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVariableBinding_Properties;
var
  Binding: TBoldSqlVariableBinding;
begin
  Binding := TBoldSqlVariableBinding.Create(10, 'myVar', 3);
  try
    Assert.AreEqual('myVar', Binding.VariableName);
    Assert.AreEqual(3, Binding.TopSortedIndex);
    Assert.AreEqual(10, Binding.Position);

    // IsLoopVar defaults to False
    Assert.IsFalse(Binding.IsLoopVar);
    Binding.IsLoopVar := True;
    Assert.IsTrue(Binding.IsLoopVar);

    // IsExternal defaults to False
    Assert.IsFalse(Binding.IsExternal);
    Binding.IsExternal := True;
    Assert.IsTrue(Binding.IsExternal);

    // ExternalVarValue
    Binding.ExternalVarValue := 42;
    Assert.AreEqual(42, Integer(Binding.ExternalVarValue));
    Binding.ExternalVarValue := 'test';
    Assert.AreEqual('test', string(Binding.ExternalVarValue));

    // Context defaults to nil
    Assert.IsNull(Binding.Context);

    // RefCount starts at 0
    Assert.AreEqual(0, Binding.RefCount);
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVariableReference_IsExternalVariable;
var
  Binding: TBoldSqlVariableBinding;
  VarRef: TBoldSqlVariableReference;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'x', 0);
  try
    VarRef := TBoldSqlVariableReference.Create(0, Binding);
    try
      // Without Context set, IsExternalVariable should be False
      Assert.IsFalse(VarRef.IsExternalVariable, 'No context => not external');

      // With Context set (non-nil), IsExternalVariable should be True
      Binding.Context := TBoldObjectIdList.Create;
      try
        Assert.IsTrue(VarRef.IsExternalVariable, 'With context => external');
      finally
        Binding.Context.Free;
        Binding.Context := nil;
      end;
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVariableReference_SetObjectMapper_Raises;
var
  Binding: TBoldSqlVariableBinding;
  VarRef: TBoldSqlVariableReference;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'x', 0);
  try
    VarRef := TBoldSqlVariableReference.Create(0, Binding);
    try
      Assert.WillRaise(
        procedure
        begin
          VarRef.ObjectMapper := nil; // calls SetObjectMapper which raises
        end,
        EBoldInternal
      );
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVariableReference_SetQuery_Raises;
var
  Binding: TBoldSqlVariableBinding;
  VarRef: TBoldSqlVariableReference;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'x', 0);
  try
    VarRef := TBoldSqlVariableReference.Create(0, Binding);
    try
      Assert.WillRaise(
        procedure
        begin
          VarRef.Query := nil; // calls SetQuery which raises
        end,
        EBoldInternal
      );
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVariableReference_GetHasQuery;
var
  Binding: TBoldSqlVariableBinding;
  VarRef: TBoldSqlVariableReference;
begin
  // When IsLoopVar = True, GetHasQuery delegates to inherited (TBoldSqlNode.GetHasQuery)
  // which checks fQuery — should be False since we haven't set a query
  Binding := TBoldSqlVariableBinding.Create(0, 'v', 0);
  try
    Binding.IsLoopVar := True;
    VarRef := TBoldSqlVariableReference.Create(0, Binding);
    try
      Assert.IsFalse(VarRef.HasQuery, 'LoopVar without query should return False');
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
  end;

  // When IsLoopVar = False, GetHasQuery delegates to VariableBinding.HasQuery
  Binding := TBoldSqlVariableBinding.Create(0, 'ext', 0);
  try
    Binding.IsLoopVar := False;
    VarRef := TBoldSqlVariableReference.Create(0, Binding);
    try
      Assert.IsFalse(VarRef.HasQuery, 'Non-loopvar binding without query should return False');
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestNode_HasQuery_HasObjectMapper;
var
  Node: TBoldSqlNode;
begin
  Node := TBoldSqlNode.Create(0);
  try
    Assert.IsFalse(Node.HasQuery, 'Newly created node should not have a query');
    Assert.IsFalse(Node.HasObjectMapper, 'Newly created node should not have an ObjectMapper');
  finally
    Node.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestNode_RelinquishWCF_NilCase;
var
  Node: TBoldSqlNode;
begin
  // When both fWCF and fQuery are nil, RelinquishWCF should return nil
  Node := TBoldSqlNode.Create(0);
  try
    Assert.IsNull(Node.RelinquishWCF, 'RelinquishWCF should return nil when both WCF and Query are nil');
  finally
    Node.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestNode_RelinquishWCF_WithWCFAssigned;
var
  Node: TBoldSqlNode;
  Binding: TBoldSqlVariableBinding;
  WCFVar: TBoldSQLWCFVariable;
  Result: TObject;
begin
  // When fWCF is assigned, RelinquishWCF should return it and clear fWCF
  Binding := TBoldSqlVariableBinding.Create(0, 'p', 0);
  try
    WCFVar := TBoldSQLWCFVariable.Create(Binding);
    Node := TBoldSqlNode.Create(0);
    try
      Node.WCF := WCFVar;
      Result := Node.RelinquishWCF;
      Assert.AreSame(WCFVar, Result, 'Should return the assigned WCF');
      Assert.IsNull(Node.WCF, 'WCF should be nil after relinquish');
    finally
      WCFVar.Free;
      Node.Free;
    end;
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestVariableReference_RelinquishWCF;
var
  Binding: TBoldSqlVariableBinding;
  VarRef: TBoldSqlVariableReference;
  WCF: TObject;
begin
  // VarRef.RelinquishWCF creates a new TBoldSQLWCFVariable from its binding
  Binding := TBoldSqlVariableBinding.Create(0, 'param1', 0);
  try
    VarRef := TBoldSqlVariableReference.Create(0, Binding);
    try
      WCF := VarRef.RelinquishWCF;
      try
        Assert.IsNotNull(WCF, 'VarRef.RelinquishWCF should return a WCF');
        Assert.IsTrue(WCF is TBoldSQLWCFVariable, 'Should be TBoldSQLWCFVariable');
      finally
        WCF.Free;
      end;
    finally
      VarRef.Free;
    end;
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestMember_QueryOfMemberOfIsEnclosing;
var
  Member: TBoldSqlMember;
  Binding: TBoldSqlVariableBinding;
  VarRef: TBoldSqlVariableReference;
  IntNode: TBoldSqlIntLiteral;
begin
  // Case 1: MemberOf is a VariableReference with IsLoopVar=True => True
  Binding := TBoldSqlVariableBinding.Create(0, 'v', 0);
  Binding.IsLoopVar := True;
  VarRef := TBoldSqlVariableReference.Create(0, Binding);
  Member := TBoldSqlMember.Create(0, 'attr', 0, VarRef, False);
  try
    Assert.IsTrue(Member.QueryOfMemberOfIsEnclosing, 'LoopVar VarRef => enclosing');
  finally
    Member.Free; // frees VarRef (MemberOf is owned)
    Binding.Free;
  end;

  // Case 2: MemberOf is a VariableReference with IsLoopVar=False => False
  Binding := TBoldSqlVariableBinding.Create(0, 'v', 0);
  Binding.IsLoopVar := False;
  VarRef := TBoldSqlVariableReference.Create(0, Binding);
  Member := TBoldSqlMember.Create(0, 'attr', 0, VarRef, False);
  try
    Assert.IsFalse(Member.QueryOfMemberOfIsEnclosing, 'Non-loopvar VarRef => not enclosing');
  finally
    Member.Free;
    Binding.Free;
  end;

  // Case 3: MemberOf is NOT a VariableReference => False
  IntNode := TBoldSqlIntLiteral.Create(0, 42);
  Member := TBoldSqlMember.Create(0, 'attr', 0, IntNode, False);
  try
    Assert.IsFalse(Member.QueryOfMemberOfIsEnclosing, 'Non-VarRef MemberOf => not enclosing');
  finally
    Member.Free; // frees IntNode
  end;
end;

procedure TTestBoldSqlNodes.TestSymbol_Methods;
var
  Symbol: TTestSqlSymbol;
  Operation: TBoldSqlOperation;
begin
  Symbol := TTestSqlSymbol.Create('mySymbol');
  try
    // GetSQLName returns GetName by default
    Assert.AreEqual('mySymbol', Symbol.SQLName);
    Assert.AreEqual('mySymbol', Symbol.Name);

    // ResolveObjectMapper returns nil by default
    Operation := TBoldSqlOperation.Create(0, 'op');
    try
      Assert.IsNull(Symbol.ResolveObjectMapper(Operation));
    finally
      Operation.Free;
    end;

    // BuildWCFOrQuery raises EBoldInternal
    Operation := TBoldSqlOperation.Create(0, 'op');
    try
      Assert.WillRaise(
        procedure
        begin
          Symbol.BuildWCFOrQuery(Operation, nil);
        end,
        EBoldInternal
      );
    finally
      Operation.Free;
    end;
  finally
    Symbol.Free;
  end;
end;

procedure TTestBoldSqlNodes.TestWCFVariable_CreateAndDestroy;
var
  Binding: TBoldSqlVariableBinding;
  WCFVar: TBoldSQLWCFVariable;
begin
  Binding := TBoldSqlVariableBinding.Create(0, 'param1', 0);
  try
    WCFVar := TBoldSQLWCFVariable.Create(Binding);
    try
      Assert.Pass('TBoldSQLWCFVariable created successfully');
    finally
      WCFVar.Free;
    end;
  finally
    Binding.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSqlNodes);

end.
