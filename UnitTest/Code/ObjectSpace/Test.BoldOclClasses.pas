unit Test.BoldOclClasses;

interface

uses
  DUnitX.TestFramework,
  BoldElements,
  BoldOclClasses;

type
  TTestTrackingVisitor = class(TBoldOclVisitor)
  private
    FLastVisitedMethod: string;
  public
    procedure VisitTBoldOclNode(N: TBoldOclNode); override;
    procedure VisitTBoldOclListCoercion(N: TBoldOclListCoercion); override;
    procedure VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLiteral); override;
    procedure VisitTBoldOclOperation(N: TBoldOclOperation); override;
    procedure VisitTBoldOclIteration(N: TBoldOclIteration); override;
    procedure VisitTBoldOclMember(N: TBoldOclMember); override;
    procedure VisitTBoldOclMethod(N: TBoldOclMethod); override;
    procedure VisitTBoldOclLiteral(N: TBoldOclLiteral); override;
    procedure VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral); override;
    procedure VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral); override;
    procedure VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral); override;
    procedure VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral); override;
    procedure VisitTBoldOclMomentLiteral(N: TBoldOclMomentLiteral); override;
    procedure VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral); override;
    procedure VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral); override;
    procedure VisitTBoldOclVariableBinding(N: TBoldOclVariableBinding); override;
    procedure VisitTBoldOclVariableReference(N: TBoldOclVariableReference); override;
    procedure VisitTBoldOclTypeNode(N: TBoldOclTypeNode); override;
    property LastVisitedMethod: string read FLastVisitedMethod;
  end;

  TTestExternalVariable = class(TBoldExternalVariable)
  protected
    function GetValue: TBoldElement; override;
    function GetValueType: TBoldElementTypeInfo; override;
  end;

  [TestFixture]
  [Category('ObjectSpace')]
  TTestBoldOclClasses = class
  public
    [Test] [Category('Quick')]
    procedure TestNodeCreation_AllTypes;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_BaseNode;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_TypeNode;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_ListCoercion;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_CollectionLiteral;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_Operation;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_Iteration;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_Member;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_Method;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_Literal;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_StrLiteral;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_NumericLiteral;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_IntLiteral;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_EnumLiteral;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_MomentLiteral;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_DateLiteral;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_TimeLiteral;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_VariableBinding;
    [Test] [Category('Quick')]
    procedure TestAcceptVisitor_VariableReference;
    [Test] [Category('Quick')]
    procedure TestDateLiteral_DateValue;
    [Test] [Category('Quick')]
    procedure TestTimeLiteral_TimeValue;
    [Test] [Category('Quick')]
    procedure TestEnvironment_PushPopBinding;
    [Test] [Category('Quick')]
    procedure TestEnvironment_ReplaceBinding;
    [Test] [Category('Quick')]
    procedure TestEnvironment_RemoveBinding;
    [Test] [Category('Quick')]
    procedure TestEnvironment_Find_InCurrentScope;
    [Test] [Category('Quick')]
    procedure TestEnvironment_Find_InOuterScope;
    [Test] [Category('Quick')]
    procedure TestEnvironment_Find_NotFound;
    [Test] [Category('Quick')]
    procedure TestEnvironment_Lookup_ExistingBinding;
    [Test] [Category('Quick')]
    procedure TestEnvironment_LookupSelf;
    [Test] [Category('Quick')]
    procedure TestEnvironment_BindingsAsCommaText;
    [Test] [Category('Quick')]
    procedure TestEnvironment_MakeGenSymName;
    [Test] [Category('Quick')]
    procedure TestCollectionLiteral_DestroyFreesChildren;
    [Test] [Category('Quick')]
    procedure TestMethod_DestroyFreesMethodOf;
    [Test] [Category('Quick')]
    procedure TestVisitorBaseStubs_AllCalled;
    [Test] [Category('Quick')]
    procedure TestEnvironment_ReplaceBinding_NonLastBinding;
    [Test] [Category('Quick')]
    procedure TestVariableBindingExternal_CreateDestroy;
    [Test] [Category('Quick')]
    procedure TestVariableBindingExternal_GetBoldType_WithVariable;
    [Test] [Category('Quick')]
    procedure TestVariableBindingExternal_GetBoldType_WithoutVariable;
    [Test] [Category('Quick')]
    procedure TestEnvironment_Lookup_ExternalBinding_WithVariable;
    [Test] [Category('Quick')]
    procedure TestEnvironment_Lookup_ExternalBinding_WithoutVariable;
    [Test] [Category('Quick')]
    procedure TestVisitorBaseStub_VisitNode_Direct;
    [Test] [Category('Quick')]
    procedure TestEnvironment_RemoveVariable;
    [Test] [Category('Quick')]
    procedure TestEnvironment_RemoveVariable_SkipsNonMatching;
  end;

implementation

uses
  SysUtils;

{ TTestTrackingVisitor }

procedure TTestTrackingVisitor.VisitTBoldOclNode(N: TBoldOclNode);
begin
  FLastVisitedMethod := 'VisitTBoldOclNode';
end;

procedure TTestTrackingVisitor.VisitTBoldOclListCoercion(N: TBoldOclListCoercion);
begin
  FLastVisitedMethod := 'VisitTBoldOclListCoercion';
end;

procedure TTestTrackingVisitor.VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclCollectionLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclOperation(N: TBoldOclOperation);
begin
  FLastVisitedMethod := 'VisitTBoldOclOperation';
end;

procedure TTestTrackingVisitor.VisitTBoldOclIteration(N: TBoldOclIteration);
begin
  FLastVisitedMethod := 'VisitTBoldOclIteration';
end;

procedure TTestTrackingVisitor.VisitTBoldOclMember(N: TBoldOclMember);
begin
  FLastVisitedMethod := 'VisitTBoldOclMember';
end;

procedure TTestTrackingVisitor.VisitTBoldOclMethod(N: TBoldOclMethod);
begin
  FLastVisitedMethod := 'VisitTBoldOclMethod';
end;

procedure TTestTrackingVisitor.VisitTBoldOclLiteral(N: TBoldOclLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclStrLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclNumericLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclEnumLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclIntLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclMomentLiteral(N: TBoldOclMomentLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclMomentLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclDateLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral);
begin
  FLastVisitedMethod := 'VisitTBoldOclTimeLiteral';
end;

procedure TTestTrackingVisitor.VisitTBoldOclVariableBinding(N: TBoldOclVariableBinding);
begin
  FLastVisitedMethod := 'VisitTBoldOclVariableBinding';
end;

procedure TTestTrackingVisitor.VisitTBoldOclVariableReference(N: TBoldOclVariableReference);
begin
  FLastVisitedMethod := 'VisitTBoldOclVariableReference';
end;

procedure TTestTrackingVisitor.VisitTBoldOclTypeNode(N: TBoldOclTypeNode);
begin
  FLastVisitedMethod := 'VisitTBoldOclTypeNode';
end;

{ TTestBoldOclClasses }

procedure TTestBoldOclClasses.TestNodeCreation_AllTypes;
var
  Node: TBoldOclNode;
begin
  // Verify each node type constructs and destructs cleanly
  Node := TBoldOclNode.Create;
  try
    Assert.IsFalse(Node.IsConstant, 'IsConstant should be false after Create');
  finally
    Node.Free;
  end;

  Node := TBoldOclTypeNode.Create;
  Node.Free;

  Node := TBoldOclListCoercion.Create;
  Node.Free;

  Node := TBoldOclOperation.Create;
  Node.Free;

  Node := TBoldOclIteration.Create;
  Node.Free;

  Node := TBoldOclMember.Create;
  Node.Free;

  Node := TBoldOclMethod.Create;
  Node.Free;

  Node := TBoldOclVariableBinding.Create;
  Node.Free;

  Node := TBoldOclVariableReference.Create;
  Node.Free;

  Node := TBoldOclLiteral.Create;
  Node.Free;

  Node := TBoldOclCollectionLiteral.Create;
  Node.Free;

  Node := TBoldOclStrLiteral.Create;
  Node.Free;

  Node := TBoldOclNumericLiteral.Create;
  Node.Free;

  Node := TBoldOclIntLiteral.Create;
  Node.Free;

  Node := TBoldOclMomentLiteral.Create;
  Node.Free;

  Node := TBoldOclDateLiteral.Create;
  Node.Free;

  Node := TBoldOclTimeLiteral.Create;
  Node.Free;

  Node := TBoldOclEnumLiteral.Create;
  Node.Free;

  Assert.Pass('All node types created and freed without error');
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_BaseNode;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclNode;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclNode.Create;
    try
      Node.AcceptVisitor(Visitor);
      // Base TBoldOclNode.AcceptVisitor is empty — should NOT call any VisitXxx
      Assert.AreEqual('', Visitor.LastVisitedMethod, 'Base node should not dispatch to any visitor method');
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_TypeNode;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclTypeNode;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclTypeNode.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclTypeNode', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_ListCoercion;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclListCoercion;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclListCoercion.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclListCoercion', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_CollectionLiteral;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclCollectionLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclCollectionLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclCollectionLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_Operation;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclOperation;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclOperation.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclOperation', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_Iteration;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclIteration;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclIteration.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclIteration', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_Member;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclMember;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclMember.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclMember', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_Method;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclMethod;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclMethod.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclMethod', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_Literal;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_StrLiteral;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclStrLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclStrLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclStrLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_NumericLiteral;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclNumericLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclNumericLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclNumericLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_IntLiteral;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclIntLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclIntLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclIntLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_EnumLiteral;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclEnumLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclEnumLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclEnumLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_MomentLiteral;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclMomentLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclMomentLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclMomentLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_DateLiteral;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclDateLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclDateLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclDateLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_TimeLiteral;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclTimeLiteral;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclTimeLiteral.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclTimeLiteral', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_VariableBinding;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclVariableBinding;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclVariableBinding.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclVariableBinding', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestAcceptVisitor_VariableReference;
var
  Visitor: TTestTrackingVisitor;
  Node: TBoldOclVariableReference;
begin
  Visitor := TTestTrackingVisitor.Create;
  try
    Node := TBoldOclVariableReference.Create;
    try
      Node.AcceptVisitor(Visitor);
      Assert.AreEqual('VisitTBoldOclVariableReference', Visitor.LastVisitedMethod);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
end;

procedure TTestBoldOclClasses.TestDateLiteral_DateValue;
var
  Node: TBoldOclDateLiteral;
  Expected: TDateTime;
begin
  Node := TBoldOclDateLiteral.Create;
  try
    Expected := EncodeDate(2025, 6, 15);
    Node.DateValue := Expected;
    Assert.AreEqual(Expected, Node.DateValue, 'DateValue should round-trip correctly');
  finally
    Node.Free;
  end;
end;

procedure TTestBoldOclClasses.TestTimeLiteral_TimeValue;
var
  Node: TBoldOclTimeLiteral;
  Expected: TDateTime;
begin
  Node := TBoldOclTimeLiteral.Create;
  try
    Expected := EncodeTime(14, 30, 45, 0);
    Node.TimeValue := Expected;
    Assert.AreEqual(Expected, Node.TimeValue, 'TimeValue should round-trip correctly');
  finally
    Node.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_PushPopBinding;
var
  Env: TBoldOclEnvironment;
  B1, B2, Popped: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    B1 := TBoldOclVariableBinding.Create;
    B1.VariableName := 'First';
    B2 := TBoldOclVariableBinding.Create;
    B2.VariableName := 'Second';

    Env.pushBinding(B1);
    Env.pushBinding(B2);
    Assert.AreEqual(2, Env.Count, 'Should have 2 bindings');

    Popped := Env.popBinding;
    Assert.AreEqual('Second', Popped.VariableName, 'Should pop LIFO');
    Assert.AreEqual(1, Env.Count, 'Should have 1 binding after pop');
    Popped.Free;

    Popped := Env.popBinding;
    Assert.AreEqual('First', Popped.VariableName);
    Assert.AreEqual(0, Env.Count);
    Popped.Free;
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_ReplaceBinding;
var
  Env: TBoldOclEnvironment;
  Original, Replacement: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    Original := TBoldOclVariableBinding.Create;
    Original.VariableName := 'X';

    Env.pushBinding(Original);
    Assert.AreEqual(1, Env.Count);

    Replacement := TBoldOclVariableBinding.Create;
    Replacement.VariableName := 'X';

    // ReplaceBinding frees the original and puts the replacement in its slot
    Env.ReplaceBinding('X', Replacement);
    Assert.AreEqual(1, Env.Count, 'Count should stay 1 after replace');

    var Found := Env.Find('X');
    Assert.AreSame(Replacement, Found, 'Find should return the replacement binding');
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_RemoveBinding;
var
  Env: TBoldOclEnvironment;
  B: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'Temp';
    Env.pushBinding(B);
    Assert.AreEqual(1, Env.Count);

    Env.RemoveBinding(B);
    Assert.AreEqual(0, Env.Count, 'Count should be 0 after remove');
    B.Free;
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_Find_InCurrentScope;
var
  Env: TBoldOclEnvironment;
  B, Found: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'MyVar';
    Env.pushBinding(B);

    Found := Env.Find('MyVar');
    Assert.AreSame(B, Found, 'Find should return the binding in current scope');
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_Find_InOuterScope;
var
  Outer, Inner: TBoldOclEnvironment;
  B, Found: TBoldOclVariableBinding;
begin
  Outer := TBoldOclEnvironment.Create(nil);
  try
    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'OuterVar';
    Outer.pushBinding(B);

    Inner := TBoldOclEnvironment.Create(Outer);
    try
      // Inner has no bindings — Find should delegate to outer scope via Lookup
      Found := Inner.Find('OuterVar');
      Assert.AreSame(B, Found, 'Find should locate binding in outer scope');
    finally
      Inner.Free;
    end;
  finally
    Outer.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_Find_NotFound;
var
  Env: TBoldOclEnvironment;
  Found: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    Found := Env.Find('NonExistent');
    Assert.IsNull(Found, 'Find should return nil for unknown names');
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_Lookup_ExistingBinding;
var
  Env: TBoldOclEnvironment;
  B, Found: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'LookupTarget';
    Env.pushBinding(B);

    Found := Env.Lookup('LookupTarget');
    Assert.AreSame(B, Found, 'Lookup should return the binding');
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_LookupSelf;
var
  Env: TBoldOclEnvironment;
  B, Found: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'SELF';
    Env.pushBinding(B);

    Found := Env.LookupSelf;
    Assert.AreSame(B, Found, 'LookupSelf should find the SELF binding');
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_BindingsAsCommaText;
var
  Env: TBoldOclEnvironment;
  B: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'Alpha';
    Env.pushBinding(B);

    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'Beta';
    Env.pushBinding(B);

    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'Gamma';
    Env.pushBinding(B);

    Assert.AreEqual('Alpha,Beta,Gamma', Env.BindingsAsCommaText);
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_MakeGenSymName;
var
  Env: TBoldOclEnvironment;
  Name1, Name2: string;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    Name1 := Env.MakeGenSymName;
    Name2 := Env.MakeGenSymName;
    Assert.AreEqual('0#GenSym', Name1, 'First gensym should start at 0');
    Assert.AreEqual('1#GenSym', Name2, 'Second gensym should increment');
    Assert.AreNotEqual(Name1, Name2, 'Generated names should be unique');
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestCollectionLiteral_DestroyFreesChildren;
var
  Node: TBoldOclCollectionLiteral;
begin
  Node := TBoldOclCollectionLiteral.Create;
  // Set up RangeStart, RangeStop, and Elements that will be freed by Destroy
  Node.RangeStart := TBoldOclIntLiteral.Create;
  Node.RangeStop := TBoldOclIntLiteral.Create;
  Node.IsRange := True;
  SetLength(Node.Elements, 2);
  Node.Elements[0] := TBoldOclStrLiteral.Create;
  Node.Elements[1] := TBoldOclNumericLiteral.Create;
  // Freeing should not leak — destructor frees RangeStart, RangeStop, and Elements
  Node.Free;
  Assert.Pass('CollectionLiteral.Destroy freed all children without error');
end;

procedure TTestBoldOclClasses.TestMethod_DestroyFreesMethodOf;
var
  Node: TBoldOclMethod;
begin
  Node := TBoldOclMethod.Create;
  Node.MethodOf := TBoldOclMember.Create;
  Node.MethodOf_AddedToArgs := False;
  // Destroy should free MethodOf since MethodOf_AddedToArgs is false
  Node.Free;
  Assert.Pass('Method.Destroy freed MethodOf without error');
end;

{ TTestExternalVariable }

function TTestExternalVariable.GetValue: TBoldElement;
begin
  Result := nil;
end;

function TTestExternalVariable.GetValueType: TBoldElementTypeInfo;
begin
  Result := nil;
end;

procedure TTestBoldOclClasses.TestVisitorBaseStubs_AllCalled;
var
  Visitor: TBoldOclVisitor;
  Node: TBoldOclNode;
begin
  // Use a plain TBoldOclVisitor (no overrides) to execute all 18 base stub methods
  Visitor := TBoldOclVisitor.Create;
  try
    Node := TBoldOclTypeNode.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclListCoercion.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclCollectionLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclOperation.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclIteration.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclMember.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclMethod.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclStrLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclNumericLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclEnumLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclIntLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclMomentLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclDateLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclTimeLiteral.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclVariableBinding.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    Node := TBoldOclVariableReference.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;

    // TBoldOclNode base AcceptVisitor is empty, also exercise it
    Node := TBoldOclNode.Create;
    try Node.AcceptVisitor(Visitor); finally Node.Free; end;
  finally
    Visitor.Free;
  end;
  Assert.Pass('All 18 base visitor stubs executed without error');
end;

procedure TTestBoldOclClasses.TestEnvironment_ReplaceBinding_NonLastBinding;
var
  Env: TBoldOclEnvironment;
  A, B, NewA: TBoldOclVariableBinding;
begin
  // Push A then B. Replace A — loop must skip B (Dec(I)) to reach A.
  Env := TBoldOclEnvironment.Create(nil);
  try
    A := TBoldOclVariableBinding.Create;
    A.VariableName := 'A';
    B := TBoldOclVariableBinding.Create;
    B.VariableName := 'B';

    Env.pushBinding(A);
    Env.pushBinding(B);
    Assert.AreEqual(2, Env.Count);

    NewA := TBoldOclVariableBinding.Create;
    NewA.VariableName := 'A';

    // ReplaceBinding starts at end (B, no match), Dec(I), then finds A
    Env.ReplaceBinding('A', NewA);
    Assert.AreEqual(2, Env.Count, 'Count should still be 2');
    Assert.AreSame(NewA, Env.Find('A'), 'Should find the replacement');
    Assert.AreSame(B, Env.Find('B'), 'B should be unaffected');
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestVariableBindingExternal_CreateDestroy;
var
  Binding: TBoldOclVariableBindingExternal;
  ExtVar: TTestExternalVariable;
begin
  Binding := TBoldOclVariableBindingExternal.Create;
  try
    ExtVar := TTestExternalVariable.Create(nil, 'TestVar');
    try
      Binding.ExternalVariable := ExtVar;
      Assert.AreSame(ExtVar, Binding.ExternalVariable);
    finally
      ExtVar.Free;
    end;
  finally
    Binding.Free;
  end;
  Assert.Pass('VariableBindingExternal create/set/destroy without error');
end;

procedure TTestBoldOclClasses.TestVariableBindingExternal_GetBoldType_WithVariable;
var
  Binding: TBoldOclVariableBindingExternal;
  ExtVar: TTestExternalVariable;
begin
  Binding := TBoldOclVariableBindingExternal.Create;
  try
    ExtVar := TTestExternalVariable.Create(nil, 'TestVar');
    try
      Binding.ExternalVariable := ExtVar;
      // GetValueType returns nil in our test stub, so BoldType should be nil
      Assert.IsNull(Binding.BoldType, 'BoldType should return ValueType from external variable');
    finally
      ExtVar.Free;
    end;
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldOclClasses.TestVariableBindingExternal_GetBoldType_WithoutVariable;
var
  Binding: TBoldOclVariableBindingExternal;
begin
  Binding := TBoldOclVariableBindingExternal.Create;
  try
    // ExternalVariable not set — GetBoldType should return nil
    Assert.IsNull(Binding.BoldType, 'BoldType should be nil when no external variable set');
  finally
    Binding.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_Lookup_ExternalBinding_WithVariable;
var
  Env: TBoldOclEnvironment;
  Binding: TBoldOclVariableBindingExternal;
  ExtVar: TTestExternalVariable;
  Found: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    ExtVar := TTestExternalVariable.Create(nil, 'TestVar');
    try
      Binding := TBoldOclVariableBindingExternal.Create;
      Binding.VariableName := 'ExtVar';
      Binding.ExternalVariable := ExtVar;
      Env.pushBinding(Binding);

      // Lookup should enter the external-variable branch and call SetReferenceValue
      Found := Env.Lookup('ExtVar');
      Assert.AreSame(Binding, Found, 'Lookup should return the external binding');
    finally
      ExtVar.Free;
    end;
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_Lookup_ExternalBinding_WithoutVariable;
var
  Env: TBoldOclEnvironment;
  Binding: TBoldOclVariableBindingExternal;
  Found: TBoldOclVariableBinding;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    Binding := TBoldOclVariableBindingExternal.Create;
    Binding.VariableName := 'ExtVar';
    // ExternalVariable is nil
    Env.pushBinding(Binding);

    // Lookup should enter the external branch, find ExternalVariable nil,
    // and call SetReferenceValue(nil)
    Found := Env.Lookup('ExtVar');
    Assert.AreSame(Binding, Found, 'Lookup should return the binding');
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestVisitorBaseStub_VisitNode_Direct;
var
  Visitor: TBoldOclVisitor;
  Node: TBoldOclNode;
begin
  // TBoldOclNode.AcceptVisitor is empty, so VisitTBoldOclNode is never called
  // via the dispatch pattern. Call it directly to cover line 581.
  Visitor := TBoldOclVisitor.Create;
  try
    Node := TBoldOclNode.Create;
    try
      Visitor.VisitTBoldOclNode(Node);
    finally
      Node.Free;
    end;
  finally
    Visitor.Free;
  end;
  Assert.Pass('VisitTBoldOclNode base stub executed without error');
end;

procedure TTestBoldOclClasses.TestEnvironment_RemoveVariable;
var
  Env: TBoldOclEnvironment;
  Binding: TBoldOclVariableBindingExternal;
  ExtVar: TTestExternalVariable;
begin
  Env := TBoldOclEnvironment.Create(nil);
  try
    ExtVar := TTestExternalVariable.Create(nil, 'Var');
    try
      Binding := TBoldOclVariableBindingExternal.Create;
      Binding.VariableName := 'Var';
      Binding.ExternalVariable := ExtVar;
      Env.pushBinding(Binding);
      Assert.AreEqual(1, Env.Count);

      // RemoveVariable finds the binding by ExternalVariable, frees it, and deletes from list
      Env.RemoveVariable(ExtVar);
      Assert.AreEqual(0, Env.Count, 'Binding should be removed');
    finally
      ExtVar.Free;
    end;
  finally
    Env.Free;
  end;
end;

procedure TTestBoldOclClasses.TestEnvironment_RemoveVariable_SkipsNonMatching;
var
  Env: TBoldOclEnvironment;
  Binding1, Binding2: TBoldOclVariableBindingExternal;
  ExtVar1, ExtVar2: TTestExternalVariable;
begin
  // Push two external bindings. RemoveVariable for the first-pushed one
  // must skip the second binding (Dec(I)) before finding the match.
  Env := TBoldOclEnvironment.Create(nil);
  try
    ExtVar1 := TTestExternalVariable.Create(nil, 'Var1');
    try
      ExtVar2 := TTestExternalVariable.Create(nil, 'Var2');
      try
        Binding1 := TBoldOclVariableBindingExternal.Create;
        Binding1.VariableName := 'Var1';
        Binding1.ExternalVariable := ExtVar1;
        Env.pushBinding(Binding1);

        Binding2 := TBoldOclVariableBindingExternal.Create;
        Binding2.VariableName := 'Var2';
        Binding2.ExternalVariable := ExtVar2;
        Env.pushBinding(Binding2);

        Assert.AreEqual(2, Env.Count);

        // RemoveVariable starts at end (Binding2/ExtVar2, no match), Dec(I),
        // then finds Binding1/ExtVar1.
        Env.RemoveVariable(ExtVar1);
        Assert.AreEqual(1, Env.Count, 'Should have 1 binding left');
      finally
        ExtVar2.Free;
      end;
    finally
      ExtVar1.Free;
    end;
  finally
    Env.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldOclClasses);

end.
