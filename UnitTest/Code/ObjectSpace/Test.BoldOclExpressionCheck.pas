unit Test.BoldOclExpressionCheck;

{ Tests for BoldOclExpressionCheck (#124): what the OCL editors, the VCL form
  and the Lazarus dialog, show for an expression: the parser messages, the
  list of members, classes and operations that can follow it, the variables,
  Remove Last and the search for an argument placeholder. Only the type
  information of the test model is used; no system is opened. }

interface

uses
  Classes,
  SysUtils,
  BoldTestFramework,
  BoldElements,
  BoldSystemRT,
  BoldOclExpressionCheck,
  maan_UndoRedoBase;

type
  {$IFNDEF FPC}
  [Category('OCL')]
  {$ENDIF}
  TTestBoldOclExpressionCheck = class(TBoldTestCase)
  private
    function SystemType: TBoldSystemTypeInfo;
    function SomeClass: TBoldElementTypeInfo;
    function Check(const AExpression: string): TBoldOclCheckResult;
    function TextOf(const AExpression: string; AShowSyntaxErrors: Boolean = False): string;
    function ListFor(const AExpression: string; AContext: TBoldElementTypeInfo;
      AOptions: TBoldOclMemberOptions): TBoldOclCompletions;
    function IndexOf(const AItems: TBoldOclCompletions; const AInsertText: string): Integer;
    function CountOf(const AItems: TBoldOclCompletions; AKind: TBoldOclCompletionKind): Integer;
  protected
    procedure SetUp; override;
  published
    procedure TestEmptyExpressionIsTheContext;
    procedure TestValidExpressionGivesTheResultType;
    procedure TestErrorNearTheStartHasThePointerBeforeTheLine;
    procedure TestErrorFurtherInHasTheLineBeforeThePointer;
    procedure TestErrorInTheSecondLineOfTheExpression;
    procedure TestSyntaxErrorIsHiddenUnlessAsked;
    procedure TestNoContextChecksNothing;
    procedure TestMembersOfTheContext;
    procedure TestMembersAndOperationsAfterAnExpression;
    procedure TestClassesOfTheSystem;
    procedure TestNothingIsListedForAnInvalidExpression;
    procedure TestMemberOptions;
    procedure TestFilter;
    procedure TestVariables;
    procedure TestRemoveLast;
    procedure TestPlaceholderAt;
  end;

implementation

const
  cAllMembers = [omoDerived, omoPersistent, omoTransient];

{ TTestBoldOclExpressionCheck }

procedure TTestBoldOclExpressionCheck.SetUp;
begin
  inherited;
  EnsureDM;
end;

function TTestBoldOclExpressionCheck.SystemType: TBoldSystemTypeInfo;
begin
  Result := dmUndoRedo.BoldSystemHandle1.StaticSystemTypeInfo;
end;

function TTestBoldOclExpressionCheck.SomeClass: TBoldElementTypeInfo;
begin
  Result := SystemType.ClassTypeInfoByExpressionName['SomeClass'];
end;

function TTestBoldOclExpressionCheck.Check(const AExpression: string): TBoldOclCheckResult;
begin
  Result := BoldCheckOclExpression(AExpression, SomeClass, nil);
end;

function TTestBoldOclExpressionCheck.TextOf(const AExpression: string; AShowSyntaxErrors: Boolean): string;
begin
  Result := BoldOclCheckResultAsText(AExpression, Check(AExpression), AShowSyntaxErrors);
end;

function TTestBoldOclExpressionCheck.ListFor(const AExpression: string; AContext: TBoldElementTypeInfo;
  AOptions: TBoldOclMemberOptions): TBoldOclCompletions;
begin
  BoldOclListCompletions(AExpression, BoldCheckOclExpression(AExpression, AContext, nil), AContext, AOptions, Result);
end;

function TTestBoldOclExpressionCheck.IndexOf(const AItems: TBoldOclCompletions; const AInsertText: string): Integer;
begin
  for Result := 0 to High(AItems) do
    if AItems[Result].InsertText = AInsertText then
      Exit;
  Result := -1;
end;

function TTestBoldOclExpressionCheck.CountOf(const AItems: TBoldOclCompletions;
  AKind: TBoldOclCompletionKind): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(AItems) do
    if AItems[i].Kind = AKind then
      Inc(Result);
end;

procedure TTestBoldOclExpressionCheck.TestEmptyExpressionIsTheContext;
var
  R: TBoldOclCheckResult;
begin
  R := Check('');
  Assert.IsTrue(R.Checked and R.Valid, 'checked and valid');
  Assert.IsTrue(R.ResultType = SomeClass, 'the result type is the context');
  Assert.AreEqual('SomeClass', R.ContextName);
  Assert.AreEqual('Empty Expression', TextOf(''));
end;

procedure TTestBoldOclExpressionCheck.TestValidExpressionGivesTheResultType;
var
  R: TBoldOclCheckResult;
begin
  R := Check('part');
  Assert.IsTrue(R.Valid, 'valid');
  Assert.AreEqual('Collection(SomeClass)', R.ResultTypeName);
  Assert.AreEqual(-1, R.ErrorPosition);
  Assert.AreEqual('Syntax is OK' + sLineBreak + sLineBreak + 'Current type is: Collection(SomeClass)',
    TextOf('part'));
end;

procedure TTestBoldOclExpressionCheck.TestErrorNearTheStartHasThePointerBeforeTheLine;
var
  R: TBoldOclCheckResult;
begin
  R := Check('part.nosuch');
  Assert.IsFalse(R.Valid, 'not valid');
  Assert.IsFalse(R.IsSyntaxError, 'not a parser error');
  Assert.AreEqual(5, R.ErrorPosition);
  Assert.AreEqual('nosuch is not a member of SomeClass', R.ErrorMessage);
  Assert.AreEqual('5:nosuch is not a member of SomeClass', R.RawErrorMessage);
  Assert.IsTrue(R.IsOclError and not R.IsInternalError, 'an OCL error');
  // The message keeps the position the evaluator puts before it, as in the editors
  Assert.AreEqual('part.nosuch' + sLineBreak + '     ^-------' + sLineBreak + R.RawErrorMessage, TextOf('part.nosuch'));
end;

procedure TTestBoldOclExpressionCheck.TestErrorFurtherInHasTheLineBeforeThePointer;
var
  R: TBoldOclCheckResult;
begin
  // From the tenth character on; the ninth still has the line after it
  R := Check('next.next.nosuch');
  Assert.AreEqual(10, R.ErrorPosition);
  Assert.AreEqual('next.next.nosuch' + sLineBreak + '   -------^' + sLineBreak + R.RawErrorMessage,
    TextOf('next.next.nosuch'));
  R := Check('aString  + 1');
  Assert.AreEqual(9, R.ErrorPosition, 'aString  + 1');
  Assert.AreEqual('aString  + 1' + sLineBreak + '         ^-------' + sLineBreak + R.RawErrorMessage,
    TextOf('aString  + 1'));
  R := Check('next.parent.next.nosuch');
  Assert.AreEqual(17, R.ErrorPosition);
  Assert.AreEqual('next.parent.next.nosuch' + sLineBreak + '          -------^' + sLineBreak + R.RawErrorMessage,
    TextOf('next.parent.next.nosuch'));
end;

procedure TTestBoldOclExpressionCheck.TestErrorInTheSecondLineOfTheExpression;
var
  Expression, Text: string;
  Lines: TStringList;
begin
  Expression := 'next.parent' + sLineBreak + '.nosuch';
  Text := TextOf(Expression);
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    Assert.AreEqual(4, Lines.Count, Text);
    Assert.AreEqual('next.parent', Lines[0]);
    Assert.AreEqual('.nosuch', Lines[1]);
    Assert.AreEqual(' ^-------', Lines[2]);
  finally
    Lines.Free;
  end;
end;

procedure TTestBoldOclExpressionCheck.TestSyntaxErrorIsHiddenUnlessAsked;
var
  R: TBoldOclCheckResult;
  Shown: string;
begin
  R := Check('part->');
  Assert.IsFalse(R.Valid, 'not valid');
  Assert.IsTrue(R.IsSyntaxError, 'a parser error');
  Assert.AreEqual('Unable to complete parse, syntax error', TextOf('part->'));
  Shown := TextOf('part->', True);
  Assert.IsTrue(Pos('part->', Shown) = 1, Shown);
  Assert.IsTrue(Pos('SSYacc', Shown) > 0, Shown);
end;

procedure TTestBoldOclExpressionCheck.TestNoContextChecksNothing;
var
  R: TBoldOclCheckResult;
  Items: TBoldOclCompletions;
begin
  R := BoldCheckOclExpression('part', nil, nil);
  Assert.IsFalse(R.Checked, 'not checked');
  Assert.IsFalse(R.Valid, 'not valid');
  Assert.AreEqual('No Context, No design support', BoldOclCheckResultAsText('part', R));
  BoldOclListCompletions('part', R, nil, cAllMembers, Items);
  Assert.AreEqual(0, Integer(Length(Items)));
  BoldOclListVariables(nil, nil, Items);
  Assert.AreEqual(0, Integer(Length(Items)));
end;

procedure TTestBoldOclExpressionCheck.TestMembersOfTheContext;
var
  Items: TBoldOclCompletions;
  Index: Integer;
begin
  Items := ListFor('', SomeClass, cAllMembers);
  Assert.AreEqual(0, CountOf(Items, ockOperation), 'no operations before an expression');
  Assert.AreEqual(0, CountOf(Items, ockType), 'no classes');
  Index := IndexOf(Items, 'aString');
  Assert.IsTrue(Index >= 0, 'aString is listed, without a dot');
  Assert.IsTrue(Items[Index].Kind = ockAttribute, 'as an attribute');
  Assert.AreEqual('aString', Items[Index].Name);
  Assert.AreEqual(': String', Items[Index].Detail);
  Index := IndexOf(Items, 'part');
  Assert.IsTrue((Index >= 0) and (Items[Index].Kind = ockMultiLink), 'part is a multi link');
  Assert.AreEqual(': List<SomeClass>', Items[Index].Detail);
  Index := IndexOf(Items, 'next');
  Assert.IsTrue((Index >= 0) and (Items[Index].Kind = ockSingleLink), 'next is a single link');
  Assert.AreEqual(': SomeClass', Items[Index].Detail);
end;

procedure TTestBoldOclExpressionCheck.TestMembersAndOperationsAfterAnExpression;
var
  Items: TBoldOclCompletions;
  Index: Integer;
begin
  Items := ListFor('part', SomeClass, cAllMembers);
  Assert.IsTrue(IndexOf(Items, '.aString') >= 0, 'members come with a dot');
  Assert.IsTrue(IndexOf(Items, 'aString') = -1, 'and only with a dot');
  Index := IndexOf(Items, '->size');
  Assert.IsTrue(Index >= 0, '->size is listed');
  Assert.IsTrue(Items[Index].Kind = ockOperation, 'as an operation');
  Assert.AreEqual('->size', Items[Index].Name);
  Assert.AreEqual(' : Integer', Items[Index].Detail);
  Assert.IsTrue(Assigned(Items[Index].Symbol), 'with its symbol');
  Index := IndexOf(Items, '->select(«Boolean»)');
  Assert.IsTrue(Index >= 0, '->select is listed with a placeholder for its argument');
  Assert.AreEqual('->select', Items[Index].Name);
  Assert.AreEqual('(«Boolean»)', Items[Index].Detail);
  Index := IndexOf(Items, '->subSequence(«Integer»,«Integer»)');
  Assert.IsTrue(Index >= 0, 'two arguments are separated by a comma');
  Index := IndexOf(Items, '->includesAll(«List<AnyArg>»)');
  Assert.IsTrue(Index >= 0, 'a list argument');
  Assert.AreEqual('(«List<AnyArg>») : Boolean', Items[Index].Detail);
  Items := ListFor('next', SomeClass, cAllMembers);
  Assert.IsTrue(IndexOf(Items, '.oclIsKindOf(«MetaType»)') >= 0, 'an operation with a dot');
  Assert.IsTrue(IndexOf(Items, '.aString') >= 0, 'the members of a single link');
end;

procedure TTestBoldOclExpressionCheck.TestClassesOfTheSystem;
var
  Items: TBoldOclCompletions;
  Index: Integer;
begin
  Items := ListFor('', SystemType, cAllMembers);
  Assert.AreEqual(SystemType.TopSortedClasses.Count, Integer(Length(Items)));
  Assert.AreEqual(Integer(Length(Items)), CountOf(Items, ockType));
  Index := IndexOf(Items, 'SomeClass');
  Assert.IsTrue(Index >= 0, 'SomeClass is listed');
  Assert.AreEqual(': TSomeClass', Items[Index].Detail);
end;

procedure TTestBoldOclExpressionCheck.TestNothingIsListedForAnInvalidExpression;
begin
  Assert.AreEqual(0, Integer(Length(ListFor('part.nosuch', SomeClass, cAllMembers))));
end;

procedure TTestBoldOclExpressionCheck.TestMemberOptions;
var
  All, Items: TBoldOclCompletions;
begin
  All := ListFor('', SomeClass, cAllMembers);
  Assert.IsTrue(IndexOf(All, 'aString') >= 0, 'aString is listed');
  // aString is a persistent attribute
  Items := ListFor('', SomeClass, [omoDerived, omoTransient]);
  Assert.IsTrue(IndexOf(Items, 'aString') = -1, 'not without Persistent');
  Assert.IsTrue(Length(Items) < Length(All), 'fewer members');
  Items := ListFor('', SomeClass, [omoPersistent]);
  Assert.IsTrue(IndexOf(Items, 'aString') >= 0, 'with Persistent only');
end;

procedure TTestBoldOclExpressionCheck.TestFilter;
var
  Items: TBoldOclCompletions;
  Attribute, Single, Multi, Operation: TBoldOclCompletion;
begin
  Items := ListFor('part', SomeClass, cAllMembers);
  Attribute := Items[IndexOf(Items, '.aString')];
  Single := Items[IndexOf(Items, '.next')];
  Multi := Items[IndexOf(Items, '.part')];
  Operation := Items[IndexOf(Items, '->size')];
  Assert.IsTrue(BoldOclCompletionPasses(Attribute, ocfAll, '') and BoldOclCompletionPasses(Operation, ocfAll, ''), 'All');
  Assert.IsTrue(BoldOclCompletionPasses(Attribute, ocfAttributes, '') and
    not BoldOclCompletionPasses(Single, ocfAttributes, ''), 'Attributes');
  Assert.IsTrue(BoldOclCompletionPasses(Single, ocfSingleLinks, '') and
    not BoldOclCompletionPasses(Multi, ocfSingleLinks, ''), 'Single links');
  Assert.IsTrue(BoldOclCompletionPasses(Multi, ocfMultiLinks, '') and
    not BoldOclCompletionPasses(Single, ocfMultiLinks, ''), 'Multi links');
  Assert.IsTrue(BoldOclCompletionPasses(Operation, ocfOperations, '') and
    not BoldOclCompletionPasses(Attribute, ocfOperations, ''), 'Operations');
  Assert.IsTrue(BoldOclCompletionPasses(Attribute, ocfText, 'STRING') and
    BoldOclCompletionPasses(Attribute, ocfText, '') and not BoldOclCompletionPasses(Single, ocfText, 'string'),
    'a text, in the name or the type, in any case');
end;

procedure TTestBoldOclExpressionCheck.TestVariables;
var
  Items: TBoldOclCompletions;
  Variables: TBoldExternalVariableList;
begin
  BoldOclListVariables(SomeClass, nil, Items);
  Assert.IsTrue(IndexOf(Items, 'today') >= 0, 'the evaluator''s variable today');
  Assert.IsTrue(IndexOf(Items, 'now') >= 0, 'and now');
  Assert.IsTrue(IndexOf(Items, 'Nil') = -1, 'Nil is hidden');
  Assert.IsTrue(IndexOf(Items, 'True') = -1, 'True is hidden');
  Assert.AreEqual(': Date', Items[IndexOf(Items, 'today')].Detail);
  Variables := TBoldExternalVariableList.CreateWithStringVariable('greeting', 'hello', SystemType.Evaluator);
  try
    BoldOclListVariables(SomeClass, Variables, Items);
    Assert.AreEqual('greeting', Items[0].InsertText, 'the component''s variables come first');
    Assert.AreEqual(': String', Items[0].Detail);
    Assert.IsTrue(Items[0].Kind = ockAttribute, 'an attribute value');
  finally
    Variables.Free;
  end;
end;

procedure TTestBoldOclExpressionCheck.TestRemoveLast;
begin
  Assert.AreEqual('part.next', BoldOclRemoveLast('part.next.aString'));
  Assert.AreEqual('part', BoldOclRemoveLast('part->size'));
  Assert.AreEqual('part', BoldOclRemoveLast('part->select(«Boolean»)'));
  Assert.AreEqual('part->select(aString =', BoldOclRemoveLast('part->select(aString = ''x'')'));
  Assert.AreEqual('', BoldOclRemoveLast('part'));
  Assert.AreEqual('', BoldOclRemoveLast(''));
  Assert.AreEqual('part', BoldOclRemoveLast('  part.next  '));
end;

procedure TTestBoldOclExpressionCheck.TestPlaceholderAt;
var
  Start, Count: Integer;
const
  cText = 'part->select(«Boolean»)->size';
begin
  // 13 characters before the placeholder, which has 9
  Assert.IsTrue(BoldOclPlaceholderAt(cText, 16, Start, Count), 'the caret inside the placeholder');
  Assert.AreEqual(13, Start);
  Assert.AreEqual(9, Count);
  Assert.IsTrue(BoldOclPlaceholderAt(cText, 14, Start, Count), 'right after the opening mark');
  Assert.IsFalse(BoldOclPlaceholderAt(cText, 5, Start, Count), 'before it');
  Assert.IsFalse(BoldOclPlaceholderAt(cText, 25, Start, Count), 'after it');
  Assert.IsFalse(BoldOclPlaceholderAt('part', 2, Start, Count), 'no placeholder');
  Assert.IsFalse(BoldOclPlaceholderAt('', 0, Start, Count), 'no text');
end;

initialization
  BoldRegisterTest(TTestBoldOclExpressionCheck);

end.
