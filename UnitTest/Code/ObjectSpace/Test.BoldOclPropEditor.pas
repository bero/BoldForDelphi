unit Test.BoldOclPropEditor;

{ Tests for TBoldOclPropEditForm, the OCL editor (#124): what the form shows
  for an expression - the parser messages, the list of members, classes and
  operations that can follow it, the variables - and what its buttons, filter
  and check boxes do. The form is created and never shown. Only the type
  information of the test model is used; no system is opened.

  Windows only: the form is a VCL form. On other platforms the unit is
  empty. }

interface

{$IFDEF MSWINDOWS}
uses
  Classes,
  SysUtils,
  Vcl.StdCtrls,
  BoldTestFramework,
  BoldElements,
  BoldSystemRT,
  BoldOclPropEditor,
  maan_UndoRedoBase;

type
  [Category('OCL')]
  TTestBoldOclPropEditor = class(TBoldTestCase)
  private
    FForm: TBoldOclPropEditForm;
    function SystemType: TBoldSystemTypeInfo;
    function SomeClass: TBoldElementTypeInfo;
    function Messages: string;
    function IndexOfLine(AList: TListBox; const ALine: string): Integer;
    procedure SelectLine(const ALine: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCaptionNamesTheContext;
    procedure TestEmptyExpressionListsTheMembersOfTheContext;
    procedure TestValidExpressionShowsTheTypeAndWhatCanFollow;
    procedure TestErrorNearTheStart;
    procedure TestErrorFurtherIn;
    procedure TestSyntaxErrorIsHiddenUnlessAsked;
    procedure TestFilterByKind;
    procedure TestFilterByText;
    procedure TestPersistentCheckBox;
    procedure TestDoubleClickAddsTheLine;
    procedure TestEnterAddsTheLine;
    procedure TestRemoveLastAndClear;
    procedure TestVariables;
    procedure TestSystemContextListsTheClasses;
    procedure TestNoContext;
    procedure TestPlaceholderIsSelectedAsAWhole;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

{ TTestBoldOclPropEditor }

procedure TTestBoldOclPropEditor.SetUp;
begin
  inherited;
  EnsureDM;
  FForm := TBoldOclPropEditForm.Create(nil);
  FForm.Context := SomeClass;
end;

procedure TTestBoldOclPropEditor.TearDown;
begin
  FreeAndNil(FForm);
  inherited;
end;

function TTestBoldOclPropEditor.SystemType: TBoldSystemTypeInfo;
begin
  Result := dmUndoRedo.BoldSystemHandle1.StaticSystemTypeInfo;
end;

function TTestBoldOclPropEditor.SomeClass: TBoldElementTypeInfo;
begin
  Result := SystemType.ClassTypeInfoByExpressionName['SomeClass'];
end;

function TTestBoldOclPropEditor.Messages: string;
begin
  Result := TrimRight(FForm.ParserMessages.Lines.Text);
end;

function TTestBoldOclPropEditor.IndexOfLine(AList: TListBox; const ALine: string): Integer;
begin
  Result := AList.Items.IndexOf(ALine);
end;

procedure TTestBoldOclPropEditor.SelectLine(const ALine: string);
begin
  FForm.SelectBox.ItemIndex := IndexOfLine(FForm.SelectBox, ALine);
  Assert.IsTrue(FForm.SelectBox.ItemIndex >= 0, 'in the list: ' + ALine);
end;

procedure TTestBoldOclPropEditor.TestCaptionNamesTheContext;
begin
  Assert.AreEqual('OCL expression editor - Context: SomeClass', FForm.Caption);
end;

procedure TTestBoldOclPropEditor.TestEmptyExpressionListsTheMembersOfTheContext;
var
  i: Integer;
begin
  Assert.AreEqual('', FForm.OclExpr);
  Assert.AreEqual('Empty Expression', Messages);
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '1 | aString: String') >= 0, 'the attribute aString, without a dot');
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '3 | part: List<SomeClass>') >= 0, 'the multi link part');
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '2 | next: SomeClass') >= 0, 'the single link next');
  for i := 0 to FForm.SelectBox.Items.Count - 1 do
    Assert.IsTrue(FForm.SelectBox.Items[i][1] <> '5', 'no operation before an expression: ' + FForm.SelectBox.Items[i]);
end;

procedure TTestBoldOclPropEditor.TestValidExpressionShowsTheTypeAndWhatCanFollow;
var
  Index: Integer;
begin
  FForm.OclExpr := 'part';
  Assert.AreEqual('part', FForm.OclExpr);
  Assert.AreEqual('Syntax is OK' + sLineBreak + sLineBreak + 'Current type is: Collection(SomeClass)', Messages);
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '1 | .aString: String') >= 0, 'members come with a dot');
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '1 | aString: String') = -1, 'and only with a dot');
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '3 | .part: List<SomeClass>') >= 0, 'the multi link');
  Index := IndexOfLine(FForm.SelectBox, '5 | ->size : Integer');
  Assert.IsTrue(Index >= 0, 'an operation with its result type');
  // No result type: the line ends with a space
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '5 | ->select(«Boolean») ') >= 0, 'an operation with an argument');
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '5 | ->subSequence(«Integer»,«Integer») ') >= 0, 'two arguments');
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '5 | ->includesAll(«List<AnyArg>») : Boolean') >= 0, 'a list argument');
end;

procedure TTestBoldOclPropEditor.TestErrorNearTheStart;
begin
  FForm.OclExpr := 'part.nosuch';
  Assert.AreEqual('part.nosuch' + sLineBreak + '     ^-------' + sLineBreak +
    '5:nosuch is not a member of SomeClass', Messages);
  Assert.AreEqual(0, FForm.SelectBox.Items.Count, 'nothing can follow an invalid expression');
end;

procedure TTestBoldOclPropEditor.TestErrorFurtherIn;
begin
  FForm.OclExpr := 'next.next.nosuch';
  Assert.AreEqual('next.next.nosuch' + sLineBreak + '   -------^' + sLineBreak +
    '10:nosuch is not a member of SomeClass', Messages);
  FForm.OclExpr := 'aString  + 1';
  Assert.IsTrue(Pos('aString  + 1' + sLineBreak + '         ^-------' + sLineBreak + '9:', Messages) = 1, Messages);
end;

procedure TTestBoldOclPropEditor.TestSyntaxErrorIsHiddenUnlessAsked;
begin
  FForm.OclExpr := 'part->';
  Assert.AreEqual('Unable to complete parse, syntax error', Messages);
  FForm.Syntaxcb.Checked := True;
  Assert.IsTrue(Pos('part->', Messages) = 1, Messages);
  Assert.IsTrue(Pos('SSYacc', Messages) > 0, Messages);
end;

procedure TTestBoldOclPropEditor.TestFilterByKind;
var
  i, All: Integer;
begin
  FForm.OclExpr := 'part';
  All := FForm.SelectBox.Items.Count;
  FForm.filterCombo.ItemIndex := 1;
  FForm.filterComboChange(nil);
  Assert.IsTrue((FForm.SelectBox.Items.Count > 0) and (FForm.SelectBox.Items.Count < All), 'fewer lines');
  for i := 0 to FForm.SelectBox.Items.Count - 1 do
    Assert.IsTrue(FForm.SelectBox.Items[i][1] = '1', 'Attributes: ' + FForm.SelectBox.Items[i]);
  FForm.filterCombo.ItemIndex := 2;
  FForm.filterComboChange(nil);
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '2 | .next: SomeClass') >= 0, 'Single links has next');
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '3 | .part: List<SomeClass>') = -1, 'and not part');
  FForm.filterCombo.ItemIndex := 3;
  FForm.filterComboChange(nil);
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '3 | .part: List<SomeClass>') >= 0, 'Multi links has part');
  FForm.filterCombo.ItemIndex := 4;
  FForm.filterComboChange(nil);
  for i := 0 to FForm.SelectBox.Items.Count - 1 do
    Assert.IsTrue(FForm.SelectBox.Items[i][1] = '5', 'Operations: ' + FForm.SelectBox.Items[i]);
  FForm.filterCombo.ItemIndex := 0;
  FForm.filterComboChange(nil);
  Assert.AreEqual(All, FForm.SelectBox.Items.Count, 'All');
end;

procedure TTestBoldOclPropEditor.TestFilterByText;
var
  i: Integer;
begin
  FForm.OclExpr := 'part';
  FForm.filterCombo.ItemIndex := -1;
  FForm.filterCombo.Text := 'STRING';
  FForm.filterComboChange(nil);
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '1 | .aString: String') >= 0, 'aString has the text');
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '2 | .next: SomeClass') = -1, 'next has not');
  for i := 0 to FForm.SelectBox.Items.Count - 1 do
    Assert.IsTrue(Pos('string', LowerCase(FForm.SelectBox.Items[i])) > 0, FForm.SelectBox.Items[i]);
end;

procedure TTestBoldOclPropEditor.TestPersistentCheckBox;
var
  All: Integer;
begin
  All := FForm.SelectBox.Items.Count;
  FForm.Persistentcb.Checked := False;
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '1 | aString: String') = -1, 'aString is persistent');
  Assert.IsTrue(FForm.SelectBox.Items.Count < All, 'fewer lines');
  FForm.Persistentcb.Checked := True;
  Assert.AreEqual(All, FForm.SelectBox.Items.Count);
end;

procedure TTestBoldOclPropEditor.TestDoubleClickAddsTheLine;
begin
  FForm.OclExpr := 'part';
  SelectLine('5 | ->select(«Boolean») ');
  FForm.SelectBoxDblClick(FForm.SelectBox);
  Assert.AreEqual('part->select(«Boolean»)', FForm.OclExpr);
  FForm.OclExpr := 'part';
  SelectLine('1 | .aString: String');
  FForm.SelectBoxDblClick(FForm.SelectBox);
  Assert.AreEqual('part.aString', FForm.OclExpr);
  Assert.AreEqual('Syntax is OK' + sLineBreak + sLineBreak + 'Current type is: Collection(String)', Messages);
end;

procedure TTestBoldOclPropEditor.TestEnterAddsTheLine;
var
  Key: Char;
begin
  FForm.OclExpr := 'part';
  SelectLine('5 | ->size : Integer');
  Key := #13;
  FForm.SelectBoxKeyPress(FForm.SelectBox, Key);
  Assert.AreEqual('part->size', FForm.OclExpr);
end;

procedure TTestBoldOclPropEditor.TestRemoveLastAndClear;
begin
  FForm.OclExpr := 'part.next.aString';
  FForm.RemoveLastClick(nil);
  Assert.AreEqual('part.next', FForm.OclExpr);
  FForm.OclExpr := 'part->size';
  FForm.RemoveLastClick(nil);
  Assert.AreEqual('part', FForm.OclExpr);
  FForm.RemoveLastClick(nil);
  Assert.AreEqual('', FForm.OclExpr);
  FForm.OclExpr := 'part';
  FForm.ClearClick(nil);
  Assert.AreEqual('', FForm.OclExpr);
  Assert.AreEqual('Empty Expression', Messages);
end;

procedure TTestBoldOclPropEditor.TestVariables;
var
  Variables: TBoldExternalVariableList;
begin
  Assert.IsTrue(IndexOfLine(FForm.VariablesListBox, '1 | today: Date') >= 0, 'the evaluator''s variable today');
  Assert.IsTrue(IndexOfLine(FForm.VariablesListBox, '1 | now: DateTime') >= 0, 'and now');
  Variables := TBoldExternalVariableList.CreateWithStringVariable('greeting', 'hello', SystemType.Evaluator);
  try
    FForm.Variables := Variables;
    Assert.AreEqual('1 | greeting: String', FForm.VariablesListBox.Items[0], 'the given variables come first');
    FForm.OclExpr := 'aString + ';
    FForm.VariablesListBox.ItemIndex := 0;
    FForm.SelectBoxDblClick(FForm.VariablesListBox);
    Assert.AreEqual('aString + greeting', FForm.OclExpr);
  finally
    FForm.Variables := nil;
    Variables.Free;
  end;
end;

procedure TTestBoldOclPropEditor.TestSystemContextListsTheClasses;
begin
  FForm.Context := SystemType;
  Assert.AreEqual(SystemType.TopSortedClasses.Count, FForm.SelectBox.Items.Count);
  Assert.IsTrue(IndexOfLine(FForm.SelectBox, '4 | SomeClass: TSomeClass') >= 0, 'SomeClass with its Delphi name');
end;

procedure TTestBoldOclPropEditor.TestNoContext;
var
  Form: TBoldOclPropEditForm;
begin
  Form := TBoldOclPropEditForm.Create(nil);
  try
    Form.Context := nil;
    Assert.AreEqual('OCL expression editor - Context: <unknown>', Form.Caption);
    Assert.AreEqual('No Context, No design support', TrimRight(Form.ParserMessages.Lines.Text));
    Assert.AreEqual(0, Form.SelectBox.Items.Count);
  finally
    Form.Free;
  end;
end;

procedure TTestBoldOclPropEditor.TestPlaceholderIsSelectedAsAWhole;
var
  Key: Word;
begin
  FForm.OclExpr := 'part->select(«Boolean»)->size';
  FForm.EditMemo.SelStart := 16;
  FForm.EditMemo.SelLength := 0;
  Key := 0;
  FForm.EditMemoKeyUp(FForm.EditMemo, Key, []);
  Assert.AreEqual(13, FForm.EditMemo.SelStart);
  Assert.AreEqual(9, FForm.EditMemo.SelLength);
  FForm.EditMemo.SelStart := 5;
  FForm.EditMemo.SelLength := 0;
  FForm.EditMemoKeyUp(FForm.EditMemo, Key, []);
  Assert.AreEqual(0, FForm.EditMemo.SelLength, 'no placeholder around the caret');
end;

initialization
  BoldRegisterTest(TTestBoldOclPropEditor);
{$ENDIF}

end.
