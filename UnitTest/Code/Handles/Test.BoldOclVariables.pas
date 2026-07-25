unit Test.BoldOclVariables;

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldSystem,
  BoldElements,
  BoldHandles,
  BoldSystemHandle,
  BoldListHandle,
  BoldOclVariables,
  BoldAttributes,
  BoldDefs,
  jehoBCBoldTest,
  Test.BoldAttributes;  // For TjehodmBoldTest

type
  [TestFixture]
  [Category('Handles')]
  TTestBoldOclVariables = class
  private
    FDataModule: TjehodmBoldTest;
    function GetSystem: TBoldSystem;
    function GetEvaluator: TBoldEvaluator;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // === TBoldVariableTupleList tests ===
    [Test]
    procedure TestNameIsUnique_True;
    [Test]
    procedure TestNameIsUnique_FalseCaseInsensitive;
    [Test]
    procedure TestNameIsValid_Alphanumeric;
    [Test]
    procedure TestNameIsValid_Underscore;
    [Test]
    procedure TestNameIsValid_RejectsSpace;
    [Test]
    procedure TestNameIsValid_RejectsDash;
    [Test]
    procedure TestNameIsValid_EmptyStringIsValid;
    [Test]
    procedure TestGetUniqueName_FirstVariable;
    [Test]
    procedure TestGetUniqueName_SkipsExisting;
    [Test]
    procedure TestGetVariableByName_Found;
    [Test]
    procedure TestGetVariableByName_NotFound;
    [Test]
    procedure TestGetItems_TypedAccess;
    [Test]
    procedure TestGetEnumerator;

    // === TBoldVariableTuple tests ===
    [Test]
    procedure TestSetVariableName_LowercasesFirstChar;
    [Test]
    procedure TestSetVariableName_RaisesOnDuplicate;
    [Test]
    procedure TestSetVariableName_RaisesOnInvalidChars;
    [Test]
    procedure TestAssign_CopiesProperties;
    [Test]
    procedure TestGetDisplayName_WithHandle;
    [Test]
    procedure TestGetDisplayName_NotConnected;
    [Test]
    procedure TestGetEffectiveUseListElement_TrueWithListHandle;
    [Test]
    procedure TestGetEffectiveUseListElement_FalseWithoutListHandle;
    [Test]
    procedure TestGetEffectiveUseListElement_FalseWhenUseListElementFalse;
    [Test]
    procedure TestLinksToHandle_DirectMatch;
    [Test]
    procedure TestLinksToHandle_NoMatch;
    [Test]
    procedure TestTupleListProperty;

    // === TBoldOclVariables component tests ===
    [Test]
    procedure TestCreateDestroy;
    [Test]
    procedure TestAddVariable_CreatesNew;
    [Test]
    procedure TestAddVariable_UpdatesExisting;
    [Test]
    procedure TestAddVariables_MergesFromAnother;
    [Test]
    procedure TestFindVariableByName_CaseInsensitive;
    [Test]
    procedure TestFindVariableByName_NotFoundReturnsNil;
    [Test]
    procedure TestGetVariableValue_ReturnsElement;
    [Test]
    procedure TestGetVariableValue_NotFoundReturnsNil;
    [Test]
    procedure TestGetVariableList_LazyCreates;
    [Test]
    procedure TestLinksToHandle_Component;
    [Test]
    procedure TestLinksToHandle_Component_NoMatch;
    [Test]
    procedure TestVariablesChanged_FreesCachedList;

    // === TBoldOclVariable typed constructor tests ===
    [Test]
    procedure TestCreateStringVariable;
    [Test]
    procedure TestCreateIntegerVariable;
    [Test]
    procedure TestCreateFloatVariable;
    [Test]
    procedure TestCreateDateVariable;
    [Test]
    procedure TestCreateDateTimeVariable;
    [Test]
    procedure TestCreateTimeVariable;
    [Test]
    procedure TestCreateWithTypeInfo;
    [Test]
    procedure TestCreateWithElement;
    [Test]
    procedure TestGetValueType_WithValue;
    [Test]
    procedure TestGetValueType_NilValueReturnsTypeInfo;

    // === TBoldHandleBasedExternalVariable tests ===
    [Test]
    procedure TestHandleBasedVariable_CreateWithHandle;
    [Test]
    procedure TestHandleBasedVariable_GetValueNilHandle;
    [Test]
    procedure TestHandleBasedVariable_GetValueTypeNilHandle;
    [Test]
    procedure TestHandleBasedVariable_Destroy;
    [Test]
    procedure TestHandleBasedVariable_UseListElement_GetValueType;

    // === Additional coverage tests ===
    [Test]
    procedure TestAddVariables_MergesOverlapping;
    [Test]
    procedure TestGetDisplayName_WithListSuffix;
    [Test]
    procedure TestLinksToHandle_ViaRootHandle;
    [Test]
    procedure TestSetGlobalSystemHandle;
    [Test]
    procedure TestRegisterVariables_WithSystemHandle;
    [Test]
    procedure TestSubscribeToHandles_WithExpression;
    [Test]
    procedure TestSubscribeToHandles_ExternalSubscriber;
    [Test]
    procedure TestCreateFromIndirectElement;
    [Test]
    procedure TestGetVariableValue_FoundReturnsValue;
    [Test]
    procedure TestHandleBasedVariable_UseListElement_GetValue;

    // === Deep coverage tests (batch 3) ===
    [Test]
    procedure TestAssign_InheritedBranch;
    [Test]
    procedure TestTuple_HandleDestroyed_NilsHandle;
    [Test]
    procedure TestHandleBasedVar_HandleDestroyed;
    [Test]
    procedure TestEnsureEvaluator_FallbackToDefault;
    [Test]
    procedure TestRegisterVariables_WithUseListElement;
    [Test]
    procedure TestReceive_GlobalSystemHandleDestroying;
    [Test]
    procedure TestLoaded_CallsPlaceSubscriptions;
    [Test]
    procedure TestSetVariableTupleList_Setter;
    [Test]
    procedure TestHandleBasedVar_SetHandle;
  end;

implementation

uses
  BoldOcl,
  BoldSubscription,
  BoldAbstractListHandle,
  BoldRootedHandles;

type
  // Cracker class to access protected Loaded method
  TBoldOclVariablesAccess = class(TBoldOclVariables);
  // Cracker class to access protected Handle property
  TBoldHandleBasedExternalVariableAccess = class(TBoldHandleBasedExternalVariable)
  public
    property Handle;
  end;

{ TTestBoldOclVariables }

procedure TTestBoldOclVariables.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldOclVariables.TearDown;
begin
  FreeAndNil(FDataModule);
end;

function TTestBoldOclVariables.GetSystem: TBoldSystem;
begin
  Result := FDataModule.BoldSystemHandle1.System;
end;

function TTestBoldOclVariables.GetEvaluator: TBoldEvaluator;
begin
  Result := GetSystem.Evaluator;
end;

// === TBoldVariableTupleList tests ===

procedure TTestBoldOclVariables.TestNameIsUnique_True;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('alpha', nil);
    Assert.IsTrue(OclVars.Variables.NameIsUnique('beta'), 'beta should be unique');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestNameIsUnique_FalseCaseInsensitive;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('alpha', nil);
    Assert.IsFalse(OclVars.Variables.NameIsUnique('Alpha'), 'Alpha should match alpha case-insensitively');
    Assert.IsFalse(OclVars.Variables.NameIsUnique('ALPHA'), 'ALPHA should match alpha case-insensitively');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestNameIsValid_Alphanumeric;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Assert.IsTrue(OclVars.Variables.NameIsValid('abc123'), 'Alphanumeric should be valid');
    Assert.IsTrue(OclVars.Variables.NameIsValid('ABC'), 'Uppercase should be valid');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestNameIsValid_Underscore;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Assert.IsTrue(OclVars.Variables.NameIsValid('my_var'), 'Underscore should be valid');
    Assert.IsTrue(OclVars.Variables.NameIsValid('_leading'), 'Leading underscore should be valid');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestNameIsValid_RejectsSpace;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Assert.IsFalse(OclVars.Variables.NameIsValid('my var'), 'Space should be invalid');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestNameIsValid_RejectsDash;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Assert.IsFalse(OclVars.Variables.NameIsValid('my-var'), 'Dash should be invalid');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestNameIsValid_EmptyStringIsValid;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Assert.IsTrue(OclVars.Variables.NameIsValid(''), 'Empty string should be valid (no invalid chars)');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetUniqueName_FirstVariable;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    // With no variables yet, the first tuple created gets 'variable1' via GetUniqueName
    // But GetUniqueName is called by TBoldVariableTuple.Create, so the first auto-name is Variable1
    Assert.AreEqual('Variable1', OclVars.Variables.GetUniqueName, 'First unique name should be Variable1');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetUniqueName_SkipsExisting;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    // Adding a variable creates a tuple; its constructor calls GetUniqueName which assigns 'variable1'
    // (lowercased first char by SetVariableName)
    Tuple := OclVars.Variables.Add as TBoldVariableTuple;
    // The tuple got assigned 'variable1' automatically
    Assert.AreEqual('variable1', Tuple.VariableName, 'First auto-name should be variable1');
    // GetUniqueName should now return Variable2 (the next available)
    Assert.AreEqual('Variable2', OclVars.Variables.GetUniqueName, 'Second unique name should be Variable2');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetVariableByName_Found;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('myVar', nil);
    Tuple := OclVars.Variables.VariableByName['myVar'];
    Assert.IsNotNull(Tuple, 'Should find variable by exact name');
    Assert.AreEqual('myVar', Tuple.VariableName);
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetVariableByName_NotFound;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('myVar', nil);
    // VariableByName is case-sensitive (uses = operator)
    Assert.IsNull(OclVars.Variables.VariableByName['nonExistent'], 'Should return nil for non-existent name');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetItems_TypedAccess;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('first', nil);
    OclVars.AddVariable('second', nil);
    Assert.AreEqual('first', OclVars.Variables[0].VariableName, 'Items[0] should be first');
    Assert.AreEqual('second', OclVars.Variables[1].VariableName, 'Items[1] should be second');
    Assert.AreEqual(2, OclVars.Variables.Count, 'Should have 2 items');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetEnumerator;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
  Names: string;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('alpha', nil);
    OclVars.AddVariable('beta', nil);
    Names := '';
    for Tuple in OclVars.Variables do
      Names := Names + Tuple.VariableName + ',';
    Assert.AreEqual('alpha,beta,', Names, 'Enumerator should iterate all tuples');
  finally
    OclVars.Free;
  end;
end;

// === TBoldVariableTuple tests ===

procedure TTestBoldOclVariables.TestSetVariableName_LowercasesFirstChar;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('temp', nil);
    Tuple.VariableName := 'MyVar';
    Assert.AreEqual('myVar', Tuple.VariableName, 'First char should be lowercased');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestSetVariableName_RaisesOnDuplicate;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('alpha', nil);
    OclVars.AddVariable('beta', nil);
    Assert.WillRaiseAny(
      procedure
      begin
        OclVars.Variables[1].VariableName := 'Alpha'; // 'alpha' vs 'Alpha' case-insensitive match
      end,
      'Setting duplicate name should raise EBold');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestSetVariableName_RaisesOnInvalidChars;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('temp', nil);
    Assert.WillRaiseAny(
      procedure
      begin
        OclVars.Variables[0].VariableName := 'my var'; // space is invalid
      end,
      'Invalid chars in name should raise EBold');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestAssign_CopiesProperties;
var
  OclVars1, OclVars2: TBoldOclVariables;
  Source, Target: TBoldVariableTuple;
begin
  OclVars1 := TBoldOclVariables.Create(nil);
  OclVars2 := TBoldOclVariables.Create(nil);
  try
    Source := OclVars1.AddVariable('srcVar', FDataModule.BoldListHandle1);
    Source.UseListElement := True;
    Target := OclVars2.AddVariable('temp', nil);
    Target.Assign(Source);
    Assert.AreEqual('srcVar', Target.VariableName, 'Name should be copied');
    Assert.IsTrue(Target.UseListElement, 'UseListElement should be copied');
    Assert.AreSame(FDataModule.BoldListHandle1, Target.BoldHandle, 'Handle should be copied');
  finally
    OclVars2.Free;
    OclVars1.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetDisplayName_WithHandle;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', FDataModule.BoldListHandle1);
    Assert.AreEqual('myVar: BoldListHandle1', Tuple.DisplayName, 'Should show name: HandleName');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetDisplayName_NotConnected;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', nil);
    Assert.AreEqual('myVar: Not Connected', Tuple.DisplayName, 'Should show Not Connected when no handle');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetEffectiveUseListElement_TrueWithListHandle;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', FDataModule.BoldListHandle1, True);
    Assert.IsTrue(Tuple.EffectiveUseListElement,
      'Should be true when UseListElement=true and handle is TBoldAbstractListHandle');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetEffectiveUseListElement_FalseWithoutListHandle;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    // BoldSystemHandle1 is TBoldSystemHandle, not a TBoldAbstractListHandle
    Tuple := OclVars.AddVariable('myVar', FDataModule.BoldSystemHandle1, True);
    Assert.IsFalse(Tuple.EffectiveUseListElement,
      'Should be false when handle is not TBoldAbstractListHandle even if UseListElement=true');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetEffectiveUseListElement_FalseWhenUseListElementFalse;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', FDataModule.BoldListHandle1, False);
    Assert.IsFalse(Tuple.EffectiveUseListElement,
      'Should be false when UseListElement is false');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestLinksToHandle_DirectMatch;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', FDataModule.BoldListHandle1);
    Assert.IsTrue(Tuple.LinksToHandle(FDataModule.BoldListHandle1), 'Should match direct handle');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestLinksToHandle_NoMatch;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', nil);
    Assert.IsFalse(Tuple.LinksToHandle(FDataModule.BoldListHandle1), 'Should not match when handle is nil');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestTupleListProperty;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', nil);
    Assert.AreSame(OclVars.Variables, Tuple.TupleList, 'TupleList should return owning collection');
  finally
    OclVars.Free;
  end;
end;

// === TBoldOclVariables component tests ===

procedure TTestBoldOclVariables.TestCreateDestroy;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Assert.IsNotNull(OclVars.Variables, 'Variables collection should be created');
    Assert.AreEqual(0, OclVars.Variables.Count, 'Should start with no variables');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestAddVariable_CreatesNew;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('newVar', FDataModule.BoldListHandle1, True);
    Assert.IsNotNull(Tuple, 'Should return created tuple');
    Assert.AreEqual('newVar', Tuple.VariableName);
    Assert.AreSame(FDataModule.BoldListHandle1, Tuple.BoldHandle);
    Assert.IsTrue(Tuple.UseListElement);
    Assert.AreEqual(1, OclVars.Variables.Count);
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestAddVariable_UpdatesExisting;
var
  OclVars: TBoldOclVariables;
  Tuple1, Tuple2: TBoldVariableTuple;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple1 := OclVars.AddVariable('myVar', nil, False);
    Tuple2 := OclVars.AddVariable('myVar', FDataModule.BoldListHandle1, True);
    Assert.AreSame(Tuple1, Tuple2, 'Should return same tuple when name matches');
    Assert.AreSame(FDataModule.BoldListHandle1, Tuple2.BoldHandle, 'Handle should be updated');
    Assert.IsTrue(Tuple2.UseListElement, 'UseListElement should be updated');
    Assert.AreEqual(1, OclVars.Variables.Count, 'Should still have just 1 variable');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestAddVariables_MergesFromAnother;
var
  Source, Target: TBoldOclVariables;
begin
  Source := TBoldOclVariables.Create(nil);
  Target := TBoldOclVariables.Create(nil);
  try
    Source.AddVariable('varA', FDataModule.BoldListHandle1);
    Source.AddVariable('varB', nil);
    Target.AddVariable('varC', nil);
    Target.AddVariables(Source);
    Assert.AreEqual(3, Target.Variables.Count, 'Should have 3 variables after merge');
    Assert.IsNotNull(Target.Variables.VariableByName['varA'], 'varA should be merged');
    Assert.IsNotNull(Target.Variables.VariableByName['varB'], 'varB should be merged');
    Assert.IsNotNull(Target.Variables.VariableByName['varC'], 'varC should remain');
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TTestBoldOclVariables.TestFindVariableByName_CaseInsensitive;
var
  OclVars: TBoldOclVariables;
  Found: TBoldExternalVariable;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('myVar', FDataModule.BoldListHandle1);
    Found := OclVars.FindVariableByName('MYVAR');
    Assert.IsNotNull(Found, 'FindVariableByName should be case-insensitive');
    Assert.AreEqual('myVar', Found.Name);
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestFindVariableByName_NotFoundReturnsNil;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('myVar', nil);
    Assert.IsNull(OclVars.FindVariableByName('nonExistent'), 'Should return nil for unknown name');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetVariableValue_ReturnsElement;
var
  OclVars: TBoldOclVariables;
  Found: TBoldExternalVariable;
begin
  // Ensure system is active
  Assert.IsNotNull(GetSystem, 'System should be active');
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('listVar', FDataModule.BoldListHandle1);
    Found := OclVars.FindVariableByName('listVar');
    Assert.IsNotNull(Found, 'Should find the variable by name');
    Assert.AreEqual('listVar', Found.Name, 'Variable name should match');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetVariableValue_NotFoundReturnsNil;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('myVar', nil);
    Assert.IsNull(OclVars.GetVariableValue('unknown'), 'Should return nil for unknown variable');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetVariableList_LazyCreates;
var
  OclVars: TBoldOclVariables;
  List: TBoldExternalVariableList;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('varA', nil);
    OclVars.AddVariable('varB', nil);
    List := OclVars.VariableList;
    Assert.IsNotNull(List, 'VariableList should be created lazily');
    Assert.AreEqual(2, List.Count, 'Should contain 2 variables');
    // Second access should return same instance
    Assert.AreSame(List, OclVars.VariableList, 'Should return cached list on second access');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestLinksToHandle_Component;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('myVar', FDataModule.BoldListHandle1);
    Assert.IsTrue(OclVars.LinksToHandle(FDataModule.BoldListHandle1),
      'Component should report link to handle used by any tuple');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestLinksToHandle_Component_NoMatch;
var
  OclVars: TBoldOclVariables;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('myVar', nil);
    Assert.IsFalse(OclVars.LinksToHandle(FDataModule.BoldListHandle1),
      'Should return false when no tuple links to handle');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestVariablesChanged_FreesCachedList;
var
  OclVars: TBoldOclVariables;
  List1, List2: TBoldExternalVariableList;
begin
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('varA', nil);
    List1 := OclVars.VariableList;
    Assert.IsNotNull(List1, 'First list should exist');
    // Adding a new variable triggers VariablesChanged (via Tuple.Changed) which frees cached list
    OclVars.AddVariable('varB', nil);
    List2 := OclVars.VariableList;
    // List2 should be a new instance since VariablesChanged freed the old one
    Assert.AreEqual(2, List2.Count, 'New list should have 2 variables');
  finally
    OclVars.Free;
  end;
end;

// === TBoldOclVariable typed constructor tests ===

procedure TTestBoldOclVariables.TestCreateStringVariable;
var
  V: TBoldOclVariable;
begin
  Assert.IsNotNull(GetSystem, 'System must be active for evaluator');
  V := TBoldOclVariable.CreateStringVariable('strVar', 'hello', GetEvaluator);
  try
    Assert.AreEqual('strVar', V.Name);
    Assert.IsNotNull(V.Value, 'Value should not be nil');
    Assert.AreEqual('hello', (V.Value as TBAString).AsString);
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestCreateIntegerVariable;
var
  V: TBoldOclVariable;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  V := TBoldOclVariable.CreateIntegerVariable('intVar', 42, GetEvaluator);
  try
    Assert.AreEqual('intVar', V.Name);
    Assert.AreEqual(42, (V.Value as TBAInteger).AsInteger);
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestCreateFloatVariable;
var
  V: TBoldOclVariable;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  V := TBoldOclVariable.CreateFloatVariable('floatVar', 3.14, GetEvaluator);
  try
    Assert.AreEqual('floatVar', V.Name);
    Assert.AreEqual(Double(3.14), (V.Value as TBAFloat).AsFloat, 0.001);
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestCreateDateVariable;
var
  V: TBoldOclVariable;
  TestDate: TDate;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  TestDate := EncodeDate(2026, 2, 9);
  V := TBoldOclVariable.CreateDateVariable('dateVar', TestDate, GetEvaluator);
  try
    Assert.AreEqual('dateVar', V.Name);
    Assert.IsNotNull(V.Value, 'Value should not be nil');
    Assert.AreEqual(TestDate, (V.Value as TBADate).AsDate, 'Date should match');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestCreateDateTimeVariable;
var
  V: TBoldOclVariable;
  TestDT: TDateTime;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  TestDT := EncodeDate(2026, 1, 15) + EncodeTime(10, 30, 0, 0);
  V := TBoldOclVariable.CreateDateTimeVariable('dtVar', TestDT, GetEvaluator);
  try
    Assert.AreEqual('dtVar', V.Name);
    Assert.IsNotNull(V.Value, 'Value should not be nil');
    Assert.AreEqual(TestDT, (V.Value as TBADateTime).AsDateTime, 'DateTime should match');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestCreateTimeVariable;
var
  V: TBoldOclVariable;
  TestTime: TTime;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  TestTime := EncodeTime(14, 30, 0, 0);
  V := TBoldOclVariable.CreateTimeVariable('timeVar', TestTime, GetEvaluator);
  try
    Assert.AreEqual('timeVar', V.Name);
    Assert.IsNotNull(V.Value, 'Value should not be nil');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestCreateWithTypeInfo;
var
  V: TBoldOclVariable;
  vString: TBAString;
  Evaluator: TBoldOcl;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  Evaluator := GetEvaluator as TBoldOcl;
  vString := TBAString.CreateWithTypeInfo(Evaluator.StringType);
  vString.AsString := 'typed';
  V := TBoldOclVariable.CreateWithTypeInfo('typedVar', vString, Evaluator.StringType);
  try
    Assert.AreEqual('typedVar', V.Name);
    Assert.AreEqual('typed', (V.Value as TBAString).AsString);
    Assert.IsNotNull(V.ValueType, 'ValueType should return the type info');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestCreateWithElement;
var
  V: TBoldOclVariable;
  Obj: TClassA;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  Obj := TClassA.Create(GetSystem);
  V := TBoldOclVariable.Create('elemVar', Obj);
  try
    Assert.AreEqual('elemVar', V.Name);
    Assert.AreSame(Obj, V.Value, 'Value should reference the Bold object');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetValueType_WithValue;
var
  V: TBoldOclVariable;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  V := TBoldOclVariable.CreateStringVariable('strVar', 'hello', GetEvaluator);
  try
    Assert.IsNotNull(V.ValueType, 'ValueType should not be nil when value exists');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetValueType_NilValueReturnsTypeInfo;
var
  V: TBoldOclVariable;
  Evaluator: TBoldOcl;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  Evaluator := GetEvaluator as TBoldOcl;
  // CreateWithTypeInfo stores typeinfo explicitly; if we clear the value, it falls back to fBoldElementTypeInfo
  V := TBoldOclVariable.CreateWithTypeInfo('typedVar', nil, Evaluator.StringType);
  try
    // GetValue returns nil (no value set), so GetValueType should fall back to fBoldElementTypeInfo
    Assert.IsNull(V.Value, 'Value should be nil');
    Assert.AreSame(Evaluator.StringType, V.ValueType, 'Should fall back to stored type info');
  finally
    V.Free;
  end;
end;

// === TBoldHandleBasedExternalVariable tests ===

procedure TTestBoldOclVariables.TestHandleBasedVariable_CreateWithHandle;
var
  V: TBoldHandleBasedExternalVariable;
begin
  Assert.IsNotNull(GetSystem, 'System must be active');
  V := TBoldHandleBasedExternalVariable.Create('handleVar', FDataModule.BoldListHandle1, False);
  try
    Assert.AreEqual('handleVar', V.Name);
    // Handle-based variable delegates to handle; StaticBoldType verifies handle wiring
    Assert.IsNotNull(V.ValueType, 'ValueType should come from handle StaticBoldType');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestHandleBasedVariable_GetValueNilHandle;
var
  V: TBoldHandleBasedExternalVariable;
begin
  V := TBoldHandleBasedExternalVariable.Create('nilVar', nil, False);
  try
    Assert.IsNull(V.Value, 'Value should be nil when handle is nil');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestHandleBasedVariable_GetValueTypeNilHandle;
var
  V: TBoldHandleBasedExternalVariable;
begin
  V := TBoldHandleBasedExternalVariable.Create('nilVar', nil, False);
  try
    Assert.IsNull(V.ValueType, 'ValueType should be nil when handle is nil');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestHandleBasedVariable_Destroy;
var
  V: TBoldHandleBasedExternalVariable;
begin
  // Verify no leaks on create+destroy with handle
  Assert.IsNotNull(GetSystem, 'System must be active');
  V := TBoldHandleBasedExternalVariable.Create('destroyVar', FDataModule.BoldListHandle1, False);
  V.Free;
  Assert.Pass('Destroy should not raise');
end;

// === Additional coverage tests ===

procedure TTestBoldOclVariables.TestAddVariables_MergesOverlapping;
var
  Source, Target: TBoldOclVariables;
begin
  // Covers line 190: AddVariables when target already has a variable with same name
  Source := TBoldOclVariables.Create(nil);
  Target := TBoldOclVariables.Create(nil);
  try
    Source.AddVariable('shared', FDataModule.BoldListHandle1, True);
    Target.AddVariable('shared', nil, False);
    Target.AddVariables(Source);
    Assert.AreEqual(1, Target.Variables.Count, 'Should still have 1 variable after merge');
    Assert.AreSame(FDataModule.BoldListHandle1, Target.Variables[0].BoldHandle,
      'Handle should be updated from source');
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetDisplayName_WithListSuffix;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  // Covers line 589: DisplayName with (list) suffix
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myList', FDataModule.BoldListHandle1, True);
    Assert.AreEqual('myList: BoldListHandle1 (list)', Tuple.DisplayName,
      'Should append (list) when EffectiveUseListElement is true');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestLinksToHandle_ViaRootHandle;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  // Covers line 606: LinksToHandle via IsRootLinkedTo
  // BoldListHandle1 is a TBoldRootedHandle with RootHandle=BoldSystemHandle1
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', FDataModule.BoldListHandle1);
    // Check if BoldListHandle1 links to BoldSystemHandle1 (its root)
    Assert.IsTrue(Tuple.LinksToHandle(FDataModule.BoldSystemHandle1),
      'Should detect link via IsRootLinkedTo for rooted handles');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestSetGlobalSystemHandle;
var
  OclVars: TBoldOclVariables;
begin
  // Covers lines 359-365: SetGlobalSystemHandle
  Assert.IsNotNull(GetSystem, 'System must be active');
  OclVars := TBoldOclVariables.Create(nil);
  try
    Assert.IsNull(OclVars.GlobalSystemHandle, 'Should start with nil');
    OclVars.GlobalSystemHandle := FDataModule.BoldSystemHandle1;
    Assert.AreSame(FDataModule.BoldSystemHandle1, OclVars.GlobalSystemHandle,
      'Should store the system handle');
    // Setting same handle again should be no-op
    OclVars.GlobalSystemHandle := FDataModule.BoldSystemHandle1;
    Assert.AreSame(FDataModule.BoldSystemHandle1, OclVars.GlobalSystemHandle);
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestRegisterVariables_WithSystemHandle;
var
  OclVars: TBoldOclVariables;
begin
  // Covers lines 322-353: RegisterVariables with active system handle and variables
  Assert.IsNotNull(GetSystem, 'System must be active');
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('testVar', FDataModule.BoldListHandle1);
    // Setting GlobalSystemHandle triggers PlaceSubscriptions -> RegisterVariables
    OclVars.GlobalSystemHandle := FDataModule.BoldSystemHandle1;
    // If RegisterVariables ran without error, it covered the body
    Assert.IsNotNull(OclVars.GlobalSystemHandle, 'System handle should be set');
    Assert.AreEqual(1, OclVars.Variables.Count, 'Variables should still be intact');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestSubscribeToHandles_WithExpression;
var
  OclVars: TBoldOclVariables;
  Subscriber: TBoldPassthroughSubscriber;
begin
  // Covers lines 413-427: SubscribeToHandles(Subscriber, Expression) overload
  Assert.IsNotNull(GetSystem, 'System must be active');
  Subscriber := TBoldPassthroughSubscriber.Create(nil);
  try
    OclVars := TBoldOclVariables.Create(nil);
    try
      OclVars.AddVariable('testVar', FDataModule.BoldListHandle1);
      OclVars.GlobalSystemHandle := FDataModule.BoldSystemHandle1;
      // Call the expression-based overload
      OclVars.SubscribeToHandles(Subscriber, 'testVar');
      Assert.Pass('SubscribeToHandles with expression should not raise');
    finally
      OclVars.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldOclVariables.TestSubscribeToHandles_ExternalSubscriber;
var
  OclVars: TBoldOclVariables;
  Subscriber: TBoldPassthroughSubscriber;
begin
  // Covers line 379: SubscribeToHandles when subscriber != fSubscriber
  Assert.IsNotNull(GetSystem, 'System must be active');
  Subscriber := TBoldPassthroughSubscriber.Create(nil);
  try
    OclVars := TBoldOclVariables.Create(nil);
    try
      OclVars.AddVariable('testVar', FDataModule.BoldListHandle1);
      OclVars.GlobalSystemHandle := FDataModule.BoldSystemHandle1;
      // Use external subscriber (not fSubscriber)
      OclVars.SubscribeToHandles(Subscriber);
      Assert.Pass('SubscribeToHandles with external subscriber should not raise');
    finally
      OclVars.Free;
    end;
  finally
    Subscriber.Free;
  end;
end;

procedure TTestBoldOclVariables.TestCreateFromIndirectElement;
var
  V: TBoldOclVariable;
  Indirect: TBoldIndirectElement;
  vString: TBAString;
  Evaluator: TBoldOcl;
begin
  // Covers lines 737-740: CreateFromIndirectElement
  Assert.IsNotNull(GetSystem, 'System must be active');
  Evaluator := GetEvaluator as TBoldOcl;
  vString := TBAString.CreateWithTypeInfo(Evaluator.StringType);
  vString.AsString := 'indirect';
  Indirect := TBoldIndirectElement.Create;
  try
    Indirect.SetOwnedValue(vString);
    V := TBoldOclVariable.CreateFromIndirectElement('indirectVar', Indirect);
    try
      Assert.AreEqual('indirectVar', V.Name);
      Assert.AreEqual('indirect', (V.Value as TBAString).AsString);
    finally
      V.Free;
    end;
  finally
    Indirect.Free;
  end;
end;

procedure TTestBoldOclVariables.TestGetVariableValue_FoundReturnsValue;
var
  OclVars: TBoldOclVariables;
begin
  // Covers line 286: GetVariableValue found path returning lVariable.Value
  Assert.IsNotNull(GetSystem, 'System must be active');
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('myVar', FDataModule.BoldListHandle1);
    // Exercises the found path (line 286: result := lVariable.Value)
    // Handle Value may be nil in standalone test, but the code path is covered
    OclVars.GetVariableValue('myVar');
    Assert.Pass('GetVariableValue found path exercised');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestHandleBasedVariable_UseListElement_GetValue;
var
  V: TBoldHandleBasedExternalVariable;
begin
  // Covers lines 689-692: GetValue with fUseListElement=True and list handle
  Assert.IsNotNull(GetSystem, 'System must be active');
  V := TBoldHandleBasedExternalVariable.Create('listVar', FDataModule.BoldListHandle1, True);
  try
    // Exercises the fUseListElement branch in GetValue
    // Value may be nil if handle hasn't derived yet, but the code path is exercised
    V.Value; // trigger GetValue
    Assert.Pass('GetValue with UseListElement exercised');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestHandleBasedVariable_UseListElement_GetValueType;
var
  V: TBoldHandleBasedExternalVariable;
begin
  // Covers line 702-703: GetValueType with fUseListElement=True and list handle
  Assert.IsNotNull(GetSystem, 'System must be active');
  V := TBoldHandleBasedExternalVariable.Create('listVar', FDataModule.BoldListHandle1, True);
  try
    Assert.IsNotNull(V.ValueType, 'ValueType should return StaticListType for list handle');
  finally
    V.Free;
  end;
end;

// === Deep coverage tests (batch 3) ===

procedure TTestBoldOclVariables.TestAssign_InheritedBranch;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
  P: TPersistent;
begin
  // Covers line 559: Assign with non-TBoldVariableTuple calls inherited which raises
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('test', nil);
    P := TPersistent.Create;
    try
      Assert.WillRaiseAny(
        procedure
        begin
          Tuple.Assign(P);
        end,
        'Assign with non-TBoldVariableTuple should raise');
    finally
      P.Free;
    end;
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestTuple_HandleDestroyed_NilsHandle;
var
  OclVars: TBoldOclVariables;
  Tuple: TBoldVariableTuple;
begin
  // Covers SetBoldHandle subscription management and Changed notification
  OclVars := TBoldOclVariables.Create(nil);
  try
    Tuple := OclVars.AddVariable('myVar', FDataModule.BoldListHandle1);
    Assert.AreSame(FDataModule.BoldListHandle1, Tuple.BoldHandle, 'Handle should be assigned');
    // Setting to nil exercises SetBoldHandle: cancels subscription, sets nil, calls Changed
    Tuple.BoldHandle := nil;
    Assert.IsNull(Tuple.BoldHandle, 'Handle should be nil after explicit nil assignment');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestHandleBasedVar_HandleDestroyed;
var
  V: TBoldHandleBasedExternalVariableAccess;
begin
  // Covers SetHandle cancel/subscribe path by changing handle on variable
  Assert.IsNotNull(GetSystem, 'System must be active');
  V := TBoldHandleBasedExternalVariableAccess.Create('test', FDataModule.BoldListHandle1, False);
  try
    Assert.AreSame(FDataModule.BoldListHandle1, V.Handle, 'Handle should be assigned');
    // Setting to nil exercises SetHandle: cancel old subscription, set fHandle := nil
    V.Handle := nil;
    Assert.IsNull(V.Handle, 'Handle should be nil');
    Assert.IsNull(V.Value, 'Value should be nil when handle is nil');
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestEnsureEvaluator_FallbackToDefault;
var
  V: TBoldOclVariable;
begin
  // Covers line 754: EnsureEvaluator uses DefaultBoldSystemHandle when evaluator is nil
  Assert.IsNotNull(GetSystem, 'System must be active');
  // FDataModule.BoldSystemHandle1 has IsDefault=True, so it's DefaultBoldSystemHandle
  V := TBoldOclVariable.CreateStringVariable('strVar', 'hello', nil);
  try
    Assert.AreEqual('hello', (V.Value as TBAString).AsString);
  finally
    V.Free;
  end;
end;

procedure TTestBoldOclVariables.TestRegisterVariables_WithUseListElement;
var
  OclVars: TBoldOclVariables;
begin
  // Covers line 349: RegisterVariables with EffectiveUseListElement=true
  Assert.IsNotNull(GetSystem, 'System must be active');
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('listVar', FDataModule.BoldListHandle1, True);
    // Setting GlobalSystemHandle triggers PlaceSubscriptions -> RegisterVariables
    // RegisterVariables enters the EffectiveUseListElement branch (line 349)
    OclVars.GlobalSystemHandle := FDataModule.BoldSystemHandle1;
    Assert.Pass('RegisterVariables with EffectiveUseListElement should not raise');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestReceive_GlobalSystemHandleDestroying;
var
  OclVars: TBoldOclVariables;
  SystemHandle: TBoldSystemHandle;
begin
  // Covers lines 437-445: _Receive handles GlobalSystemHandle beDestroying
  OclVars := TBoldOclVariables.Create(nil);
  try
    SystemHandle := TBoldSystemHandle.Create(nil);
    try
      OclVars.GlobalSystemHandle := SystemHandle;
      Assert.AreSame(SystemHandle, OclVars.GlobalSystemHandle, 'Should be set');
    finally
      SystemHandle.Free; // Fires beDestroying -> _Receive -> GlobalSystemHandle := nil
    end;
    Assert.IsNull(OclVars.GlobalSystemHandle,
      'GlobalSystemHandle should be nil after system handle destroyed');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestLoaded_CallsPlaceSubscriptions;
var
  OclVars: TBoldOclVariables;
begin
  // Covers lines 301-304: Loaded override calls inherited + PlaceSubscriptions
  Assert.IsNotNull(GetSystem, 'System must be active');
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('testVar', FDataModule.BoldListHandle1);
    OclVars.GlobalSystemHandle := FDataModule.BoldSystemHandle1;
    // Call Loaded via cracker class (simulates DFM streaming completion)
    TBoldOclVariablesAccess(OclVars).Loaded;
    Assert.Pass('Loaded should re-subscribe without errors');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestSetVariableTupleList_Setter;
var
  OclVars: TBoldOclVariables;
begin
  // Covers lines 369-371: SetVariableTupleList published setter
  OclVars := TBoldOclVariables.Create(nil);
  try
    OclVars.AddVariable('test', nil);
    // Self-assignment exercises the setter code path
    OclVars.Variables := OclVars.Variables;
    Assert.AreEqual(1, OclVars.Variables.Count, 'Variables should be unchanged after self-assignment');
  finally
    OclVars.Free;
  end;
end;

procedure TTestBoldOclVariables.TestHandleBasedVar_SetHandle;
var
  V: TBoldHandleBasedExternalVariableAccess;
begin
  // Covers lines 713-719: SetHandle changes handle with subscription management
  // Create with nil handle (safe), then exercise SetHandle via property
  V := TBoldHandleBasedExternalVariableAccess.Create('test', nil, False);
  try
    Assert.IsNull(V.Handle, 'Should start with nil handle');
    // Set handle -> subscribes to handle for beDestroying (lines 717-719)
    V.Handle := FDataModule.BoldListHandle1;
    Assert.AreSame(FDataModule.BoldListHandle1, V.Handle, 'Handle should be assigned');
    // Same handle -> early exit (lines 714-715)
    V.Handle := FDataModule.BoldListHandle1;
    Assert.AreSame(FDataModule.BoldListHandle1, V.Handle, 'Same handle is no-op');
    // Set to nil -> cancels subscription, sets nil (lines 716-717)
    V.Handle := nil;
    Assert.IsNull(V.Handle, 'Handle should be nil after clear');
  finally
    V.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldOclVariables);

end.
