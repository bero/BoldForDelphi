unit Test.BoldTaggedValueList;

interface

uses
  DUnitX.TestFramework,
  BoldTaggedValueList;

type
  [TestFixture]
  TTestBoldTaggedValueList = class
  public
    // TBoldTaggedValueDefinition tests
    [Test]
    procedure TestTaggedValueDefinitionCreate;
    [Test]
    procedure TestTaggedValueDefinitionProperties;

    // TBoldTaggedValueList tests
    [Test]
    procedure TestTaggedValueListCreate;
    [Test]
    procedure TestTaggedValueListAdd;
    [Test]
    procedure TestTaggedValueListCount;
    [Test]
    procedure TestTaggedValueListDefinition;
    [Test]
    procedure TestTaggedValueListDefaultValueForTag;
    [Test]
    procedure TestTaggedValueListDefaultValueForTag_NotFound;
    [Test]
    procedure TestTaggedValueListDefinitionForTag;
    [Test]
    procedure TestTaggedValueListDefinitionForTag_NotFound;

    // TBoldTaggedValuePerClassList tests
    [Test]
    procedure TestPerClassListCreate;
    [Test]
    procedure TestPerClassListListForClassName_New;
    [Test]
    procedure TestPerClassListListForClassName_Existing;
    [Test]
    procedure TestPerClassListDefaultForClassAndTag;
  end;

implementation

uses
  SysUtils;

{ TTestBoldTaggedValueList }

procedure TTestBoldTaggedValueList.TestTaggedValueDefinitionCreate;
var
  Def: TBoldTaggedValueDefinition;
begin
  Def := TBoldTaggedValueDefinition.Create('String', 'testTag', 'defaultVal');
  try
    Assert.IsNotNull(Def);
  finally
    Def.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueDefinitionProperties;
var
  Def: TBoldTaggedValueDefinition;
begin
  Def := TBoldTaggedValueDefinition.Create('Integer', 'myTag', '42');
  try
    Assert.AreEqual('Integer', Def.TypeName);
    Assert.AreEqual('myTag', Def.Tag);
    Assert.AreEqual('42', Def.DefaultValue);
  finally
    Def.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueListCreate;
var
  List: TBoldTaggedValueList;
begin
  List := TBoldTaggedValueList.Create;
  try
    Assert.IsNotNull(List);
    Assert.AreEqual(0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueListAdd;
var
  List: TBoldTaggedValueList;
begin
  List := TBoldTaggedValueList.Create;
  try
    List.Add('String', 'tag1', 'value1');
    Assert.AreEqual(1, List.Count);
    List.Add('Integer', 'tag2', 'value2');
    Assert.AreEqual(2, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueListCount;
var
  List: TBoldTaggedValueList;
begin
  List := TBoldTaggedValueList.Create;
  try
    Assert.AreEqual(0, List.Count);
    List.Add('String', 'tag1', 'val1');
    Assert.AreEqual(1, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueListDefinition;
var
  List: TBoldTaggedValueList;
  Def: TBoldTaggedValueDefinition;
begin
  List := TBoldTaggedValueList.Create;
  try
    List.Add('String', 'tag1', 'val1');
    List.Add('Integer', 'tag2', 'val2');
    Def := List.Definition[0];
    Assert.IsNotNull(Def);
    Assert.AreEqual('tag1', Def.Tag);
    Def := List.Definition[1];
    Assert.AreEqual('tag2', Def.Tag);
  finally
    List.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueListDefaultValueForTag;
var
  List: TBoldTaggedValueList;
begin
  List := TBoldTaggedValueList.Create;
  try
    List.Add('String', 'myTag', 'myDefault');
    Assert.AreEqual('myDefault', List.DefaultValueForTag['myTag']);
  finally
    List.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueListDefaultValueForTag_NotFound;
var
  List: TBoldTaggedValueList;
begin
  List := TBoldTaggedValueList.Create;
  try
    List.Add('String', 'myTag', 'myDefault');
    Assert.AreEqual('', List.DefaultValueForTag['nonExistentTag']);
  finally
    List.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueListDefinitionForTag;
var
  List: TBoldTaggedValueList;
  Def: TBoldTaggedValueDefinition;
begin
  List := TBoldTaggedValueList.Create;
  try
    List.Add('String', 'tag1', 'val1');
    List.Add('Integer', 'tag2', 'val2');
    Def := List.DefinitionForTag['tag2'];
    Assert.IsNotNull(Def);
    Assert.AreEqual('Integer', Def.TypeName);
  finally
    List.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestTaggedValueListDefinitionForTag_NotFound;
var
  List: TBoldTaggedValueList;
begin
  List := TBoldTaggedValueList.Create;
  try
    Assert.IsNull(List.DefinitionForTag['nonExistent']);
  finally
    List.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestPerClassListCreate;
var
  PerClassList: TBoldTaggedValuePerClassList;
begin
  PerClassList := TBoldTaggedValuePerClassList.Create;
  try
    Assert.IsNotNull(PerClassList);
  finally
    PerClassList.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestPerClassListListForClassName_New;
var
  PerClassList: TBoldTaggedValuePerClassList;
  List: TBoldTaggedValueList;
begin
  PerClassList := TBoldTaggedValuePerClassList.Create;
  try
    List := PerClassList.ListForClassName['TestClass'];
    Assert.IsNotNull(List);
    Assert.AreEqual(0, List.Count);
  finally
    PerClassList.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestPerClassListListForClassName_Existing;
var
  PerClassList: TBoldTaggedValuePerClassList;
  List1, List2: TBoldTaggedValueList;
begin
  PerClassList := TBoldTaggedValuePerClassList.Create;
  try
    List1 := PerClassList.ListForClassName['MyClass'];
    List1.Add('String', 'tag1', 'val1');
    List2 := PerClassList.ListForClassName['MyClass'];
    Assert.AreSame(List1, List2);
    Assert.AreEqual(1, List2.Count);
  finally
    PerClassList.Free;
  end;
end;

procedure TTestBoldTaggedValueList.TestPerClassListDefaultForClassAndTag;
var
  PerClassList: TBoldTaggedValuePerClassList;
begin
  PerClassList := TBoldTaggedValuePerClassList.Create;
  try
    PerClassList.ListForClassName['MyClass'].Add('String', 'testTag', 'testDefault');
    Assert.AreEqual('testDefault', PerClassList.DefaultForClassAndTag['MyClass', 'testTag']);
  finally
    PerClassList.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldTaggedValueList);

end.
