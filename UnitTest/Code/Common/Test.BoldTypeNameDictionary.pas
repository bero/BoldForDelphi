unit Test.BoldTypeNameDictionary;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestBoldTypeNameDictionary = class
  public
    // Create and basic structure
    [Test] [Category('Quick')]
    procedure TestCreate;
    [Test] [Category('Quick')]
    procedure TestAddDefaultMappings;
    [Test] [Category('Quick')]
    procedure TestAddMapping;

    // Lookup
    [Test] [Category('Quick')]
    procedure TestGetMapping;
    [Test] [Category('Quick')]
    procedure TestExactMappingForModelName;
    [Test] [Category('Quick')]
    procedure TestMappingForModelName_ExactMatch;
    [Test] [Category('Quick')]
    procedure TestMappingForModelName_FallbackToDefault;
    [Test] [Category('Quick')]
    procedure TestMappingForModelName_NotFound;

    // Expanded properties
    [Test] [Category('Quick')]
    procedure TestExpandedDelphiName;
    [Test] [Category('Quick')]
    procedure TestExpandedContentsName;
    [Test] [Category('Quick')]
    procedure TestExpandedMapperName;
    [Test] [Category('Quick')]
    procedure TestExpandedAccessor;
    [Test] [Category('Quick')]
    procedure TestExpandedNativeType;
    [Test] [Category('Quick')]
    procedure TestExpandedComType;

    // Serialization
    [Test] [Category('Quick')]
    procedure TestGetAsString;
    [Test] [Category('Quick')]
    procedure TestSetAsString;
    [Test] [Category('Quick')]
    procedure TestSetAsString_LegacyStreamName;

    // AssignTo
    [Test] [Category('Quick')]
    procedure TestAssignTo;

    // File I/O
    [Test] [Category('Quick')]
    procedure TestSaveAndLoadFile;

    // GetOwner
    [Test] [Category('Quick')]
    procedure TestGetOwner;
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldDefs,
  BoldTypeNameDictionary;

{ TTestBoldTypeNameDictionary }

procedure TTestBoldTypeNameDictionary.TestCreate;
var
  Dict: TBoldTypeNameDictionary;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Assert.AreEqual(0, Dict.Count);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestAddDefaultMappings;
var
  Dict: TBoldTypeNameDictionary;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    Assert.AreEqual(33, Dict.Count); // 0..32 = 33 entries
    // First entry is the default literal
    Assert.AreEqual(DEFAULTNAMELITERAL, Dict.Mapping[0].ModelName);
    Assert.AreEqual('String', Dict.Mapping[1].ModelName);
    Assert.AreEqual('Integer', Dict.Mapping[7].ModelName);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestAddMapping;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    M := Dict.AddMapping;
    M.ModelName := 'TestType';
    M.ExpressionName := 'TestExpr';
    Assert.AreEqual(1, Dict.Count);
    Assert.AreEqual('TestType', Dict.Mapping[0].ModelName);
    Assert.AreEqual('TestExpr', Dict.Mapping[0].ExpressionName);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestGetMapping;
var
  Dict: TBoldTypeNameDictionary;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    Assert.IsNotNull(Dict.Mapping[0]);
    Assert.AreEqual(DEFAULTNAMELITERAL, Dict.Mapping[0].ModelName);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestExactMappingForModelName;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    M := Dict.ExactMappingForModelName['Integer'];
    Assert.IsNotNull(M);
    Assert.AreEqual('Integer', M.ModelName);

    // Non-existent returns nil
    M := Dict.ExactMappingForModelName['NonExistent'];
    Assert.IsNull(M);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestMappingForModelName_ExactMatch;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    M := Dict.MappingForModelName['Float'];
    Assert.IsNotNull(M);
    Assert.AreEqual('Float', M.ModelName);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestMappingForModelName_FallbackToDefault;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    // Non-existent model name falls back to DEFAULTNAME mapping
    M := Dict.MappingForModelName['NonExistent'];
    Assert.IsNotNull(M);
    Assert.AreEqual(DEFAULTNAMELITERAL, M.ModelName);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestMappingForModelName_NotFound;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    // Empty dictionary: no fallback either
    M := Dict.MappingForModelName['Anything'];
    Assert.IsNull(M);
  finally
    Dict.Free;
  end;
end;

// --- Expanded properties ---

procedure TTestBoldTypeNameDictionary.TestExpandedDelphiName;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    M := Dict.ExactMappingForModelName['Integer'];
    // DelphiName='TBA<Name>', ExpressionName='Integer' -> 'TBAInteger'
    Assert.AreEqual('TBAInteger', M.ExpandedDelphiName);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestExpandedContentsName;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    M := Dict.ExactMappingForModelName['Integer'];
    // ContentsName='<Name>', ExpressionName='Integer' -> 'Integer'
    Assert.AreEqual('Integer', M.ExpandedContentsName);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestExpandedMapperName;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    M := Dict.ExactMappingForModelName['Integer'];
    // MapperName='TBoldPM<Name>', ExpressionName='Integer' -> 'TBoldPMInteger'
    Assert.AreEqual('TBoldPMInteger', M.ExpandedMapperName);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestExpandedAccessor;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    M := Dict.ExactMappingForModelName['Integer'];
    // Accessor='As<Name>', ExpressionName='Integer' -> 'AsInteger'
    Assert.AreEqual('AsInteger', M.ExpandedAccessor);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestExpandedNativeType;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    M := Dict.ExactMappingForModelName['Integer'];
    // NativeType='<Name>', ExpressionName='Integer' -> 'Integer'
    Assert.AreEqual('Integer', M.ExpandedNativeType);
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestExpandedComType;
var
  Dict: TBoldTypeNameDictionary;
  M: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    M := Dict.ExactMappingForModelName['Integer'];
    // ComType='Integer' (no <Name> tag) -> 'Integer'
    Assert.AreEqual('Integer', M.ExpandedComType);
  finally
    Dict.Free;
  end;
end;

// --- Serialization ---

procedure TTestBoldTypeNameDictionary.TestGetAsString;
var
  Dict: TBoldTypeNameDictionary;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Dict.AddDefaultMappings;
    // GetAsString is accessed indirectly via SaveToStringList.
    // Verify first mapping produces correct CSV format.
    var SL := TStringList.Create;
    try
      // Save uses GetAsString for each mapping
      Dict.SaveToFile(IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'BoldTND_AsString.txt');
      SL.LoadFromFile(IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'BoldTND_AsString.txt');

      // Integer mapping should contain all field names
      var IntLine := '';
      var i: Integer;
      for i := 0 to SL.Count - 1 do
        if Pos('ModelName=Integer', SL[i]) > 0 then
        begin
          IntLine := SL[i];
          Break;
        end;
      Assert.IsTrue(Length(IntLine) > 0, 'Integer mapping not found');
      Assert.IsTrue(Pos('ExpressionName=Integer', IntLine) > 0);
      Assert.IsTrue(Pos('DelphiName=TBA<Name>', IntLine) > 0);
      Assert.IsTrue(Pos('MapperName=TBoldPM<Name>', IntLine) > 0);
      Assert.IsTrue(Pos('UnitName=BoldAttributes', IntLine) > 0);
    finally
      SL.Free;
      DeleteFile(IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'BoldTND_AsString.txt');
    end;
  finally
    Dict.Free;
  end;
end;

procedure TTestBoldTypeNameDictionary.TestSetAsString;
var
  Dict: TBoldTypeNameDictionary;
  SL: TStringList;
  TempFile: string;
  M: TBoldTypeNameMapping;
begin
  // SetAsString is private, test via LoadFromFile which calls LoadFromStringList -> SetAsString
  TempFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'BoldTND_SetAsString.txt';
  Dict := TBoldTypeNameDictionary.Create(nil);
  SL := TStringList.Create;
  try
    SL.Add('ModelName=MyModel,ExpressionName=MyExpr,DelphiName=TBAMy,' +
      'ContentsName=My,MapperName=TBoldPMMy,AccessorName=AsMy,' +
      'NativeType=My,UnitName=MyUnit,ComType=MyCom,IDLType=MyIDL,' +
      'ValueInterface=IMy,ValueInterfaceAccessor=AsMyVal,' +
      'ValueInterfaceNativeType=MyNative');
    SL.SaveToFile(TempFile);
    SL.Free;
    SL := nil;

    Dict.LoadFromFile(TempFile);
    Assert.AreEqual(1, Dict.Count);
    M := Dict.Mapping[0];
    Assert.AreEqual('MyModel', M.ModelName);
    Assert.AreEqual('MyExpr', M.ExpressionName);
    Assert.AreEqual('TBAMy', M.DelphiName);
    Assert.AreEqual('My', M.ContentsName);
    Assert.AreEqual('TBoldPMMy', M.MapperName);
    Assert.AreEqual('AsMy', M.Accessor);
    Assert.AreEqual('My', M.NativeType);
    Assert.AreEqual('MyUnit', M.BoldUnitName);
    Assert.AreEqual('MyCom', M.ComType);
    Assert.AreEqual('MyIDL', M.IDLType);
    Assert.AreEqual('IMy', M.ValueInterface);
    Assert.AreEqual('AsMyVal', M.ValueInterfaceAccessor);
    Assert.AreEqual('MyNative', M.ValueInterfaceNativeType);
  finally
    SL.Free;
    Dict.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

procedure TTestBoldTypeNameDictionary.TestSetAsString_LegacyStreamName;
var
  Dict: TBoldTypeNameDictionary;
  SL: TStringList;
  TempFile: string;
  M: TBoldTypeNameMapping;
begin
  // Legacy format used 'StreamName' instead of 'ContentsName'.
  // SetAsString should fall back to StreamName when ContentsName is empty.
  TempFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'BoldTND_Legacy.txt';
  Dict := TBoldTypeNameDictionary.Create(nil);
  SL := TStringList.Create;
  try
    SL.Add('ModelName=TestType,ExpressionName=TestExpr,DelphiName=TBATest,' +
      'StreamName=LegacyStream,MapperName=TBoldPMTest,AccessorName=AsTest,' +
      'NativeType=Test,UnitName=TestUnit,ComType=TestCom,IDLType=TestIDL,' +
      'ValueInterface=ITest,ValueInterfaceAccessor=AsTestVal,' +
      'ValueInterfaceNativeType=TestNative');
    SL.SaveToFile(TempFile);
    SL.Free;
    SL := nil;

    Dict.LoadFromFile(TempFile);
    Assert.AreEqual(1, Dict.Count);
    M := Dict.Mapping[0];
    Assert.AreEqual('TestType', M.ModelName);
    Assert.AreEqual('TestExpr', M.ExpressionName);
    Assert.AreEqual('TBATest', M.DelphiName);
    Assert.AreEqual('LegacyStream', M.ContentsName); // StreamName fallback
    Assert.AreEqual('TBoldPMTest', M.MapperName);
    Assert.AreEqual('AsTest', M.Accessor);
    Assert.AreEqual('Test', M.NativeType);
    Assert.AreEqual('TestUnit', M.BoldUnitName);
    Assert.AreEqual('TestCom', M.ComType);
    Assert.AreEqual('TestIDL', M.IDLType);
    Assert.AreEqual('ITest', M.ValueInterface);
    Assert.AreEqual('AsTestVal', M.ValueInterfaceAccessor);
    Assert.AreEqual('TestNative', M.ValueInterfaceNativeType);
  finally
    SL.Free;
    Dict.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

// --- AssignTo ---

procedure TTestBoldTypeNameDictionary.TestAssignTo;
var
  Dict: TBoldTypeNameDictionary;
  Src, Dst: TBoldTypeNameMapping;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Src := Dict.AddMapping;
    Src.ModelName := 'SrcModel';
    Src.ExpressionName := 'SrcExpr';
    Src.DelphiName := 'TBASrc';
    Src.ContentsName := 'Src';
    Src.MapperName := 'TBoldPMSrc';
    Src.Accessor := 'AsSrc';
    Src.NativeType := 'Src';
    Src.BoldUnitName := 'SrcUnit';
    Src.ComType := 'SrcCom';
    Src.IDLType := 'SrcIDL';
    Src.ValueInterface := 'ISrc';
    Src.ValueInterfaceAccessor := 'AsSrcVal';
    Src.ValueInterfaceNativeType := 'SrcNative';

    Dst := Dict.AddMapping;
    Dst.Assign(Src);

    Assert.AreEqual('SrcModel', Dst.ModelName);
    Assert.AreEqual('SrcExpr', Dst.ExpressionName);
    Assert.AreEqual('TBASrc', Dst.DelphiName);
    Assert.AreEqual('Src', Dst.ContentsName);
    Assert.AreEqual('TBoldPMSrc', Dst.MapperName);
    Assert.AreEqual('AsSrc', Dst.Accessor);
    Assert.AreEqual('Src', Dst.NativeType);
    Assert.AreEqual('SrcUnit', Dst.BoldUnitName);
    Assert.AreEqual('SrcCom', Dst.ComType);
    Assert.AreEqual('SrcIDL', Dst.IDLType);
    Assert.AreEqual('ISrc', Dst.ValueInterface);
    Assert.AreEqual('AsSrcVal', Dst.ValueInterfaceAccessor);
    Assert.AreEqual('SrcNative', Dst.ValueInterfaceNativeType);
  finally
    Dict.Free;
  end;
end;

// --- File I/O ---

procedure TTestBoldTypeNameDictionary.TestSaveAndLoadFile;
var
  Dict1, Dict2: TBoldTypeNameDictionary;
  TempFile: string;
  M1, M2: TBoldTypeNameMapping;
begin
  TempFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'BoldTypeNameDict_Test.txt';
  Dict1 := TBoldTypeNameDictionary.Create(nil);
  Dict2 := TBoldTypeNameDictionary.Create(nil);
  try
    Dict1.AddDefaultMappings;
    Dict1.SaveToFile(TempFile);

    Dict2.LoadFromFile(TempFile);
    Assert.AreEqual(Dict1.Count, Dict2.Count);

    // Verify a known mapping survived the roundtrip
    M1 := Dict1.ExactMappingForModelName['Integer'];
    M2 := Dict2.ExactMappingForModelName['Integer'];
    Assert.IsNotNull(M2, 'Integer mapping not found after load');
    Assert.AreEqual(M1.ExpressionName, M2.ExpressionName);
    Assert.AreEqual(M1.DelphiName, M2.DelphiName);
    Assert.AreEqual(M1.MapperName, M2.MapperName);
    Assert.AreEqual(M1.BoldUnitName, M2.BoldUnitName);
    Assert.AreEqual(M1.ContentsName, M2.ContentsName);
  finally
    Dict1.Free;
    Dict2.Free;
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

// --- GetOwner ---

procedure TTestBoldTypeNameDictionary.TestGetOwner;
var
  Dict: TBoldTypeNameDictionary;
begin
  Dict := TBoldTypeNameDictionary.Create(nil);
  try
    Assert.IsNull(Dict.Owner);
  finally
    Dict.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldTypeNameDictionary);

end.
