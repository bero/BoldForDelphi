unit Test.BoldSharedStrings;

interface

uses
  BoldTestFramework,
  BoldSharedStrings;

type
  TTestBoldSharedStrings = class(TBoldTestCase)
  published
    // TBoldSharedStringManager tests
    procedure TestBoldSharedStringManagerSingleton;
    procedure TestGetSharedString_EmptyString;
    procedure TestGetSharedString_NonEmpty;
    procedure TestGetSharedString_SameStringReturned;
    procedure TestGetSharedAnsiString_EmptyString;
    procedure TestGetSharedAnsiString_NonEmpty;
    procedure TestGarbageCollect;
    procedure TestSavedMemory;
    procedure TestInfoString;
  end;

implementation

uses
  SysUtils;

{ TTestBoldSharedStrings }

procedure TTestBoldSharedStrings.TestBoldSharedStringManagerSingleton;
var
  Manager1, Manager2: TBoldSharedStringManager;
begin
  Manager1 := BoldSharedStringManager;
  Manager2 := BoldSharedStringManager;
  Assert.IsNotNull(Manager1);
  Assert.AreSame(Manager1, Manager2);
end;

procedure TTestBoldSharedStrings.TestGetSharedString_EmptyString;
var
  Result: string;
begin
  Result := BoldSharedStringManager.GetSharedString('');
  Assert.AreEqual('', Result);
end;

procedure TTestBoldSharedStrings.TestGetSharedString_NonEmpty;
var
  Result: string;
begin
  Result := BoldSharedStringManager.GetSharedString('TestString');
  Assert.AreEqual('TestString', Result);
end;

procedure TTestBoldSharedStrings.TestGetSharedString_SameStringReturned;
var
  Result1, Result2: string;
begin
  Result1 := BoldSharedStringManager.GetSharedString('SharedTest');
  Result2 := BoldSharedStringManager.GetSharedString('SharedTest');
  Assert.AreEqual(Result1, Result2);
  // Both should point to the same string instance (shared)
  Assert.AreEqual(PChar(Result1), PChar(Result2));
end;

procedure TTestBoldSharedStrings.TestGetSharedAnsiString_EmptyString;
var
  Result: AnsiString;
begin
  Result := BoldSharedStringManager.GetSharedAnsiString('');
  Assert.AreEqual(AnsiString(''), Result);
end;

procedure TTestBoldSharedStrings.TestGetSharedAnsiString_NonEmpty;
var
  Result: AnsiString;
begin
  Result := BoldSharedStringManager.GetSharedAnsiString('AnsiTest');
  Assert.AreEqual(AnsiString('AnsiTest'), Result);
end;

procedure TTestBoldSharedStrings.TestGarbageCollect;
begin
  // Just verify it doesn't raise an exception
  BoldSharedStringManager.GarbageCollect;
  Assert.Pass;
end;

procedure TTestBoldSharedStrings.TestSavedMemory;
var
  SavedMem: Integer;
begin
  // Add some shared strings first
  BoldSharedStringManager.GetSharedString('MemoryTest1');
  BoldSharedStringManager.GetSharedString('MemoryTest2');
  SavedMem := BoldSharedStringManager.SavedMemory;
  // SavedMemory should be >= 0
  Assert.IsTrue(SavedMem >= 0);
end;

procedure TTestBoldSharedStrings.TestInfoString;
var
  Info: string;
begin
  Info := BoldSharedStringManager.InfoString;
  Assert.IsNotEmpty(Info);
  // Should contain either "shared strings" or "disabled" text
  Assert.IsTrue((Pos('shared strings', LowerCase(Info)) > 0) or
                (Pos('disabled', LowerCase(Info)) > 0));
end;

initialization
  BoldRegisterTest(TTestBoldSharedStrings);

end.
