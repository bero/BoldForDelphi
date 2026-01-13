unit Test.BoldGuard;

interface

uses
  DUnitX.TestFramework,
  BoldGuard;

type
  [TestFixture]
  TTestBoldGuard = class
  private
    class var FInstanceCount: Integer;
  public
    [Setup]
    procedure Setup;
    [Test]
    procedure TestGuardWith6Objects;
    [Test]
    procedure TestGuardWith7Objects;
    [Test]
    procedure TestGuardWith8Objects;
    [Test]
    procedure TestGuardWith9Objects;
    [Test]
    procedure TestGuardWith10Objects;
    [Test]
    procedure TestGuardWithNilObjects;
  end;

  TCountedObject = class
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TCountedObject }

constructor TCountedObject.Create;
begin
  inherited;
  Inc(TTestBoldGuard.FInstanceCount);
end;

destructor TCountedObject.Destroy;
begin
  Dec(TTestBoldGuard.FInstanceCount);
  inherited;
end;

{ TTestBoldGuard }

procedure TTestBoldGuard.Setup;
begin
  FInstanceCount := 0;
end;

procedure TTestBoldGuard.TestGuardWith6Objects;
var
  o0, o1, o2, o3, o4, o5: TCountedObject;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(o0, o1, o2, o3, o4, o5);
  o0 := TCountedObject.Create;
  o1 := TCountedObject.Create;
  o2 := TCountedObject.Create;
  o3 := TCountedObject.Create;
  o4 := TCountedObject.Create;
  o5 := TCountedObject.Create;
  Assert.AreEqual(6, FInstanceCount, 'Should have 6 instances');
  Guard := nil;
  Assert.AreEqual(0, FInstanceCount, 'All instances should be freed');
end;

procedure TTestBoldGuard.TestGuardWith7Objects;
var
  o0, o1, o2, o3, o4, o5, o6: TCountedObject;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(o0, o1, o2, o3, o4, o5, o6);
  o0 := TCountedObject.Create;
  o1 := TCountedObject.Create;
  o2 := TCountedObject.Create;
  o3 := TCountedObject.Create;
  o4 := TCountedObject.Create;
  o5 := TCountedObject.Create;
  o6 := TCountedObject.Create;
  Assert.AreEqual(7, FInstanceCount, 'Should have 7 instances');
  Guard := nil;
  Assert.AreEqual(0, FInstanceCount, 'All instances should be freed');
end;

procedure TTestBoldGuard.TestGuardWith8Objects;
var
  o0, o1, o2, o3, o4, o5, o6, o7: TCountedObject;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(o0, o1, o2, o3, o4, o5, o6, o7);
  o0 := TCountedObject.Create;
  o1 := TCountedObject.Create;
  o2 := TCountedObject.Create;
  o3 := TCountedObject.Create;
  o4 := TCountedObject.Create;
  o5 := TCountedObject.Create;
  o6 := TCountedObject.Create;
  o7 := TCountedObject.Create;
  Assert.AreEqual(8, FInstanceCount, 'Should have 8 instances');
  Guard := nil;
  Assert.AreEqual(0, FInstanceCount, 'All instances should be freed');
end;

procedure TTestBoldGuard.TestGuardWith9Objects;
var
  o0, o1, o2, o3, o4, o5, o6, o7, o8: TCountedObject;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(o0, o1, o2, o3, o4, o5, o6, o7, o8);
  o0 := TCountedObject.Create;
  o1 := TCountedObject.Create;
  o2 := TCountedObject.Create;
  o3 := TCountedObject.Create;
  o4 := TCountedObject.Create;
  o5 := TCountedObject.Create;
  o6 := TCountedObject.Create;
  o7 := TCountedObject.Create;
  o8 := TCountedObject.Create;
  Assert.AreEqual(9, FInstanceCount, 'Should have 9 instances');
  Guard := nil;
  Assert.AreEqual(0, FInstanceCount, 'All instances should be freed');
end;

procedure TTestBoldGuard.TestGuardWith10Objects;
var
  o0, o1, o2, o3, o4, o5, o6, o7, o8, o9: TCountedObject;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(o0, o1, o2, o3, o4, o5, o6, o7, o8, o9);
  o0 := TCountedObject.Create;
  o1 := TCountedObject.Create;
  o2 := TCountedObject.Create;
  o3 := TCountedObject.Create;
  o4 := TCountedObject.Create;
  o5 := TCountedObject.Create;
  o6 := TCountedObject.Create;
  o7 := TCountedObject.Create;
  o8 := TCountedObject.Create;
  o9 := TCountedObject.Create;
  Assert.AreEqual(10, FInstanceCount, 'Should have 10 instances');
  Guard := nil;
  Assert.AreEqual(0, FInstanceCount, 'All instances should be freed');
end;

procedure TTestBoldGuard.TestGuardWithNilObjects;
var
  o0, o1, o2, o3, o4, o5: TCountedObject;
  Guard: IBoldGuard;
begin
  // Test that guard handles mix of nil and assigned objects
  Guard := TBoldGuard.Create(o0, o1, o2, o3, o4, o5);
  o0 := TCountedObject.Create;
  // o1 stays nil
  o2 := TCountedObject.Create;
  // o3 stays nil
  o4 := TCountedObject.Create;
  // o5 stays nil
  Assert.AreEqual(3, FInstanceCount, 'Should have 3 instances');
  Guard := nil;
  Assert.AreEqual(0, FInstanceCount, 'All instances should be freed');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldGuard);

end.
