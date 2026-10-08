unit Test.BoldOclObjectTimeStamp;

{ DUnitX tests for the OCL operations objectTimeStamp and boldTime applied
  behind a nil single-valued role.

  Both are registered OCL operations (TBOS_ObjectTimeStamp and TBOS_BoldTime in
  BoldOclSymbolImplementations), not model members, so they bypass the nil
  handling that member navigation receives in VisitTBoldOclMember. Without a
  nil guard their Evaluate dereferences Params.values[0] directly, and
  <nilRole>.objectTimeStamp raises an Access Violation instead of yielding
  null. boldTime used to be registered twice; the symbol dictionary answered
  with the later registration, TBOS_BoldTime, so a guard on the shadowed
  TBOS_ObjectTime changed nothing. That duplicate is gone. Member navigation
  through the same nil role was already safe; one test pins that asymmetry so
  the fix stays confined to the operation symbols.

  Model: TestModel1. TClassA.parent is a single-valued role that is nil on a
  freshly created object. The system is transient; no database is needed. }

interface

uses
  DUnitX.TestFramework,
  BoldSystem,
  BoldSystemHandle,
  BoldHandles,
  BoldElements,
  BoldSubscription,
  BoldDefs,
  TestModel1;

type
  [TestFixture]
  [Category('OCL')]
  TTestBoldOclObjectTimeStamp = class
  private
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
    FSystemHandle: TBoldSystemHandle;
    FEventCount: Integer;
    function GetSystem: TBoldSystem;
    function CreateObjectWithoutParent: TClassA;
    function EvaluateAndCatch(AObject: TBoldObject; const AOcl: string; out AErrorMsg: string): Boolean;
    procedure AssertEvaluatesToNull(AObject: TBoldObject; const AOcl: string);
    function EvaluateAsInteger(AObject: TBoldObject; const AOcl: string): Integer;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure NilSingleRole_ObjectTimeStamp_ReturnsNull;
    [Test]
    procedure NilSingleRole_BoldTime_ReturnsNull;
    [Test]
    procedure NilSingleRole_MaxChain_DoesNotRaise;
    [Test]
    procedure NilSingleRole_ObjectTimeStamp_ReEvaluatesWhenRoleIsSet;
    [Test]
    procedure NilSingleRole_MemberNavigation_IsAlreadySafe;
    [Test]
    procedure ObjectTimeStamp_OnObject_ReturnsBoldTimeStamp;
    [Test]
    procedure BoldTime_OnObject_ReturnsIdTimeStamp;
  end;

implementation

uses
  SysUtils,
  BoldAttributes,
  dmModel1;

const
  // The chain from the production query that first hit the fault, cut down
  // to the one link that reproduces it.
  cMaxChainOcl = 'objectTimeStamp->max(parent.objectTimeStamp->maxValue)';

{ TTestBoldOclObjectTimeStamp }

procedure TTestBoldOclObjectTimeStamp.SetUp;
begin
  Ensuredm_Model;
  FSystemTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  FSystemTypeInfoHandle.BoldModel := dm_Model1.BoldModel1;
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FSystemTypeInfoHandle;
  FSystemHandle.Active := True;
  FEventCount := 0;
end;

procedure TTestBoldOclObjectTimeStamp.TearDown;
begin
  if Assigned(FSystemHandle) and FSystemHandle.Active then
  begin
    FSystemHandle.System.Discard;
    FSystemHandle.Active := False;
  end;

  FreeAndNil(FSystemHandle);
  FreeAndNil(FSystemTypeInfoHandle);
end;

function TTestBoldOclObjectTimeStamp.GetSystem: TBoldSystem;
begin
  Result := FSystemHandle.System;
end;

function TTestBoldOclObjectTimeStamp.CreateObjectWithoutParent: TClassA;
begin
  Result := TClassA.Create(GetSystem);
  Assert.IsNull(Result.parent, 'Precondition: a new TClassA has no parent');
end;

{ True when AOcl evaluated without raising. }
function TTestBoldOclObjectTimeStamp.EvaluateAndCatch(AObject: TBoldObject; const AOcl: string;
  out AErrorMsg: string): Boolean;
var
  IndirectElement: TBoldIndirectElement;
begin
  AErrorMsg := '';
  IndirectElement := TBoldIndirectElement.Create;
  try
    try
      AObject.EvaluateExpression(AOcl, IndirectElement);
      Result := True;
    except
      on E: Exception do
      begin
        AErrorMsg := E.ClassName + ': ' + E.Message;
        Result := False;
      end;
    end;
  finally
    IndirectElement.Free;
  end;
end;

procedure TTestBoldOclObjectTimeStamp.AssertEvaluatesToNull(AObject: TBoldObject; const AOcl: string);
var
  IndirectElement: TBoldIndirectElement;
begin
  IndirectElement := TBoldIndirectElement.Create;
  try
    AObject.EvaluateExpression(AOcl, IndirectElement);
    Assert.IsNotNull(IndirectElement.Value, AOcl + ' must yield a null value, not no value');
    Assert.IsTrue(IndirectElement.Value is TBoldAttribute,
      AOcl + ' must yield an attribute, got ' + IndirectElement.Value.ClassName);
    Assert.IsTrue(TBoldAttribute(IndirectElement.Value).IsNull, AOcl + ' must yield null when parent is nil');
  finally
    IndirectElement.Free;
  end;
end;

function TTestBoldOclObjectTimeStamp.EvaluateAsInteger(AObject: TBoldObject; const AOcl: string): Integer;
var
  IndirectElement: TBoldIndirectElement;
begin
  IndirectElement := TBoldIndirectElement.Create;
  try
    AObject.EvaluateExpression(AOcl, IndirectElement);
    Assert.IsTrue(IndirectElement.Value is TBAInteger, AOcl + ' must yield an integer');
    Result := TBAInteger(IndirectElement.Value).AsInteger;
  finally
    IndirectElement.Free;
  end;
end;

procedure TTestBoldOclObjectTimeStamp.Receive(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  Inc(FEventCount);
end;

procedure TTestBoldOclObjectTimeStamp.NilSingleRole_ObjectTimeStamp_ReturnsNull;
begin
  AssertEvaluatesToNull(CreateObjectWithoutParent, 'parent.objectTimeStamp');
end;

procedure TTestBoldOclObjectTimeStamp.NilSingleRole_BoldTime_ReturnsNull;
begin
  AssertEvaluatesToNull(CreateObjectWithoutParent, 'parent.boldTime');
end;

procedure TTestBoldOclObjectTimeStamp.NilSingleRole_MaxChain_DoesNotRaise;
var
  ErrorMsg: string;
begin
  Assert.IsTrue(EvaluateAndCatch(CreateObjectWithoutParent, cMaxChainOcl, ErrorMsg),
    cMaxChainOcl + ' must skip a nil parent, not raise -- ' + ErrorMsg);
end;

// The nil path returns before the operation subscribes to anything. The
// evaluator still subscribes to the parent reference itself, so a subscriber
// is told to re-evaluate once the role is set.
procedure TTestBoldOclObjectTimeStamp.NilSingleRole_ObjectTimeStamp_ReEvaluatesWhenRoleIsSet;
var
  Obj: TClassA;
  Subscriber: TBoldPassthroughSubscriber;
  IndirectElement: TBoldIndirectElement;
begin
  Obj := CreateObjectWithoutParent;
  Subscriber := TBoldPassthroughSubscriber.Create(Receive);
  IndirectElement := TBoldIndirectElement.Create;
  try
    Obj.EvaluateAndSubscribeToExpression('parent.objectTimeStamp', Subscriber, IndirectElement);
    Assert.IsTrue(TBoldAttribute(IndirectElement.Value).IsNull, 'parent.objectTimeStamp must be null while parent is nil');
    FEventCount := 0;

    Obj.parent := TClassA.Create(GetSystem);

    Assert.IsTrue(FEventCount > 0, 'Setting parent must notify the subscriber of the nil evaluation');
  finally
    IndirectElement.Free;
    Subscriber.Free;
  end;
end;

procedure TTestBoldOclObjectTimeStamp.NilSingleRole_MemberNavigation_IsAlreadySafe;
var
  ErrorMsg: string;
begin
  Assert.IsTrue(EvaluateAndCatch(CreateObjectWithoutParent, 'parent.aString', ErrorMsg),
    'Member navigation through a nil role was already nil-safe and must stay so -- ' + ErrorMsg);
end;

procedure TTestBoldOclObjectTimeStamp.ObjectTimeStamp_OnObject_ReturnsBoldTimeStamp;
var
  Obj: TClassA;
begin
  Obj := CreateObjectWithoutParent;
  Assert.AreEqual(Integer(Obj.BoldTimeStamp), EvaluateAsInteger(Obj, 'objectTimeStamp'),
    'objectTimeStamp on an object must still return BoldTimeStamp');
end;

procedure TTestBoldOclObjectTimeStamp.BoldTime_OnObject_ReturnsIdTimeStamp;
var
  Obj: TClassA;
begin
  Obj := CreateObjectWithoutParent;
  Assert.AreEqual(Integer(Obj.BoldObjectLocator.BoldObjectId.TimeStamp), EvaluateAsInteger(Obj, 'boldTime'),
    'boldTime on an object must still return the id time stamp');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldOclObjectTimeStamp);

end.
