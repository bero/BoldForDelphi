unit Test.BoldOclError;

interface

uses
  BoldTestFramework,
  BoldOclError;

type
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldOclError = class(TBoldTestCase)
  published
    procedure TestEBoldOclAbort_FixError_WithPosition;
    procedure TestEBoldOclAbort_FixError_WithoutColon;
    procedure TestEBoldOclAbort_FixError_InvalidNumber;
    procedure TestEBoldOclAbort_FixError_AlreadyFixed;
    procedure TestEBoldOclAbort_ErrorPointer;

    procedure TestEBoldOclError_FixError_WithPosition;
    procedure TestEBoldOclError_FixError_InvalidNumber;
    procedure TestEBoldOclError_FixError_AlreadyFixed;
    procedure TestEBoldOclError_ErrorPointer;

    procedure TestEBoldOclInternalError_IsSubclass;
    procedure TestEBoldOclRunTimeError_IsSubclass;
  end;

implementation

uses
  SysUtils;

{ TTestBoldOclError }

procedure TTestBoldOclError.TestEBoldOclAbort_FixError_WithPosition;
var
  E: EBoldOClAbort;
begin
  E := EBoldOClAbort.Create('15:Some error message');
  try
    E.FixError;
    Assert.AreEqual(15, E.Position);
    Assert.IsTrue(E.ErrorFixed);
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclAbort_FixError_WithoutColon;
var
  E: EBoldOClAbort;
begin
  E := EBoldOClAbort.Create('No colon in message');
  try
    E.FixError;
    // When no colon found, Position should be 0
    Assert.AreEqual(0, E.Position);
    Assert.IsTrue(E.ErrorFixed);
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclAbort_FixError_InvalidNumber;
var
  E: EBoldOClAbort;
begin
  E := EBoldOClAbort.Create('ABC:Invalid position');
  try
    E.FixError;
    // When conversion fails, Position should be 0
    Assert.AreEqual(0, E.Position);
    Assert.IsTrue(E.ErrorFixed);
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclAbort_FixError_AlreadyFixed;
var
  E: EBoldOClAbort;
begin
  E := EBoldOClAbort.Create('10:Error');
  try
    E.FixError;
    Assert.AreEqual(10, E.Position);
    // Manually change position
    E.Position := 99;
    // Call FixError again - should not change position since already fixed
    E.FixError;
    Assert.AreEqual(99, E.Position);
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclAbort_ErrorPointer;
var
  E: EBoldOClAbort;
  Pointer: string;
begin
  E := EBoldOClAbort.Create('5:Error');
  try
    E.FixError;
    Pointer := E.ErrorPointer;
    Assert.AreEqual('     ^', Pointer); // 5 spaces + ^
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclError_FixError_WithPosition;
var
  E: EBoldOClError;
begin
  E := EBoldOClError.Create('20:Some error message');
  try
    E.FixError;
    Assert.AreEqual(20, E.Position);
    Assert.IsTrue(E.ErrorFixed);
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclError_FixError_InvalidNumber;
var
  E: EBoldOClError;
begin
  E := EBoldOClError.Create('XYZ:Invalid position');
  try
    E.FixError;
    // When conversion fails, Position should be 0
    Assert.AreEqual(0, E.Position);
    Assert.IsTrue(E.ErrorFixed);
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclError_FixError_AlreadyFixed;
var
  E: EBoldOClError;
begin
  E := EBoldOClError.Create('12:Error');
  try
    E.FixError;
    Assert.AreEqual(12, E.Position);
    // Manually change position
    E.Position := 50;
    // Call FixError again - should not change position since already fixed
    E.FixError;
    Assert.AreEqual(50, E.Position);
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclError_ErrorPointer;
var
  E: EBoldOClError;
  Pointer: string;
begin
  E := EBoldOClError.Create('3:Error');
  try
    E.FixError;
    Pointer := E.ErrorPointer;
    Assert.AreEqual('   ^', Pointer); // 3 spaces + ^
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclInternalError_IsSubclass;
var
  E: EBoldOCLInternalError;
begin
  E := EBoldOCLInternalError.Create('Internal error');
  try
    Assert.IsTrue(E is EBoldOClError);
  finally
    E.Free;
  end;
end;

procedure TTestBoldOclError.TestEBoldOclRunTimeError_IsSubclass;
var
  E: EBoldOclRunTimeError;
begin
  E := EBoldOclRunTimeError.Create('Runtime error');
  try
    Assert.IsTrue(E is EBoldOClError);
  finally
    E.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldOclError);

end.
