unit Test.BoldMD5;

interface

uses
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('ProductControl')]
  {$ENDIF}
  TTestBoldMD5 = class(TBoldTestCase)
  private
    procedure TestMD5SelfTest_Raiser;
  published
    procedure TestMD5SelfTest;
  end;

implementation

uses
  BoldMD5;

{ TTestBoldMD5 }

procedure TTestBoldMD5.TestMD5SelfTest_Raiser;
begin
  TBoldMD5.SelfTest;
end;

procedure TTestBoldMD5.TestMD5SelfTest;
begin
  // TBoldMD5.SelfTest raises an exception if it fails
  Assert.WillNotRaiseAny(TestMD5SelfTest_Raiser);
end;

initialization
  BoldRegisterTest(TTestBoldMD5);

end.
