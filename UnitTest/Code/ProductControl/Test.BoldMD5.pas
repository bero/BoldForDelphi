unit Test.BoldMD5;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  [Category('ProductControl')]
  TTestBoldMD5 = class
  public
    [Test]
    procedure TestMD5SelfTest;
  end;

implementation

uses
  BoldMD5;

{ TTestBoldMD5 }

procedure TTestBoldMD5.TestMD5SelfTest;
begin
  // TBoldMD5.SelfTest raises an exception if it fails
  Assert.WillNotRaiseAny(
    procedure
    begin
      TBoldMD5.SelfTest;
    end
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldMD5);

end.
