unit Test.BoldFreeStandingValueFactories;

interface

uses
  DUnitX.TestFramework,
  BoldFreeStandingValueFactories,
  BoldDefs;

type
  [TestFixture]
  TTestBoldFreeStandingValueFactories = class
  public
    [Test]
    procedure TestCreateInstanceWithInvalidContentNameRaisesException;
  end;

implementation

{ TTestBoldFreeStandingValueFactories }

procedure TTestBoldFreeStandingValueFactories.TestCreateInstanceWithInvalidContentNameRaisesException;
begin
  // This test covers line 66 - the exception path when ContentName is not found
  Assert.WillRaise(
    procedure
    begin
      FreeStandingValueFactory.CreateInstance('NonExistentContentName');
    end,
    EBold,
    'Should raise EBold for unregistered content name'
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldFreeStandingValueFactories);

end.
