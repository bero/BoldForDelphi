unit Test.BoldFreeStandingValueFactories;

interface

uses
  BoldTestFramework,
  BoldFreeStandingValueFactories,
  BoldDefs;

type
  {$IFNDEF FPC}
  [Category('FreeStandingValues')]
  {$ENDIF}
  TTestBoldFreeStandingValueFactories = class(TBoldTestCase)
  private
    procedure TestCreateInstanceWithInvalidContentNameRaisesException_Raiser;
  published
    procedure TestCreateInstanceWithInvalidContentNameRaisesException;
  end;

implementation

{ TTestBoldFreeStandingValueFactories }

procedure TTestBoldFreeStandingValueFactories.TestCreateInstanceWithInvalidContentNameRaisesException_Raiser;
begin
  FreeStandingValueFactory.CreateInstance('NonExistentContentName');
end;

procedure TTestBoldFreeStandingValueFactories.TestCreateInstanceWithInvalidContentNameRaisesException;
begin
  // This test covers line 66 - the exception path when ContentName is not found
  Assert.WillRaise(TestCreateInstanceWithInvalidContentNameRaisesException_Raiser, EBold, 'Should raise EBold for unregistered content name' );
end;

initialization
  BoldRegisterTest(TTestBoldFreeStandingValueFactories);

end.
