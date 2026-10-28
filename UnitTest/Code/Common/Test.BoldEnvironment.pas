unit Test.BoldEnvironment;

interface

uses
  BoldTestFramework;

type
  TTestBoldEnvironment = class(TBoldTestCase)
  private
    procedure RegisterFreestandingFramework;
  published
    procedure TestFreestandingConfigurationHasQueue;
    procedure TestFreestandingQueueActivationNests;
    procedure TestVCLEnvironmentIsEffectiveInVCLApplication;
    procedure TestSecondFrameworkRegistrationRaises;
  end;

implementation

uses
  BoldDefs,
  BoldEnvironment,
  BoldQueue;

const
  // The Delphi test application links Bold VCL units, which register the VCL
  // environment. The FPC test runner links no GUI framework environment.
  {$IFDEF FPC}
  cExpectedEnvironment = 'Freestanding';
  {$ELSE}
  cExpectedEnvironment = 'VCL';
  {$ENDIF}

procedure TTestBoldEnvironment.RegisterFreestandingFramework;
begin
  BoldRegisterFrameworkEnvironment(TBoldFreestandingEnvironmentConfiguration);
end;

procedure TTestBoldEnvironment.TestFreestandingConfigurationHasQueue;
var
  Configuration: TBoldEnvironmentConfiguration;
begin
  // An application without a GUI framework environment falls back to the
  // freestanding configuration, so it must provide a queue of its own
  Configuration := TBoldFreestandingEnvironmentConfiguration.Create;
  try
    Assert.IsNotNull(Configuration.Queue);
    Assert.IsTrue(Configuration.Queue.IsActive);
  finally
    Configuration.Free;
  end;
end;

procedure TTestBoldEnvironment.TestFreestandingQueueActivationNests;
var
  Configuration: TBoldEnvironmentConfiguration;
  Queue: TBoldQueue;
begin
  Configuration := TBoldFreestandingEnvironmentConfiguration.Create;
  try
    Queue := Configuration.Queue;
    // An unmatched activation must not let a later deactivation be skipped
    Queue.ActivateDisplayQueue;
    Queue.DeActivateDisplayQueue;
    Assert.IsFalse(Queue.IsActive, 'Deactivated after an unmatched activation');
    Queue.DeActivateDisplayQueue;
    Queue.ActivateDisplayQueue;
    Assert.IsFalse(Queue.IsActive, 'One deactivation still open');
    Queue.ActivateDisplayQueue;
    Assert.IsTrue(Queue.IsActive, 'All deactivations matched');
  finally
    Configuration.Free;
  end;
end;

procedure TTestBoldEnvironment.TestVCLEnvironmentIsEffectiveInVCLApplication;
begin
  // The test application links Bold VCL units, so the VCL environment must be
  // registered even when BoldEnvironment itself does not import it
  Assert.AreEqual(cExpectedEnvironment, BoldEffectiveEnvironment.Name);
end;

procedure TTestBoldEnvironment.TestSecondFrameworkRegistrationRaises;
begin
  // BoldEnvironmentVCL has already registered itself in the Delphi test
  // application. The FPC runner has no framework environment, so it registers a
  // freestanding one first (same name and behaviour as the fallback).
  if not Assigned(BoldInternalFrameworkConfiguration) then
    RegisterFreestandingFramework;
  Assert.WillRaise(RegisterFreestandingFramework, EBold);
  Assert.AreEqual(cExpectedEnvironment, BoldEffectiveEnvironment.Name);
end;

initialization
  BoldRegisterTest(TTestBoldEnvironment);

end.
