unit Test.BoldEnvironment;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestBoldEnvironment = class
  public
    [Test]
    procedure TestFreestandingConfigurationHasQueue;
    [Test]
    procedure TestFreestandingQueueActivationNests;
    [Test]
    procedure TestVCLEnvironmentIsEffectiveInVCLApplication;
    [Test]
    procedure TestSecondFrameworkRegistrationRaises;
  end;

implementation

uses
  BoldDefs,
  BoldEnvironment,
  BoldQueue;

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
  Assert.AreEqual('VCL', BoldEffectiveEnvironment.Name);
end;

procedure TTestBoldEnvironment.TestSecondFrameworkRegistrationRaises;
begin
  // BoldEnvironmentVCL has already registered itself in this application
  Assert.WillRaise(
    procedure
    begin
      BoldRegisterFrameworkEnvironment(TBoldFreestandingEnvironmentConfiguration);
    end,
    EBold);
  Assert.AreEqual('VCL', BoldEffectiveEnvironment.Name);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldEnvironment);

end.
