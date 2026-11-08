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
    procedure TestFreestandingAskUserNamesEnvironmentUnit;
    procedure TestFreestandingQueryUserNamesEnvironmentUnit;
  end;

implementation

uses
  BoldDefs,
  BoldEnvironment,
  BoldQueue;

const
  // The Delphi test application on Windows links Bold VCL units, which
  // register the VCL environment. The FPC test runner and Delphi on other
  // platforms link no GUI framework environment.
  {$IF defined(FPC) or not defined(MSWINDOWS)}
  cExpectedEnvironment = 'Freestanding';
  {$ELSE}
  cExpectedEnvironment = 'VCL';
  {$IFEND}

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
  // application on Windows. The FPC runner and Delphi on other platforms have
  // no framework environment, so they register a freestanding one first (same
  // name and behaviour as the fallback).
  if not Assigned(BoldInternalFrameworkConfiguration) then
    RegisterFreestandingFramework;
  Assert.WillRaise(RegisterFreestandingFramework, EBold);
  Assert.AreEqual(cExpectedEnvironment, BoldEffectiveEnvironment.Name);
end;

// The Bold core links no GUI framework (#117). Without a framework environment
// nothing can ask the user, so the error must name the unit that registers one.
procedure TTestBoldEnvironment.TestFreestandingAskUserNamesEnvironmentUnit;
var
  Configuration: TBoldEnvironmentConfiguration;
  ErrorText: string;
begin
  Configuration := TBoldFreestandingEnvironmentConfiguration.Create;
  try
    ErrorText := '';
    try
      Configuration.AskUser('Continue?');
    except
      on E: EBold do
        ErrorText := E.Message;
    end;
    Assert.IsTrue(Pos('BoldEnvironmentVCL', ErrorText) > 0, 'AskUser error: ' + ErrorText);
  finally
    Configuration.Free;
  end;
end;

procedure TTestBoldEnvironment.TestFreestandingQueryUserNamesEnvironmentUnit;
var
  Configuration: TBoldEnvironmentConfiguration;
  ErrorText: string;
begin
  Configuration := TBoldFreestandingEnvironmentConfiguration.Create;
  try
    ErrorText := '';
    try
      Configuration.QueryUser('Delete table', 'Delete table X?');
    except
      on E: EBold do
        ErrorText := E.Message;
    end;
    Assert.IsTrue(Pos('BoldEnvironmentVCL', ErrorText) > 0, 'QueryUser error: ' + ErrorText);
  finally
    Configuration.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldEnvironment);

end.
