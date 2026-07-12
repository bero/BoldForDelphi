unit Test.BoldThread;

{ DUnitX tests for BoldThread - suspended-thread wakeup (H10) }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldThread;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldThread = class
  public
    [Test]
    [Category('Quick')]
    procedure TestQuitResumesSuspendedThread;
  end;

implementation

procedure TTestBoldThread.TestQuitResumesSuspendedThread;
var
  T: TBoldNotifiableThread;
begin
  // Regression for H10 (5dfe274): TThread.Resume was replaced with Start.
  // InternalStart only tolerates FCreateSuspended=True; a thread whose FIRST
  // start went through Start (clearing that flag - as the propagator listener
  // and COM sender threads do) raises EThread on every later wake-from-suspend.
  T := TBoldNotifiableThread.Create(True);
  try
    T.Start;  // legitimate first start - clears FCreateSuspended
    Assert.IsTrue(T.WaitUntilReady(5000), 'thread should start its message loop');
    T.Suspended := True;  // the idle state of a self-suspended sender/listener
    try
      Assert.IsTrue(T.Quit(True), 'Quit must resume a suspended thread and stop it');
    except
      on E: EThread do
      begin
        // Unsuspend so Destroy's WaitFor can join the thread - without this
        // the red phase deadlocks instead of failing.
        T.Suspended := False;
        T.Quit(True);
        Assert.Fail('waking the suspended thread raised EThread: ' + E.Message);
      end;
    end;
  finally
    T.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldThread);

end.
