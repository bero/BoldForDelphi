{ Global compiler directives }
{$include bold.inc}
unit BoldExceptionLogLCL;

{ Bold's exception log in an LCL (Lazarus) program (#130). Naming this unit
  early in the program's uses clause switches on the exception log of
  BoldExceptionLog (the log file and the last-resort handler) and logs each
  exception that the LCL's message loop handles, with its call stack, before
  it is shown. Free Pascal needs no stack-trace provider; compile with -gl for
  unit names and line numbers. A handler the program sets in
  Application.OnException during its start-up (in a form's OnCreate) is
  called instead of the LCL's dialog. }

interface

implementation

uses
  SysUtils,
  Classes,
  Forms,
  BoldExceptionLog;

type
  TBoldLCLExceptionLog = class(TBoldApplicationExceptionLog)
  private
    procedure HookFromTheQueue(Data: PtrInt);
  protected
    function ApplicationExists: Boolean; override;
    function GetOnException: TBoldExceptionEvent; override;
    procedure SetOnException(const AHandler: TBoldExceptionEvent); override;
    procedure ShowException(E: Exception); override;
    procedure QueueHook; override;
    procedure UnqueueHook; override;
  end;

var
  GExceptionLog: TBoldLCLExceptionLog;

{ TBoldLCLExceptionLog }

function TBoldLCLExceptionLog.ApplicationExists: Boolean;
begin
  Result := Assigned(Application);
end;

function TBoldLCLExceptionLog.GetOnException: TBoldExceptionEvent;
begin
  Result := Application.OnException;
end;

procedure TBoldLCLExceptionLog.SetOnException(const AHandler: TBoldExceptionEvent);
begin
  Application.OnException := AHandler;
end;

procedure TBoldLCLExceptionLog.ShowException(E: Exception);
begin
  Application.ShowException(E);
end;

procedure TBoldLCLExceptionLog.HookFromTheQueue(Data: PtrInt);
begin
  Hook;
end;

procedure TBoldLCLExceptionLog.QueueHook;
begin
  // Not TThread.ForceQueue: Free Pascal runs that at once while the program
  // has started no thread
  Application.QueueAsyncCall(HookFromTheQueue, 0);
end;

procedure TBoldLCLExceptionLog.UnqueueHook;
begin
  Application.RemoveAsyncCalls(Self);
end;

initialization
  GExceptionLog := TBoldLCLExceptionLog.Create;
  GExceptionLog.HookNowAndAfterStartUp;

finalization
  GExceptionLog.Unhook;
  FreeAndNil(GExceptionLog);

end.
