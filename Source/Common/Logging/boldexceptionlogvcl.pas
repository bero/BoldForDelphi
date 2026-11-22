{ Global compiler directives }
{$include bold.inc}
unit BoldExceptionLogVCL;

{ Bold's exception log in a VCL program (#130). Naming this unit early in
  the program's uses clause, after a stack-trace provider such as Stacker,
  switches on the exception log of BoldExceptionLog (the log file and the
  last-resort handler) and logs each exception that the VCL's message loop
  handles, with its call stack, before it is shown. A handler the program
  sets in Application.OnException during its start-up (in a form's OnCreate)
  is called instead of the VCL's message box. }

interface

implementation

uses
  SysUtils,
  Classes,
  Forms,
  BoldExceptionLog;

type
  TBoldVCLExceptionLog = class(TBoldApplicationExceptionLog)
  private
  protected
    function ApplicationExists: Boolean; override;
    function GetOnException: TBoldExceptionEvent; override;
    procedure SetOnException(const AHandler: TBoldExceptionEvent); override;
    procedure ShowException(E: Exception); override;
    procedure QueueHook; override;
    procedure UnqueueHook; override;
  end;

var
  GExceptionLog: TBoldVCLExceptionLog;

{ TBoldVCLExceptionLog }

function TBoldVCLExceptionLog.ApplicationExists: Boolean;
begin
  Result := Assigned(Application);
end;

function TBoldVCLExceptionLog.GetOnException: TBoldExceptionEvent;
begin
  Result := Application.OnException;
end;

procedure TBoldVCLExceptionLog.SetOnException(const AHandler: TBoldExceptionEvent);
begin
  Application.OnException := AHandler;
end;

procedure TBoldVCLExceptionLog.ShowException(E: Exception);
begin
  Application.ShowException(E);
end;

procedure TBoldVCLExceptionLog.QueueHook;
begin
  TThread.ForceQueue(nil, Hook);
end;

procedure TBoldVCLExceptionLog.UnqueueHook;
begin
  TThread.RemoveQueuedEvents(nil, Hook);
end;

initialization
  GExceptionLog := TBoldVCLExceptionLog.Create;
  GExceptionLog.HookNowAndAfterStartUp;

finalization
  GExceptionLog.Unhook;
  FreeAndNil(GExceptionLog);

end.
