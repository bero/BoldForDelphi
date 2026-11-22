{ Global compiler directives }
{$include bold.inc}
unit BoldExceptionLogFMX;

{ Bold's exception log in an FMX program (#130). Naming this unit early in
  the program's uses clause, after a stack-trace provider such as Stacker,
  switches on the exception log of BoldExceptionLog (the log file and the
  last-resort handler) and logs each exception that FMX's message loop
  handles, with its call stack, before it is shown. A handler the program
  sets in Application.OnException during its start-up (in a form's OnCreate)
  is called instead of FMX's dialog. }

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  BoldExceptionLog;

type
  TBoldFMXExceptionLog = class(TBoldApplicationExceptionLog)
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
  GExceptionLog: TBoldFMXExceptionLog;

{ TBoldFMXExceptionLog }

function TBoldFMXExceptionLog.ApplicationExists: Boolean;
begin
  Result := Assigned(Application);
end;

function TBoldFMXExceptionLog.GetOnException: TBoldExceptionEvent;
begin
  Result := Application.OnException;
end;

procedure TBoldFMXExceptionLog.SetOnException(const AHandler: TBoldExceptionEvent);
begin
  Application.OnException := AHandler;
end;

procedure TBoldFMXExceptionLog.ShowException(E: Exception);
begin
  Application.ShowException(E);
end;

procedure TBoldFMXExceptionLog.QueueHook;
begin
  TThread.ForceQueue(nil, Hook);
end;

procedure TBoldFMXExceptionLog.UnqueueHook;
begin
  TThread.RemoveQueuedEvents(nil, Hook);
end;

initialization
  GExceptionLog := TBoldFMXExceptionLog.Create;
  GExceptionLog.HookNowAndAfterStartUp;

finalization
  GExceptionLog.Unhook;
  FreeAndNil(GExceptionLog);

end.
