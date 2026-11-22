{ Global compiler directives }
{$include bold.inc}
unit BoldExceptionLog;

{ Bold's exception log (#130). Naming this unit early in a program's uses
  clause, after a stack-trace provider such as Stacker, switches it on when
  the unit is initialized:

  - A sink of Bold's log (BoldLogManager) that writes to a file:
    <program>.log next to the program; <program>-<process id>.log there when
    another instance of the program holds that file; the same two in the
    user's folder for logs when the program's folder cannot be written
    (BoldUserLogFolder). The file is opened at the first entry, so a program
    that logs nothing leaves none.
  - The RTL's last-resort handler (System.ExceptProc): an exception that no
    handler takes (in the program's main block, in a console program) goes
    to the log with its call stack, then the handler that was there reports
    it and the program ends. One raised in a unit's initialization is logged
    under Free Pascal only: Delphi finalizes the units, this one among them,
    before it reports it.

  A GUI program names BoldExceptionLogVCL, BoldExceptionLogFMX or
  BoldExceptionLogLCL instead. Each names this unit and also logs the
  exceptions that the application's message loop handles, before they are
  shown (TBoldApplicationExceptionLog).

  Not covered: an exception that ends a thread. The RTL keeps it in the
  thread's FatalException and calls no handler, and Free Pascal's frames are
  gone once the thread's except block ends; a thread logs it in its Execute
  with BoldLogException. }

interface

uses
  SysUtils,
  BoldLogInterfaces;

type
  { An application's OnException; the VCL, FMX and the LCL declare the same }
  TBoldExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  { The hook into an application's message loop, without the framework, which
    a subclass in a framework unit gives access to. The hook sets the
    application's OnException to a handler that logs the exception with its
    call stack, then calls the handler the program had set, or shows the
    exception as the application does when there was none. }
  TBoldApplicationExceptionLog = class
  private
    FProgramsHandler: TBoldExceptionEvent;
    procedure LogException(Sender: TObject; E: Exception);
    function IsHooked: Boolean;
  protected
    function ApplicationExists: Boolean; virtual; abstract;
    function GetOnException: TBoldExceptionEvent; virtual; abstract;
    procedure SetOnException(const AHandler: TBoldExceptionEvent); virtual; abstract;
    procedure ShowException(E: Exception); virtual; abstract;
    { Has Hook called from the application's queue, which runs when the
      message loop starts. The VCL and FMX take TThread.ForceQueue; the LCL
      takes its own queue, since Free Pascal runs a TThread.ForceQueue from
      the main thread at once while the program has started no thread }
    procedure QueueHook; virtual; abstract;
    { Takes back what QueueHook queued and has not run yet }
    procedure UnqueueHook; virtual; abstract;
  public
    { Hooks into the application, keeping the handler the program has set;
      does nothing when hooked already or when there is no application }
    procedure Hook;
    { Hook now, and once more from the main thread's queue, which runs when
      the message loop starts: after the program's start-up, which may set a
      handler of its own (in a form's OnCreate) }
    procedure HookNowAndAfterStartUp;
    { Gives the application back the program's handler }
    procedure Unhook;
  end;

{ The folder for the user's logs, to which a program adds its own:
  %LOCALAPPDATA% on Windows, ~/Library/Logs on macOS, $XDG_STATE_HOME or
  ~/.local/state elsewhere. Empty when the environment does not say }
function BoldUserLogFolder: string;

{ The files the log of the program AProgramFile tries, in this order:
  <name>.log and <name>-<AProcessId>.log in the program's folder, then the
  same two in the folder <name> in AUserLogFolder, when that is given }
function BoldLogFileNames(const AProgramFile: string; AProcessId: Cardinal;
  const AUserLogFolder: string): TArray<string>;

{ BoldLogFileNames of this program, this process and BoldUserLogFolder }
function BoldExceptionLogFileNames: TArray<string>;

{ The sink of the exception log, registered with BoldLogManager when this
  unit is initialized. Enabled := False switches its file off }
function BoldExceptionLogSink: IBoldLogSink;

{ The file the exception log writes to; before its first entry, the first
  one it tries }
function BoldExceptionLogFileName: string;

{ BoldUnhandledExceptionReport to Bold's log as an error, saying that no
  handler took the exception: what this unit's last-resort handler does
  before it calls the one that was there. It never raises }
procedure BoldLogUnhandledException(AObject: TObject; AAddress: Pointer;
  AFrameCount: Integer; AFrames: Pointer);

implementation

uses
  BoldLogSinks,
  BoldPlatform,
  BoldExceptionTrace;

resourcestring
  sUnhandled = '%s: an exception that no handler took; the program ends';

var
  GFileSink: TBoldFileLogSink;
  GSink: IBoldLogSink;

{ TBoldApplicationExceptionLog }

function TBoldApplicationExceptionLog.IsHooked: Boolean;
var
  Current, Mine: TBoldExceptionEvent;
begin
  // The parentheses call it: without them Delphi takes the function itself
  Current := GetOnException();
  Mine := LogException;
  Result := (TMethod(Current).Code = TMethod(Mine).Code) and
    (TMethod(Current).Data = TMethod(Mine).Data);
end;

procedure TBoldApplicationExceptionLog.Hook;
begin
  if not ApplicationExists or IsHooked then
    Exit;
  FProgramsHandler := GetOnException();
  SetOnException(LogException);
end;

procedure TBoldApplicationExceptionLog.HookNowAndAfterStartUp;
begin
  Hook;
  QueueHook;
end;

procedure TBoldApplicationExceptionLog.Unhook;
begin
  UnqueueHook;
  if ApplicationExists and IsHooked then
    SetOnException(FProgramsHandler);
  FProgramsHandler := nil;
end;

procedure TBoldApplicationExceptionLog.LogException(Sender: TObject; E: Exception);
begin
  BoldLogException(E, ExtractFileName(ParamStr(0)));
  if Assigned(FProgramsHandler) then
    FProgramsHandler(Sender, E)
  else
    ShowException(E);
end;

{ The log files }

{$IFDEF MSWINDOWS}
function BoldUserLogFolder: string;
begin
  Result := GetEnvironmentVariable('LOCALAPPDATA');
end;
{$ELSE}
function BoldUserLogFolder: string;
var
  Home: string;
begin
  Home := GetEnvironmentVariable('HOME');
  Result := '';
  {$IF DEFINED(MACOS) OR DEFINED(DARWIN)}
  if Home <> '' then
    Result := IncludeTrailingPathDelimiter(Home) + 'Library' + PathDelim + 'Logs';
  {$ELSE}
  Result := GetEnvironmentVariable('XDG_STATE_HOME');
  if (Result = '') and (Home <> '') then
    Result := IncludeTrailingPathDelimiter(Home) + '.local' + PathDelim + 'state';
  {$IFEND}
end;
{$ENDIF}

function BoldLogFileNames(const AProgramFile: string; AProcessId: Cardinal;
  const AUserLogFolder: string): TArray<string>;
var
  Name, Folder, WithProcessId: string;
begin
  Name := ChangeFileExt(ExtractFileName(AProgramFile), '');
  WithProcessId := Name + '-' + IntToStr(AProcessId) + '.log';
  Result := nil;
  if AUserLogFolder <> '' then
    SetLength(Result, 4)
  else
    SetLength(Result, 2);
  Folder := ExtractFilePath(AProgramFile);
  Result[0] := Folder + Name + '.log';
  Result[1] := Folder + WithProcessId;
  if AUserLogFolder <> '' then
  begin
    Folder := IncludeTrailingPathDelimiter(AUserLogFolder) + Name + PathDelim;
    Result[2] := Folder + Name + '.log';
    Result[3] := Folder + WithProcessId;
  end;
end;

function BoldExceptionLogFileNames: TArray<string>;
begin
  // Delphi gives the program as it was started on Linux (./program): the
  // full name, before the program changes its current folder
  Result := BoldLogFileNames(ExpandFileName(ParamStr(0)), BoldCurrentProcessId, BoldUserLogFolder);
end;

function BoldExceptionLogSink: IBoldLogSink;
begin
  Result := GSink;
end;

function BoldExceptionLogFileName: string;
begin
  if Assigned(GFileSink) then
    Result := GFileSink.FileName
  else
    Result := '';
end;

{ The last-resort handler }

procedure BoldLogUnhandledException(AObject: TObject; AAddress: Pointer;
  AFrameCount: Integer; AFrames: Pointer);
begin
  // The program is about to end: what cannot be logged is left to the
  // handler that reports it
  try
    BoldLogManager.Error(Format(sUnhandled, [ExtractFileName(ParamStr(0))]) + sLineBreak +
      BoldUnhandledExceptionReport(AObject, AAddress, AFrameCount, AFrames));
  except
  end;
end;

{$IFDEF FPC}
var
  GPreviousExceptProc: TExceptProc;

procedure LogUnhandledException(Obj: TObject; Addr: CodePointer; FrameCount: Longint;
  Frames: PCodePointer);
begin
  BoldLogUnhandledException(Obj, Addr, FrameCount, Frames);
  if Assigned(GPreviousExceptProc) then
    GPreviousExceptProc(Obj, Addr, FrameCount, Frames);
end;

procedure InstallExceptProc;
begin
  GPreviousExceptProc := ExceptProc;
  ExceptProc := @LogUnhandledException;
end;

procedure UninstallExceptProc;
begin
  if @ExceptProc = @LogUnhandledException then
    ExceptProc := GPreviousExceptProc;
end;
{$ELSE}
type
  // How System calls ExceptProc
  TBoldExceptProc = procedure(Obj: TObject; Addr: Pointer);

var
  GPreviousExceptProc: Pointer;

procedure LogUnhandledException(Obj: TObject; Addr: Pointer);
begin
  BoldLogUnhandledException(Obj, Addr, 0, nil);
  if Assigned(GPreviousExceptProc) then
    TBoldExceptProc(GPreviousExceptProc)(Obj, Addr);
end;

procedure InstallExceptProc;
begin
  GPreviousExceptProc := ExceptProc;
  ExceptProc := @LogUnhandledException;
end;

procedure UninstallExceptProc;
begin
  if ExceptProc = @LogUnhandledException then
    ExceptProc := GPreviousExceptProc;
end;
{$ENDIF}

initialization
  GFileSink := TBoldFileLogSink.Create(BoldExceptionLogFileNames);
  GSink := GFileSink;
  BoldLogManager.RegisterSink(GSink);
  InstallExceptProc;

finalization
  UninstallExceptProc;
  BoldLogManager.UnregisterSink(GSink);
  GSink := nil;
  GFileSink := nil;

end.
