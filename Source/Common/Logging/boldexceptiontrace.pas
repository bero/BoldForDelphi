{ Global compiler directives }
{$include bold.inc}
unit BoldExceptionTrace;

{ The call stack of an exception, for Bold's log (#130).

  Bold names no stack-trace library. What it reads is the contract of the
  runtime library, so the library that provides the frames can be exchanged
  without a change here:

  - Delphi: Exception.StackTrace. The RTL fills it through
    Exception.GetExceptionStackInfoProc, GetStackInfoStringProc and
    CleanUpStackInfoProc, which a provider sets: Stacker names itself first
    in the program's uses clause, an adapter can do the same for another
    library. Without a provider StackTrace is empty, and the report gives the
    address the exception was raised at and the load address of its module;
    the detailed map file of the build resolves the address afterwards.
    A provider's text often starts with its own capture routines and the
    RTL's raise routines; those lines are left out, so the raising routine
    comes first.
  - Free Pascal: the frames the RTL keeps for the exception while it is
    handled (RaiseList), each as BackTraceStrFunc writes it: routine, line
    and file when the program is compiled with -gl, the address otherwise.
    The RTL keeps RaiseMaxFrameCount callers, 16 by default; this unit raises
    that to 64, so a stack that comes through a GUI's message loop keeps its
    outer callers.

  Call BoldExceptionCallStack, BoldExceptionReport and BoldLogException in the
  except block or exception handler that handles the exception: Free Pascal
  keeps the frames only until then, and the raise address is that of the
  exception being handled. }

interface

uses
  SysUtils;

{ The call stack of E, one frame per line, the raising routine first; empty
  when there is none (Delphi without a provider) }
function BoldExceptionCallStack(E: Exception): string;

{ What the log gets for E: its class and message, then its call stack, or,
  when there is none, where it was raised }
function BoldExceptionReport(E: Exception): string;

{ BoldExceptionReport(E) to Bold's log (BoldLogManager) as an error, after
  AContext when one is given }
procedure BoldLogException(E: Exception; const AContext: string = '');

implementation

uses
  Classes,
  {$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Dlfcn,
  {$ENDIF}
  {$ENDIF}
  BoldLogInterfaces;

resourcestring
  sRaisedAt = 'Raised at $%p (module loaded at $%p); no call stack: the program has no stack-trace provider';
  sRaisedAtUnknown = 'No call stack: the program has no stack-trace provider, and the exception is not the one being handled';

{$IFDEF FPC}

const
  // How many callers Free Pascal records for an exception, at least
  cMaxFrameCount = 64;

function BoldExceptionCallStack(E: Exception): string;
var
  Raised: PExceptObject;
  Frames: PCodePointer;
  Lines: TStringList;
  i: Integer;
begin
  Result := '';
  // The exceptions being handled, the latest first; E is one of them in its
  // except block
  Raised := RaiseList;
  while Assigned(Raised) and (Raised^.FObject <> E) do
    Raised := Raised^.Next;
  if not Assigned(Raised) then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.Add(Trim(string(BackTraceStrFunc(Raised^.Addr))));
    Frames := Raised^.Frames;
    for i := 0 to Raised^.Framecount - 1 do
    begin
      Lines.Add(Trim(string(BackTraceStrFunc(Frames^))));
      Inc(Frames);
    end;
    Result := TrimRight(Lines.Text);
  finally
    Lines.Free;
  end;
end;

function RaiseAddressText(E: Exception): string;
begin
  Result := sRaisedAtUnknown;
end;

{$ELSE}

{ True when ALine is the frame of the RTL's raise routine: System.@RaiseExcept
  on Windows, System._RaiseExcept on Linux. A routine whose name only
  contains the word, such as RaiseExceptionIfInvalid, is not it. }
function IsTheRaiseFrame(const ALine: string): Boolean;
const
  cRaiseFrames: array[0..1] of string = ('System.@RaiseExcept', 'System._RaiseExcept');
var
  Name: string;
  P, After: Integer;
begin
  Result := False;
  for Name in cRaiseFrames do
  begin
    P := Pos(Name, ALine);
    if P > 0 then
    begin
      After := P + Length(Name);
      if (After > Length(ALine)) or not CharInSet(ALine[After], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
        Exit(True);
    end;
  end;
end;

{ The lines of a provider's text from the raising routine on: what comes
  before it are the provider's capture routines and the RTL's raise routines,
  the last of which is the raise routine itself. The first raise frame is the
  one of this exception: an exception raised while another one unwinds (in a
  finally block or a destructor) has the other one's raise frame further
  down, and the frames in between tell where this one came from. }
function WithoutTheRaiseFrames(const AStackTrace: string): string;
var
  Lines: TStringList;
  i, FirstRaise: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AStackTrace;
    FirstRaise := -1;
    for i := 0 to Lines.Count - 1 do
      if IsTheRaiseFrame(Lines[i]) then
      begin
        FirstRaise := i;
        Break;
      end;
    for i := FirstRaise downto 0 do
      Lines.Delete(i);
    Result := TrimRight(Lines.Text);
  finally
    Lines.Free;
  end;
end;

function BoldExceptionCallStack(E: Exception): string;
begin
  Result := WithoutTheRaiseFrames(E.StackTrace);
end;

function ModuleBase(AAddress: Pointer): Pointer;
{$IFDEF MSWINDOWS}
var
  Info: TMemoryBasicInformation;
begin
  if VirtualQuery(AAddress, Info, SizeOf(Info)) <> 0 then
    Result := Info.AllocationBase
  else
    Result := nil;
end;
{$ELSE}
{$IFDEF POSIX}
var
  Info: dl_info;
begin
  if dladdr(NativeUInt(AAddress), Info) <> 0 then
    Result := Info.dli_fbase
  else
    Result := nil;
end;
{$ELSE}
begin
  Result := nil;
end;
{$ENDIF}
{$ENDIF}

function RaiseAddressText(E: Exception): string;
begin
  // ExceptAddr is the address of the exception being handled
  if (ExceptObject = E) and (ExceptAddr <> nil) then
    Result := Format(sRaisedAt, [ExceptAddr, ModuleBase(ExceptAddr)])
  else
    Result := sRaisedAtUnknown;
end;

{$ENDIF}

function BoldExceptionReport(E: Exception): string;
var
  CallStack: string;
begin
  CallStack := BoldExceptionCallStack(E);
  if CallStack = '' then
    CallStack := RaiseAddressText(E);
  Result := E.ClassName + ': ' + E.Message + sLineBreak + CallStack;
end;

procedure BoldLogException(E: Exception; const AContext: string);
begin
  if AContext <> '' then
    BoldLogManager.Error(AContext + sLineBreak + BoldExceptionReport(E))
  else
    BoldLogManager.Error(BoldExceptionReport(E));
end;

initialization
  {$IFDEF FPC}
  // The RTL records the callers when an exception is raised, at most
  // RaiseMaxFrameCount of them (16 by default); a program that set more
  // keeps its value
  if RaiseMaxFrameCount < cMaxFrameCount then
    RaiseMaxFrameCount := cMaxFrameCount;
  {$ENDIF}

end.
