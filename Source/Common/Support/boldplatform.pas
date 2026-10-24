/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldPlatform;

// Operating-system calls used by the Bold runtime, one implementation per
// platform, so that the units calling them stay platform neutral (#117).

interface

// CPU time the current process has spent in user mode, in 100 ns units.
// Off Windows this is wall-clock time for now; it only feeds OCL timing statistics.
function BoldProcessUserTimeInTicks: Int64;
// Full file name of the module (exe or package) that contains this code.
function BoldModuleFileName: string;
// True when a debugger is attached to the process.
function BoldDebuggerPresent: Boolean;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows;
{$ELSE}
  Classes,
  SysUtils;
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  TFileTimeAligner = record
  case integer of
  0: (asFileTime: TFileTime);
  1: (asInt64: Int64);
  end;

var
  CurrentProcess: THANDLE = INVALID_HANDLE_VALUE;

function BoldProcessUserTimeInTicks: Int64;
var
  UserTime, CreationTime, ExitTime, KernelTime: TFileTimeAligner;
begin
  if CurrentProcess <> 0 then
    CloseHandle(CurrentProcess);
  CurrentProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, GetCurrentProcessId);
  if GetProcessTimes(CurrentProcess, CreationTime.asFileTime, ExitTime.asFileTime, KernelTime.asFileTime, UserTime.asFileTime) then
    Result := UserTime.asInt64
  else
    Result := 0;
end;

function BoldModuleFileName: string;
var
  Buffer: array[0..261] of Char;
begin
  SetString(Result, Buffer, GetModuleFileName(HInstance, Buffer, SizeOf(Buffer)));
end;
{$ELSE}
function BoldProcessUserTimeInTicks: Int64;
begin
  Result := Int64(TThread.GetTickCount64) * 10000;
end;

function BoldModuleFileName: string;
begin
  Result := ParamStr(0);
end;
{$ENDIF}

{$IFNDEF FPC}
function BoldDebuggerPresent: Boolean;
begin
  Result := DebugHook <> 0;
end;
{$ELSE}
{$IFDEF MSWINDOWS}
function BoldDebuggerPresent: Boolean;
begin
  Result := IsDebuggerPresent;
end;
{$ELSE}
{$IFDEF LINUX}
// The kernel reports the process id of an attached debugger as TracerPid in /proc/self/status.
function BoldDebuggerPresent: Boolean;
const
  TracerPid = 'TracerPid:';
var
  Status: TextFile;
  Line: string;
begin
  Result := False;
  AssignFile(Status, '/proc/self/status');
  {$I-}
  Reset(Status);
  {$I+}
  if IOResult <> 0 then
    Exit;
  try
    while not Eof(Status) do
    begin
      ReadLn(Status, Line);
      if Copy(Line, 1, Length(TracerPid)) = TracerPid then
      begin
        Result := StrToIntDef(Trim(Copy(Line, Length(TracerPid) + 1, MaxInt)), 0) <> 0;
        Break;
      end;
    end;
  finally
    CloseFile(Status);
  end;
end;
{$ELSE}
function BoldDebuggerPresent: Boolean;
begin
  Result := False;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

initialization

finalization
{$IFDEF MSWINDOWS}
  if CurrentProcess <> INVALID_HANDLE_VALUE then
    CloseHandle(CurrentProcess);
{$ENDIF}
end.
