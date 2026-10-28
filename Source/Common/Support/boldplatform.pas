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
// Login name of the user running the process.
function BoldLoginName: string;
// Locale-aware comparison of the first n characters of s1 and s2 (<0, 0, >0),
// case-insensitive unless CaseSensitive. Used for string attribute comparison.
function BoldUnicodeCompareLen(CaseSensitive: Boolean; s1, s2: string; n: Integer): Integer;
// COM initialisation for a thread that uses COM-based drivers (ADO, MSXML).
// No-ops off Windows.
procedure BoldCoInitialize;
procedure BoldCoUninitialize;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
  ActiveX,
{$ELSE}
  Classes,
{$ENDIF}
  SysUtils;

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

{$IFDEF MSWINDOWS}
function BoldLoginName: string;
var
  User: PChar;
  Size: DWord;
begin
  Result := '';
  Size := 256;
  User := StrAlloc(Size);
  try
    try
      if WNetGetUser(PChar(0), User, Size) = 0 then begin
        Result := string(User);
      end else begin
        RaiseLastOSError;
      end;
    except
      if GetUserName(User, Size) then begin
        Result := string(User);
      end else begin
        RaiseLastOSError;
      end;
    end;
  finally
    StrDispose(User);
  end;
end;
{$ELSE}
function BoldLoginName: string;
begin
  Result := GetEnvironmentVariable('USER');
  if Result = '' then
    Result := GetEnvironmentVariable('LOGNAME');
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

{$IFNDEF FPC}
function BoldUnicodeCompareLen(CaseSensitive: Boolean; s1, s2: string; n: Integer): Integer;
const
   CSTR_EQUAL = 2;
begin
  if CaseSensitive then
    Result:=CompareStringEx(nil, 0, PWideChar(s1), n, PWideChar(s2), n, nil, nil, 0)-CSTR_EQUAL
  else
    Result:=CompareStringEx(nil, NORM_IGNORECASE, PWideChar(s1), n, PWideChar(s2), n, nil, nil, 0)-CSTR_EQUAL
end;
{$ELSE}
function BoldUnicodeCompareLen(CaseSensitive: Boolean; s1, s2: string; n: Integer): Integer;
begin
  if CaseSensitive then
    Result := SysUtils.UnicodeCompareStr(Copy(s1, 1, n), Copy(s2, 1, n))
  else
    Result := SysUtils.UnicodeCompareText(Copy(s1, 1, n), Copy(s2, 1, n));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure BoldCoInitialize;
begin
  CoInitialize(nil);
end;

procedure BoldCoUninitialize;
begin
  CoUninitialize;
end;
{$ELSE}
procedure BoldCoInitialize;
begin
end;

procedure BoldCoUninitialize;
begin
end;
{$ENDIF}

initialization

finalization
{$IFDEF MSWINDOWS}
  if CurrentProcess <> INVALID_HANDLE_VALUE then
    CloseHandle(CurrentProcess);
{$ENDIF}
end.
