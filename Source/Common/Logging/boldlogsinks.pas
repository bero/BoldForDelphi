
{ Global compiler directives }
{$include bold.inc}
unit BoldLogSinks;

{$REGION 'Documentation'}
(*
  🗒️ Unit: BoldLogSinks

  📜 Description:
    Provides concrete implementations of IBoldLogSink for various log destinations.

  🔖 Sink Implementations:
    1. 📁 TBoldFileLogSink: Writes logs to a file using TFileLogging
       - Thread-safe file writing
       - Configurable max file size
       - ISO timestamp format
       - Several files to choose from: the first that opens when the first
         entry is written (another instance of the program holds one, a
         folder cannot be written)

    2. 🖥️ TBoldConsoleLogSink: Writes logs to console (stdout)
       - Useful for console applications and debugging

    3. 🐛 TBoldDebugLogSink: Writes logs to the debugger output (OutputDebugString on Windows,
       standard error elsewhere)
       - Visible in IDE debugger
       - Useful during development

    4. 📋 TBoldMemoryLogSink: Stores logs in memory (TStringList)
       - Useful for testing and in-app log viewers

  🛠️ Usage:
    // File logging
    BoldLogManager.RegisterSink(TBoldFileLogSink.Create('app.log', 10*1024*1024));

    // File logging to the first of these files that opens
    BoldLogManager.RegisterSink(TBoldFileLogSink.Create(['app.log', 'app-2.log']));

    // Console logging
    BoldLogManager.RegisterSink(TBoldConsoleLogSink.Create);

    // Debug output (IDE)
    BoldLogManager.RegisterSink(TBoldDebugLogSink.Create);

    // Memory logging for testing
    var MemSink := TBoldMemoryLogSink.Create;
    BoldLogManager.RegisterSink(MemSink);
    // ... log some messages ...
    // Check: MemSink.Lines contains all logged messages
*)
{$ENDREGION}

interface

uses
  Classes,
  SyncObjs,
  BoldLogInterfaces;

type
  TBoldBaseLogSink = class(TInterfacedObject, IBoldLogSink)
  private
    FEnabled: Boolean;
    FLevels: TBoldLogLevels;
    FName: string;
    FIncludeLevel: Boolean;
    FIncludeTimestamp: Boolean;
    FIncludeThreadId: Boolean;
  protected
    function FormatLogEntry(const Msg: string; Level: TBoldLogLevel;
      const TimeStamp: TDateTime; ThreadID: Cardinal): string; virtual;
    procedure DoWriteLog(const FormattedMsg: string); virtual; abstract;
  public
    constructor Create(const AName: string);
    procedure WriteLog(const Msg: string; Level: TBoldLogLevel;
      const TimeStamp: TDateTime; ThreadID: Cardinal);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetLevels: TBoldLogLevels;
    procedure SetLevels(Value: TBoldLogLevels);
    function GetName: string;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Levels: TBoldLogLevels read GetLevels write SetLevels;
    property Name: string read GetName;
    property IncludeLevel: Boolean read FIncludeLevel write FIncludeLevel;
    property IncludeTimestamp: Boolean read FIncludeTimestamp write FIncludeTimestamp;
    property IncludeThreadId: Boolean read FIncludeThreadId write FIncludeThreadId;
  end;

  TBoldFileLogSink = class(TBoldBaseLogSink)
  private
    FFileNames: array of string;
    FFileName: string;
    FMaxSize: Integer;
    FFileStream: TFileStream;
    FLock: TCriticalSection;
    procedure EnsureFileOpen;
    procedure CheckFileSize;
    class function OpenFile(const AFileName: string): TFileStream;
  protected
    procedure DoWriteLog(const FormattedMsg: string); override;
  public
    constructor Create(const AFileName: string; AMaxSize: Integer = 10 * 1024 * 1024); overload;
    { Writes to the first of AFileNames that opens when the first entry is
      written: a file that another sink of this program or another process
      holds open, or one whose folder cannot be created or written, is passed
      over. Writing raises when none opens }
    constructor Create(const AFileNames: array of string; AMaxSize: Integer = 10 * 1024 * 1024); overload;
    destructor Destroy; override;
    { The file the entries go to; before the first entry, the first file }
    property FileName: string read FFileName;
  end;

  TBoldConsoleLogSink = class(TBoldBaseLogSink)
  protected
    procedure DoWriteLog(const FormattedMsg: string); override;
  public
    constructor Create;
  end;

  TBoldDebugLogSink = class(TBoldBaseLogSink)
  protected
    procedure DoWriteLog(const FormattedMsg: string); override;
  public
    constructor Create;
  end;

  TBoldMemoryLogSink = class(TBoldBaseLogSink)
  private
    FLines: TStringList;
    FLock: TCriticalSection;
    FMaxLines: Integer;
  protected
    procedure DoWriteLog(const FormattedMsg: string); override;
  public
    constructor Create(AMaxLines: Integer = 10000);
    destructor Destroy; override;
    procedure Clear;
    property Lines: TStringList read FLines;
    property MaxLines: Integer read FMaxLines write FMaxLines;
  end;

implementation

uses
  SysUtils,
  BoldPlatform;

{ TBoldBaseLogSink }

constructor TBoldBaseLogSink.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FEnabled := True;
  FLevels := DefaultBoldLogLevels;
  FIncludeLevel := True;
  FIncludeTimestamp := True;
  FIncludeThreadId := False;
end;

function TBoldBaseLogSink.FormatLogEntry(const Msg: string; Level: TBoldLogLevel;
  const TimeStamp: TDateTime; ThreadID: Cardinal): string;
begin
  Result := '';

  if FIncludeTimestamp then
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', TimeStamp) + ' ';

  if FIncludeLevel then
    Result := Result + '[' + BoldLogLevelToString(Level) + '] ';

  Result := Result + Msg;

  if FIncludeThreadId then
    Result := Result + Format(' (TID=%d)', [ThreadID]);
end;

procedure TBoldBaseLogSink.WriteLog(const Msg: string; Level: TBoldLogLevel;
  const TimeStamp: TDateTime; ThreadID: Cardinal);
var
  FormattedMsg: string;
begin
  FormattedMsg := FormatLogEntry(Msg, Level, TimeStamp, ThreadID);
  DoWriteLog(FormattedMsg);
end;

function TBoldBaseLogSink.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TBoldBaseLogSink.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

function TBoldBaseLogSink.GetLevels: TBoldLogLevels;
begin
  Result := FLevels;
end;

procedure TBoldBaseLogSink.SetLevels(Value: TBoldLogLevels);
begin
  FLevels := Value;
end;

function TBoldBaseLogSink.GetName: string;
begin
  Result := FName;
end;

{ TBoldFileLogSink }

const
  {$IFDEF MSWINDOWS}
  // Others can read the log while the program writes it, no other process
  // can write to it
  cLogShareMode = fmShareDenyWrite;
  {$ELSE}
  // An advisory lock that no other process gets while the program writes the
  // log; readers take none, so they still read it. fmShareDenyWrite would ask
  // for a shared lock, which a second instance gets as well (Free Pascal's
  // flock), or for a read lock, which Delphi cannot take on a file it opens
  // for writing only
  cLogShareMode = fmShareExclusive;
  {$ENDIF}

var
  // The files that the file sinks of this program hold open. The lock of a
  // file keeps other processes out; on Linux and macOS Delphi's locks belong
  // to the process, so they would let a second sink of this program in
  GHeldFiles: TStringList;
  GHeldFilesLock: TCriticalSection;

{ True when no other sink of this program holds AFileName; it is then held
  until ReleaseFile }
function HoldFile(const AFileName: string): Boolean;
begin
  GHeldFilesLock.Enter;
  try
    Result := GHeldFiles.IndexOf(ExpandFileName(AFileName)) < 0;
    if Result then
      GHeldFiles.Add(ExpandFileName(AFileName));
  finally
    GHeldFilesLock.Leave;
  end;
end;

procedure ReleaseFile(const AFileName: string);
var
  i: Integer;
begin
  // A sink that Bold's log frees after this unit is finalized finds no list
  if not Assigned(GHeldFilesLock) then
    Exit;
  GHeldFilesLock.Enter;
  try
    i := GHeldFiles.IndexOf(ExpandFileName(AFileName));
    if i >= 0 then
      GHeldFiles.Delete(i);
  finally
    GHeldFilesLock.Leave;
  end;
end;

constructor TBoldFileLogSink.Create(const AFileName: string; AMaxSize: Integer);
begin
  Create([AFileName], AMaxSize);
end;

constructor TBoldFileLogSink.Create(const AFileNames: array of string; AMaxSize: Integer);
var
  i: Integer;
begin
  if Length(AFileNames) = 0 then
    raise EArgumentException.Create('TBoldFileLogSink: no file to write to');
  inherited Create('FileLogSink:' + AFileNames[0]);
  SetLength(FFileNames, Length(AFileNames));
  for i := 0 to High(AFileNames) do
    FFileNames[i] := AFileNames[i];
  FFileName := FFileNames[0];
  FMaxSize := AMaxSize;
  FLock := TCriticalSection.Create;
  FFileStream := nil;
end;

destructor TBoldFileLogSink.Destroy;
begin
  // Also after a constructor that raised, before the lock was created
  if Assigned(FLock) then
  begin
    FLock.Enter;
    try
      if Assigned(FFileStream) then
        ReleaseFile(FFileName);
      FreeAndNil(FFileStream);
    finally
      FLock.Leave;
    end;
    FLock.Free;
  end;
  inherited;
end;

class function TBoldFileLogSink.OpenFile(const AFileName: string): TFileStream;
var
  Dir: string;
begin
  Dir := ExtractFilePath(AFileName);
  if (Dir <> '') and not DirectoryExists(Dir) then
    ForceDirectories(Dir);
  // Created first, then opened as every later run opens it, so the lock is
  // the same whether the file was there or not
  if not FileExists(AFileName) then
    TFileStream.Create(AFileName, fmCreate).Free;
  Result := TFileStream.Create(AFileName, fmOpenWrite or cLogShareMode);
  Result.Seek(0, soEnd);
end;

procedure TBoldFileLogSink.EnsureFileOpen;
var
  i: Integer;
begin
  if FFileStream <> nil then
    Exit;
  for i := 0 to High(FFileNames) do
    if HoldFile(FFileNames[i]) then
      try
        FFileStream := OpenFile(FFileNames[i]);
        FFileName := FFileNames[i];
        Exit;
      except
        // Another process holds it, or its folder cannot be written: the next
        ReleaseFile(FFileNames[i]);
        if i = High(FFileNames) then
          raise;
      end
    else if i = High(FFileNames) then
      raise EFOpenError.CreateFmt('%s: another log sink of this program writes to it', [FFileNames[i]]);
end;

procedure TBoldFileLogSink.CheckFileSize;
begin
  if (FMaxSize > 0) and (FFileStream <> nil) and (FFileStream.Size > FMaxSize) then
    FFileStream.Size := 0;
end;

procedure TBoldFileLogSink.DoWriteLog(const FormattedMsg: string);
var
  Line: string;
  Bytes: TBytes;
begin
  FLock.Enter;
  try
    EnsureFileOpen;
    CheckFileSize;

    Line := FormattedMsg + sLineBreak;
    Bytes := TEncoding.UTF8.GetBytes(Line);
    FFileStream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    FLock.Leave;
  end;
end;

{ TBoldConsoleLogSink }

constructor TBoldConsoleLogSink.Create;
begin
  inherited Create('ConsoleLogSink');
end;

procedure TBoldConsoleLogSink.DoWriteLog(const FormattedMsg: string);
begin
  WriteLn(FormattedMsg);
end;

{ TBoldDebugLogSink }

constructor TBoldDebugLogSink.Create;
begin
  inherited Create('DebugLogSink');
end;

procedure TBoldDebugLogSink.DoWriteLog(const FormattedMsg: string);
begin
  BoldOutputDebugString(FormattedMsg);
end;

{ TBoldMemoryLogSink }

constructor TBoldMemoryLogSink.Create(AMaxLines: Integer);
begin
  inherited Create('MemoryLogSink');
  FLines := TStringList.Create;
  FLock := TCriticalSection.Create;
  FMaxLines := AMaxLines;
end;

destructor TBoldMemoryLogSink.Destroy;
begin
  FLines.Free;
  FLock.Free;
  inherited;
end;

procedure TBoldMemoryLogSink.DoWriteLog(const FormattedMsg: string);
begin
  FLock.Enter;
  try
    FLines.Add(FormattedMsg);
    while (FMaxLines > 0) and (FLines.Count > FMaxLines) do
      FLines.Delete(0);
  finally
    FLock.Leave;
  end;
end;

procedure TBoldMemoryLogSink.Clear;
begin
  FLock.Enter;
  try
    FLines.Clear;
  finally
    FLock.Leave;
  end;
end;

initialization
  GHeldFiles := TStringList.Create;
  {$IFNDEF MSWINDOWS}
  GHeldFiles.CaseSensitive := True;
  {$ENDIF}
  GHeldFilesLock := TCriticalSection.Create;

finalization
  FreeAndNil(GHeldFilesLock);
  FreeAndNil(GHeldFiles);

end.
