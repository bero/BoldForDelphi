
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

    2. 🖥️ TBoldConsoleLogSink: Writes logs to console (stdout)
       - Useful for console applications and debugging

    3. 🐛 TBoldDebugLogSink: Writes logs to OutputDebugString
       - Visible in IDE debugger
       - Useful during development

    4. 📋 TBoldMemoryLogSink: Stores logs in memory (TStringList)
       - Useful for testing and in-app log viewers

  🛠️ Usage:
    // File logging
    BoldLogManager.RegisterSink(TBoldFileLogSink.Create('app.log', 10*1024*1024));

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
  System.Classes,
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
    FFileName: string;
    FMaxSize: Integer;
    FFileStream: TFileStream;
    FLock: TObject;
    procedure EnsureFileOpen;
    procedure CheckFileSize;
  protected
    procedure DoWriteLog(const FormattedMsg: string); override;
  public
    constructor Create(const AFileName: string; AMaxSize: Integer = 10 * 1024 * 1024);
    destructor Destroy; override;
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
  System.SysUtils,
  System.SyncObjs,
  WinApi.Windows;

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

constructor TBoldFileLogSink.Create(const AFileName: string; AMaxSize: Integer);
begin
  inherited Create('FileLogSink:' + AFileName);
  FFileName := AFileName;
  FMaxSize := AMaxSize;
  FLock := TObject.Create;
  FFileStream := nil;
end;

destructor TBoldFileLogSink.Destroy;
begin
  TMonitor.Enter(FLock);
  try
    FreeAndNil(FFileStream);
  finally
    TMonitor.Exit(FLock);
  end;
  FLock.Free;
  inherited;
end;

procedure TBoldFileLogSink.EnsureFileOpen;
var
  Dir: string;
begin
  if FFileStream = nil then
  begin
    Dir := ExtractFilePath(FFileName);
    if (Dir <> '') and not DirectoryExists(Dir) then
      ForceDirectories(Dir);

    if FileExists(FFileName) then
      FFileStream := TFileStream.Create(FFileName, fmOpenWrite or fmShareDenyWrite)
    else
      FFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyWrite);

    FFileStream.Seek(0, soEnd);
  end;
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
  TMonitor.Enter(FLock);
  try
    EnsureFileOpen;
    CheckFileSize;

    Line := FormattedMsg + sLineBreak;
    Bytes := TEncoding.UTF8.GetBytes(Line);
    FFileStream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    TMonitor.Exit(FLock);
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
  OutputDebugString(PChar(FormattedMsg));
end;

{ TBoldMemoryLogSink }

constructor TBoldMemoryLogSink.Create(AMaxLines: Integer);
begin
  inherited Create('MemoryLogSink');
  FLines := TStringList.Create;
  FMaxLines := AMaxLines;
end;

destructor TBoldMemoryLogSink.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TBoldMemoryLogSink.DoWriteLog(const FormattedMsg: string);
begin
  TMonitor.Enter(Self);
  try
    FLines.Add(FormattedMsg);
    while (FMaxLines > 0) and (FLines.Count > FMaxLines) do
      FLines.Delete(0);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBoldMemoryLogSink.Clear;
begin
  TMonitor.Enter(Self);
  try
    FLines.Clear;
  finally
    TMonitor.Exit(Self);
  end;
end;

end.
