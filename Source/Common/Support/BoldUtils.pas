
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldUtils;

interface

uses
  Variants,
  SysUtils,
  Classes,
  TypInfo,
  Windows,
  BoldDefs,
  WideStrings;

type
  TBoldNotificationEvent = procedure(AComponent: TComponent; Operation: TOperation) of object;

  TBoldPassthroughNotifier = class(TComponent)
  private
    fNotificationEvent: TBoldNotificationEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor CreateWithEvent(NotificationEvent: TBoldNotificationEvent; Owner: TComponent = nil);
  end;

function CharCount(c: char; const s: string): integer;
procedure BoldAppendToStrings(strings: TStrings; const aString: string; const ForceNewLine: Boolean);
function BoldSeparateStringList(strings: TStringList; const Separator, PreString, PostString: String; AIndex: integer = -1): String;
function BoldCaseIndependentPos(const Substr, S: string): Integer;
function StringToBoolean(StrValue: String): Boolean;
function GetUpperLimitForMultiplicity(const Multiplicity: String): Integer;
function GetLowerLimitForMultiplicity(const Multiplicity: String): Integer;
function StringListToVarArray(List: TStringList): variant;
function TimeStampComp(const Time1, Time2: TTimeStamp): Integer;
function StrToDateFmt(const ADateString: string; const ADateFormat: string;
  const ATimeFormat: string; const ADateSeparatorChar: char = '/';
  const ADateTimeSeperatorChar: char = ' '): TDateTime;
function UserTimeInTicks: Int64;
function TicksToDateTime(Ticks: Int64): TDateTime;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}

function BoldRootRegistryKey: string;
function GetModuleFileNameAsString(IncludePath: Boolean): string;

{variant support}
function BoldVariantToStrings(V: OleVariant; Strings: TStrings): Integer;

var BoldRunningAsDesignTimePackage: boolean = false;

implementation

uses
  BoldCoreConsts,
  BoldRev;

type
  TFileTimeAligner = record
  case integer of
  0: (asFileTime: TFileTime);
  1: (asInt64: Int64);
  end;
var
  CurrentProcess: THANDLE = INVALID_HANDLE_VALUE ;

function UserTimeInTicks: Int64;
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

function TicksToDateTime(Ticks: Int64): TDateTime;
const
  Nr100nsPerDay = 3600.0*24.0*10000000.0;
begin
  Result := Ticks/Nr100nsPerDay;
end;

function BoldCaseIndependentPos(const Substr, S: string): Integer;
var
  SubstrLen: integer;
begin
  SubStrLen := Length(Substr);
  if SubstrLen > Length(S) then
    Result := 0
  else
  begin
    Result := Pos(Substr, S);
    if (Result = 0) or (Result > SubStrLen) then
      Result := Pos(AnsiUpperCase(Substr), AnsiUpperCase(S));
  end;
end;


function StringToBoolean(StrValue: String): Boolean;
begin
  Result := False;
  if (UpperCase(StrValue)= 'Y') or (UpperCase(StrValue) = 'T') or (UpperCase(StrValue) = 'TRUE') then
    Result := True;
end;

function BoldRootRegistryKey: string;
begin
  Result := Format('Software\BoldSoft\%s\%s',  [BoldProductNameShort,BoldProductVersion]);
end;

function GetModuleFileNameAsString(IncludePath: Boolean): string;
var
 Buffer: array[0..261] of Char;
 ModuleName: string;
begin
  SetString(ModuleName, Buffer, Windows.GetModuleFileName(HInstance,
        Buffer, SizeOf(Buffer)));
  if IncludePath then
    Result := ModuleName
  else
    Result := ExtractFileName(ModuleName);  
end;

function BoldSeparateStringList(strings: TStringList; const Separator, PreString, PostString: String; AIndex: integer): String;
var
  i, Cnt, Size: integer;
  SB: TStringBuilder;
begin
  Cnt := strings.Count;
  case Cnt of
    0: Result := '';
    1: Result := PreString + Strings[0] + PostString;
  else
  begin
    Size := length(PreString) + length(PostString);
    for I := 0 to Cnt - 1 do
      Inc(Size, Length(Strings[I]));
    Inc(Size, Length(Separator) * Cnt);
    SB := TStringBuilder.Create(Size);
    SB.Append(PreString);
    for i := 0 to Cnt-2 do
    begin
      SB.Append(Strings[i]);
      if AIndex <> -1 then
        SB.Append(IntToStr(AIndex));
      SB.Append(Separator);
    end;
    SB.Append(Strings[Cnt-1]);
    if AIndex <> -1 then
      SB.Append(IntToStr(AIndex));
    SB.Append(PostString);
    Result := SB.ToString;
    FreeAndNil(SB);
  end;
  end;
end;

procedure BoldAppendToStrings(Strings: TStrings; const aString: string; const ForceNewLine: Boolean);
var
  StrCount, SplitterPos: Integer;
  SB: TStringBuilder;
  TempStr: string;
begin
  if (Pos(BOLDLF, aString)>0) or (Pos(BOLDCR, aString)>0) then
  begin
    SB := TStringBuilder.Create(aString);
    SB.Replace(BOLDCR, ' ');
    SB.Replace(BOLDLF, ' ');
    TempStr := SB.ToString;
    FreeAndNil(SB);
  end
  else
    TempStr := aString;

  Strings.BeginUpdate;
  try
    StrCount := Strings.Count-1;
    if (StrCount = -1) or ForceNewLine then
    begin
      Strings.Add(TempStr);
      Inc(StrCount);
    end
    else
      Strings[StrCount] := Strings[StrCount] + TempStr;

    { break lines into max 80 chars per line }
    while Length(Strings[StrCount]) > 80 do
    begin
      SplitterPos := 80;

      while (Pos(Strings[StrCount][SplitterPos],' ,=')=0) and (SplitterPos > 1)  do
        Dec(SplitterPos);

      Strings.Append(Copy(Strings[StrCount], SplitterPos + 1, 65536));
      Strings[StrCount] := Copy(Strings[StrCount], 1, SplitterPos);

      Inc(StrCount);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{ TBoldPassThroughNotification }

constructor TBoldPassthroughNotifier.CreateWithEvent(NotificationEvent: TBoldNotificationEvent; Owner: TComponent = nil);
begin
  inherited create(Owner);
  fNotificationEvent := NotificationEvent;
end;

procedure TBoldPassthroughNotifier.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  fNotificationEvent(AComponent, Operation);
end;

function GetLowerLimitForMultiplicity(const Multiplicity: String): Integer;
var
  p: Integer;
begin
  p := Pos('..', Multiplicity);
  if p = 0 then
    Result := StrToIntDef(Multiplicity, 0)
  else
    Result := StrToIntDef(Copy(Multiplicity, 1, p - 1), -1);
end;

function GetUpperLimitForMultiplicity(const Multiplicity: String): Integer;
var
  p: Integer;
begin

  if (Multiplicity = '') or (Trim(Multiplicity) = '') then
    result := 1
  else
  begin
    p := Pos('..', Multiplicity);
    if p = 0 then
      Result := StrToIntDef(Multiplicity, MaxInt)
    else
      Result := StrToIntDef(Copy(Multiplicity, p + 2, MaxInt), MaxInt);
  end;
  if Result < 0 then
    result :=  MaxInt;
end;

function StringListToVarArray(List: TStringList): variant;
var
  Count, i: integer;
  varList: variant;
begin
  Count := List.Count;
  if Count = 0 then
    Result := UnAssigned
  else
  begin
    varList := VarArrayCreate([0,Count - 1],varOleStr);
    for i := 0 to Count - 1 do
      varList[i] := List[i];
    Result := varList;
  end;
end;

function TimeStampComp(const Time1, Time2: TTimeStamp): Integer;
var
  cTime1, cTime2: Real;
begin
  cTime1 := TimeStampToMSecs(Time1);
  cTime2 := TimeStampToMSecs(Time2);
  if (cTime1 = cTime2) then
    Result := 0
  else if (cTime1 > cTime2) then
    Result := 1
  else
    Result := -1;
end;

function StrToDateFmt(const ADateString: string; const ADateFormat: string;
  const ATimeFormat: string; const ADateSeparatorChar: char = '/';
  const ADateTimeSeperatorChar: char = ' '): TDateTime;
var
  sPreviousShortDateFormat: string;
  sPreviousDateSeparator: char;
  {$IFDEF BOLD_DELPHI28_OR_LATER}
  sPreviousShortTimeFormat: string;
  {$ENDIF}
begin
  sPreviousShortDateFormat := FormatSettings.ShortDateFormat;
  {$IFNDEF BOLD_DELPHI28_OR_LATER}
  FormatSettings.ShortDateFormat := ADateFormat;
  if ATimeFormat <> '' then
    FormatSettings.ShortDateFormat := FormatSettings.ShortDateFormat + ADateTimeSeperatorChar + ATimeFormat;
  {$ELSE}
  FormatSettings.ShortDateFormat := ADateFormat;
  sPreviousShortTimeFormat := FormatSettings.LongTimeFormat;
  FormatSettings.LongTimeFormat := ATimeFormat;
  {$ENDIF}
  sPreviousDateSeparator := FormatSettings.DateSeparator;
  FormatSettings.DateSeparator := ADateSeparatorChar;
  try
    Result := StrToDateTime(ADateString);
  finally
    FormatSettings.ShortDateFormat := sPreviousShortDateFormat;
    FormatSettings.DateSeparator := sPreviousDateSeparator;
    {$IFDEF BOLD_DELPHI28_OR_LATER}
    FormatSettings.LongTimeFormat := sPreviousShortTimeFormat;
    {$ENDIF}
  end;
end;

function BoldVariantToStrings(V: OleVariant; Strings: TStrings): Integer;
var
  I: Integer;
begin
  Result := 0;
  if VarIsArray(V) and (VarArrayDimCount(V) = 1) then
  begin
    for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
    begin
      Strings.Add(V[I]);
      Inc(Result);
    end;
  end;
end;

function CharCount(c: char; const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = c then Inc(Result);
end;

initialization

finalization
  if CurrentProcess<>INVALID_HANDLE_VALUE then
    CloseHandle(CurrentProcess);
end.
