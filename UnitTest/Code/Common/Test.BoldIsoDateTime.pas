unit Test.BoldIsoDateTime;

interface

uses
  BoldTestFramework,
  BoldIsoDateTime,
  BoldDefs;

type
  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldIsoDateTime = class(TBoldTestCase)
  private
    fInput: string;
    procedure CallParseISODate;
    procedure CallParseISODateTime;
    procedure CallParseISOTime;
  published
    procedure TestAsISODateTime;
    procedure TestAsISODateTimeMS;
    procedure TestParseISODate;
    procedure TestParseISODateInvalidFormat;
    procedure TestParseISODateInvalidMonth;
    procedure TestParseISODateInvalidDay;
    procedure TestParseISODateTime;
    procedure TestParseISODateTimeWithT;
    procedure TestParseISODateTimeInvalid;
    procedure TestParseISOTime;
    procedure TestParseISOTimeShortFormat;
    procedure TestParseISOTimeInvalidFormat;
    procedure TestParseISOTimeInvalidHour;
    procedure TestParseISOTimeInvalidMinute;
    procedure TestParseISOTimeInvalidSecond;
  end;

implementation

uses
  SysUtils;

{ TTestBoldIsoDateTime }

procedure TTestBoldIsoDateTime.CallParseISODate;
begin
  ParseISODate(fInput);
end;

procedure TTestBoldIsoDateTime.CallParseISODateTime;
begin
  ParseISODateTime(fInput);
end;

procedure TTestBoldIsoDateTime.CallParseISOTime;
begin
  ParseISOTime(fInput);
end;

procedure TTestBoldIsoDateTime.TestAsISODateTime;
var
  dt: TDateTime;
  s: string;
begin
  dt := EncodeDate(2025, 12, 31) + EncodeTime(14, 30, 45, 0);
  s := AsISODateTime(dt);
  Assert.AreEqual('2025-12-31T14:30:45', s);
end;

procedure TTestBoldIsoDateTime.TestAsISODateTimeMS;
var
  dt: TDateTime;
  s: string;
begin
  dt := EncodeDate(2025, 6, 15) + EncodeTime(10, 20, 30, 123);
  s := AsISODateTimeMS(dt);
  Assert.AreEqual('2025-06-15T10:20:30:123', s);
end;

procedure TTestBoldIsoDateTime.TestParseISODate;
var
  dt: TDateTime;
  y, m, d: Word;
begin
  dt := ParseISODate('2025-12-31');
  DecodeDate(dt, y, m, d);
  Assert.AreEqual(Word(2025), y);
  Assert.AreEqual(Word(12), m);
  Assert.AreEqual(Word(31), d);
end;

procedure TTestBoldIsoDateTime.TestParseISODateInvalidFormat;
begin
  fInput := '2025/12/31';  // Wrong separator
  Assert.WillRaise(CallParseISODate, EBold);
end;

procedure TTestBoldIsoDateTime.TestParseISODateInvalidMonth;
begin
  // Month > 12
  fInput := '2025-13-01';
  Assert.WillRaise(CallParseISODate, EBold);
  // Month < 1
  fInput := '2025-00-01';
  Assert.WillRaise(CallParseISODate, EBold);
end;

procedure TTestBoldIsoDateTime.TestParseISODateInvalidDay;
begin
  // Day < 1
  fInput := '2025-01-00';
  Assert.WillRaise(CallParseISODate, EBold);
  // Day > days in month
  fInput := '2025-02-30';  // February has max 29 days
  Assert.WillRaise(CallParseISODate, EBold);
end;

procedure TTestBoldIsoDateTime.TestParseISODateTime;
var
  dt: TDateTime;
  y, m, d, h, mi, s, ms: Word;
begin
  dt := ParseISODateTime('2025-12-31 14:30:45');
  DecodeDate(dt, y, m, d);
  DecodeTime(dt, h, mi, s, ms);
  Assert.AreEqual(Word(2025), y);
  Assert.AreEqual(Word(12), m);
  Assert.AreEqual(Word(31), d);
  Assert.AreEqual(Word(14), h);
  Assert.AreEqual(Word(30), mi);
  Assert.AreEqual(Word(45), s);
end;

procedure TTestBoldIsoDateTime.TestParseISODateTimeWithT;
var
  dt: TDateTime;
  y, m, d, h, mi, s, ms: Word;
begin
  dt := ParseISODateTime('2025-12-31T14:30:45');
  DecodeDate(dt, y, m, d);
  DecodeTime(dt, h, mi, s, ms);
  Assert.AreEqual(Word(2025), y);
  Assert.AreEqual(Word(12), m);
  Assert.AreEqual(Word(31), d);
  Assert.AreEqual(Word(14), h);
  Assert.AreEqual(Word(30), mi);
  Assert.AreEqual(Word(45), s);
end;

procedure TTestBoldIsoDateTime.TestParseISODateTimeInvalid;
begin
  fInput := '2025-12-31X14:30:45';  // Invalid separator
  Assert.WillRaise(CallParseISODateTime, EBold);
end;

procedure TTestBoldIsoDateTime.TestParseISOTime;
var
  dt: TDateTime;
  h, m, s, ms: Word;
begin
  dt := ParseISOTime('14:30:45');
  DecodeTime(dt, h, m, s, ms);
  Assert.AreEqual(Word(14), h);
  Assert.AreEqual(Word(30), m);
  Assert.AreEqual(Word(45), s);
end;

procedure TTestBoldIsoDateTime.TestParseISOTimeShortFormat;
var
  dt: TDateTime;
  h, m, s, ms: Word;
begin
  // Bug fix: ##:## format should work with seconds defaulting to 0
  dt := ParseISOTime('14:30');
  DecodeTime(dt, h, m, s, ms);
  Assert.AreEqual(Word(14), h);
  Assert.AreEqual(Word(30), m);
  Assert.AreEqual(Word(0), s);
end;

procedure TTestBoldIsoDateTime.TestParseISOTimeInvalidFormat;
begin
  fInput := '14-30-45';  // Wrong separator
  Assert.WillRaise(CallParseISOTime, EBold);
end;

procedure TTestBoldIsoDateTime.TestParseISOTimeInvalidHour;
begin
  fInput := '24:00:00';  // Hour > 23
  Assert.WillRaise(CallParseISOTime, EBold);
end;

procedure TTestBoldIsoDateTime.TestParseISOTimeInvalidMinute;
begin
  fInput := '12:60:00';  // Minute > 59
  Assert.WillRaise(CallParseISOTime, EBold);
end;

procedure TTestBoldIsoDateTime.TestParseISOTimeInvalidSecond;
begin
  fInput := '12:30:60';  // Second > 59
  Assert.WillRaise(CallParseISOTime, EBold);
end;

initialization
  BoldRegisterTest(TTestBoldIsoDateTime);

end.
