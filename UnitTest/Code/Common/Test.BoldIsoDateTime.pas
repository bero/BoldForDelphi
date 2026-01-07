unit Test.BoldIsoDateTime;

interface

uses
  DUnitX.TestFramework,
  BoldIsoDateTime,
  BoldDefs;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldIsoDateTime = class
  public
    [Test]
    [Category('Quick')]
    procedure TestAsISODateTime;
    [Test]
    [Category('Quick')]
    procedure TestAsISODateTimeMS;
    [Test]
    [Category('Quick')]
    procedure TestParseISODate;
    [Test]
    [Category('Quick')]
    procedure TestParseISODateInvalidFormat;
    [Test]
    [Category('Quick')]
    procedure TestParseISODateInvalidMonth;
    [Test]
    [Category('Quick')]
    procedure TestParseISODateInvalidDay;
    [Test]
    [Category('Quick')]
    procedure TestParseISODateTime;
    [Test]
    [Category('Quick')]
    procedure TestParseISODateTimeWithT;
    [Test]
    [Category('Quick')]
    procedure TestParseISODateTimeInvalid;
    [Test]
    [Category('Quick')]
    procedure TestParseISOTime;
    [Test]
    [Category('Quick')]
    procedure TestParseISOTimeShortFormat;
    [Test]
    [Category('Quick')]
    procedure TestParseISOTimeInvalidFormat;
    [Test]
    [Category('Quick')]
    procedure TestParseISOTimeInvalidHour;
    [Test]
    [Category('Quick')]
    procedure TestParseISOTimeInvalidMinute;
    [Test]
    [Category('Quick')]
    procedure TestParseISOTimeInvalidSecond;
  end;

implementation

uses
  SysUtils;

{ TTestBoldIsoDateTime }

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
  Assert.WillRaise(
    procedure
    begin
      ParseISODate('2025/12/31');  // Wrong separator
    end,
    EBold
  );
end;

procedure TTestBoldIsoDateTime.TestParseISODateInvalidMonth;
begin
  // Month > 12
  Assert.WillRaise(
    procedure
    begin
      ParseISODate('2025-13-01');
    end,
    EBold
  );
  // Month < 1
  Assert.WillRaise(
    procedure
    begin
      ParseISODate('2025-00-01');
    end,
    EBold
  );
end;

procedure TTestBoldIsoDateTime.TestParseISODateInvalidDay;
begin
  // Day < 1
  Assert.WillRaise(
    procedure
    begin
      ParseISODate('2025-01-00');
    end,
    EBold
  );
  // Day > days in month
  Assert.WillRaise(
    procedure
    begin
      ParseISODate('2025-02-30');  // February has max 29 days
    end,
    EBold
  );
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
  Assert.WillRaise(
    procedure
    begin
      ParseISODateTime('2025-12-31X14:30:45');  // Invalid separator
    end,
    EBold
  );
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
  Assert.WillRaise(
    procedure
    begin
      ParseISOTime('14-30-45');  // Wrong separator
    end,
    EBold
  );
end;

procedure TTestBoldIsoDateTime.TestParseISOTimeInvalidHour;
begin
  Assert.WillRaise(
    procedure
    begin
      ParseISOTime('24:00:00');  // Hour > 23
    end,
    EBold
  );
end;

procedure TTestBoldIsoDateTime.TestParseISOTimeInvalidMinute;
begin
  Assert.WillRaise(
    procedure
    begin
      ParseISOTime('12:60:00');  // Minute > 59
    end,
    EBold
  );
end;

procedure TTestBoldIsoDateTime.TestParseISOTimeInvalidSecond;
begin
  Assert.WillRaise(
    procedure
    begin
      ParseISOTime('12:30:60');  // Second > 59
    end,
    EBold
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldIsoDateTime);

end.
