unit Test.BoldFileHandler;

{ TBoldDiskFileHandler writes the files of Bold's code generator outside the
  Delphi IDE (the standalone model editor, #128). It writes them as the IDE
  does, UTF-8 with a byte order mark and CRLF line breaks, on every platform.
  It reads an existing file, such as an include file with methods written by
  hand, as UTF-8 after a byte order mark, as UTF-8 without one when its bytes
  are UTF-8, and otherwise in the ANSI code page, as an editor without
  Unicode wrote it. }

interface

uses
  SysUtils,
  Classes,
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldFileHandler = class(TBoldTestCase)
  private
    fFolder: string;
    function FileNamed(const AName: string): string;
    // The lines a disk file handler reads from a file with these bytes
    function LinesRead(const AName: string; const ABytes: TBytes): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWritesUTF8WithBOMAndCRLF;
    procedure TestReadsUTF8WithBOMAndWritesOneBOM;
    procedure TestReadsUTF8WithoutBOM;
    procedure TestReadsOtherBytesInTheANSICodePage;
  end;

implementation

uses
  BoldDefs,
  BoldFileHandler;

const
  // kär, with a character outside ASCII that UTF-8 and ANSI write differently
  cKaer = 'k'#$00E4'r';
  // Characters of three and four bytes in UTF-8: the euro sign and an emoji
  cEuroAndEmoji = #$20AC' '#$D83D#$DE00;

{ A line as a TStringList, and so a file handler, holds it. Delphi's holds any
  text. Free Pascal's keeps AnsiStrings in the system code page: UTF-8 on
  Linux, but on Windows the ANSI code page of a console program such as the
  test runner, which has no emoji. }
function AsAStringListHoldsIt(const ALine: string): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(ALine);
    Result := Lines[0];
  finally
    Lines.Free;
  end;
end;

function UTF8BOM: TBytes;
begin
  Result := TEncoding.UTF8.GetPreamble;
end;

function UTF8(const AText: string): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(AText);
end;

function JoinBytes(const A, B: TBytes): TBytes;
begin
  SetLength(Result, Length(A) + Length(B));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A));
  if Length(B) > 0 then
    Move(B[0], Result[Length(A)], Length(B));
end;

{ The bytes as hex, so that a failure shows where they differ }
function Hex(const ABytes: TBytes): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(ABytes) - 1 do
    Result := Result + IntToHex(ABytes[i], 2) + ' ';
end;

procedure WriteBytes(const AFileName: string; const ABytes: TBytes);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    if Length(ABytes) > 0 then
      Stream.WriteBuffer(ABytes[0], Length(ABytes));
  finally
    Stream.Free;
  end;
end;

function ReadBytes(const AFileName: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

{ TTestBoldFileHandler }

procedure TTestBoldFileHandler.SetUp;
begin
  inherited;
  fFolder := IncludeTrailingPathDelimiter(BoldTestNewTempDir('BoldFileHandler'));
end;

procedure TTestBoldFileHandler.TearDown;
begin
  BoldTestDeleteTree(fFolder);
  inherited;
end;

function TTestBoldFileHandler.FileNamed(const AName: string): string;
begin
  Result := fFolder + AName;
end;

function TTestBoldFileHandler.LinesRead(const AName: string; const ABytes: TBytes): string;
var
  Handler: TBoldDiskFileHandler;
  i: Integer;
begin
  WriteBytes(FileNamed(AName), ABytes);
  Handler := TBoldDiskFileHandler.Create(FileNamed(AName), mtIncFile, False, nil);
  try
    Result := '';
    for i := 0 to Handler.Count - 1 do
      Result := Result + Handler.Content[i] + '|';
  finally
    Handler.Free;
  end;
end;

procedure TTestBoldFileHandler.TestWritesUTF8WithBOMAndCRLF;
var
  Handler: TBoldDiskFileHandler;
begin
  Handler := TBoldDiskFileHandler.Create(FileNamed('Written.pas'), mtUnit, False, nil);
  try
    Handler.WriteLn('unit Written;');
    Handler.WriteLn('// ' + cKaer);
  finally
    // writes the file
    Handler.Free;
  end;
  Assert.AreEqual(Hex(JoinBytes(UTF8BOM, UTF8('unit Written;'#13#10'// ' + cKaer + #13#10))),
    Hex(ReadBytes(FileNamed('Written.pas'))), 'the bytes of the file');
end;

procedure TTestBoldFileHandler.TestReadsUTF8WithBOMAndWritesOneBOM;
var
  Handler: TBoldDiskFileHandler;
begin
  WriteBytes(FileNamed('Methods.inc'), JoinBytes(UTF8BOM, UTF8(cKaer + #13#10)));
  Handler := TBoldDiskFileHandler.Create(FileNamed('Methods.inc'), mtIncFile, False, nil);
  try
    Assert.AreEqual(1, Handler.Count, 'the lines read');
    Assert.AreEqual(cKaer, Handler.Content[0], 'the line, without the byte order mark');
    Handler.WriteLn('end;');
  finally
    Handler.Free;
  end;
  Assert.AreEqual(Hex(JoinBytes(UTF8BOM, UTF8(cKaer + #13#10'end;'#13#10))),
    Hex(ReadBytes(FileNamed('Methods.inc'))), 'written back with one byte order mark');
end;

procedure TTestBoldFileHandler.TestReadsUTF8WithoutBOM;
begin
  // As an editor on Linux writes it: no byte order mark, LF
  Assert.AreEqual(AsAStringListHoldsIt(cKaer) + '|' + AsAStringListHoldsIt(cEuroAndEmoji) + '|',
    LinesRead('Lazarus.inc', UTF8(cKaer + #10 + cEuroAndEmoji + #10)), 'the lines read');
end;

procedure TTestBoldFileHandler.TestReadsOtherBytesInTheANSICodePage;
var
  Kaer, Degrees: TBytes;
begin
  // kär and 25° in Windows-1252: in UTF-8 the E4 would need continuation bytes,
  // not the r, and B0 cannot start a UTF-8 character at all
  Kaer := TBytes.Create(Ord('k'), $E4, Ord('r'), 13, 10);
  Degrees := TBytes.Create(Ord('2'), Ord('5'), $B0, 13, 10);
  Assert.AreEqual(TEncoding.ANSI.GetString(Kaer, 0, 3) + '|', LinesRead('Kaer.inc', Kaer), 'kär');
  Assert.AreEqual(TEncoding.ANSI.GetString(Degrees, 0, 3) + '|', LinesRead('Degrees.inc', Degrees), '25 degrees');
end;

initialization
  BoldRegisterTest(TTestBoldFileHandler);

end.
