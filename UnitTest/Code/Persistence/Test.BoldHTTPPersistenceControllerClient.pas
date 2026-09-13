unit Test.BoldHTTPPersistenceControllerClient;

{ TBoldHTTPSOAPService.Get confuses character counts with byte counts, which was
  correct when Bold was written for ANSI Delphi and has been wrong since Delphi
  2009:

    len := Length(StrRequest);           // characters
    dataSent.Write(StrRequest[1], len);  // bytes - sends half a UTF-16 buffer

    SetString(StrReply, nil, Size);      // Size characters = 2*Size bytes
    dataReceived.Stream.Read(Pointer(StrReply)^, Size);   // Size bytes

  These tests run a real loopback HTTP server and assert on what arrives, rather
  than on what the code looks like. Both currently fail.

  Prerequisite: the BoldSOAP type library has to be registered, because
  TBoldHTTPSOAPService.Create calls LoadRegTypeLib(LIBID_BoldSOAP). Register it
  once per machine, no elevation needed:

    "<Delphi>\bin\tregsvr.exe" -c <repo>\Source\Common\SOAP\BoldSOAP.tlb

  Without it the tests fail with "Unable to load type library LIBID_BoldSOAP"
  rather than on their own assertions. }

interface

uses
  System.SysUtils,
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestBoldHTTPPersistenceControllerClient = class
  private
    FPort: Integer;
    FReceived: TBytes;
    FReceivedContentLength: Integer;
    procedure StartEchoServer;
    procedure StopEchoServer;
    function RoundTrip(const ARequest: WideString): WideString;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { The document has to arrive at the server byte for byte. Today it arrives
      UTF-16 encoded and truncated to half its length. }
    [Test]
    procedure RequestArrivesIntact;

    { No single-byte document should turn into a NUL-interleaved one in transit. }
    [Test]
    procedure RequestContainsNoEmbeddedNulls;

    { What the server echoes back has to come out of Get unchanged. Today it
      comes back as UTF-16 nonsense. }
    [Test]
    procedure ReplyRoundTrips;
  end;

implementation

uses
  System.Classes,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdContext,
  IdGlobal,
  BoldWebConnection,
  BoldHTTPPersistenceControllerClient;

const
  { Deliberately plain ASCII: any NUL or doubling seen on the wire is the
    transport's doing, not the payload's. }
  cRequest =
    '<SOAP-ENV:Envelope><SOAP-ENV:Body><PMFetchIDListWithCondition>' +
    '<FetchMode>0</FetchMode></PMFetchIDListWithCondition>' +
    '</SOAP-ENV:Body></SOAP-ENV:Envelope>';

var
  GServer: TIdHTTPServer;
  GReceived: TBytes;
  GReceivedContentLength: Integer;

type
  { Indy's events are "of object", so they need a real method. }
  TEchoHandler = class
    { Indy dispatches every HTTP command through OnCommandGet, whatever the
      command actually is. }
    procedure CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

var
  GHandler: TEchoHandler;

procedure TEchoHandler.CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Stream: TMemoryStream;
begin
  SetLength(GReceived, 0);
  GReceivedContentLength := ARequestInfo.ContentLength;

  if Assigned(ARequestInfo.PostStream) then
  begin
    ARequestInfo.PostStream.Position := 0;
    SetLength(GReceived, ARequestInfo.PostStream.Size);
    if Length(GReceived) > 0 then
      ARequestInfo.PostStream.ReadBuffer(GReceived[0], Length(GReceived));
  end;

  { Echo the bytes straight back, so the reply is exactly what was sent. }
  Stream := TMemoryStream.Create;
  if Length(GReceived) > 0 then
    Stream.WriteBuffer(GReceived[0], Length(GReceived));
  Stream.Position := 0;
  AResponseInfo.ContentStream := Stream;
  AResponseInfo.ContentType := 'text/xml';
  AResponseInfo.FreeContentStream := True;
end;

procedure TTestBoldHTTPPersistenceControllerClient.StartEchoServer;
begin
  GHandler := TEchoHandler.Create;
  GServer := TIdHTTPServer.Create(nil);
  GServer.DefaultPort := 0;          // let the OS pick a free port
  GServer.OnCommandGet := GHandler.CommandGet;
  GServer.Active := True;
  FPort := GServer.Bindings[0].Port;
end;

procedure TTestBoldHTTPPersistenceControllerClient.StopEchoServer;
begin
  if Assigned(GServer) then
  begin
    GServer.Active := False;
    FreeAndNil(GServer);
  end;
  FreeAndNil(GHandler);
end;

procedure TTestBoldHTTPPersistenceControllerClient.Setup;
begin
  SetLength(GReceived, 0);
  GReceivedContentLength := 0;
  StartEchoServer;
end;

procedure TTestBoldHTTPPersistenceControllerClient.TearDown;
begin
  StopEchoServer;
end;

function TTestBoldHTTPPersistenceControllerClient.RoundTrip(
  const ARequest: WideString): WideString;
var
  Connection: TBoldWebConnection;
  Service: TBoldHTTPSOAPService;
begin
  Connection := TBoldWebConnection.Create(nil);
  try
    Connection.URL := Format('http://localhost:%d/persistence', [FPort]);
    Service := TBoldHTTPSOAPService.Create;
    try
      Service.WebConnection := Connection;
      Service.Get(ARequest, Result);
    finally
      Service.Free;
    end;
  finally
    Connection.Free;
  end;
  FReceived := GReceived;
  FReceivedContentLength := GReceivedContentLength;
end;

procedure TTestBoldHTTPPersistenceControllerClient.RequestArrivesIntact;
var
  Arrived: string;
begin
  RoundTrip(cRequest);

  { Compare content, not length. The payload is ASCII, so its UTF-8 encoding is
    exactly as many bytes as the document has characters - which is also how
    many bytes the buggy code writes. A length assertion therefore passes while
    the content is half a UTF-16 buffer. }
  Arrived := TEncoding.UTF8.GetString(FReceived);
  Assert.AreEqual(string(cRequest), Arrived,
    Format('The server received %d bytes for a %d character document, and they ' +
           'do not decode to it. Length() counts characters but Write() takes ' +
           'bytes, so half a UTF-16 buffer goes on the wire.',
           [Length(FReceived), Length(cRequest)]));
end;

procedure TTestBoldHTTPPersistenceControllerClient.RequestContainsNoEmbeddedNulls;
var
  I, Nulls: Integer;
begin
  RoundTrip(cRequest);
  Nulls := 0;
  for I := 0 to High(FReceived) do
    if FReceived[I] = 0 then
      Inc(Nulls);
  Assert.AreEqual(0, Nulls,
    Format('%d of %d bytes on the wire are NUL, so the document was sent as ' +
           'UTF-16 rather than as bytes.', [Nulls, Length(FReceived)]));
end;

procedure TTestBoldHTTPPersistenceControllerClient.ReplyRoundTrips;
var
  Reply: WideString;
begin
  Reply := RoundTrip(cRequest);
  Assert.AreEqual(string(cRequest), string(Reply),
    'The echo server returned exactly what it was given, so Get should hand ' +
    'back the original document.');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldHTTPPersistenceControllerClient);

end.
