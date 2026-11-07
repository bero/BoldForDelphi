unit Test.BoldPersistenceOperationXMLStreaming;

{ TBoldPersistenceOperation.RemoteExecute, the client side of a persistence
  operation over SOAP. A stub IBoldSOAPService records the request and returns
  a canned reply, so no SOAP server is needed. Delphi only: the service
  interface comes from a COM type library (BoldSOAP_TLB). }

interface

uses
  BoldTestFramework;

type
  [Category('Persistence')]
  TTestBoldPersistenceOperationRemoteExecute = class(TBoldTestCase)
  published
    procedure TestRequestCarriesTheOperationAndItsParameters;
    procedure TestReplyIsReadIntoTheOutParameters;
    procedure TestFaultRaisesTheFaultString;
    procedure TestReplyThatIsNotXMLRaisesLoadError;
    procedure TestFaultDoesNotLeak;
  end;

implementation

uses
  Windows,
  SysUtils,
  BoldDefs,
  BoldMeta,
  BoldXMLStreaming,
  BoldDefaultXMLStreaming,
  BoldPersistenceController,
  BoldPersistenceOperationXMLStreaming,
  BoldSOAP_TLB;

const
  cEnvelopeStart = '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"><SOAP-ENV:Body>';
  cEnvelopeEnd = '</SOAP-ENV:Body></SOAP-ENV:Envelope>';
  cPingReply = cEnvelopeStart + '<PingResponse><Answer>pong</Answer></PingResponse>' + cEnvelopeEnd;
  cFaultReply = cEnvelopeStart +
    '<SOAP-ENV:Fault><SOAP-ENV:faultstring>Server is down</SOAP-ENV:faultstring></SOAP-ENV:Fault>' +
    cEnvelopeEnd;

type
  // Records the request and answers with a fixed reply.
  TStubSOAPService = class(TInterfacedObject, IBoldSOAPService)
  private
    fRequest: string;
    fReply: string;
  public
    constructor Create(const Reply: string);
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure Get(const request: WideString; out reply: WideString); safecall;
    property Request: string read fRequest;
  end;

  // Sends Value = 42 and reads Answer from the reply.
  TPingOperation = class(TBoldPersistenceOperation)
  private
    fAnswer: string;
  protected
    function GetName: string; override;
  public
    procedure WriteInParams(XMLNode: TBoldXMLNode); override;
    procedure WriteOutParams(XMLNode: TBoldXMLNode); override;
    procedure ReadInParams(XMLNode: TBoldXMLNode); override;
    procedure ReadOutParams(XMLNode: TBoldXMLNode); override;
    procedure Execute(PersistenceController: TBoldPersistenceController); override;
    property Answer: string read fAnswer;
  end;

{ TStubSOAPService }

constructor TStubSOAPService.Create(const Reply: string);
begin
  inherited Create;
  fReply := Reply;
end;

function TStubSOAPService.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := E_NOTIMPL;
end;

function TStubSOAPService.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TStubSOAPService.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TStubSOAPService.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TStubSOAPService.Get(const request: WideString; out reply: WideString);
begin
  fRequest := request;
  reply := fReply;
end;

{ TPingOperation }

function TPingOperation.GetName: string;
begin
  Result := 'Ping';
end;

procedure TPingOperation.WriteInParams(XMLNode: TBoldXMLNode);
begin
  XMLNode.WriteSubNodeString('Value', '42');
end;

procedure TPingOperation.WriteOutParams(XMLNode: TBoldXMLNode);
begin
end;

procedure TPingOperation.ReadInParams(XMLNode: TBoldXMLNode);
begin
end;

procedure TPingOperation.ReadOutParams(XMLNode: TBoldXMLNode);
begin
  fAnswer := XMLNode.ReadSubNodeString('Answer');
end;

procedure TPingOperation.Execute(PersistenceController: TBoldPersistenceController);
begin
end;

// Runs the Ping operation against a stub that answers Reply. Returns the
// operation's Answer; Request receives what was sent, also when it raises.
function RunPing(const Reply: string; out Request: string): string;
var
  Model: TMoldModel;
  Manager: TBoldDefaultXMLStreamManager;
  Operation: TPingOperation;
  Service: TStubSOAPService;
  ServiceIntf: IBoldSOAPService;
begin
  Request := '';
  Model := TMoldModel.Create(nil, 'SOAPTestModel');
  Manager := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, Model);
  Operation := TPingOperation.Create(Manager);
  Service := TStubSOAPService.Create(Reply);
  ServiceIntf := Service;
  try
    Operation.RemoteExecute(ServiceIntf);
    Result := Operation.Answer;
  finally
    Request := Service.Request;
    ServiceIntf := nil;
    Operation.Free;
    Manager.Free;
    Model.Free;
  end;
end;

{ TTestBoldPersistenceOperationRemoteExecute }

procedure TTestBoldPersistenceOperationRemoteExecute.TestRequestCarriesTheOperationAndItsParameters;
var
  Request: string;
begin
  RunPing(cPingReply, Request);
  Assert.IsTrue(Pos('<SOAP-ENV:Envelope ', Request) > 0, Request);
  Assert.IsTrue(Pos('<SOAP-ENV:Body>', Request) > 0, Request);
  Assert.IsTrue(Pos('<Ping>', Request) > 0, Request);
  Assert.IsTrue(Pos('<Value>42</Value>', Request) > 0, Request);
end;

procedure TTestBoldPersistenceOperationRemoteExecute.TestReplyIsReadIntoTheOutParameters;
var
  Request: string;
begin
  Assert.AreEqual('pong', RunPing(cPingReply, Request));
end;

procedure TTestBoldPersistenceOperationRemoteExecute.TestFaultRaisesTheFaultString;
var
  Request, ErrorText: string;
begin
  ErrorText := '';
  try
    RunPing(cFaultReply, Request);
  except
    on E: EBold do
      ErrorText := E.Message;
  end;
  Assert.AreEqual('Error: Server is down', ErrorText);
end;

procedure TTestBoldPersistenceOperationRemoteExecute.TestReplyThatIsNotXMLRaisesLoadError;
var
  Request, ErrorText: string;
begin
  ErrorText := '';
  try
    RunPing('not xml', Request);
  except
    on E: EBoldXMLLoadError do
      ErrorText := E.Message;
  end;
  Assert.IsTrue(Pos('not xml', ErrorText) > 0, 'EBoldXMLLoadError naming the reply: ' + ErrorText);
end;

// The leak column of the DUnitX runner measures nothing (its default leak
// monitor always reports 0), so this counts the heap blocks instead.
procedure TTestBoldPersistenceOperationRemoteExecute.TestFaultDoesNotLeak;
var
  Request: string;
  i: Integer;
  Before, After: Int64;

  procedure RunFault;
  begin
    try
      RunPing(cFaultReply, Request);
    except
      on EBold do
        ; // the fault is the expected outcome
    end;
  end;

begin
  for i := 1 to 5 do
    RunFault; // warm up caches and one-time allocations
  Before := BoldTestAllocatedBlocks;
  for i := 1 to 50 do
    RunFault;
  After := BoldTestAllocatedBlocks;
  Assert.IsTrue(After = Before, Format('50 fault replies left %d blocks', [After - Before]));
end;

initialization
  BoldRegisterTest(TTestBoldPersistenceOperationRemoteExecute);

end.
