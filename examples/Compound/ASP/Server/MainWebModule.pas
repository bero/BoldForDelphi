unit MainWebModule;

interface

uses
  { BoldComServerHandles was in this list but nothing here uses it: the demo
    talks HTTP and XML, and there is no COM component on the web module. It
    dragged in Bold's whole COM server chain (BoldComServer, ComObj, ActiveX),
    which an ISAPI extension is better off without. }
  Windows, Messages, SysUtils, Classes, IniFiles, HTTPApp, BoldServerHandles,
  BoldXMLDispatcher,
  BoldHTTPServerPersistenceHandlePassthrough, BoldPersistenceHandleSystem,
  BoldHandle, BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldHandles, BoldSubscription, BoldSystemHandle,
  BoldAbstractModel, BoldModel, BoldXMLRequests, DB,

  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.Client,

  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterFireDAC,
  BoldSQLDatabaseConfig, BoldAbstractPersistenceHandleDB;

type
  TwmASPServer = class(TWebModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleSystem1: TBoldPersistenceHandleSystem;
    BoldHTTPServerPersistenceHandlePassthrough1: TBoldHTTPServerPersistenceHandlePassthrough;
    BoldXMLDispatcher1: TBoldXMLDispatcher;
    BoldModel1: TBoldModel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    procedure wmASPServerPersistenceAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure wmASPServerSOAPCallsAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure BoldXMLDispatcher1Actions0Action(
      const request: TBoldXMLRequest; out response: String);
    procedure BoldXMLDispatcher1Actions1Action(
      const request: TBoldXMLRequest; out response: String);
  private
    FAdapter: TBoldDatabaseAdapterFireDAC;
    function DatabaseFile: string;
    procedure LogFailure(const Context: string; E: Exception);
    procedure SetUpPersistence;
  public
    { Public declarations }
  end;

var
  wmASPServer: TwmASPServer;

implementation

uses
  BoldId,
  BoldDefaultId,
  BoldDefs,
  BoldSystem,
  BuildingClasses,
  BoldUtils;

{$R *.DFM}

procedure TwmASPServer.wmASPServerPersistenceAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  reply: WideString;
begin
  try
    BoldHTTPServerPersistenceHandlePassthrough1.Get(Request.Content, reply);
  except
    on E: Exception do
    begin
      LogFailure('PersistenceAction', E);
      raise;
    end;
  end;
  Response.Content := Reply;
  Handled := true;
end;

procedure TwmASPServer.wmASPServerSOAPCallsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  XMLRequest: TBoldXMLRequest;
  reply: String;
begin
  try
    XMLRequest := TBoldXMLRequest.CreateFromXML(Request.Content);
    BoldXMLDispatcher1.DispatchAction(XMLRequest, reply);
  except
    on E: Exception do
    begin
      LogFailure('SOAPCallsAction', E);
      raise;
    end;
  end;
  Response.Content := reply;
  Handled := true;
end;

const
  cSectionSQLite = 'SQLite';
  cKeyDatabase = 'Database';
  cDefaultDatabase = 'ASPDemo.db';
  cDriverSQLite = 'SQLite';

{ The directory of this DLL, which is not the directory of the process hosting
  it. Inside an ISAPI extension Application.ExeName is the IIS worker process,
  so the database has to be found through the module handle instead.

  GetModuleFileNameAsString(True) returns the module path INCLUDING the file
  name despite what the parameter name suggests, hence ExtractFilePath. The
  IBX version of this unit omitted that and built database names like
  ...\ASPserver.dllASPDemo.gdb. }
function ModuleDirectory: string;
begin
  Result := ExtractFilePath(GetModuleFileNameAsString(True));
end;

{ Matches DBGenerator: the name comes from an ini beside the module and a bare
  name is taken as relative to it, so the generator and the server agree on
  where the database lives. }
function TwmASPServer.DatabaseFile: string;
var
  Ini: TIniFile;
  FileName: string;
begin
  Ini := TIniFile.Create(ModuleDirectory + 'ASPserver.ini');
  try
    FileName := Ini.ReadString(cSectionSQLite, cKeyDatabase, cDefaultDatabase);
  finally
    Ini.Free;
  end;

  if ExtractFilePath(FileName) = '' then
    FileName := ModuleDirectory + FileName;
  Result := FileName;
end;

{ Diagnostic log written beside the DLL.

  An exception raised here cannot be read from the HTTP response: the ISAPI
  response path puts the WebBroker exception page on the wire as UTF-16 and
  truncates it, so the status line arrives as 'HTTP/1.1 5' and the message is
  cut off partway through. Writing the failure to a file before it ever reaches
  HTTP is the only reliable way to see what went wrong.

  EBoldDatabaseError keeps the exception it wrapped in OriginalExceptionClass
  and OriginalExceptionMessage. Its own Message is only a formatted summary
  ('Unknown Error: ...'), so the original is logged on its own line. }
procedure TwmASPServer.LogFailure(const Context: string; E: Exception);
var
  Log: TextFile;
  FileName: string;
  Stamp: string;
begin
  try
    FileName := ModuleDirectory + 'ASPserver.log';
    Stamp := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    AssignFile(Log, FileName);
    if FileExists(FileName) then
      Append(Log)
    else
      Rewrite(Log);
    try
      WriteLn(Log, Format('%s  %s  %s: %s',
        [Stamp, Context, E.ClassName, E.Message]));
      if E is EBoldDatabaseError then
        WriteLn(Log, Format('%s  %s  wrapped %s: %s',
          [Stamp, Context,
           EBoldDatabaseError(E).OriginalExceptionClass,
           EBoldDatabaseError(E).OriginalExceptionMessage]));
    finally
      CloseFile(Log);
    end;
  except
    { A failure to log must not replace the failure being logged. }
  end;
end;

procedure TwmASPServer.SetUpPersistence;
var
  Connection: TFDConnection;
begin
  Connection := TFDConnection.Create(Self);
  Connection.LoginPrompt := False;
  Connection.Params.Clear;
  Connection.Params.Add('DriverID=' + cDriverSQLite);
  Connection.Params.Add(cKeyDatabase + '=' + DatabaseFile);

  FAdapter := TBoldDatabaseAdapterFireDAC.Create(Self);
  FAdapter.Connection := Connection;
  FAdapter.DatabaseEngine := dbeGenericANSISQL92;
  BoldPersistenceHandleDB1.DatabaseAdapter := FAdapter;
end;

procedure TwmASPServer.WebModuleCreate(Sender: TObject);
begin
  try
    { The adapter is built in code, so it has to exist before anything activates. }
    SetUpPersistence;
    BoldSystemHandle1.Active := true;
    BoldPersistenceHandleSystem1.Active := true;
  except
    on E: Exception do
    begin
      LogFailure('WebModuleCreate', E);
      raise;
    end;
  end;
end;

procedure TwmASPServer.BoldXMLDispatcher1Actions0Action(
  const request: TBoldXMLRequest; out response: String);
begin
  try
    BoldSystemHandle1.UpdateDatabase;
    response := 'OK';
  except
    on E: Exception do
      response := E.Message;
  end;
end;

procedure TwmASPServer.BoldXMLDispatcher1Actions1Action(
  const request: TBoldXMLRequest; out response: String);
var
  anObjectId: TBoldDefaultId;
  aLocator: TBoldObjectLocator;
begin
  anObjectId := TBoldDefaultID.CreateWithClassID(0, false);
  anObjectId.AsInteger := StrToInt(request.Params.Values['Building']);
  aLocator := BoldPersistenceHandleSystem1.PersistenceControllerSystem.LocatorById[anObjectId];
  anObjectId.Free;

  if assigned(aLocator) then
  begin
    (aLocator.EnsuredBoldObject as TResidential_Building).ChargeRent;
    response := 'Rent charged';
  end
  else
    response := 'Building not found on server';
end;

end.
