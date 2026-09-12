{$INCLUDE bold.inc}
{-----------------------------------------------------------------------------
  OclWorkbenchEngines

  Answers one question: if I switch this demo to PostgreSQL right now, what
  would stop it working?

  Bold itself is engine-agnostic. Everything that can go wrong when you change
  engine is below Bold: a missing client library, an .ini section nobody filled
  in, a server that is not running, a database that was never created. So the
  checks here are deliberately not Bold checks. They walk the same ladder a
  connection attempt walks, and stop at the first rung that breaks, because
  every later rung would fail for the same reason and say less about why.

  The ladder:

    1. the .ini exists and has a section for this engine
    2. the keys that engine needs are filled in, and no password is still the
       <PW> placeholder the template ships with
    3. FireDAC has a driver linked for this engine
    4. the client library is there AND has the same bitness as this build,
       which is the trap that costs people an afternoon: a 64-bit libpq.dll
       sitting at exactly the path the .ini names, unloadable by a 32-bit demo
    5. the SERVER is reachable and takes those credentials, asked against an
       administrative database that always exists
    6. the database the demo wants opens
    7. the Bold schema is present in it

  Rungs 5 and 6 are deliberately separate. A single connection attempt cannot
  tell "the server refused you" from "the database is not there", and those two
  need completely different answers: one is a login to fix, the other is a
  database to create. Asking the server first settles which one you have, and
  turns a missing database from a failure into a warning the demo can act on by
  offering to create it.

  Rungs 5 to 7 touch the network, which is why the caller is told a check may
  take a few seconds, and why every connection carries a login timeout.
-----------------------------------------------------------------------------}
unit OclWorkbenchEngines;

interface

uses
  System.Classes;

type
  { The engines the shared .ini template already has a section for. }
  TEngineId = (egSQLite, egMSSQL, egPostgreSQL, egFirebird, egMariaDB,
    egOracle, egXML);

  TCheckStatus = (
    csPass,   // the rung holds
    csWarn,   // not an obstacle, but worth knowing
    csFail,   // this is what stops the engine working
    csSkip);  // does not apply to this engine

  TPreconditionCheck = record
    Name: string;
    Status: TCheckStatus;
    Detail: string;   // what was actually found
    Remedy: string;   // what to do about it, when it failed
  end;

  TPreconditionReport = record
    Engine: TEngineId;
    Checks: TArray<TPreconditionCheck>;
    function Failed: Boolean;
    function Warned: Boolean;
    function Summary: string;
    function AsText: string;
  end;

const
  cEngineCaptions: array[TEngineId] of string = (
    'SQLite', 'SQL Server', 'PostgreSQL', 'Firebird', 'MariaDB',
    'Oracle', 'XML file');

function EngineCaption(AEngine: TEngineId): string;

{ The two values that go into [Database] in the .ini. }
function EnginePersistence(AEngine: TEngineId): string;
function EngineDbType(AEngine: TEngineId): string;

{ Reads the .ini and reports which engine it currently selects. }
function EngineFromIni(const AConfigFile: string): TEngineId;

{ Walks the ladder. Never raises: a failure becomes a csFail entry. }
function CheckEngine(AEngine: TEngineId;
  const AConfigFile: string): TPreconditionReport;

implementation

uses
  System.SysUtils,
  System.IniFiles,
  System.StrUtils,
  System.IOUtils,
  Data.DB,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.Intf,
  FireDAC.Comp.Client,
  DemoDataModule;

const
  { The placeholder the shipped .ini uses for passwords the reader must supply. }
  cPasswordPlaceholder = '<PW>';

  { Seconds to wait for a server before giving up. Without this a switched-off
    host freezes the UI for the operating system's own TCP timeout. }
  cLoginTimeoutSeconds = 5;

{ TPreconditionReport }

function TPreconditionReport.Failed: Boolean;
var
  Check: TPreconditionCheck;
begin
  for Check in Checks do
    if Check.Status = csFail then
      Exit(True);
  Result := False;
end;

function TPreconditionReport.Warned: Boolean;
var
  Check: TPreconditionCheck;
begin
  for Check in Checks do
    if Check.Status = csWarn then
      Exit(True);
  Result := False;
end;

function TPreconditionReport.Summary: string;
var
  Check: TPreconditionCheck;
begin
  for Check in Checks do
    if Check.Status = csFail then
      Exit(Check.Name + ': ' + Check.Detail);
  if Warned then
    Exit(EngineCaption(Engine) + ' is usable, with warnings');
  Result := EngineCaption(Engine) + ' is ready';
end;

function TPreconditionReport.AsText: string;

  function Mark(AStatus: TCheckStatus): string;
  begin
    case AStatus of
      csPass: Result := '  OK   ';
      csWarn: Result := '  note ';
      csFail: Result := '  FAIL ';
    else
      Result := '  --   ';
    end;
  end;

var
  Check: TPreconditionCheck;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('Preconditions for ' + EngineCaption(Engine));
    Lines.Add('');
    for Check in Checks do
    begin
      Lines.Add(Mark(Check.Status) + Check.Name);
      if Check.Detail <> '' then
        Lines.Add('         ' + Check.Detail);
      if (Check.Status = csFail) and (Check.Remedy <> '') then
        Lines.Add('         -> ' + Check.Remedy);
    end;
    Lines.Add('');
    Lines.Add(Summary);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

{ Engine identity }

function EngineCaption(AEngine: TEngineId): string;
begin
  Result := cEngineCaptions[AEngine];
end;

function EnginePersistence(AEngine: TEngineId): string;
begin
  if AEngine = egXML then
    Result := cPersistenceXML
  else
    Result := cPersistenceFireDAC;
end;

function EngineDbType(AEngine: TEngineId): string;
begin
  case AEngine of
    egSQLite:     Result := cDbTypeSQLite;
    egMSSQL:      Result := cDbTypeMSSQL;
    egPostgreSQL: Result := cDbTypePostgreSQL;
    egFirebird:   Result := cDbTypeFirebird;
    egMariaDB:    Result := cDbTypeMariaDB;
    egOracle:     Result := cDbTypeOracle;
  else
    Result := '';
  end;
end;

function EngineSection(AEngine: TEngineId): string;
begin
  case AEngine of
    egSQLite:     Result := cSectionSQLite;
    egMSSQL:      Result := cSectionMSSQL;
    egPostgreSQL: Result := cSectionPostgreSQL;
    egFirebird:   Result := cSectionFirebird;
    egMariaDB:    Result := cSectionMariaDB;
    egOracle:     Result := cSectionOracle;
  else
    Result := cSectionXML;
  end;
end;

{ The FireDAC DriverID, which is what GetDriverNames reports. }
function EngineDriverID(AEngine: TEngineId): string;
begin
  case AEngine of
    egSQLite:     Result := cDriverSQLite;
    egMSSQL:      Result := cDriverMSSQL;
    egPostgreSQL: Result := cDriverPG;
    egFirebird:   Result := cDriverFB;
    egMariaDB:    Result := cDriverMySQL;
    egOracle:     Result := cDriverOracle;
  else
    Result := '';
  end;
end;

{ A database that exists on every server of this kind, used to prove the host
  and the credentials without involving the database the demo actually wants.
  An empty result means the engine has no such thing, so the two questions
  cannot be separated and only the direct attempt is made. Oracle is the case
  in point: it organises by schema rather than by database, and connecting as a
  user who does not exist yet is itself the thing being tested. }
function AdminDatabase(AEngine: TEngineId): string;
begin
  case AEngine of
    egMSSQL:      Result := cAdminDbMaster;
    egPostgreSQL: Result := cAdminDbPostgres;
    egMariaDB:    Result := 'information_schema';
  else
    Result := '';
  end;
end;

function EngineFromIni(const AConfigFile: string): TEngineId;
var
  Ini: TIniFile;
  Persistence, DbType: string;
  Engine: TEngineId;
begin
  Result := egSQLite;
  if not FileExists(AConfigFile) then
    Exit;
  Ini := TIniFile.Create(AConfigFile);
  try
    Persistence := Ini.ReadString(cSectionDatabase, cKeyPersistence,
      cPersistenceFireDAC);
    if SameText(Persistence, cPersistenceXML) then
      Exit(egXML);
    DbType := Ini.ReadString(cSectionDatabase, cKeyType, cDbTypeSQLite);
    for Engine := Low(TEngineId) to High(TEngineId) do
      if SameText(EngineDbType(Engine), DbType) then
        Exit(Engine);
  finally
    Ini.Free;
  end;
end;

{ Building the report }

type
  TReportBuilder = record
    Report: TPreconditionReport;
    procedure Add(const AName: string; AStatus: TCheckStatus;
      const ADetail: string = ''; const ARemedy: string = '');
    function LastFailed: Boolean;
  end;

procedure TReportBuilder.Add(const AName: string; AStatus: TCheckStatus;
  const ADetail, ARemedy: string);
var
  Check: TPreconditionCheck;
begin
  Check.Name := AName;
  Check.Status := AStatus;
  Check.Detail := ADetail;
  Check.Remedy := ARemedy;
  Report.Checks := Report.Checks + [Check];
end;

function TReportBuilder.LastFailed: Boolean;
begin
  Result := (Length(Report.Checks) > 0) and
    (Report.Checks[High(Report.Checks)].Status = csFail);
end;

{ Which .ini keys each engine cannot do without. OSAuthent complicates SQL
  Server, so that one is handled in the check itself rather than here. }
function RequiredKeys(AEngine: TEngineId): TArray<string>;
begin
  case AEngine of
    egSQLite:     Result := [cKeyDatabase];
    egMSSQL:      Result := [cKeyServer, cKeyDatabase];
    egPostgreSQL: Result := [cKeyServer, cKeyDatabase, cKeyUser, cKeyPassword];
    egFirebird:   Result := [cKeyServer, cKeyDatabase, cKeyUser, cKeyPassword];
    egMariaDB:    Result := [cKeyServer, cKeyDatabase, cKeyUser, cKeyPassword];
    egOracle:     Result := [cKeyDatabase, cKeyUser, cKeyPassword];
  else
    Result := [];
  end;
end;

{ Rung 3 and 4. AValidate = True makes FireDAC try to load the driver's client
  library, which is exactly the difference between "Delphi knows about
  PostgreSQL" and "libpq.dll is actually on this machine". }
function DriverIsLinked(const ADriverID: string): Boolean;
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    FDManager.GetDriverNames(Names, False);
    Result := Names.IndexOf(ADriverID) >= 0;
  finally
    Names.Free;
  end;
end;

{ Reads the machine type out of a DLL's PE header and returns 32, 64, or 0 when
  the file cannot be read as a PE image.

  This rung exists because of one specific trap that costs people an afternoon.
  A 32-bit build cannot load a 64-bit client DLL, and the installer for a
  64-bit server puts a 64-bit DLL exactly where the .ini points. The file is
  there, the path is right, and nothing works. FireDAC does mention bitness in
  its hint, but only after the connection fails, and only as a suggestion. }
function LibraryBitness(const AFileName: string): Integer;
const
  cMachineI386 = $014C;
  cMachineAMD64 = $8664;
var
  Stream: TFileStream;
  PEOffset: Cardinal;
  Signature: Cardinal;
  Machine: Word;
begin
  Result := 0;
  if not FileExists(AFileName) then
    Exit;
  try
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      if Stream.Size < $40 then
        Exit;
      // e_lfanew, the offset of the PE header, lives at 0x3C in the DOS header
      Stream.Position := $3C;
      Stream.ReadBuffer(PEOffset, SizeOf(PEOffset));
      if PEOffset + 6 > Stream.Size then
        Exit;
      Stream.Position := PEOffset;
      Stream.ReadBuffer(Signature, SizeOf(Signature));
      if Signature <> $00004550 then   // 'PE'#0#0
        Exit;
      Stream.ReadBuffer(Machine, SizeOf(Machine));
      case Machine of
        cMachineI386:  Result := 32;
        cMachineAMD64: Result := 64;
      end;
    finally
      Stream.Free;
    end;
  except
    // An unreadable file is reported as unknown rather than as a failure; the
    // connection rung will say what really happened.
    on Exception do
      Result := 0;
  end;
end;

function ProcessBitness: Integer;
begin
  Result := SizeOf(Pointer) * 8;
end;

{ Rung 6. Builds a connection with the same parameters TDemoDataModule would
  build for this engine, opens it, and reports FireDAC's own message on failure.
  That message is almost always more precise than anything worth synthesising:
  it distinguishes a refused connection from a wrong password from a missing
  database. }
procedure ConfigureConnection(AConnection: TFDConnection; AEngine: TEngineId;
  AIni: TIniFile);
var
  Section, VendorLib: string;
  DefaultPort: Integer;
begin
  Section := EngineSection(AEngine);
  VendorLib := AIni.ReadString(Section, cKeyVendorLib, '');

  AConnection.Params.Clear;
  AConnection.Params.Add(cParamDriverID + '=' + EngineDriverID(AEngine));
  AConnection.LoginPrompt := False;

  case AEngine of
    egSQLite:
      begin
        AConnection.Params.Add(cParamDatabase + '=' +
          AIni.ReadString(Section, cKeyDatabase, ''));
        // A check must not change anything, and FireDAC's SQLite driver creates
        // the file on connect by default. An empty file left behind here is
        // worse than no file: TDemoDataModule would then see a database, skip
        // creating the schema, and Bold would fail on a missing BOLD_TYPE.
        AConnection.Params.Add('OpenMode=ReadOnly');
      end;

    egMSSQL:
      begin
        AConnection.Params.Add(cParamServer + '=' +
          AIni.ReadString(Section, cKeyServer, cDefaultServer));
        AConnection.Params.Add(cParamDatabase + '=' +
          AIni.ReadString(Section, cKeyDatabase, ''));
        // Same reader and same default as TDemoDataModule.ConfigureMSSQL. If
        // the check used different rules it would happily report credentials
        // the demo will never use.
        if IniReadFlag(AIni, Section, cKeyOSAuthent, True) then
          AConnection.Params.Add(cParamOSAuthent + '=' + cValueYes)
        else
        begin
          AConnection.Params.Add(cParamUserName + '=' +
            AIni.ReadString(Section, cKeyUser, ''));
          AConnection.Params.Add(cParamPassword + '=' +
            AIni.ReadString(Section, cKeyPassword, ''));
        end;
      end;

    egPostgreSQL, egMariaDB:
      begin
        if AEngine = egPostgreSQL then
          DefaultPort := 5432
        else
          DefaultPort := 3306;
        AConnection.Params.Add(cParamServer + '=' +
          AIni.ReadString(Section, cKeyServer, cDefaultServer));
        AConnection.Params.Add(cParamPort + '=' +
          IntToStr(AIni.ReadInteger(Section, cKeyPort, DefaultPort)));
        AConnection.Params.Add(cParamDatabase + '=' +
          AIni.ReadString(Section, cKeyDatabase, ''));
        AConnection.Params.Add(cParamUserName + '=' +
          AIni.ReadString(Section, cKeyUser, ''));
        AConnection.Params.Add(cParamPassword + '=' +
          AIni.ReadString(Section, cKeyPassword, ''));
      end;

    egFirebird, egOracle:
      begin
        if AEngine = egFirebird then
          AConnection.Params.Add(cParamServer + '=' +
            AIni.ReadString(Section, cKeyServer, cDefaultServer));
        AConnection.Params.Add(cParamDatabase + '=' +
          AIni.ReadString(Section, cKeyDatabase, ''));
        AConnection.Params.Add(cParamUserName + '=' +
          AIni.ReadString(Section, cKeyUser, ''));
        AConnection.Params.Add(cParamPassword + '=' +
          AIni.ReadString(Section, cKeyPassword, ''));
      end;
  end;

  if VendorLib <> '' then
    AConnection.Params.Add(cParamVendorLib + '=' + VendorLib);
  AConnection.Params.Add('LoginTimeout=' + IntToStr(cLoginTimeoutSeconds));
end;

function CheckEngine(AEngine: TEngineId;
  const AConfigFile: string): TPreconditionReport;
var
  Builder: TReportBuilder;
  Ini: TIniFile;
  Section, Key, Value, VendorLib, DriverID, FileName, Detail: string;
  LibBits: Integer;
  Connection: TFDConnection;
  Missing, Placeholders: string;
  SchemaFound, ServerReachable: Boolean;
begin
  Builder := Default(TReportBuilder);
  Builder.Report.Engine := AEngine;
  Section := EngineSection(AEngine);
  DriverID := EngineDriverID(AEngine);

  // ---- 1. is there a configuration file at all -------------------------
  if not FileExists(AConfigFile) then
  begin
    Builder.Add('Configuration file', csFail,
      'Not found: ' + AConfigFile,
      'The demo reads the .ini named after its executable. Copy the one from ' +
      'the project folder next to the .exe.');
    Exit(Builder.Report);
  end;
  Builder.Add('Configuration file', csPass, AConfigFile);

  Ini := TIniFile.Create(AConfigFile);
  try
    // ---- 2. the engine's own section -----------------------------------
    if not Ini.SectionExists(Section) then
    begin
      Builder.Add('Section [' + Section + ']', csFail, 'Missing from the .ini',
        'Add a [' + Section + '] section. The shipped OclWorkbench.ini has a ' +
        'template for every engine.');
      Exit(Builder.Report);
    end;
    Builder.Add('Section [' + Section + ']', csPass, 'Present');

    // ---- 3. the keys that engine needs ---------------------------------
    Missing := '';
    Placeholders := '';
    for Key in RequiredKeys(AEngine) do
    begin
      Value := Trim(Ini.ReadString(Section, Key, ''));
      if Value = '' then
        Missing := Missing + IfThen(Missing <> '', ', ') + Key
      else if Value = cPasswordPlaceholder then
        Placeholders := Placeholders + IfThen(Placeholders <> '', ', ') + Key;
    end;

    if Missing <> '' then
      Builder.Add('Required keys', csFail, 'Empty or absent: ' + Missing,
        'Fill these in under [' + Section + '] in ' + AConfigFile)
    else if Placeholders <> '' then
      Builder.Add('Required keys', csFail,
        'Still the shipped placeholder ' + cPasswordPlaceholder + ': ' + Placeholders,
        'Replace the placeholder with the real value under [' + Section + ']')
    else if Length(RequiredKeys(AEngine)) = 0 then
      Builder.Add('Required keys', csSkip, 'This engine needs none')
    else
      Builder.Add('Required keys', csPass, 'All present');

    if Builder.LastFailed then
      Exit(Builder.Report);

    // ---- 3b. SQL Server picks one of two authentication modes ----------
    if AEngine = egMSSQL then
    begin
      if IniReadFlag(Ini, Section, cKeyOSAuthent, True) then
      begin
        // Naming the account matters. The .ini still carries User and Password
        // lines, and a reader who does not know they are dead in this mode will
        // go and fix the password when the login that actually failed was the
        // Windows one.
        Detail := 'Windows authentication as ' +
          GetEnvironmentVariable('USERDOMAIN') + '\' +
          GetEnvironmentVariable('USERNAME');
        if Trim(Ini.ReadString(Section, cKeyUser, '')) <> '' then
          Detail := Detail + '. The User and Password keys are ignored while ' +
            'OSAuthent is True';
        Builder.Add('Authentication', csPass, Detail);
      end
      else if Trim(Ini.ReadString(Section, cKeyUser, '')) = '' then
        Builder.Add('Authentication', csFail,
          'OSAuthent is off but no User is set',
          'Set User and Password under [' + Section + '], or set ' +
          'OSAuthent=True to use Windows authentication.')
      else if Trim(Ini.ReadString(Section, cKeyPassword, '')) = '' then
        // Legal, so not a failure, but almost always a mistake.
        Builder.Add('Authentication', csWarn,
          'SQL login as ' + Ini.ReadString(Section, cKeyUser, '') +
          ' with an empty Password',
          'Set Password under [' + Section + '] unless that login really has ' +
          'a blank password.')
      else
        Builder.Add('Authentication', csPass,
          'SQL login as ' + Ini.ReadString(Section, cKeyUser, ''));

      if Builder.LastFailed then
        Exit(Builder.Report);
    end;

    // ---- XML takes a shorter ladder from here --------------------------
    if AEngine = egXML then
    begin
      FileName := Ini.ReadString(cSectionXML, cKeyFileName, '');
      if FileName = '' then
        Builder.Add('XML file', csWarn, 'No FileName set',
          'The demo will default to a file named after the executable.')
      else if FileExists(FileName) then
        Builder.Add('XML file', csPass, FileName + ' exists')
      else
        Builder.Add('XML file', csWarn, FileName + ' does not exist yet',
          'It is created on the first save.');
      Builder.Add('Evaluate in PS', csWarn,
        'Not available with XML persistence',
        'There is no SQL to translate to, so every expression is answered ' +
        'from memory.');
      Exit(Builder.Report);
    end;

    // ---- 4. is the FireDAC driver linked into this executable -----------
    if not DriverIsLinked(DriverID) then
    begin
      Builder.Add('FireDAC driver ' + DriverID, csFail, 'Not registered',
        'The driver unit is not linked in. TDemoDataModule uses all six ' +
        'FireDAC.Phys.* units, so this should not happen in this demo.');
      Exit(Builder.Report);
    end;
    Builder.Add('FireDAC driver ' + DriverID, csPass, 'Registered');

    // ---- 5. the client library named by VendorLib ----------------------
    VendorLib := Trim(Ini.ReadString(Section, cKeyVendorLib, ''));

    if AEngine = egSQLite then
      Builder.Add('Client library', csSkip,
        'SQLite is compiled into FireDAC, nothing to install')
    else if AEngine = egMSSQL then
      Builder.Add('Client library', csSkip,
        'SQL Server uses the ODBC driver installed on this machine')
    else if VendorLib = '' then
      Builder.Add('Client library', csWarn,
        'No VendorLib set, so FireDAC will search the system path',
        'If the connection below fails to load the library, name it ' +
        'explicitly with a VendorLib key under [' + Section + '].')
    else if not FileExists(VendorLib) then
      Builder.Add('Client library', csFail, 'Not found: ' + VendorLib,
        'Correct the VendorLib path under [' + Section + '], or install the ' +
        'client for ' + EngineCaption(AEngine) + '.')
    else
    begin
      // The file is there. The question that remains is whether this build can
      // load it at all.
      LibBits := LibraryBitness(VendorLib);
      if LibBits = 0 then
        Builder.Add('Client library', csWarn,
          VendorLib + ' found, but its bitness could not be read')
      else if LibBits <> ProcessBitness then
        Builder.Add('Client library', csFail,
          Format('%s is %d-bit, and this build is %d-bit',
            [VendorLib, LibBits, ProcessBitness]),
          Format('Point VendorLib at the %d-bit client DLL, or rebuild the ' +
            'demo for Win%d. A %d-bit process cannot load a %d-bit library, ' +
            'whatever the path says.',
            [ProcessBitness, LibBits, ProcessBitness, LibBits]))
      else
        Builder.Add('Client library', csPass,
          Format('%s, %d-bit, matches this build', [VendorLib, LibBits]));
    end;

    if Builder.LastFailed then
      Exit(Builder.Report);

    // ---- 5b. is the SERVER reachable, separately from the database -----
    //
    // Asked before the database itself, because the two failures need very
    // different answers and a single attempt cannot tell them apart. Every
    // server engine here has an administrative database that always exists, so
    // connecting to that one proves the host, the port and the credentials
    // without saying anything about the database the demo wants.
    ServerReachable := False;
    if AdminDatabase(AEngine) <> '' then
    begin
      Connection := TFDConnection.Create(nil);
      try
        try
          ConfigureConnection(Connection, AEngine, Ini);
          Connection.Params.Values[cParamDatabase] := AdminDatabase(AEngine);
          Connection.Connected := True;
          ServerReachable := True;
          Builder.Add('Server', csPass,
            'Reachable, and the credentials are accepted');
        except
          on E: Exception do
          begin
            if (AEngine = egMSSQL) and
               IniReadFlag(Ini, Section, cKeyOSAuthent, True) then
              Builder.Add('Server', csFail, E.Message,
                'This was a Windows login, not a SQL one: the User and ' +
                'Password keys under [' + Section + '] were not sent, so ' +
                'changing them will not help. Either give that Windows ' +
                'account a login on the server, or set OSAuthent=False and ' +
                'fill in User and Password.')
            else
              Builder.Add('Server', csFail, E.Message,
                'Check that the server is running and that the credentials ' +
                'under [' + Section + '] are right. FireDAC''s message above ' +
                'names the actual cause.');
            Exit(Builder.Report);
          end;
        end;
      finally
        Connection.Free;
      end;
    end;

    // ---- 6. does the database itself open ------------------------------
    //
    // For a file engine, answer from the file system rather than by connecting.
    // Connecting would be the thing that creates the file, and a check that
    // creates what it is checking for is no check at all.
    if AEngine in [egSQLite, egFirebird] then
    begin
      FileName := Trim(Ini.ReadString(Section, cKeyDatabase, ''));
      if (FileName <> '') and not FileExists(ExpandFileName(FileName)) then
      begin
        Builder.Add('Connection', csWarn,
          'No database file at ' + ExpandFileName(FileName),
          'Switch anyway and the demo offers to create it, schema and all.');
        Exit(Builder.Report);
      end;
    end;

    Connection := TFDConnection.Create(nil);
    try
      try
        ConfigureConnection(Connection, AEngine, Ini);
        Connection.Connected := True;
        Builder.Add('Connection', csPass,
          'Opened ' + Connection.Params.Values[cParamDatabase]);
      except
        on E: Exception do
        begin
          // A file-based engine with no file yet is the normal first run, not
          // a misconfiguration, so say so rather than crying failure.
          if (AEngine in [egSQLite, egFirebird]) and
             not FileExists(Ini.ReadString(Section, cKeyDatabase, '')) then
            Builder.Add('Connection', csWarn,
              'The database file does not exist yet',
              'Open the system and answer yes when the demo offers to create it.')
          else if ServerReachable then
            // The server took the same credentials a moment ago, so this is not
            // a login problem: the database is simply not there. A warning
            // rather than a failure, because the demo can create it.
            Builder.Add('Connection', csWarn,
              'The server is fine, but the database "' +
              Ini.ReadString(Section, cKeyDatabase, '') + '" does not exist',
              'Switch anyway and the demo offers to create it.')
          else if (AEngine = egMSSQL) and
                  IniReadFlag(Ini, Section, cKeyOSAuthent, True) then
            // The most misleading case in the whole ladder. The .ini shows a
            // User and a Password, so a refused login looks like a password
            // problem, when in fact neither key was sent.
            Builder.Add('Connection', csFail, E.Message,
              'This was a Windows login, not a SQL one: the User and Password ' +
              'keys under [' + Section + '] were not sent, so changing them ' +
              'will not help. Either give that Windows account a login on the ' +
              'server, or set OSAuthent=False and fill in User and Password.')
          else
            Builder.Add('Connection', csFail, E.Message,
              'Check that the server is running and that the credentials ' +
              'under [' + Section + '] are right. FireDAC''s message above ' +
              'names the actual cause.');
          Exit(Builder.Report);
        end;
      end;

      // ---- 7. is the Bold schema there ---------------------------------
      // No initial value: both the try and the handler assign it.
      try
        SchemaFound := Connection.ExecSQLScalar(
          'select count(*) from BOLD_TYPE') >= 0;
      except
        on Exception do
          SchemaFound := False;
      end;

      if SchemaFound then
        Builder.Add('Bold schema', csPass, 'BOLD_TYPE is present')
      else
        Builder.Add('Bold schema', csWarn, 'BOLD_TYPE not found',
          'The database is reachable but has no Bold schema yet. Open the ' +
          'system and the demo offers to create it.');
    finally
      Connection.Free;
    end;
  finally
    Ini.Free;
  end;

  Result := Builder.Report;
end;

end.
