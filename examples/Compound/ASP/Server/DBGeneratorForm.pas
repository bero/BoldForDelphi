unit DBGeneratorForm;

{ Creates the database and the schema for the ASP example.

  Originally this used IBX against an InterBase server. IBX now lives in
  Source\Deprecated and no current Bold package references it, so the database
  layer is FireDAC, and SQLite by default, which means the demo needs no server
  installed to do anything.

  UniDAC is supported as an alternative through the UNIDAC conditional define,
  which the DebugUniDAC build configuration in DBGenerator.dproj sets. That
  configuration needs the UniDAC environment variable pointing at a UniDAC
  installation; the default Debug and Release builds never need UniDAC present.

  The connection is built in code rather than dropped on the form, because the
  database name comes from DBGenerator.ini. }

interface

uses
  SysUtils,
  Classes,
  IniFiles,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  UITypes,

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
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,

  {$IFDEF UNIDAC}
  Uni,
  SQLiteUniProvider,
  BoldDatabaseAdapterUniDAC,
  {$ENDIF}

  BoldSubscription,
  BoldHandle,
  BoldPersistenceHandle,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB,
  BoldPersistenceHandleDB,
  BoldDatabaseAdapterFireDAC,
  BoldSQLDatabaseConfig;

type
  TfrmDBGen = class(TForm)
    Button1: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { The concrete adapter differs between builds, so everything below the
      creation step works through the abstract one. }
    FAdapter: TBoldAbstractDatabaseAdapter;
    function DatabaseFile: string;
    procedure SetUpPersistence;
  end;

var
  frmDBGen: TfrmDBGen;

implementation

uses ModelDM;

{$R *.dfm}

const
  cSectionSQLite = 'SQLite';
  cKeyDatabase = 'Database';
  cDefaultDatabase = 'ASPDemo.db';
  cDriverSQLite = 'SQLite';
  cProviderSQLite = 'SQLite';
  cOptForceCreateDatabase = 'ForceCreateDatabase';

resourcestring
  sAlreadyExists =
    'The database already exists:' + sLineBreak + sLineBreak + '%s' +
    sLineBreak + sLineBreak +
    'Creating it again deletes everything in it. Continue?';
  sCreated =
    'Database and schema created:' + sLineBreak + sLineBreak + '%s';
  sCannotDelete =
    'Could not delete the existing database. Close anything using it and try ' +
    'again:' + sLineBreak + sLineBreak + '%s';

{ Where the database lives. A bare name in the ini is taken as relative to the
  executable, so the demo does not depend on the working directory. }
function TfrmDBGen.DatabaseFile: string;
var
  Ini: TIniFile;
  FileName: string;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    FileName := Ini.ReadString(cSectionSQLite, cKeyDatabase, cDefaultDatabase);
  finally
    Ini.Free;
  end;

  if ExtractFilePath(FileName) = '' then
    FileName := ExtractFilePath(Application.ExeName) + FileName;
  Result := FileName;
end;

procedure TfrmDBGen.SetUpPersistence;
{$IFDEF UNIDAC}
var
  Connection: TUniConnection;
  Adapter: TBoldDatabaseAdapterUniDAC;
begin
  Connection := TUniConnection.Create(Self);
  Connection.LoginPrompt := False;
  Connection.ProviderName := cProviderSQLite;
  Connection.Database := DatabaseFile;
  { UniDAC's SQLite provider opens READWRITE only and will not create a missing
    file, so without this the first run fails with "unable to open database
    file" (SQLite error 14). The option adds SQLITE_OPEN_CREATE. FireDAC creates
    the file when it connects, which is why the branch below needs no
    equivalent. }
  Connection.SpecificOptions.Values[cOptForceCreateDatabase] := 'True';

  Adapter := TBoldDatabaseAdapterUniDAC.Create(Self);
  Adapter.Connection := Connection;
{$ELSE}
var
  Connection: TFDConnection;
  Adapter: TBoldDatabaseAdapterFireDAC;
begin
  Connection := TFDConnection.Create(Self);
  Connection.LoginPrompt := False;
  Connection.Params.Clear;
  Connection.Params.Add('DriverID=' + cDriverSQLite);
  Connection.Params.Add(cKeyDatabase + '=' + DatabaseFile);

  Adapter := TBoldDatabaseAdapterFireDAC.Create(Self);
  Adapter.Connection := Connection;
{$ENDIF}
  // SQLite speaks close enough to ANSI SQL-92 for Bold's generic mapping.
  Adapter.DatabaseEngine := dbeGenericANSISQL92;
  FAdapter := Adapter;
  BoldPersistenceHandleDB1.DatabaseAdapter := FAdapter;
end;

procedure TfrmDBGen.FormCreate(Sender: TObject);
begin
  SetUpPersistence;
end;

{ Creating a SQLite database means creating a file, so this deliberately does
  not go through the adapter.

  TBoldDatabaseAdapterFireDAC.CreateDatabase and DatabaseExists are written for
  server engines. Both clear the Database parameter, connect to the server, and
  run a statement built from an SQLDatabaseConfig template. There is no server
  here, and the generic ANSI SQL-92 configuration leaves DatabaseExistsTemplate
  empty, so either call raises

    Please set the template in the SQLDatabaseConfig for DatabaseExistsTemplate.

  For a file engine the file system answers both questions, and FireDAC creates
  the file itself when the schema is written. }
procedure TfrmDBGen.Button1Click(Sender: TObject);
var
  FileName: string;
begin
  FileName := DatabaseFile;

  if FileExists(FileName) then
  begin
    if MessageDlg(Format(sAlreadyExists, [FileName]),
                  mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;
    if not DeleteFile(FileName) then
      raise Exception.CreateFmt(sCannotDelete, [FileName]);
  end;

  BoldPersistenceHandleDB1.CreateDataBaseSchema;
  MessageDlg(Format(sCreated, [FileName]), mtInformation, [mbOK], 0);
end;

end.
