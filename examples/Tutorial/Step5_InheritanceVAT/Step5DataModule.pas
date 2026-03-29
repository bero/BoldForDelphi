{$INCLUDE bold.inc}
unit Step5DataModule;

{ ============================================================================
  Step5DataModule - Bold System Infrastructure (Extended Model)
  ============================================================================

  This data module is identical in structure to the Shared InvoiceDataModule
  used by Steps 1-4. The only difference is the UML model embedded in the
  DFM, which now includes:

    - Customer INHERITANCE: Company and PrivatePerson subclasses
    - VATRate lookup class with Name and Percentage
    - InvoiceItem -> VATRate association
    - VATAmount derived attribute on InvoiceItem

  WHY A SEPARATE DATA MODULE?
  ---------------------------
  Steps 1-4 share a single model (in Shared/InvoiceDataModule.dfm). Step 5
  uses an extended model with more classes and associations. Since the Bold
  model is embedded in the DFM, we need a separate data module with the
  extended model. The Pascal code is essentially identical.

  See InvoiceDataModule.pas in the Shared folder for detailed comments
  explaining each component and the persistence chain architecture.
  ============================================================================ }

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.IOUtils,
  System.UITypes,
  System.Actions,
  System.StrUtils,
  Data.DB,
  Vcl.ActnList,
  Vcl.Dialogs,
  Vcl.Forms,

  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,
  FireDAC.DApt,
  FireDAC.Phys.SQLite,

  BoldAbstractDatabaseAdapter,
  BoldAbstractModel,
  BoldAbstractPersistenceHandleDB,
  BoldActions,
  BoldDatabaseAdapterFireDAC,
  BoldDefs,
  BoldHandle,
  BoldHandleAction,
  BoldHandles,
  BoldModel,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSQLDatabaseConfig,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle;

type
  TStep5DataModule = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure BoldActivateSystemAction1Execute(Sender: TObject);
  private
    FPersistenceHandleDB: TBoldPersistenceHandleDB;
    FFDConnection: TFDConnection;
    FFireDACAdapter: TBoldDatabaseAdapterFireDAC;

    function GetIniFileName: string;
    procedure SetupPersistence;
    procedure EnsureDatabaseExists;
  public
    procedure OpenSystem;
    function DatabaseName: string;
  end;

var
  dmStep5: TStep5DataModule;

implementation

{$R *.dfm}

function TStep5DataModule.GetIniFileName: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

function TStep5DataModule.DatabaseName: string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniFileName);
  try
    Result := Ini.ReadString('SQLite', 'Database', ChangeFileExt(ExtractFileName(Application.ExeName), '.db'));
    if not TPath.IsPathRooted(Result) then
      Result := TPath.Combine(ExtractFilePath(Application.ExeName), Result);
  finally
    Ini.Free;
  end;
end;

procedure TStep5DataModule.SetupPersistence;
var
  DbFile: string;
begin
  DbFile := DatabaseName;

  FFDConnection := TFDConnection.Create(Self);
  FFDConnection.LoginPrompt := False;
  FFDConnection.Params.DriverID := 'SQLite';
  FFDConnection.Params.Database := DbFile;
  FFDConnection.Params.Add('CreateDatabase=True');

  FFireDACAdapter := TBoldDatabaseAdapterFireDAC.Create(Self);
  FFireDACAdapter.DatabaseEngine := dbeInterbaseSQLDialect3;
  FFireDACAdapter.Connection := FFDConnection;

  FPersistenceHandleDB := TBoldPersistenceHandleDB.Create(Self);
  FPersistenceHandleDB.BoldModel := BoldModel1;
  FPersistenceHandleDB.DatabaseAdapter := FFireDACAdapter;

  BoldSystemHandle1.PersistenceHandle := FPersistenceHandleDB;
end;

procedure TStep5DataModule.EnsureDatabaseExists;
begin
  try
    FFDConnection.Open;
    if not FPersistenceHandleDB.DatabaseInterface.TableExists('BOLD_TYPE') then
      FPersistenceHandleDB.CreateDataBaseSchema(True);
    FFDConnection.Close;
  except
    on E: Exception do
      MessageDlg('Database error: ' + E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TStep5DataModule.DataModuleCreate(Sender: TObject);
begin
  SetupPersistence;
end;

procedure TStep5DataModule.DataModuleDestroy(Sender: TObject);
begin
  if BoldSystemHandle1.Active then
  begin
    if BoldSystemHandle1.System.BoldDirty then
      BoldSystemHandle1.System.Discard;
    BoldSystemHandle1.Active := False;
  end;
  BoldSystemHandle1.PersistenceHandle := nil;
  FreeAndNil(FPersistenceHandleDB);
  FreeAndNil(FFireDACAdapter);
  if FFDConnection.Connected then
    FFDConnection.Close;
  FreeAndNil(FFDConnection);
end;

procedure TStep5DataModule.BoldActivateSystemAction1Execute(Sender: TObject);
begin
  if not BoldSystemHandle1.Active then
  begin
    EnsureDatabaseExists;
    BoldSystemHandle1.Active := True;
  end
  else
  begin
    if BoldSystemHandle1.System.BoldDirty then
    begin
      case MessageDlg('Save changes before closing?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: BoldSystemHandle1.System.UpdateDatabase;
        mrNo: BoldSystemHandle1.System.Discard;
        mrCancel: Exit;
      end;
    end;
    BoldSystemHandle1.Active := False;
  end;
end;

procedure TStep5DataModule.OpenSystem;
begin
  BoldActivateSystemAction1.Execute;
end;

end.
