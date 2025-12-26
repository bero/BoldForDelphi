{$INCLUDE bold.inc}
unit TestMainForm;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  BoldModel,
  BoldModelChangeTracker,
  BoldUMLModelEdit;

type
  TfrmTestMain = class(TForm)
    pnlTop: TPanel;
    btnSaveAndGenerateAll: TButton;
    btnModelEditor: TButton;
    memoLog: TMemo;
    pnlButtons: TPanel;
    btnValidateModel: TButton;
    btnSaveModel: TButton;
    btnGenerateCode: TButton;
    btnUpdateDfm: TButton;
    btnGenerateSQLScript: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveAndGenerateAllClick(Sender: TObject);
    procedure btnValidateModelClick(Sender: TObject);
    procedure btnSaveModelClick(Sender: TObject);
    procedure btnGenerateCodeClick(Sender: TObject);
    procedure btnUpdateDfmClick(Sender: TObject);
    procedure btnGenerateSQLScriptClick(Sender: TObject);
    procedure btnModelEditorClick(Sender: TObject);
  private
    FModelChangeTracker: TBoldModelChangeTracker;
    procedure Log(const AMessage: string);
    procedure ExecuteSaveAndGenerateAll(ABoldModel: TBoldModel; const ABldFilePath: string);
    function ValidateModel(ABoldModel: TBoldModel): Boolean;
    procedure SaveModelToBld(ABoldModel: TBoldModel; const ABldFilePath: string);
    procedure GenerateCode(ABoldModel: TBoldModel; const ACodePath: string);
    function UpdateDfmFromBld(const ABldFilePath, ADfmFilePath: string): Boolean;
    procedure CheckAndEvolveDatabase;
  end;

var
  frmTestMain: TfrmTestMain;

implementation

uses
  System.UITypes,
  StrUtils,
  Vcl.Dialogs,
  BoldDefs,
  BoldGuard,
  BoldMeta,
  BoldGen,
  BoldGeneratorTemplates,
  BoldGeneratorTemplatesDelphi,
  BoldUMLAttributes,
  BoldUMLModel,
  BoldUMLBldLink,
  BoldUMLModelValidator,
  BoldUMLAbstractModelValidator,
  BoldDefaultTaggedValues,
  BoldDbEvolutorForm,
  DemoDataModule;

{$R *.dfm}

procedure TfrmTestMain.FormCreate(Sender: TObject);
begin
  memoLog.Clear;
  Log('Test application started');
  Log('Config file: ' + dmDemo.ConfigFile);
  Log('BoldModel: ' + dmDemo.BoldModel1.Name);

  // Create model change tracker and capture baseline state
  FModelChangeTracker := TBoldModelChangeTracker.Create;
  FModelChangeTracker.CaptureBeforeState(dmDemo.BoldModel1.MoldModel);
  Log('Captured baseline model state for change detection');
end;

procedure TfrmTestMain.FormDestroy(Sender: TObject);
begin
  FModelChangeTracker.Free;
end;

procedure TfrmTestMain.Log(const AMessage: string);
begin
  memoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' - ' + AMessage);
end;

procedure TfrmTestMain.btnValidateModelClick(Sender: TObject);
begin
  Log('--- Validating Model ---');
  if ValidateModel(dmDemo.BoldModel1) then
    Log('Model validation: OK')
  else
    Log('Model validation: ERRORS FOUND');
end;

procedure TfrmTestMain.btnSaveModelClick(Sender: TObject);
var
  BldPath: string;
begin
  Log('--- Saving Model ---');
  BldPath := TDemoDataModule.GetModelFilePath;
  SaveModelToBld(dmDemo.BoldModel1, BldPath);
end;

procedure TfrmTestMain.btnGenerateCodeClick(Sender: TObject);
begin
  Log('--- Generating Code ---');
  GenerateCode(dmDemo.BoldModel1, TDemoDataModule.GetSharedPath);
end;

procedure TfrmTestMain.btnUpdateDfmClick(Sender: TObject);
var
  BldPath, DfmPath: string;
begin
  Log('--- Updating DFM ---');
  BldPath := TDemoDataModule.GetModelFilePath;
  DfmPath := TDemoDataModule.GetSharedPath + 'DemoDataModule.dfm';
  if UpdateDfmFromBld(BldPath, DfmPath) then
    Log('DFM updated successfully')
  else
    Log('DFM update failed');
end;

procedure TfrmTestMain.btnSaveAndGenerateAllClick(Sender: TObject);
var
  BldPath: string;
begin
  Log('=== Save and Generate All ===');
  BldPath := TDemoDataModule.GetModelFilePath;
  ExecuteSaveAndGenerateAll(dmDemo.BoldModel1, BldPath);
  Log('=== Completed ===');
end;

function TfrmTestMain.ValidateModel(ABoldModel: TBoldModel): Boolean;
var
  Validator: TBoldUMLModelValidator;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Validator);
  Validator := TBoldUMLModelValidator.Create(ABoldModel, nil, BoldDefaultValidatorSourceLanguage);
  Validator.Validate(ABoldModel.TypeNameDictionary);

  Result := ABoldModel.EnsuredUMLModel.Validator.HighestSeverity <> sError;

  case ABoldModel.EnsuredUMLModel.Validator.HighestSeverity of
    sNone: Log('  Severity: None');
    sHint: Log('  Severity: Hint');
    sWarning: Log('  Severity: Warning');
    sError: Log('  Severity: Error');
  end;
end;

procedure TfrmTestMain.SaveModelToBld(ABoldModel: TBoldModel; const ABldFilePath: string);
var
  BldLink: TBoldUMLBldLink;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(BldLink);
  BldLink := TBoldUMLBldLink.Create(nil);
  BldLink.FileName := ABldFilePath;
  Log('  Saving to: ' + ABldFilePath);
  BldLink.ExportModel(ABoldModel.EnsuredUMLModel);
  Log('  Model saved successfully');
end;

procedure TfrmTestMain.GenerateCode(ABoldModel: TBoldModel; const ACodePath: string);
var
  Generator: TBoldGenerator;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Generator);
  Generator := TBoldGenerator.Create(ABoldModel.TypeNameDictionary);
  Generator.BaseFilePath := ACodePath;
  Generator.UseTypedLists := True;
  Generator.MoldModel := ABoldModel.MoldModel;
  Log('  Generating to: ' + ACodePath);
  Generator.GenerateBusinessObjectCode;
  Generator.EnsureMethodImplementations;
  Log('  Code generated successfully');
end;

function TfrmTestMain.UpdateDfmFromBld(const ABldFilePath, ADfmFilePath: string): Boolean;
var
  BldFile, DfmFile: TextFile;
  StrList: TStringList;
  Str: string;
  Index, ModelStart, Indents, I: Integer;
  KeepLine: Boolean;
begin
  Result := False;
  Log('  BLD file: ' + ABldFilePath);
  Log('  DFM file: ' + ADfmFilePath);

  if not FileExists(ABldFilePath) then
  begin
    Log('  ERROR: BLD file not found');
    Exit;
  end;

  if not FileExists(ADfmFilePath) then
  begin
    Log('  ERROR: DFM file not found');
    Exit;
  end;

  AssignFile(BldFile, ABldFilePath);
  AssignFile(DfmFile, ADfmFilePath);
  Reset(BldFile);
  Reset(DfmFile);
  StrList := TStringList.Create;
  Index := -1;
  ModelStart := -1;
  try
    KeepLine := True;
    // Put the DFM file's lines, except the model definition, into StrList
    while not EOF(DfmFile) do
    begin
      Readln(DfmFile, Str);
      Inc(Index);
      if KeepLine then
      begin
        StrList.Add(Str);
        // Model def starts
        if Str = '    Model = (' then
        begin
          KeepLine := False;
          ModelStart := Index;
          Log('  Found model start at line ' + IntToStr(Index));
        end;
      end
      else if Str = '      '')'')' then
        KeepLine := True;
    end;

    // Put the model definition from the BLD file into StrList
    if ModelStart > -1 then
    begin
      Index := ModelStart;
      while not EOF(BldFile) do
      begin
        Readln(BldFile, Str);

        // Make the Bld lines into DFM compatible ones
        Str := AnsiReplaceStr(Str, '''', '''#39''');
        Indents := 0;
        while AnsiStartsStr(#9, Str) do
        begin
          Inc(Indents);
          Str := AnsiRightStr(Str, Length(Str) - 1);
        end;
        Str := '''' + Str + '''';
        for I := 1 to Indents do
          Str := '#9' + Str;
        Str := '      ' + Str;
        // Insert into StrList at current index & increment
        StrList.Insert(Index + 1, Str);
        Inc(Index);
      end;
      // Closing parenthesis
      StrList.Strings[Index] := StrList.Strings[Index] + ')';
    end;
  finally
    CloseFile(BldFile);
    CloseFile(DfmFile);
    // Save the Dfm to file
    if ModelStart > -1 then
    begin
      StrList.SaveToFile(ADfmFilePath);
      Log('  DFM saved with ' + IntToStr(StrList.Count) + ' lines');
      Result := True;
    end
    else
      Log('  ERROR: No model definition found in DFM file');
    StrList.Free;
  end;
end;

procedure TfrmTestMain.ExecuteSaveAndGenerateAll(ABoldModel: TBoldModel; const ABldFilePath: string);
var
  CodePath, DfmPath: string;
begin
  // 1. Validate
  Log('Step 1: Validating model...');
  if not ValidateModel(ABoldModel) then
  begin
    Log('ERROR: Model has errors - stopping');
    Exit;
  end;

  // 2. Save to BLD
  Log('Step 2: Saving model to BLD...');
  SaveModelToBld(ABoldModel, ABldFilePath);

  // 3. Generate code
  Log('Step 3: Generating code...');
  CodePath := ExtractFilePath(ABldFilePath);
  GenerateCode(ABoldModel, CodePath);

  // 4. Update DFM
  Log('Step 4: Updating DFM...');
  DfmPath := ChangeFileExt(ABldFilePath, '.dfm');
  // Try DemoDataModule.dfm in same folder
  if not FileExists(DfmPath) then
    DfmPath := ExtractFilePath(ABldFilePath) + 'DemoDataModule.dfm';
  if FileExists(DfmPath) then
    UpdateDfmFromBld(ABldFilePath, DfmPath)
  else
    Log('  No DFM file found to update');

  // 5. Check for SQL schema changes (based on model changes, no DB connection yet)
  Log('Step 5: Checking for schema changes...');
  CheckAndEvolveDatabase;
end;

procedure TfrmTestMain.CheckAndEvolveDatabase;
var
  i: Integer;
begin
  // Check if using database persistence
  if dmDemo.PersistenceType = ptXML then
  begin
    Log('  Skipped - using XML persistence');
    Exit;
  end;

  if not Assigned(dmDemo.PersistenceHandleDB) then
  begin
    Log('  Skipped - no database persistence handle');
    Exit;
  end;

  // Capture current model state and compare with baseline
  FModelChangeTracker.CaptureAfterState(dmDemo.BoldModel1.MoldModel);

  if FModelChangeTracker.RequiresSqlEvolution then
  begin
    Log('  Schema-affecting model changes detected:');
    for i := 0 to FModelChangeTracker.SchemaChangeReasons.Count - 1 do
      Log('    - ' + FModelChangeTracker.SchemaChangeReasons[i]);

    // Check if database exists before trying to evolve
    try
      if not dmDemo.DatabaseExists then
      begin
        Log('  Database does not exist - create it first via MasterDetail app');
        Exit;
      end;
    except
      on E: Exception do
      begin
        Log('  Cannot connect to database: ' + E.Message);
        Exit;
      end;
    end;

    Log('  Opening DB Evolutor dialog...');
    TfrmBoldDbEvolutor.EvolveDB(dmDemo.PersistenceHandleDB, False);
    Log('  DB Evolutor completed');

    // Update baseline to current state after evolution
    FModelChangeTracker.CaptureBeforeState(dmDemo.BoldModel1.MoldModel);
  end
  else
    Log('  No schema changes needed');
end;

procedure TfrmTestMain.btnGenerateSQLScriptClick(Sender: TObject);
begin
  Log('--- Generate SQL Script ---');
  CheckAndEvolveDatabase;
end;

procedure TfrmTestMain.btnModelEditorClick(Sender: TObject);
var
  EditorForm: TForm;
begin
  Log('--- Opening Model Editor ---');
  EditorForm := UMLModelEditor.EnsureFormForBoldModel(dmDemo.BoldModel1);
  (EditorForm as IBoldModelEditForm).LoadedFrom := TDemoDataModule.GetModelFilePath;
  EditorForm.Show;
end;

end.
