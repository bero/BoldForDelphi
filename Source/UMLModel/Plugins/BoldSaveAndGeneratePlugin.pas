{$include bold.inc}
unit BoldSaveAndGeneratePlugin;

interface

uses
  Graphics,
  BoldUMLModelEditPlugIn,
  BoldUMLPlugins;

type
  { TUMLSaveAndGenerateAll }
  TUMLSaveAndGenerateAll = class(TUMLPlugInFunction)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageMaskColor: TColor; override;
    function GetOptions: TBoldUMLPluginOptions; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

implementation

uses
  SysUtils,
  Classes,
  Controls,
  Dialogs,
  System.UITypes,
  StrUtils,
  BoldDefs,
  BoldGuard,
  BoldLogHandler,
  BoldModel,
  BoldMeta,
  BoldGen,
  BoldTypeNameDictionary,
  BoldUMLAttributes,
  BoldUMLModel,
  BoldUMLModelLink,
  BoldUMLBldLink,
  BoldUMLModelValidator,
  BoldUMLAbstractModelValidator,
  BoldDbEvolutorForm,
  BoldAbstractPersistenceHandleDB,
  BoldHandle,
  {$IFNDEF NO_OTA}
  BoldOTASupport,
  {$ENDIF}
  BoldDefaultTaggedValues;

var
  _SaveAndGenerateAll: TUMLSaveAndGenerateAll;

{ Helper function to update DFM file with model from BLD file }
function UpdateDfmFromBld(const aBldFileName, aDfmFileName: String): Boolean;
var
  vBldFile, vDfmFile: TextFile;
  vStrList: TStringList;
  vStr: String;
  vIndex, vModelStart, vIndents, I: Integer;
  vKeepLine: Boolean;
begin
  Result := False;

  if not FileExists(aBldFileName) then
  begin
    BoldLog.Log('BLD file not found: ' + aBldFileName, ltError);
    Exit;
  end;

  if not FileExists(aDfmFileName) then
  begin
    BoldLog.Log('DFM file not found: ' + aDfmFileName, ltError);
    Exit;
  end;

  AssignFile(vBldFile, aBldFileName);
  AssignFile(vDfmFile, aDfmFileName);
  Reset(vBldFile);
  Reset(vDfmFile);
  vStrList := TStringList.Create;
  vIndex := -1;
  vModelStart := -1;
  try
    vKeepLine := True;
    // Put the DFM file's lines, except the model definition, into vStrList
    while not EOF(vDfmFile) do
    begin
      Readln(vDfmFile, vStr);
      Inc(vIndex);
      if vKeepLine then
      begin
        vStrList.Add(vStr);
        // Model def starts
        if vStr = '    Model = (' then
        begin
          vKeepLine := False;
          vModelStart := vIndex;
        end;
      end
      else if vStr = '      '')'')' then
        vKeepLine := True;
    end;

    // Put the model definition from the BLD file into vStrList
    if vModelStart > -1 then
    begin
      vIndex := vModelStart;
      while not EOF(vBldFile) do
      begin
        Readln(vBldFile, vStr);

        // Make the Bld lines into DFM compatible ones
        vStr := AnsiReplaceStr(vStr, '''', '''#39''');
        vIndents := 0;
        while AnsiStartsStr(#9, vStr) do
        begin
          Inc(vIndents);
          vStr := AnsiRightStr(vStr, Length(vStr) - 1);
        end;
        vStr := '''' + vStr + '''';
        for I := 1 to vIndents do
          vStr := '#9' + vStr;
        vStr := '      ' + vStr;
        // Insert into vStrList at current index & increment
        vStrList.Insert(vIndex + 1, vStr);
        Inc(vIndex);
      end;
      // Closing parenthesis
      vStrList.Strings[vIndex] := vStrList.Strings[vIndex] + ')';
    end;
  finally
    CloseFile(vBldFile);
    CloseFile(vDfmFile);
    // Save the Dfm to file
    if vModelStart > -1 then
    begin
      vStrList.SaveToFile(aDfmFileName);
      BoldLog.Log('Updated DFM: ' + aDfmFileName);
      Result := True;
    end
    else
      BoldLog.Log('No model definition found in DFM file', ltWarning);
    vStrList.Free;
  end;
end;

{ Helper function to find persistence handle for a model }
function GetPersistenceHandle(BoldModel: TBoldModel): TBoldAbstractPersistenceHandleDB;
var
  i: integer;
  temp: TBoldAbstractPersistenceHandleDB;
begin
  Result := nil;
  for i := 0 to BoldHandleList.Count - 1 do
    if (BoldHandleList[i] is TBoldAbstractPersistenceHandleDB) then
    begin
      temp := BoldHandleList[i] as TBoldAbstractPersistenceHandleDB;
      if (temp.BoldModel = BoldModel) then
      begin
        Result := Temp;
        Break;
      end;
    end;
end;

{ TUMLSaveAndGenerateAll }

function TUMLSaveAndGenerateAll.GetMenuItemName: String;
begin
  Result := 'Save and Generate All';
end;

function TUMLSaveAndGenerateAll.GetImageResourceName: String;
begin
  Result := 'UMLPluginGenCodeImage';  // Reuse existing code generation image
end;

function TUMLSaveAndGenerateAll.GetPlugInType: TPlugInType;
begin
  Result := ptFile;
end;

function TUMLSaveAndGenerateAll.GetImageMaskColor: TColor;
begin
  Result := clTeal;
end;

function TUMLSaveAndGenerateAll.GetOptions: TBoldUMLPluginOptions;
begin
  Result := [poRequireBoldified];
end;

procedure TUMLSaveAndGenerateAll.Execute(context: IUMLModelPlugInContext);
var
  BoldModel: TBoldModel;
  UMLLink: TBoldUMLModelLink;
  BldLink: TBoldUMLBldLink;
  Validator: TBoldUMLModelValidator;
  Generator: TBoldGenerator;
  PHandle: TBoldAbstractPersistenceHandleDB;
  BoldGuard: IBoldGuard;
  BldFilePath: string;
  DfmFilePath: string;
  CodePath: string;
  SaveDialog: TSaveDialog;
  Res: Word;
begin
  BoldGuard := TBoldGuard.Create(Validator, Generator, BldLink, SaveDialog);

  BoldModel := Context.GetCurrentModelHandle;
  if not Assigned(BoldModel) then
    raise EBold.Create('No model component found');

  // 1. Validate model first
  BoldLog.StartLog('Save and Generate All');
  BoldLog.Log('Validating model...');

  Validator := TBoldUMLModelValidator.Create(BoldModel, nil, BoldDefaultValidatorSourceLanguage);
  Validator.Validate(BoldModel.TypeNameDictionary);

  if BoldModel.EnsuredUMLModel.Validator.HighestSeverity = sError then
  begin
    BoldLog.Log('Model has errors - cannot proceed', ltError);
    BoldLog.EndLog;
    raise EBold.Create('Model has errors. Please fix them before generating.');
  end;

  // 2. Determine BLD file path
  UMLLink := BoldGetUMLModelLinkForComponent(BoldModel);
  if Assigned(UMLLink) and (UMLLink is TBoldUMLBldLink) then
    BldFilePath := TBoldUMLBldLink(UMLLink).FileName
  else
    BldFilePath := '';

  // If no existing BLD file, ask user
  if (BldFilePath = '') or not FileExists(BldFilePath) then
  begin
    SaveDialog := TSaveDialog.Create(nil);
    SaveDialog.Title := 'Save Model As BLD File';
    SaveDialog.Filter := 'Bold Model Files (*.bld)|*.bld';
    SaveDialog.DefaultExt := 'bld';
    if Assigned(UMLLink) and (UMLLink.FileName <> '') then
      SaveDialog.FileName := ChangeFileExt(UMLLink.FileName, '.bld');

    if not SaveDialog.Execute then
    begin
      BoldLog.Log('Operation cancelled by user');
      BoldLog.EndLog;
      Exit;
    end;
    BldFilePath := SaveDialog.FileName;
  end;

  // 3. Save model to BLD file
  BoldLog.Log('Saving model to: ' + BldFilePath);
  BldLink := TBoldUMLBldLink.Create(nil);
  BldLink.FileName := BldFilePath;
  BldLink.ExportModel(BoldModel.EnsuredUMLModel);
  BoldLog.Log('Model saved successfully');

  // 4. Generate code
  BoldLog.Log('Generating code...');
  {$IFDEF NO_OTA}
  CodePath := ExtractFilePath(BldFilePath);
  {$ELSE}
  CodePath := BoldFilePathForComponent(BoldModel);
  if CodePath = '' then
    CodePath := ExtractFilePath(BldFilePath);
  {$ENDIF}

  Generator := TBoldGenerator.Create(BoldModel.TypeNameDictionary);
  Generator.BaseFilePath := CodePath;
  Generator.UseTypedLists := True;
  Generator.MoldModel := BoldModel.MoldModel;
  Generator.GenerateBusinessObjectCode;
  Generator.EnsureMethodImplementations;
  BoldLog.Log('Code generated in: ' + CodePath);

  // 5. Update DFM file if it exists
  // Try to find DFM file with same name as BLD file
  DfmFilePath := ChangeFileExt(BldFilePath, '.dfm');
  if not FileExists(DfmFilePath) then
  begin
    // Try finding DFM in parent directory (common pattern: model/Model.bld -> units/DMModel.dfm)
    DfmFilePath := '';
    if Assigned(UMLLink) and (UMLLink.FileName <> '') then
      DfmFilePath := ChangeFileExt(UMLLink.FileName, '.dfm');
  end;

  if FileExists(DfmFilePath) then
  begin
    BoldLog.Log('Updating DFM file: ' + DfmFilePath);
    if UpdateDfmFromBld(BldFilePath, DfmFilePath) then
      BoldLog.Log('DFM updated successfully')
    else
      BoldLog.Log('Failed to update DFM file', ltWarning);
  end
  else
    BoldLog.Log('No DFM file found to update (this is OK for external model files)');

  // 6. Offer database evolution if persistence handle exists
  PHandle := GetPersistenceHandle(BoldModel);
  if Assigned(PHandle) and Assigned(PHandle.SQLDataBaseConfig) then
  begin
    Res := MessageDlg('Do you want to check for database schema changes?',
      mtConfirmation, [mbYes, mbNo], 0);
    if Res = mrYes then
    begin
      BoldLog.Log('Opening database evolution dialog...');
      // Ask about generic script
      Res := MessageDlg('Generate generic script?' + sLineBreak +
        '(Generic scripts can be used on other databases with the same schema)',
        mtConfirmation, [mbYes, mbNo], 0);
      TfrmBoldDbEvolutor.EvolveDB(PHandle, Res = mrYes);
    end;
  end;

  BoldLog.Log('Save and Generate All completed successfully');
  BoldLog.EndLog;

  MessageDlg('Save and Generate All completed successfully!' + sLineBreak + sLineBreak +
    'Model saved to: ' + BldFilePath + sLineBreak +
    'Code generated in: ' + CodePath,
    mtInformation, [mbOK], 0);
end;

initialization
  _SaveAndGenerateAll := TUMLSaveAndGenerateAll.Create(True);

finalization
  FreeAndNil(_SaveAndGenerateAll);

end.
