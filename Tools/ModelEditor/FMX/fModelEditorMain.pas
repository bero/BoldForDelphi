unit fModelEditorMain;

{ The FMX client of the standalone model editor (#128), the FireMonkey twin
  of ../LCL/fModelEditorMain.pas: the classes of a model in one grid, the
  attributes of the selected class in another, and the findings of the
  consistency check in a third; a double click on a finding selects what it
  is about.

  What it shows and does is in the data module the two clients share
  (../Core/BoldModelEditorData.pas): the model document, the Bold handles,
  the two TBoldDataSets the grids show, the commands, and the question about
  unsaved changes. The form adds what is FireMonkey's: the grids (a
  TStringGrid bound to each dataset through LiveBindings, a TBindSourceDB
  and a TLinkGridToDataSource), an action per command with its menu item and
  button, and the dialogs. The bind sources get their datasets in
  FormCreate, as the data module is made there. The grids show and select;
  a class or an attribute is added and changed in its dialog
  (fModelEditorClass, fModelEditorAttribute), opened by Add, Edit or a
  double click on a cell (OnCellDblClick), and deleted by its command.

  The project defines BOLD_NO_VCL, so Bold's control packs link no VCL
  unit. Nothing drains Bold's display queue by itself under FMX: AppIdle
  does it from FMX's idle loop. }

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Rtti,
  System.Actions,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Grid,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  FMX.Menus,
  FMX.ActnList,
  FMX.Dialogs,
  Data.DB,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.Grid,
  Data.Bind.EngExt,
  Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid,
  Fmx.Bind.Editors,
  System.Bindings.Outputs,
  BoldUMLModel,
  BoldModelEditorData;

type
  TfrmModelEditorMain = class(TForm)
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mniNew: TMenuItem;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    mniSaveAs: TMenuItem;
    mniFileSep1: TMenuItem;
    mniGenerate: TMenuItem;
    mniTypeNames: TMenuItem;
    mniFileSep2: TMenuItem;
    mniExit: TMenuItem;
    mnuModel: TMenuItem;
    mniAddClass: TMenuItem;
    mniEditClass: TMenuItem;
    mniDeleteClass: TMenuItem;
    mniModelSep1: TMenuItem;
    mniAddAttribute: TMenuItem;
    mniEditAttribute: TMenuItem;
    mniDeleteAttribute: TMenuItem;
    mniModelSep2: TMenuItem;
    mniValidate: TMenuItem;
    alActions: TActionList;
    actNew: TAction;
    actOpen: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    actGenerate: TAction;
    actTypeNames: TAction;
    actExit: TAction;
    actAddClass: TAction;
    actEditClass: TAction;
    actDeleteClass: TAction;
    actAddAttribute: TAction;
    actEditAttribute: TAction;
    actDeleteAttribute: TAction;
    actValidate: TAction;
    layToolbar: TLayout;
    btnNew: TButton;
    btnOpen: TButton;
    btnSave: TButton;
    btnSaveAs: TButton;
    btnGenerate: TButton;
    layClasses: TLayout;
    layClassBar: TLayout;
    lblClasses: TLabel;
    btnAddClass: TButton;
    btnEditClass: TButton;
    btnDeleteClass: TButton;
    grdClasses: TStringGrid;
    splGrids: TSplitter;
    layAttributes: TLayout;
    layAttributeBar: TLayout;
    lblAttributes: TLabel;
    btnAddAttribute: TButton;
    btnEditAttribute: TButton;
    btnDeleteAttribute: TButton;
    grdAttributes: TStringGrid;
    splFindings: TSplitter;
    layFindings: TLayout;
    layFindingBar: TLayout;
    btnValidate: TButton;
    lblFindings: TLabel;
    grdFindings: TStringGrid;
    layStatus: TLayout;
    lblStatus: TLabel;
    bsClasses: TBindSourceDB;
    bsAttributes: TBindSourceDB;
    bsFindings: TBindSourceDB;
    BindingsList1: TBindingsList;
    lnkClasses: TLinkGridToDataSource;
    lnkAttributes: TLinkGridToDataSource;
    lnkFindings: TLinkGridToDataSource;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    dlgTypeNames: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actGenerateExecute(Sender: TObject);
    procedure actTypeNamesExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actAddClassExecute(Sender: TObject);
    procedure actEditClassExecute(Sender: TObject);
    procedure actDeleteClassExecute(Sender: TObject);
    procedure actAddAttributeExecute(Sender: TObject);
    procedure actEditAttributeExecute(Sender: TObject);
    procedure actDeleteAttributeExecute(Sender: TObject);
    procedure grdClassesCellDblClick(const Column: TColumn; const Row: Integer);
    procedure grdAttributesCellDblClick(const Column: TColumn; const Row: Integer);
    procedure actValidateExecute(Sender: TObject);
    procedure grdFindingsCellDblClick(const Column: TColumn; const Row: Integer);
    procedure alActionsUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FData: TdmModelEditor;
    FSelfTest: Boolean;
    FSelfTestException: Boolean;
    // The self-test's own exception in the event loop (#130)
    FExceptionExpected: Boolean;
    FExpectedExceptionSeen: Boolean;
    // Where the self-test saves and generates, instead of asking
    FSelfTestFolder: string;
    // What the self-test's dialogs answer: OK or Cancel, and a name and a
    // superclass that replace those in the dialog when they are not empty
    FSelfTestDialogOK: Boolean;
    FSelfTestName: string;
    FSelfTestSuperclass: string;
    FStatus: string;
    function EditClass(const ACaption: string; AClass: TUMLClass; var AValues: TModelEditorClassValues): Boolean;
    function EditAttribute(const ACaption: string; var AValues: TModelEditorAttributeValues): Boolean;
    function AskSave(const ADocumentName: string): TModelEditorSaveAnswer;
    function ChooseSaveFile(var AFileName: string): Boolean;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure ShowState;
    procedure Settle;
    procedure SelfTestException(Sender: TObject; E: Exception);
  public
    { Opens the model named on the command line, checks what the grids and
      the commands do, as the dpr's --selftest describes, and returns the
      number of failed checks, or 100 after an exception }
    function RunSelfTest: Integer;
  end;

var
  frmModelEditorMain: TfrmModelEditorMain;

{ True when the command line has --selftest, -selftest or /selftest }
function IsSelfTestRun: Boolean;

{ Starts ModelEditorFMX.selftest.txt next to the program; the self-test
  writes its steps there as it goes, so a run that stops shows where }
procedure StartSelfTestLog;
procedure SelfTestTrace(const S: string);

implementation

{$R *.fmx}

uses
  System.IOUtils,
  FMX.DialogService.Sync,
  BoldLogInterfaces,
  BoldLogSinks,
  BoldExceptionTrace,
  BoldExceptionLog,
  fModelEditorClass,
  fModelEditorAttribute;

resourcestring
  sSaveChanges = 'Save the changes of %s?';
  sAttributesOf = 'Attributes of %s';
  sAttributes = 'Attributes';
  sConsistencyCheck = 'Consistency check';
  sAddClass = 'Add class';
  sEditClass = 'Edit class %s';
  sAddAttribute = 'Add attribute to %s';
  sEditAttribute = 'Edit attribute %s';
  sGenerated = 'Business classes generated into %s';
  sClassCount = '%d classes';
  sTypeNamesFrom = 'type names: %s';
  sFormFilter = 'Forms (*.dfm;*.lfm;*.fmx)|*.dfm;*.lfm;*.fmx|All files|*';
  sGenerateInto = 'Generate the business classes into';

var
  SelfTestLog: string = '';

function IsSelfTestRun: Boolean;
begin
  // FindCmdLineSwitch strips one switch character, so --selftest needs its own check
  Result := FindCmdLineSwitch('selftest', ['-', '/'], True) or FindCmdLineSwitch('-selftest', ['-'], True);
end;

procedure StartSelfTestLog;
begin
  SelfTestLog := ChangeFileExt(ParamStr(0), '.selftest.txt');
  TFile.WriteAllText(SelfTestLog, 'ModelEditorFMX self-test' + sLineBreak);
end;

procedure SelfTestTrace(const S: string);
begin
  if SelfTestLog <> '' then
    TFile.AppendAllText(SelfTestLog, S + sLineBreak);
end;

{ TfrmModelEditorMain }

procedure TfrmModelEditorMain.FormCreate(Sender: TObject);
var
  ModelFile, TypeNamesFile: string;
begin
  FSelfTest := IsSelfTestRun;
  FSelfTestDialogOK := True;
  FData := TdmModelEditor.Create(Self);
  // Exceptions go to ModelEditorFMX.log next to the program with their call
  // stacks, then to the user: BoldExceptionLogFMX, named in the .dpr (#130)
  if FSelfTest then
  begin
    // No window to show an error in: the self-test log gets it, and the exit
    // code is 100. The exception log still logs it first, but not to its file
    Application.OnException := SelfTestException;
    BoldExceptionLogSink.Enabled := False;
  end;
  FData.OnAskSave := AskSave;
  FData.OnChooseSaveFile := ChooseSaveFile;
  // The grids are bound to the datasets of the data module through these
  bsClasses.DataSet := FData.bdsClasses;
  bsAttributes.DataSet := FData.bdsAttributes;
  bsFindings.DataSet := FData.bdsViolations;
  // The headings in bold, over the style's font
  lblClasses.StyledSettings := lblClasses.StyledSettings - [TStyledSetting.Style];
  lblClasses.TextSettings.Font.Style := [TFontStyle.fsBold];
  lblAttributes.StyledSettings := lblAttributes.StyledSettings - [TStyledSetting.Style];
  lblAttributes.TextSettings.Font.Style := [TFontStyle.fsBold];
  lblFindings.StyledSettings := lblFindings.StyledSettings - [TStyledSetting.Style];
  lblFindings.TextSettings.Font.Style := [TFontStyle.fsBold];
  dlgOpen.Filter := 'Bold models (*.bld)|*.bld|All files|*';
  dlgSave.Filter := dlgOpen.Filter;
  dlgTypeNames.Filter := sFormFilter;
  TypeNamesFile := TypeNamesFileFromCommandLine;
  if TypeNamesFile <> '' then
    FData.LoadTypeNames(TypeNamesFile);
  ModelFile := ModelFileFromCommandLine;
  if ModelFile <> '' then
    FData.Open(ModelFile);
  Application.OnIdle := AppIdle;
  ShowState;
  SelfTestTrace('form created');
end;

procedure TfrmModelEditorMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  // The bindings let go of the datasets before the data module frees them
  bsFindings.DataSet := nil;
  bsAttributes.DataSet := nil;
  bsClasses.DataSet := nil;
  FreeAndNil(FData);
end;

procedure TfrmModelEditorMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FSelfTest or FData.CloseModel;
end;

function TfrmModelEditorMain.AskSave(const ADocumentName: string): TModelEditorSaveAnswer;
begin
  case TDialogServiceSync.MessageDialog(Format(sSaveChanges, [ADocumentName]), TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbYes, 0) of
    mrYes: Result := saSave;
    mrNo: Result := saDiscard;
  else
    Result := saCancel;
  end;
end;

function TfrmModelEditorMain.ChooseSaveFile(var AFileName: string): Boolean;
begin
  if FSelfTest then
  begin
    AFileName := TPath.Combine(FSelfTestFolder, ExtractFileName(AFileName));
    Exit(True);
  end;
  if AFileName <> '' then
  begin
    dlgSave.InitialDir := ExtractFilePath(AFileName);
    dlgSave.FileName := ExtractFileName(AFileName);
  end;
  Result := dlgSave.Execute;
  if Result then
    AFileName := dlgSave.FileName;
end;

procedure TfrmModelEditorMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
  // Brings the Bold-aware followers, TBoldDataSet's among them, up to date
  FData.Refresh;
  ShowState;
  Done := True;
end;

procedure TfrmModelEditorMain.ShowState;
var
  AClass: TUMLClass;
begin
  if not Assigned(FData) then
    Exit;
  Caption := FData.Title;
  AClass := FData.CurrentClass;
  if Assigned(AClass) then
    lblAttributes.Text := Format(sAttributesOf, [AClass.name])
  else
    lblAttributes.Text := sAttributes;
  if FData.ValidationSummary <> '' then
    lblFindings.Text := FData.ValidationSummary
  else
    lblFindings.Text := sConsistencyCheck;
  if FStatus <> '' then
    lblStatus.Text := FStatus
  else if FData.bdsClasses.Active then
    lblStatus.Text := FData.Document.FileName + '  ' + Format(sClassCount, [FData.bdsClasses.RecordCount]) +
      '  ' + Format(sTypeNamesFrom, [FData.TypeNamesName]);
end;

procedure TfrmModelEditorMain.alActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  if not Assigned(FData) then
    Exit;
  actEditClass.Enabled := FData.CanEditClass;
  actDeleteClass.Enabled := FData.CanDeleteClass;
  actAddAttribute.Enabled := FData.CanAddAttribute;
  actEditAttribute.Enabled := FData.CanEditAttribute;
  actDeleteAttribute.Enabled := FData.CanDeleteAttribute;
end;

procedure TfrmModelEditorMain.actNewExecute(Sender: TObject);
begin
  if FData.CloseModel then
  begin
    FData.NewModel;
    FStatus := '';
  end;
end;

procedure TfrmModelEditorMain.actOpenExecute(Sender: TObject);
begin
  if not FData.CloseModel then
    Exit;
  if FData.Document.FileName <> '' then
    dlgOpen.InitialDir := ExtractFilePath(FData.Document.FileName);
  if dlgOpen.Execute then
  begin
    FData.Open(dlgOpen.FileName);
    FStatus := '';
  end;
end;

procedure TfrmModelEditorMain.actSaveExecute(Sender: TObject);
begin
  FData.SaveModel;
end;

procedure TfrmModelEditorMain.actSaveAsExecute(Sender: TObject);
begin
  FData.SaveModelAs;
end;

procedure TfrmModelEditorMain.actGenerateExecute(Sender: TObject);
var
  Folder: string;
begin
  if FSelfTest then
    Folder := FSelfTestFolder
  else
  begin
    Folder := FData.DefaultCodeFolder;
    if not SelectDirectory(sGenerateInto, '', Folder) then
      Exit;
  end;
  FData.GenerateCode(Folder);
  FStatus := Format(sGenerated, [Folder]);
end;

procedure TfrmModelEditorMain.actTypeNamesExecute(Sender: TObject);
begin
  if FData.Document.TypeNamesFile <> '' then
    dlgTypeNames.InitialDir := ExtractFilePath(FData.Document.TypeNamesFile)
  else
    dlgTypeNames.InitialDir := FData.DefaultCodeFolder;
  if dlgTypeNames.Execute then
  begin
    FData.LoadTypeNames(dlgTypeNames.FileName);
    FStatus := '';
  end;
end;

procedure TfrmModelEditorMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

function TfrmModelEditorMain.EditClass(const ACaption: string; AClass: TUMLClass; var AValues: TModelEditorClassValues): Boolean;
var
  SuperclassNames: TStringList;
begin
  if FSelfTest then
  begin
    if FSelfTestName <> '' then
      AValues.Name := FSelfTestName;
    if FSelfTestSuperclass <> '' then
      AValues.Superclass := FSelfTestSuperclass;
    Exit(FSelfTestDialogOK);
  end;
  SuperclassNames := TStringList.Create;
  try
    // AClass is nil for a new class
    FData.GetSuperclassNames(AClass, SuperclassNames);
    Result := EditClassValues(ACaption, SuperclassNames, AValues);
  finally
    SuperclassNames.Free;
  end;
end;

function TfrmModelEditorMain.EditAttribute(const ACaption: string; var AValues: TModelEditorAttributeValues): Boolean;
var
  TypeNames: TStringList;
begin
  if FSelfTest then
  begin
    if FSelfTestName <> '' then
      AValues.Name := FSelfTestName;
    Exit(FSelfTestDialogOK);
  end;
  TypeNames := TStringList.Create;
  try
    FData.GetTypeNames(TypeNames);
    Result := EditAttributeValues(ACaption, TypeNames, AValues);
  finally
    TypeNames.Free;
  end;
end;

procedure TfrmModelEditorMain.actAddClassExecute(Sender: TObject);
var
  Values: TModelEditorClassValues;
begin
  Values := FData.NewClassValues;
  if EditClass(sAddClass, nil, Values) then
    FData.AddClass(Values);
end;

procedure TfrmModelEditorMain.actEditClassExecute(Sender: TObject);
var
  Values: TModelEditorClassValues;
begin
  if not FData.CanEditClass then
    Exit;
  Values := FData.ClassValues(FData.CurrentClass);
  if EditClass(Format(sEditClass, [Values.Name]), FData.CurrentClass(), Values) then
    FData.ApplyClassValues(FData.CurrentClass, Values);
end;

procedure TfrmModelEditorMain.grdClassesCellDblClick(const Column: TColumn; const Row: Integer);
begin
  // The click before selected the row, and the binding moved the dataset
  actEditClass.Execute;
end;

procedure TfrmModelEditorMain.actDeleteClassExecute(Sender: TObject);
begin
  FData.DeleteClass;
end;

procedure TfrmModelEditorMain.actAddAttributeExecute(Sender: TObject);
var
  Values: TModelEditorAttributeValues;
begin
  Values := FData.NewAttributeValues;
  if EditAttribute(Format(sAddAttribute, [FData.CurrentClass().name]), Values) then
    FData.AddAttribute(Values);
end;

procedure TfrmModelEditorMain.actEditAttributeExecute(Sender: TObject);
var
  Values: TModelEditorAttributeValues;
begin
  if not FData.CanEditAttribute then
    Exit;
  Values := FData.AttributeValues(FData.CurrentAttribute);
  if EditAttribute(Format(sEditAttribute, [Values.Name]), Values) then
    FData.ApplyAttributeValues(FData.CurrentAttribute, Values);
end;

procedure TfrmModelEditorMain.grdAttributesCellDblClick(const Column: TColumn; const Row: Integer);
begin
  actEditAttribute.Execute;
end;

procedure TfrmModelEditorMain.actDeleteAttributeExecute(Sender: TObject);
begin
  FData.DeleteAttribute;
end;

procedure TfrmModelEditorMain.actValidateExecute(Sender: TObject);
begin
  FData.Validate;
end;

procedure TfrmModelEditorMain.grdFindingsCellDblClick(const Column: TColumn; const Row: Integer);
begin
  // FMX's TStringGrid publishes OnCellDblClick, not OnDblClick. The click
  // before selected the row, and the binding moved the dataset
  FData.SelectViolationElement;
end;

procedure TfrmModelEditorMain.SelfTestException(Sender: TObject; E: Exception);
begin
  if FExceptionExpected then
  begin
    FExpectedExceptionSeen := True;
    Exit;
  end;
  // No Halt: the dpr frees the form before Bold's units are finalized
  SelfTestTrace('EXCEPTION ' + BoldExceptionReport(E));
  FSelfTestException := True;
end;

{ Raises for the self-test's check of the call stack }
procedure RaiseForTheSelfTest;
begin
  raise Exception.Create('raised for the self-test');
end;

procedure TfrmModelEditorMain.Settle;
var
  Start: Cardinal;
  i: Integer;
begin
  Start := TThread.GetTickCount;
  repeat
    FData.Refresh;
    CheckSynchronize(10);
    Application.ProcessMessages;
  until TThread.GetTickCount - Start > 300;
  // FMX updates the actions of the forms that are shown; this one is not
  for i := 0 to alActions.ActionCount - 1 do
    alActions.Actions[i].Update;
  ShowState;
end;

function TfrmModelEditorMain.RunSelfTest: Integer;
var
  Log: TStringList;
  Failures: Integer;
  ClassCount: Integer;

  procedure Check(const AWhat: string; APassed: Boolean; const AActual: string);
  begin
    if APassed then
      Log.Add('  OK    ' + AWhat + ' = ' + AActual)
    else
    begin
      Log.Add('  FAIL  ' + AWhat + ' = ' + AActual);
      Inc(Failures);
    end;
  end;

  { The index of a grid's column by its header, -1 when there is none }
  function ColumnIndex(AGrid: TStringGrid; const AHeader: string): Integer;
  var
    Col: Integer;
  begin
    for Col := 0 to AGrid.ColumnCount - 1 do
      if AGrid.Columns[Col].Header = AHeader then
        Exit(Col);
    Result := -1;
  end;

  { The row of a grid whose Name column shows AName, -1 when there is none }
  function RowNamed(AGrid: TStringGrid; const AName: string): Integer;
  var
    Col, Row: Integer;
  begin
    Col := ColumnIndex(AGrid, 'Name');
    if Col >= 0 then
      for Row := 0 to AGrid.RowCount - 1 do
        if AGrid.Cells[Col, Row] = AName then
          Exit(Row);
    Result := -1;
  end;

  { The row of a grid whose column AHeader has AText, -1 when there is none }
  function RowWith(AGrid: TStringGrid; const AHeader, AText: string): Integer;
  var
    Col, Row: Integer;
  begin
    Col := ColumnIndex(AGrid, AHeader);
    if Col >= 0 then
      for Row := 0 to AGrid.RowCount - 1 do
        if Pos(AText, AGrid.Cells[Col, Row]) > 0 then
          Exit(Row);
    Result := -1;
  end;

  { The text of a column in the selected row of a grid }
  function SelectedCell(AGrid: TStringGrid; const AHeader: string): string;
  var
    Col: Integer;
  begin
    Col := ColumnIndex(AGrid, AHeader);
    if (Col >= 0) and (AGrid.Row >= 0) then
      Result := AGrid.Cells[Col, AGrid.Row]
    else
      Result := '(no cell)';
  end;

var
  Guid: TGUID;
  Row: Integer;
  Report: string;
  Memory: TBoldMemoryLogSink;
  MemoryRef: IBoldLogSink;
  ClassDialog: TfrmModelEditorClass;
  AttributeDialog: TfrmModelEditorAttribute;
  ClassValues: TModelEditorClassValues;
  SuperclassNames: TStringList;
  AttributeValues: TModelEditorAttributeValues;
  TypeNames: TStringList;
begin
  Log := TStringList.Create;
  Failures := 0;
  CreateGUID(Guid);
  FSelfTestFolder := TPath.Combine(TPath.GetTempPath, 'ModelEditorFMX-selftest-' + Copy(GUIDToString(Guid), 2, 36));
  TDirectory.CreateDirectory(FSelfTestFolder);
  try
    try
      Settle;
      SelfTestTrace('settled');
      Check('the model of the command line is open', ExtractFileName(FData.Document.FileName) = 'Invoicing.bld',
        FData.Document.FileName);
      Check('the window title', Caption = 'Invoicing.bld - ' + cModelEditorTitle, Caption);
      Check('the type names of the form on the command line', FData.TypeNamesName = 'MainForm.dfm', FData.TypeNamesName);
      Check('the status names them', Pos('type names: MainForm.dfm', lblStatus.Text) > 0, lblStatus.Text);
      ClassCount := FData.Document.UMLModel.Classes.Count;
      Check('the class grid has a row per class', grdClasses.RowCount = ClassCount,
        Format('%d rows, %d classes', [grdClasses.RowCount, ClassCount]));
      Check('the class grid has the columns of the dataset', (ColumnIndex(grdClasses, 'Name') >= 0) and
        (ColumnIndex(grdClasses, 'Superclass') >= 0) and (ColumnIndex(grdClasses, 'Abstract') >= 0) and
        (ColumnIndex(grdClasses, 'Persistent') >= 0), IntToStr(grdClasses.ColumnCount) + ' columns');
      Check('the class and attribute grids are read-only, the dialogs edit',
        grdClasses.ReadOnly and grdAttributes.ReadOnly and not (TGridOption.Editing in grdClasses.Options) and
        not (TGridOption.Editing in grdAttributes.Options), '');

      // A class selected in the grid as a click selects it: SelectRow tells
      // the binding, so the dataset follows (setting Row does not)
      Row := RowNamed(grdClasses, 'Customer');
      Check('Customer is in the class grid', Row >= 0, IntToStr(Row));
      grdClasses.SelectRow(Row);
      Settle;
      Check('selecting a row of the class grid selects the class', (FData.CurrentClass <> nil) and
        (FData.CurrentClass.name = 'Customer'), SelectedCell(grdClasses, 'Name'));
      Check('the attribute grid follows', grdAttributes.RowCount = 10, IntToStr(grdAttributes.RowCount));
      Check('the attribute grid shows the attributes of Customer', RowNamed(grdAttributes, 'PaymentTermDays') >= 0,
        IntToStr(RowNamed(grdAttributes, 'PaymentTermDays')));
      Check('the label of the attribute grid', lblAttributes.Text = 'Attributes of Customer', lblAttributes.Text);
      Check('Delete class is enabled for Customer', actDeleteClass.Enabled, '');

      grdClasses.SelectRow(RowNamed(grdClasses, 'InvoicingRoot'));
      Settle;
      Check('Delete class is disabled for the root class', not actDeleteClass.Enabled,
        SelectedCell(grdClasses, 'Name'));

      actAddClass.Execute;
      Settle;
      Check('Add class adds a row', grdClasses.RowCount = ClassCount + 1, IntToStr(grdClasses.RowCount));
      Check('the new class is the selected row', SelectedCell(grdClasses, 'Name') = 'NewClass',
        SelectedCell(grdClasses, 'Name'));
      Check('with the root class as its superclass', SelectedCell(grdClasses, 'Superclass') = 'InvoicingRoot',
        SelectedCell(grdClasses, 'Superclass'));
      Check('the title shows the change', Caption = 'Invoicing.bld* - ' + cModelEditorTitle, Caption);

      // A new name as the class dialog gives it
      FSelfTestName := 'Reminder';
      actEditClass.Execute;
      FSelfTestName := '';
      Settle;
      Check('Edit class renames the class, and the grid shows it', RowNamed(grdClasses, 'Reminder') >= 0,
        IntToStr(RowNamed(grdClasses, 'Reminder')));
      FSelfTestDialogOK := False;
      FSelfTestName := 'Ignored';
      actEditClass.Execute;
      FSelfTestDialogOK := True;
      FSelfTestName := '';
      Settle;
      Check('Edit class, then Cancel, keeps the name', FData.CurrentClass().name = 'Reminder', FData.CurrentClass().name);

      Check('Add attribute is enabled', actAddAttribute.Enabled, '');
      actAddAttribute.Execute;
      Settle;
      Check('Add attribute adds a row', grdAttributes.RowCount = 1, IntToStr(grdAttributes.RowCount));
      Check('the new attribute has the type String', SelectedCell(grdAttributes, 'Type') = 'String',
        SelectedCell(grdAttributes, 'Type'));
      Check('Delete attribute is enabled', actDeleteAttribute.Enabled, '');
      actDeleteAttribute.Execute;
      Settle;
      Check('Delete attribute removes the row', grdAttributes.RowCount = 0, IntToStr(grdAttributes.RowCount));
      actDeleteClass.Execute;
      Settle;
      Check('Delete class removes the row', grdClasses.RowCount = ClassCount, IntToStr(grdClasses.RowCount));

      // Add opens the class dialog: Cancel adds nothing
      FSelfTestDialogOK := False;
      actAddClass.Execute;
      FSelfTestDialogOK := True;
      Settle;
      Check('Add class, then Cancel, adds nothing', grdClasses.RowCount = ClassCount, IntToStr(grdClasses.RowCount));
      FSelfTestName := 'Subtype';
      FSelfTestSuperclass := 'Customer';
      actAddClass.Execute;
      FSelfTestName := '';
      FSelfTestSuperclass := '';
      Settle;
      Check('Add class, then OK, adds the class under the superclass the dialog chose',
        (SelectedCell(grdClasses, 'Name') = 'Subtype') and (SelectedCell(grdClasses, 'Superclass') = 'Customer'),
        SelectedCell(grdClasses, 'Name') + ' ' + SelectedCell(grdClasses, 'Superclass'));
      actDeleteClass.Execute;
      Settle;

      // The two dialogs show the values and give them back; created, not
      // shown. A text set in code does not fire OnChangeTracking, so the
      // check of the OK button calls the handler, as typing does
      grdClasses.SelectRow(RowNamed(grdClasses, 'Customer'));
      Settle;
      ClassDialog := TfrmModelEditorClass.Create(nil);
      SuperclassNames := TStringList.Create;
      try
        ClassValues := FData.ClassValues(FData.CurrentClass());
        FData.GetSuperclassNames(FData.CurrentClass(), SuperclassNames);
        ClassDialog.LoadValues(ClassValues, SuperclassNames);
        Check('the class dialog shows the name', ClassDialog.edtName.Text = 'Customer', ClassDialog.edtName.Text);
        Check('and the superclass', ClassDialog.SelectedSuperclassText = ClassValues.Superclass,
          ClassDialog.SelectedSuperclassText);
        Check('the superclass box offers the other classes, not the class itself',
          ClassDialog.cbSuperclass.Enabled and (ClassDialog.cbSuperclass.Items.IndexOf('Invoice') >= 0) and
          (ClassDialog.cbSuperclass.Items.IndexOf('Customer') < 0), IntToStr(ClassDialog.cbSuperclass.Items.Count));
        Check('and persistent', ClassDialog.chkPersistent.IsChecked = ClassValues.Persistent, '');
        Check('OK is enabled for a class with a name', ClassDialog.btnOK.Enabled, '');
        ClassDialog.edtName.Text := '';
        ClassDialog.edtNameChange(ClassDialog.edtName);
        Check('and disabled for one without', not ClassDialog.btnOK.Enabled, '');
        ClassDialog.edtName.Text := 'Client';
        ClassDialog.cbSuperclass.ItemIndex := ClassDialog.cbSuperclass.Items.IndexOf('Invoice');
        ClassDialog.chkAbstract.IsChecked := True;
        ClassDialog.SaveValues(ClassValues);
        Check('the class dialog gives the values back', (ClassValues.Name = 'Client') and ClassValues.Abstract and
          (ClassValues.Superclass = 'Invoice'), ClassValues.Name + ' ' + ClassValues.Superclass);
        grdClasses.SelectRow(RowNamed(grdClasses, 'InvoicingRoot'));
        Settle;
        FData.GetSuperclassNames(FData.CurrentClass(), SuperclassNames);
        ClassDialog.LoadValues(FData.ClassValues(FData.CurrentClass()), SuperclassNames);
        Check('the root class has no superclass to pick', not ClassDialog.cbSuperclass.Enabled and
          (ClassDialog.SelectedSuperclassText = '(none)'), ClassDialog.SelectedSuperclassText);
        grdClasses.SelectRow(RowNamed(grdClasses, 'Customer'));
        Settle;
      finally
        SuperclassNames.Free;
        ClassDialog.Free;
      end;
      AttributeDialog := TfrmModelEditorAttribute.Create(nil);
      TypeNames := TStringList.Create;
      try
        FData.bdsAttributes.First;
        FData.GetTypeNames(TypeNames);
        AttributeValues := FData.AttributeValues(FData.CurrentAttribute());
        AttributeDialog.LoadValues(AttributeValues, TypeNames);
        Check('the attribute dialog shows the name', AttributeDialog.edtName.Text = AttributeValues.Name,
          AttributeDialog.edtName.Text);
        Check('and the type', AttributeDialog.cbType.Text = AttributeValues.TypeName, AttributeDialog.cbType.Text);
        Check('the type box offers the type names', (AttributeDialog.cbType.Items.IndexOf('Integer') >= 0) and
          (AttributeDialog.cbType.Items.IndexOf('Coordinate') >= 0), IntToStr(AttributeDialog.cbType.Items.Count));
        AttributeDialog.cbType.Text := 'String';
        AttributeDialog.chkDerived.IsChecked := True;
        AttributeDialog.SaveValues(AttributeValues);
        Check('the attribute dialog gives the values back', (AttributeValues.TypeName = 'String') and
          AttributeValues.Derived, AttributeValues.TypeName);
      finally
        TypeNames.Free;
        AttributeDialog.Free;
      end;

      actAddClass.Execute;
      actSaveAs.Execute;
      Settle;
      Check('Save as writes the model', TFile.Exists(TPath.Combine(FSelfTestFolder, 'Invoicing.bld')),
        FData.Document.FileName);
      Check('the title after saving', Caption = 'Invoicing.bld - ' + cModelEditorTitle, Caption);
      actGenerate.Execute;
      Settle;
      Check('Generate writes the unit', TFile.Exists(TPath.Combine(FSelfTestFolder, 'InvoicingClasses.pas')), '');
      Check('the status tells where', Pos(FSelfTestFolder, lblStatus.Text) > 0, lblStatus.Text);

      // The consistency check: a class renamed to the name of another is a
      // finding, and a double click on it selects the class
      Check('the findings grid has the columns of the dataset', (ColumnIndex(grdFindings, 'Severity') >= 0) and
        (ColumnIndex(grdFindings, 'Description') >= 0) and (ColumnIndex(grdFindings, 'Element') >= 0),
        IntToStr(grdFindings.ColumnCount) + ' columns');
      Check('the findings grid is read-only', grdFindings.ReadOnly, '');
      Check('no findings before the check', grdFindings.RowCount = 0, IntToStr(grdFindings.RowCount));
      Check('the label before the check', lblFindings.Text = 'Consistency check', lblFindings.Text);
      Row := RowNamed(grdClasses, 'NewClass');
      Check('NewClass is in the class grid', Row >= 0, IntToStr(Row));
      grdClasses.SelectRow(Row);
      Settle;
      FSelfTestName := 'Customer';
      actEditClass.Execute;
      FSelfTestName := '';
      grdClasses.SelectRow(RowNamed(grdClasses, 'Invoice'));
      Settle;
      actValidate.Execute;
      Settle;
      Check('the check fills the findings grid', grdFindings.RowCount > 0, IntToStr(grdFindings.RowCount));
      Check('the label sums them up', Pos(' errors', lblFindings.Text) > 0, lblFindings.Text);
      Row := RowWith(grdFindings, 'Description', 'Duplicate class name');
      Check('a finding for the duplicate class name', Row >= 0, IntToStr(Row));
      grdFindings.SelectRow(Row);
      Settle;
      Check('its severity', SelectedCell(grdFindings, 'Severity') = 'Error', SelectedCell(grdFindings, 'Severity'));
      grdFindingsCellDblClick(nil, grdFindings.Selected);
      Settle;
      Check('a double click selects the class it is about',
        (FData.CurrentClass <> nil) and (FData.CurrentViolation <> nil) and
        (TObject(FData.CurrentClass()) = TObject(FData.CurrentViolation.ModelElement)),
        SelectedCell(grdClasses, 'Name'));

      // The report of an exception (#130): with Stacker linked it names the
      // routine that raised it; without a provider it gives the address
      Report := '';
      try
        RaiseForTheSelfTest;
      except
        on E: Exception do
          Report := BoldExceptionReport(E);
      end;
      {$IFDEF BOLD_STACKER}
      Check('the report of an exception names the routine that raised it (Stacker)',
        Pos('RAISEFORTHESELFTEST', UpperCase(Report)) > 0, Report);
      {$ELSE}
      Check('the report of an exception gives where it was raised (no provider)',
        Pos('Raised at $', Report) > 0, Report);
      {$ENDIF}

      // An exception in the event loop goes to Bold's log with its call
      // stack, then to the program's own handler, here SelfTestException: the
      // hook of BoldExceptionLogFMX took that handler over when the event
      // loop started
      Memory := TBoldMemoryLogSink.Create;
      MemoryRef := Memory;
      BoldLogManager.RegisterSink(MemoryRef);
      FExceptionExpected := True;
      try
        try
          RaiseForTheSelfTest;
        except
          // What FMX's event loop does with an exception
          Application.HandleException(Self);
        end;
      finally
        FExceptionExpected := False;
        BoldLogManager.UnregisterSink(MemoryRef);
      end;
      {$IFDEF BOLD_STACKER}
      Check('an exception in the event loop goes to the log with its call stack',
        Pos('RAISEFORTHESELFTEST', UpperCase(Memory.Lines.Text)) > 0, Memory.Lines.Text);
      {$ELSE}
      Check('an exception in the event loop goes to the log with where it was raised',
        Pos('Raised at $', Memory.Lines.Text) > 0, Memory.Lines.Text);
      {$ENDIF}
      MemoryRef := nil;
      Check('and then to the program''s own handler', FExpectedExceptionSeen, BoolToStr(FExpectedExceptionSeen, True));

      Check('no exception in the event loop', not FSelfTestException, BoolToStr(FSelfTestException, True));
    except
      on E: Exception do
      begin
        Log.Add('  EXCEPTION ' + E.ClassName + ': ' + E.Message);
        Failures := 100;
      end;
    end;
  finally
    TDirectory.Delete(FSelfTestFolder, True);
  end;
  Log.Add(IntToStr(Failures) + ' failed');
  SelfTestTrace(Log.Text);
  Log.Free;
  Result := Failures;
end;

end.
