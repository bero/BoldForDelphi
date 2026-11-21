unit fModelEditorMain;

{ The FMX client of the standalone model editor (#128), the FireMonkey twin
  of ../LCL/fModelEditorMain.pas: the classes of a model in one grid and the
  attributes of the selected class in another.

  What it shows and does is in the data module the two clients share
  (../Core/BoldModelEditorData.pas): the model document, the Bold handles,
  the two TBoldDataSets the grids show, the commands, and the question about
  unsaved changes. The form adds what is FireMonkey's: the grids (a
  TStringGrid bound to each dataset through LiveBindings, a TBindSourceDB
  and a TLinkGridToDataSource), an action per command with its menu item and
  button, and the dialogs. The bind sources get their datasets in
  FormCreate, as the data module is made there. The grids edit the names
  and the other columns in place; adding and deleting goes through the
  commands.

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
    mniFileSep2: TMenuItem;
    mniExit: TMenuItem;
    mnuModel: TMenuItem;
    mniAddClass: TMenuItem;
    mniDeleteClass: TMenuItem;
    mniModelSep1: TMenuItem;
    mniAddAttribute: TMenuItem;
    mniDeleteAttribute: TMenuItem;
    alActions: TActionList;
    actNew: TAction;
    actOpen: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    actGenerate: TAction;
    actExit: TAction;
    actAddClass: TAction;
    actDeleteClass: TAction;
    actAddAttribute: TAction;
    actDeleteAttribute: TAction;
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
    btnDeleteClass: TButton;
    grdClasses: TStringGrid;
    splGrids: TSplitter;
    layAttributes: TLayout;
    layAttributeBar: TLayout;
    lblAttributes: TLabel;
    btnAddAttribute: TButton;
    btnDeleteAttribute: TButton;
    grdAttributes: TStringGrid;
    layStatus: TLayout;
    lblStatus: TLabel;
    bsClasses: TBindSourceDB;
    bsAttributes: TBindSourceDB;
    BindingsList1: TBindingsList;
    lnkClasses: TLinkGridToDataSource;
    lnkAttributes: TLinkGridToDataSource;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actGenerateExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actAddClassExecute(Sender: TObject);
    procedure actDeleteClassExecute(Sender: TObject);
    procedure actAddAttributeExecute(Sender: TObject);
    procedure actDeleteAttributeExecute(Sender: TObject);
    procedure alActionsUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FData: TdmModelEditor;
    FSelfTest: Boolean;
    FSelfTestException: Boolean;
    // Where the self-test saves and generates, instead of asking
    FSelfTestFolder: string;
    FStatus: string;
    function AskSave(const ADocumentName: string): TModelEditorSaveAnswer;
    function ChooseSaveFile(var AFileName: string): Boolean;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure ShowState;
    procedure Settle;
    procedure SelfTestException(Sender: TObject; E: Exception);
    procedure AppException(Sender: TObject; E: Exception);
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
  BoldExceptionTrace,
  BoldUMLModel;

resourcestring
  sSaveChanges = 'Save the changes of %s?';
  sAttributesOf = 'Attributes of %s';
  sAttributes = 'Attributes';
  sGenerated = 'Business classes generated into %s';
  sClassCount = '%d classes';
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
  ModelFile: string;
begin
  FSelfTest := IsSelfTestRun;
  FData := TdmModelEditor.Create(Self);
  if FSelfTest then
    // No window to show an error in: the log gets it, and the exit code is 100
    Application.OnException := SelfTestException
  else
  begin
    // Exceptions go to ModelEditorFMX.log next to the program with their
    // call stacks (#130), then to the user
    FData.StartLogFile(ChangeFileExt(ParamStr(0), '.log'));
    Application.OnException := AppException;
  end;
  FData.OnAskSave := AskSave;
  FData.OnChooseSaveFile := ChooseSaveFile;
  // The grids are bound to the datasets of the data module through these
  bsClasses.DataSet := FData.bdsClasses;
  bsAttributes.DataSet := FData.bdsAttributes;
  // The two headings in bold, over the style's font
  lblClasses.StyledSettings := lblClasses.StyledSettings - [TStyledSetting.Style];
  lblClasses.TextSettings.Font.Style := [TFontStyle.fsBold];
  lblAttributes.StyledSettings := lblAttributes.StyledSettings - [TStyledSetting.Style];
  lblAttributes.TextSettings.Font.Style := [TFontStyle.fsBold];
  dlgOpen.Filter := 'Bold models (*.bld)|*.bld|All files|*';
  dlgSave.Filter := dlgOpen.Filter;
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
  if FStatus <> '' then
    lblStatus.Text := FStatus
  else if FData.bdsClasses.Active then
    lblStatus.Text := FData.Document.FileName + '  ' + Format(sClassCount, [FData.bdsClasses.RecordCount]);
end;

procedure TfrmModelEditorMain.alActionsUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  if not Assigned(FData) then
    Exit;
  actDeleteClass.Enabled := FData.CanDeleteClass;
  actAddAttribute.Enabled := FData.CanAddAttribute;
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

procedure TfrmModelEditorMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmModelEditorMain.actAddClassExecute(Sender: TObject);
begin
  FData.AddClass;
  // Ready to type the name of the new class
  if grdClasses.CanFocus then
    grdClasses.SetFocus;
end;

procedure TfrmModelEditorMain.actDeleteClassExecute(Sender: TObject);
begin
  FData.DeleteClass;
end;

procedure TfrmModelEditorMain.actAddAttributeExecute(Sender: TObject);
begin
  FData.AddAttribute;
  if grdAttributes.CanFocus then
    grdAttributes.SetFocus;
end;

procedure TfrmModelEditorMain.actDeleteAttributeExecute(Sender: TObject);
begin
  FData.DeleteAttribute;
end;

procedure TfrmModelEditorMain.SelfTestException(Sender: TObject; E: Exception);
begin
  // No Halt: the dpr frees the form before Bold's units are finalized
  SelfTestTrace('EXCEPTION ' + BoldExceptionReport(E));
  FSelfTestException := True;
end;

procedure TfrmModelEditorMain.AppException(Sender: TObject; E: Exception);
begin
  FData.LogException(E);
  Application.ShowException(E);
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
      ClassCount := FData.Document.UMLModel.Classes.Count;
      Check('the class grid has a row per class', grdClasses.RowCount = ClassCount,
        Format('%d rows, %d classes', [grdClasses.RowCount, ClassCount]));
      Check('the class grid has the columns of the dataset', (ColumnIndex(grdClasses, 'Name') >= 0) and
        (ColumnIndex(grdClasses, 'Superclass') >= 0) and (ColumnIndex(grdClasses, 'Abstract') >= 0) and
        (ColumnIndex(grdClasses, 'Persistent') >= 0), IntToStr(grdClasses.ColumnCount) + ' columns');

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

      // A new name typed as the grid writes it: into the dataset of the grid
      FData.bdsClasses.Edit;
      FData.bdsClasses.FieldByName('Name').AsString := 'Reminder';
      FData.bdsClasses.Post;
      Settle;
      Check('the new name shows in the grid', RowNamed(grdClasses, 'Reminder') >= 0,
        IntToStr(RowNamed(grdClasses, 'Reminder')));

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
