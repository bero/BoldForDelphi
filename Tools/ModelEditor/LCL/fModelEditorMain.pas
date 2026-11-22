{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit fModelEditorMain;

{ The LCL client of the standalone model editor (#128): the classes of a
  model in one grid and the attributes of the selected class in another.

  What it shows and does is in the data module the LCL and the FMX client
  share (../Core/BoldModelEditorData.pas): the model document, the Bold
  handles, the two TBoldDataSets the grids show, the commands, and the
  question about unsaved changes. The form adds what is the LCL's: the
  grids (a TDBGrid on a TDataSource on each dataset), an action per command
  with its menu item and button, and the dialogs. The grids edit the names
  and the other columns in place; adding and deleting goes through the
  commands, so the grids neither insert nor delete rows themselves.

  BoldEnvironmentLCL, named in the uses clause below, drains Bold's display
  queue from the LCL's idle handling, so the grids follow a change of the
  model with no code here. }

interface

uses
  SysUtils,
  Classes,
  Forms,
  Controls,
  Graphics,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Menus,
  ActnList,
  Dialogs,
  DB,
  DBGrids,
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
    pnlClasses: TPanel;
    pnlClassBar: TPanel;
    lblClasses: TLabel;
    btnAddClass: TButton;
    btnDeleteClass: TButton;
    grdClasses: TDBGrid;
    splGrids: TSplitter;
    pnlAttributes: TPanel;
    pnlAttributeBar: TPanel;
    lblAttributes: TLabel;
    btnAddAttribute: TButton;
    btnDeleteAttribute: TButton;
    grdAttributes: TDBGrid;
    sbStatus: TStatusBar;
    dsClasses: TDataSource;
    dsAttributes: TDataSource;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    dlgCodeFolder: TSelectDirectoryDialog;
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
    procedure alActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
  private
    FData: TdmModelEditor;
    FSelfTest: Boolean;
    // Where the self-test saves and generates, instead of asking
    FSelfTestFolder: string;
    FStatus: string;
    // The self-test's own exception in the event loop (#130)
    FExceptionExpected: Boolean;
    FExpectedExceptionSeen: Boolean;
    function AskSave(const ADocumentName: string): TModelEditorSaveAnswer;
    function ChooseSaveFile(var AFileName: string): Boolean;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure ShowState;
    procedure Settle;
    procedure SelfTestException(Sender: TObject; E: Exception);
  public
    { Opens the model named on the command line, checks what the grids and
      the commands do, as the lpr's --selftest describes, and returns the
      number of failed checks, or 100 after an exception }
    function RunSelfTest: Integer;
  end;

var
  frmModelEditorMain: TfrmModelEditorMain;

{ True when the command line has --selftest, -selftest or /selftest }
function IsSelfTestRun: Boolean;

{ Starts ModelEditorLCL.selftest.txt next to the program; the self-test
  writes its steps there as it goes, so a run that stops shows where }
procedure StartSelfTestLog;
procedure SelfTestTrace(const S: string);

implementation

{$R *.lfm}

uses
  BoldEnvironmentLCL, // registers Bold's LCL environment
  BoldLogInterfaces,
  BoldLogSinks,
  BoldExceptionTrace,
  BoldExceptionLog,
  BoldUMLModel;

resourcestring
  sSaveChanges = 'Save the changes of %s?';
  sAttributesOf = 'Attributes of %s';
  sAttributes = 'Attributes';
  sGenerated = 'Business classes generated into %s';
  sClassCount = '%d classes';

var
  SelfTestLog: string = '';

function IsSelfTestRun: Boolean;
var
  I: Integer;
  Param: string;
begin
  Result := False;
  for I := 1 to ParamCount do
  begin
    Param := ParamStr(I);
    if SameText(Param, '--selftest') or SameText(Param, '-selftest') or SameText(Param, '/selftest') then
      Result := True;
  end;
end;

procedure WriteLog(const S: string; AAppend: Boolean);
var
  Log: TextFile;
begin
  AssignFile(Log, SelfTestLog);
  if AAppend then
    Append(Log)
  else
    Rewrite(Log);
  try
    Writeln(Log, S);
  finally
    CloseFile(Log);
  end;
end;

procedure StartSelfTestLog;
begin
  SelfTestLog := ChangeFileExt(ParamStr(0), '.selftest.txt');
  WriteLog('ModelEditorLCL self-test', False);
end;

procedure SelfTestTrace(const S: string);
begin
  if SelfTestLog <> '' then
    WriteLog(S, True);
end;

procedure DeleteFolder(const AFolder: string);
var
  Search: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(AFolder) + '*', faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Attr and faDirectory) = 0 then
        DeleteFile(IncludeTrailingPathDelimiter(AFolder) + Search.Name);
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(AFolder);
end;

{ TfrmModelEditorMain }

procedure TfrmModelEditorMain.FormCreate(Sender: TObject);
var
  ModelFile: string;
begin
  FSelfTest := IsSelfTestRun;
  FData := TdmModelEditor.Create(Self);
  // Exceptions go to ModelEditorLCL.log next to the program with their call
  // stacks, then to the user: BoldExceptionLogLCL, named in the .lpr (#130)
  if FSelfTest then
  begin
    // No window to show an error in: the self-test log gets it. The exception
    // log still logs it first, but not to its file
    Application.OnException := SelfTestException;
    BoldExceptionLogSink.Enabled := False;
  end;
  FData.OnAskSave := AskSave;
  FData.OnChooseSaveFile := ChooseSaveFile;
  dsClasses.DataSet := FData.bdsClasses;
  dsAttributes.DataSet := FData.bdsAttributes;
  dlgOpen.Filter := 'Bold models (*.bld)|*.bld|All files|*';
  dlgSave.Filter := dlgOpen.Filter;
  ModelFile := ModelFileFromCommandLine;
  if ModelFile <> '' then
    FData.Open(ModelFile);
  Application.OnIdle := AppIdle;
  ShowState;
end;

procedure TfrmModelEditorMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  // The grids let go of the datasets before the data module frees them
  dsAttributes.DataSet := nil;
  dsClasses.DataSet := nil;
  FreeAndNil(FData);
end;

procedure TfrmModelEditorMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FData.CloseModel;
end;

function TfrmModelEditorMain.AskSave(const ADocumentName: string): TModelEditorSaveAnswer;
begin
  case MessageDlg(Format(sSaveChanges, [ADocumentName]), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
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
    AFileName := IncludeTrailingPathDelimiter(FSelfTestFolder) + ExtractFileName(AFileName);
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
  // Done is left alone: Bold's queue has an idle handler of its own
  ShowState;
end;

procedure TfrmModelEditorMain.ShowState;
var
  AClass: TUMLClass;
begin
  Caption := FData.Title;
  AClass := FData.CurrentClass;
  if Assigned(AClass) then
    lblAttributes.Caption := Format(sAttributesOf, [AClass.name])
  else
    lblAttributes.Caption := sAttributes;
  if FStatus <> '' then
    sbStatus.SimpleText := FStatus
  else if FData.bdsClasses.Active then
    sbStatus.SimpleText := FData.Document.FileName + '  ' +
      Format(sClassCount, [FData.bdsClasses.RecordCount]);
end;

procedure TfrmModelEditorMain.alActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
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
    dlgCodeFolder.InitialDir := FData.DefaultCodeFolder;
    if not dlgCodeFolder.Execute then
      Exit;
    Folder := dlgCodeFolder.FileName;
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
  if grdClasses.CanSetFocus then
  begin
    grdClasses.SetFocus;
    grdClasses.SelectedIndex := 0;
  end;
end;

procedure TfrmModelEditorMain.actDeleteClassExecute(Sender: TObject);
begin
  FData.DeleteClass;
end;

procedure TfrmModelEditorMain.actAddAttributeExecute(Sender: TObject);
begin
  FData.AddAttribute;
  if grdAttributes.CanSetFocus then
  begin
    grdAttributes.SetFocus;
    grdAttributes.SelectedIndex := 0;
  end;
end;

procedure TfrmModelEditorMain.actDeleteAttributeExecute(Sender: TObject);
begin
  FData.DeleteAttribute;
end;

procedure TfrmModelEditorMain.SelfTestException(Sender: TObject; E: Exception);
begin
  if FExceptionExpected then
  begin
    FExpectedExceptionSeen := True;
    Exit;
  end;
  SelfTestTrace('EXCEPTION ' + BoldExceptionReport(E));
end;

{ Raises for the self-test's check of the call stack }
procedure RaiseForTheSelfTest;
begin
  raise Exception.Create('raised for the self-test');
end;

procedure TfrmModelEditorMain.Settle;
var
  Start: QWord;
  i: Integer;
begin
  // What Application.Run does, for a while and without waiting: messages,
  // then the idle handlers, where the LCL environment drains Bold's display
  // queue. TBoldDataSet refreshes after a Bold-side change through
  // BoldSharedClock, which arrives on the main thread with the messages.
  Start := GetTickCount64;
  repeat
    Application.ProcessMessages;
    Application.Idle(False);
    Sleep(10);
  until GetTickCount64 - Start > 300;
  // The idle handling updates the actions of the forms that are shown only;
  // this one is not
  for i := 0 to alActions.ActionCount - 1 do
    alActions.Actions[i].Update;
end;

function TfrmModelEditorMain.RunSelfTest: Integer;
var
  Failures: Integer;
  Guid: TGUID;
  ClassCount: Integer;
  Report: string;
  Memory: TBoldMemoryLogSink;
  MemoryRef: IBoldLogSink;

  procedure Check(const AWhat: string; APassed: Boolean; const AActual: string);
  begin
    if APassed then
      SelfTestTrace('  OK    ' + AWhat + ' = ' + AActual)
    else
    begin
      SelfTestTrace('  FAIL  ' + AWhat + ' = ' + AActual);
      Inc(Failures);
    end;
  end;

  function SelectClassNamed(const AName: string): Boolean;
  begin
    Result := False;
    FData.bdsClasses.First;
    while not FData.bdsClasses.Eof do
    begin
      if FData.bdsClasses.FieldByName('Name').AsString = AName then
        Exit(True);
      FData.bdsClasses.Next;
    end;
  end;

begin
  Failures := 0;
  CreateGUID(Guid);
  FSelfTestFolder := IncludeTrailingPathDelimiter(GetTempDir) + 'ModelEditorLCL-selftest-' + Copy(GUIDToString(Guid), 2, 36);
  ForceDirectories(FSelfTestFolder);
  try
    Settle;
    Check('the model of the command line is open', ExtractFileName(FData.Document.FileName) = 'Invoicing.bld',
      FData.Document.FileName);
    Check('the window title', Caption = 'Invoicing.bld - ' + cModelEditorTitle, Caption);
    Check('the class grid shows the class dataset', grdClasses.DataSource.DataSet = FData.bdsClasses, '');
    Check('the attribute grid shows the attribute dataset', grdAttributes.DataSource.DataSet = FData.bdsAttributes, '');
    ClassCount := FData.Document.UMLModel.Classes.Count;
    Check('a row per class', FData.bdsClasses.RecordCount = ClassCount, IntToStr(FData.bdsClasses.RecordCount));
    Check('the grids neither insert nor delete rows themselves',
      ([dgDisableInsert, dgDisableDelete] <= grdClasses.Options) and
      ([dgDisableInsert, dgDisableDelete] <= grdAttributes.Options), '');

    Check('select Customer in the class grid', SelectClassNamed('Customer'), '');
    Settle;
    Check('the attribute grid follows', FData.bdsAttributes.RecordCount = 10, IntToStr(FData.bdsAttributes.RecordCount));
    Check('the label of the attribute grid', lblAttributes.Caption = 'Attributes of Customer', lblAttributes.Caption);
    Check('Delete class is enabled for Customer', actDeleteClass.Enabled, '');

    Check('select the root class', SelectClassNamed('InvoicingRoot'), '');
    Settle;
    Check('Delete class is disabled for the root class', not actDeleteClass.Enabled, '');

    actAddClass.Execute;
    Settle;
    Check('Add class adds a row', FData.bdsClasses.RecordCount = ClassCount + 1, IntToStr(FData.bdsClasses.RecordCount));
    Check('the new class is selected', FData.bdsClasses.FieldByName('Name').AsString = 'NewClass',
      FData.bdsClasses.FieldByName('Name').AsString);
    Check('the title shows the change', Caption = 'Invoicing.bld* - ' + cModelEditorTitle, Caption);
    Check('Add attribute is enabled', actAddAttribute.Enabled, '');
    actAddAttribute.Execute;
    Settle;
    Check('Add attribute adds a row', FData.bdsAttributes.RecordCount = 1, IntToStr(FData.bdsAttributes.RecordCount));
    Check('Delete attribute is enabled', actDeleteAttribute.Enabled, '');
    actDeleteAttribute.Execute;
    Settle;
    Check('Delete attribute removes the row', FData.bdsAttributes.RecordCount = 0, IntToStr(FData.bdsAttributes.RecordCount));
    actDeleteClass.Execute;
    Settle;
    Check('Delete class removes the row', FData.bdsClasses.RecordCount = ClassCount, IntToStr(FData.bdsClasses.RecordCount));

    actAddClass.Execute;
    actSaveAs.Execute;
    Settle;
    Check('Save as writes the model', FileExists(IncludeTrailingPathDelimiter(FSelfTestFolder) + 'Invoicing.bld'),
      FData.Document.FileName);
    Check('the title after saving', Caption = 'Invoicing.bld - ' + cModelEditorTitle, Caption);
    actGenerate.Execute;
    Settle;
    Check('Generate writes the unit', FileExists(IncludeTrailingPathDelimiter(FSelfTestFolder) + 'InvoicingClasses.pas'), '');
    Check('the status bar tells where', Pos(FSelfTestFolder, sbStatus.SimpleText) > 0, sbStatus.SimpleText);

    // The report of an exception names the routine that raised it: Free
    // Pascal's own frames, with line information from -gl (#130)
    Report := '';
    try
      RaiseForTheSelfTest;
    except
      on E: Exception do
        Report := BoldExceptionReport(E);
    end;
    Check('the report of an exception names the routine that raised it',
      Pos('RAISEFORTHESELFTEST', UpperCase(Report)) > 0, Report);

    // An exception in the event loop goes to Bold's log with its call
    // stack, then to the program's own handler, here SelfTestException: the
    // hook of BoldExceptionLogLCL took that handler over when the event
    // loop started
    Memory := TBoldMemoryLogSink.Create;
    MemoryRef := Memory;
    BoldLogManager.RegisterSink(MemoryRef);
    FExceptionExpected := True;
    try
      try
        RaiseForTheSelfTest;
      except
        // What the LCL's event loop does with an exception
        Application.HandleException(Self);
      end;
    finally
      FExceptionExpected := False;
      BoldLogManager.UnregisterSink(MemoryRef);
    end;
    Check('an exception in the event loop goes to the log with its call stack',
      Pos('RAISEFORTHESELFTEST', UpperCase(Memory.Lines.Text)) > 0, Memory.Lines.Text);
    MemoryRef := nil;
    Check('and then to the program''s own handler', FExpectedExceptionSeen, BoolToStr(FExpectedExceptionSeen, True));
  finally
    Result := Failures;
    SelfTestTrace(Format('%d checks failed', [Failures]));
    DeleteFolder(FSelfTestFolder);
  end;
end;

end.
