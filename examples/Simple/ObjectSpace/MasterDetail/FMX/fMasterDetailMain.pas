unit fMasterDetailMain;

{ The Master-Detail demo in FireMonkey (#117 step 10), the counterpart of the
  VCL form in ..\DemoForm.pas.

  Three list handles, as in the VCL demo: lhaProjects (Project.allInstances),
  lhaProjectTasks rooted on lhaProjects with the expression tasks, and lhaTasks
  (Task.allInstances). Only the root differs between the second and the third,
  and that one property is the whole master-detail mechanism: select another
  project and the bottom left grid follows, with no code, while the bottom
  right one shows every task with the name of its project.

  FMX has no Bold-aware controls. Each handle feeds a TBoldDataSet, and
  LiveBindings binds the dataset to an FMX TStringGrid (TBindSourceDB and
  TLinkGridToDataSource) and to a TBindNavigator, which takes the place of
  TBoldNavigator. The handles, datasets, bindings and controls are components
  in the form file. The system handle is in the data module, which the code
  creates, so the code gives the handles their system and their roots.

  The project defines BOLD_NO_VCL, so Bold's control packs link no VCL unit.
  Without the VCL environment nothing drains Bold's display queue by itself:
  AppIdle does it from FMX's idle loop, and shows the project count with the
  OCL expression of the VCL demo's TBoldLabel. }

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Rtti,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Grid,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  Data.DB,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.Grid,
  Data.Bind.EngExt,
  Data.Bind.Controls,
  Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid,
  Fmx.Bind.Editors,
  Fmx.Bind.Navigator,
  System.Bindings.Outputs,
  BoldHandles,
  BoldListHandle,
  BoldDataSet,
  dmMasterDetail;

type
  TfrmMasterDetailMain = class(TForm)
    layStatus: TLayout;
    lblStatus: TLabel;
    btnDropDatabase: TButton;
    layTop: TLayout;
    layProjectsHeader: TLayout;
    lblProjects: TLabel;
    lblProjectCount: TLabel;
    btnClear: TButton;
    grdProjects: TStringGrid;
    navProjects: TBindNavigator;
    splTop: TSplitter;
    layBottom: TLayout;
    layBottomLeft: TLayout;
    lblProjectTasks: TLabel;
    grdProjectTasks: TStringGrid;
    navProjectTasks: TBindNavigator;
    splBottom: TSplitter;
    layBottomRight: TLayout;
    lblTasks: TLabel;
    grdTasks: TStringGrid;
    layTasksBar: TLayout;
    navTasks: TBindNavigator;
    btnSave: TButton;
    lhaProjects: TBoldListHandle;
    lhaProjectTasks: TBoldListHandle;
    lhaTasks: TBoldListHandle;
    bdsProjects: TBoldDataSet;
    bdsProjectTasks: TBoldDataSet;
    bdsTasks: TBoldDataSet;
    bsProjects: TBindSourceDB;
    bsProjectTasks: TBindSourceDB;
    bsTasks: TBindSourceDB;
    BindingsList1: TBindingsList;
    lnkProjects: TLinkGridToDataSource;
    lnkProjectTasks: TLinkGridToDataSource;
    lnkTasks: TLinkGridToDataSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ClearClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure DropDatabaseClick(Sender: TObject);
  private
    FData: TdmMasterDetail;
    FDatabase: string;
    FSelfTest: Boolean;
    FSelfTestException: Boolean;
    // The self-test's own exception in the event loop (#130)
    FExceptionExpected: Boolean;
    FExpectedExceptionSeen: Boolean;
    procedure OpenData;
    procedure SystemOpened;
    procedure OpenDataSets;
    procedure CloseDataSets;
    procedure ReopenSystem(ADropDatabase: Boolean);
    procedure ClearAllData;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure ShowStatus;
    procedure Settle;
    procedure SelfTestException(Sender: TObject; E: Exception);
  public
    { Drives the form as a user would, through its navigators, datasets and
      buttons, checks what the grids show, and returns the number of failed
      checks, or 100 after an exception }
    function RunSelfTest: Integer;
  end;

var
  frmMasterDetailMain: TfrmMasterDetailMain;

{ True when the command line has --selftest, -selftest or /selftest }
function IsSelfTestRun: Boolean;

{ Starts MasterDetailFMX.selftest.txt next to the exe; the self-test writes
  its steps there as it goes, so a run that stops shows where }
procedure StartSelfTestLog;
procedure SelfTestTrace(const S: string);

implementation

{$R *.fmx}

uses
  System.IOUtils,
  System.StrUtils,
  {$IFDEF POSIX}
  Posix.Unistd, // SysUtils.DeleteFile is inlined from it (H2443 without)
  {$ENDIF}
  FMX.DialogService,
  FMX.DialogService.Sync,
  BoldQueue,
  BoldLogInterfaces,
  BoldLogSinks,
  BoldExceptionTrace,
  BoldExceptionLog,
  BoldSystem,
  DemoClasses;

const
  // The expression of the VCL demo's TBoldLabel
  cProjectCountExpression = '''('' + Project.allInstances->size.asString + '')''';

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
  TFile.WriteAllText(SelfTestLog, 'MasterDetailFMX self-test' + sLineBreak);
end;

procedure SelfTestTrace(const S: string);
begin
  if SelfTestLog <> '' then
    TFile.AppendAllText(SelfTestLog, S + sLineBreak);
end;

{ The model next to the program, where the Linux64 deployment puts it, else
  examples/Shared, where it is kept, for a program run where it was built }
function ModelFileName: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'DemoModel.bld');
  if not TFile.Exists(Result) then
    Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)),
      '..' + PathDelim + '..' + PathDelim + '..' + PathDelim + '..' + PathDelim + 'Shared' + PathDelim + 'DemoModel.bld'));
end;

{ TfrmMasterDetailMain }

procedure TfrmMasterDetailMain.FormCreate(Sender: TObject);
begin
  FSelfTest := IsSelfTestRun;
  // Exceptions go to <program>.log next to the program with their call
  // stacks, then to the user: BoldExceptionLogFMX, named in the .dpr (#130)
  if FSelfTest then
  begin
    // No window to show an error in: the self-test log gets it, and the exit
    // code is 100. The exception log still logs it first, but not to its file
    Application.OnException := SelfTestException;
    BoldExceptionLogSink.Enabled := False;
    FDatabase := TPath.Combine(TPath.GetTempPath, 'MasterDetailFMX-selftest-' + TGUID.NewGuid.ToString + '.db');
  end
  else
    FDatabase := ChangeFileExt(ParamStr(0), '.db');
  SelfTestTrace('form created; database ' + FDatabase);
  OpenData;
  SelfTestTrace('system open');

  // lhaProjectTasks is rooted on lhaProjects in the form file. The system
  // handle is in the data module, so the two lists of all instances get
  // their root here, and all three their system.
  lhaProjects.StaticSystemHandle := FData.BoldSystemHandle1;
  lhaProjects.RootHandle := FData.BoldSystemHandle1;
  lhaProjectTasks.StaticSystemHandle := FData.BoldSystemHandle1;
  lhaTasks.StaticSystemHandle := FData.BoldSystemHandle1;
  lhaTasks.RootHandle := FData.BoldSystemHandle1;
  OpenDataSets;

  Application.OnIdle := AppIdle;
  ShowStatus;
  SelfTestTrace('grids bound');
end;

procedure TfrmMasterDetailMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  // Details before masters, datasets before their handles, all before the system
  FreeAndNil(bdsTasks);
  FreeAndNil(bdsProjectTasks);
  FreeAndNil(bdsProjects);
  FreeAndNil(lhaTasks);
  FreeAndNil(lhaProjectTasks);
  FreeAndNil(lhaProjects);
  if Assigned(FData) and FData.IsOpen then
    FData.CloseSystem(False);
  if FSelfTest then
    System.SysUtils.DeleteFile(FDatabase);
end;

procedure TfrmMasterDetailMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if FSelfTest or not Assigned(FData) or not FData.IsOpen or not FData.BoldSystem.BoldDirty then
    Exit;
  case TDialogServiceSync.MessageDialog('There are unsaved changes. Save them before exit?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbYes, 0) of
    mrYes: FData.Save;
    mrNo: FData.BoldSystem.Discard;
  else
    CanClose := False;
  end;
end;

procedure TfrmMasterDetailMain.OpenData;
begin
  FData := TdmMasterDetail.Create(Self);
  FData.LoadModelFromFile(ModelFileName);
  SelfTestTrace('model loaded from ' + ModelFileName);
  FData.OpenSystem(FDatabase);
  SystemOpened;
end;

{ New objects are named Project1, Task1 and so on from a counter in the model
  code (TProject.CompleteCreate). The counter continues from the highest
  number already in the database, as in the VCL demo's HandleSystemOpened. }
procedure TfrmMasterDetailMain.SystemOpened;
begin
  TProject.InitializeCounter(FData.BoldSystem);
  TTask.InitializeCounter(FData.BoldSystem);
end;

procedure TfrmMasterDetailMain.OpenDataSets;
begin
  // Each dataset opens on its handle, and its grid fills through its binding
  bdsProjects.Open;
  bdsProjectTasks.Open;
  bdsTasks.Open;
end;

procedure TfrmMasterDetailMain.CloseDataSets;
begin
  bdsTasks.Close;
  bdsProjectTasks.Close;
  bdsProjects.Close;
end;

{ Closes the system and opens it again on the same file, or on a new empty
  one after the old one is deleted }
procedure TfrmMasterDetailMain.ReopenSystem(ADropDatabase: Boolean);
begin
  CloseDataSets;
  if ADropDatabase then
    FData.DropDatabase
  else
    FData.CloseSystem(False);
  FData.OpenSystem(FDatabase);
  SystemOpened;
  OpenDataSets;
  ShowStatus;
end;

{ Deletes every project, as the VCL demo's Clear All Data does. The list from
  EvaluateExpressionAsNewElement belongs to the caller, and it is walked
  backwards because it shrinks as the projects go. Nothing is written until
  Save Changes. }
procedure TfrmMasterDetailMain.ClearAllData;
var
  List: TBoldList;
  i: Integer;
begin
  List := FData.BoldSystem.EvaluateExpressionAsNewElement('Project.allInstances') as TBoldList;
  try
    for i := List.Count - 1 downto 0 do
      (List[i] as TBoldObject).Delete;
  finally
    List.Free;
  end;
end;

procedure TfrmMasterDetailMain.ClearClick(Sender: TObject);
begin
  if TDialogServiceSync.MessageDialog('Really delete all projects?', TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0) = mrYes then
    ClearAllData;
end;

procedure TfrmMasterDetailMain.SaveClick(Sender: TObject);
begin
  FData.Save;
  ShowStatus;
end;

procedure TfrmMasterDetailMain.DropDatabaseClick(Sender: TObject);
begin
  if TDialogServiceSync.MessageDialog('Drop the database ' + FData.Database + '?' + sLineBreak +
    'This deletes all data, also what is not saved, and starts an empty database.',
    TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0) = mrYes then
    ReopenSystem(True);
end;

procedure TfrmMasterDetailMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
  // Brings the Bold-aware followers, TBoldDataSet's among them, up to date
  TBoldQueueable.DisplayAll;
  ShowStatus;
  Done := True;
end;

procedure TfrmMasterDetailMain.ShowStatus;
var
  IsOpen: Boolean;
begin
  if not Assigned(lblStatus) or not Assigned(FData) then
    Exit;
  IsOpen := FData.IsOpen;
  if IsOpen then
  begin
    lblProjectCount.Text := FData.BoldSystem.EvaluateExpressionAsString(cProjectCountExpression);
    lblStatus.Text := Format('%s  -  FireDAC, SQLite  -  Bold system active  -  %d projects, %d tasks%s',
      [FData.Database,
       FData.BoldSystem.EvaluateExpressionAsInteger('Project.allInstances->size'),
       FData.BoldSystem.EvaluateExpressionAsInteger('Task.allInstances->size'),
       IfThen(FData.BoldSystem.BoldDirty, '  -  unsaved changes', '')]);
  end
  else
  begin
    lblProjectCount.Text := '';
    lblStatus.Text := FData.Database + '  -  Bold system inactive';
  end;
  btnSave.Enabled := IsOpen;
  btnClear.Enabled := IsOpen;
end;

procedure TfrmMasterDetailMain.SelfTestException(Sender: TObject; E: Exception);
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

procedure TfrmMasterDetailMain.Settle;
var
  Start: Cardinal;
begin
  Start := TThread.GetTickCount;
  repeat
    TBoldQueueable.DisplayAll;
    CheckSynchronize(10);
    Application.ProcessMessages;
  until TThread.GetTickCount - Start > 300;
  ShowStatus;
end;

{ Raises for the self-test's check of the call stack }
procedure RaiseForTheSelfTest;
begin
  raise Exception.Create('raised for the self-test');
end;

function TfrmMasterDetailMain.RunSelfTest: Integer;
var
  Log: TStringList;
  Failures: Integer;
  Report: string;
  Memory: TBoldMemoryLogSink;
  MemoryRef: IBoldLogSink;

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

  { The texts of one column of a grid, top to bottom, separated by commas }
  function Column(AGrid: TStringGrid; const AHeader: string): string;
  var
    Col, Row: Integer;
  begin
    Result := '';
    for Col := 0 to AGrid.ColumnCount - 1 do
      if AGrid.Columns[Col].Header = AHeader then
      begin
        for Row := 0 to AGrid.RowCount - 1 do
        begin
          if Row > 0 then
            Result := Result + ',';
          Result := Result + AGrid.Cells[Col, Row];
        end;
        Exit;
      end;
    Result := '(no column ' + AHeader + ')';
  end;

  { A new row through a navigator, as its Insert and Post buttons make it }
  procedure InsertThrough(ANavigator: TBindNavigator);
  begin
    ANavigator.BtnClick(TNavigateButton.nbInsert);
    ANavigator.BtnClick(TNavigateButton.nbPost);
    Settle;
  end;

  procedure SelectProject(const AName: string);
  begin
    bdsProjects.First;
    while not bdsProjects.Eof and (bdsProjects.FieldByName('Name').AsString <> AName) do
      bdsProjects.Next;
    Settle;
  end;

var
  Project1: TProject;
  Task1: TTask;
  Expected: string;
begin
  Log := TStringList.Create;
  Failures := 0;
  try
    Settle;
    SelfTestTrace('settled');

    // An empty database
    Check('an empty database: no projects', (grdProjects.RowCount = 0) and (lblProjectCount.Text = '(0)'),
      Format('%d rows, count %s', [grdProjects.RowCount, lblProjectCount.Text]));

    // Two projects through the projects navigator; the model names them
    InsertThrough(navProjects);
    InsertThrough(navProjects);
    Check('two projects through the navigator', Column(grdProjects, 'Name') = 'Project1,Project2',
      Column(grdProjects, 'Name'));
    Check('the count label, the OCL of the VCL demo''s TBoldLabel', lblProjectCount.Text = '(2)',
      lblProjectCount.Text);

    // Two tasks for Project1 and one for Project2 through the navigator of
    // the detail grid: the dataset asks the list of its handle, the tasks of
    // the selected project, for the new object
    SelectProject('Project1');
    InsertThrough(navProjectTasks);
    InsertThrough(navProjectTasks);
    SelectProject('Project2');
    InsertThrough(navProjectTasks);
    // And one task without a project, through the navigator of all tasks
    InsertThrough(navTasks);
    SelfTestTrace('objects made');

    Check('the detail grid shows the tasks of Project2', Column(grdProjectTasks, 'Title') = 'Task3',
      Column(grdProjectTasks, 'Title'));
    SelectProject('Project1');
    Check('after another project is selected, the tasks of Project1', Column(grdProjectTasks, 'Title') = 'Task1,Task2',
      Column(grdProjectTasks, 'Title'));
    Check('the grid of all tasks shows all four', Column(grdTasks, 'Title') = 'Task1,Task2,Task3,Task4',
      Column(grdTasks, 'Title'));
    Check('with the project of each, none for the fourth', Column(grdTasks, 'Project') = 'Project1,Project1,Project2,',
      Column(grdTasks, 'Project'));

    // The derived dueDate: the end date of the project plus 7 days
    Project1 := bdsProjects.ActiveBoldObject as TProject;
    Expected := DateToStr(Project1.EndDate + 7);
    Check('the derived due date of a task', Column(grdProjectTasks, 'Due Date') = Expected + ',' + Expected,
      Column(grdProjectTasks, 'Due Date') + ', expected ' + Expected);

    // Edited as the grids edit: through the dataset of the grid
    bdsProjects.Edit;
    bdsProjects.FieldByName('Name').AsString := 'Apollo';
    bdsProjects.Post;
    Settle;
    Check('a renamed project shows in the grid of all tasks', Column(grdTasks, 'Project') = 'Apollo,Apollo,Project2,',
      Column(grdTasks, 'Project'));
    bdsProjectTasks.First;
    Task1 := bdsProjectTasks.ActiveBoldObject as TTask;
    bdsProjectTasks.Edit;
    bdsProjectTasks.FieldByName('Done').AsBoolean := True;
    bdsProjectTasks.Post;
    Settle;
    Check('a task marked done in the detail grid', Task1.IsCompleted and
      StartsText('True', Column(grdTasks, 'Done')), Column(grdTasks, 'Done'));
    Check('the status names the unsaved changes', ContainsText(lblStatus.Text, 'unsaved changes'), lblStatus.Text);

    // Save Changes, then the system closed and opened again on the file
    SaveClick(nil);
    Check('Save Changes', not FData.BoldSystem.BoldDirty and not ContainsText(lblStatus.Text, 'unsaved'),
      lblStatus.Text);
    ReopenSystem(False);
    Settle;
    SelfTestTrace('reopened');
    Check('after reopening: the saved projects', Column(grdProjects, 'Name') = 'Apollo,Project2',
      Column(grdProjects, 'Name'));
    Check('after reopening: the saved tasks', Column(grdTasks, 'Project') = 'Apollo,Apollo,Project2,',
      Column(grdTasks, 'Project'));
    InsertThrough(navProjects);
    Check('the name counter continues from the database', Column(grdProjects, 'Name') = 'Apollo,Project2,Project3',
      Column(grdProjects, 'Name'));

    // Clear All Data, without its question
    ClearAllData;
    Settle;
    Check('Clear All Data: no projects', (grdProjects.RowCount = 0) and (lblProjectCount.Text = '(0)'),
      Format('%d rows, count %s', [grdProjects.RowCount, lblProjectCount.Text]));
    Check('Clear All Data: no task belongs to a deleted project', ReplaceStr(Column(grdTasks, 'Project'), ',', '') = '',
      IntToStr(grdTasks.RowCount) + ' tasks, projects "' + Column(grdTasks, 'Project') + '"');

    // Drop Database, without its question: an empty database, and the
    // counter starts again
    ReopenSystem(True);
    Settle;
    SelfTestTrace('dropped');
    Check('Drop Database: empty', (grdProjects.RowCount = 0) and (grdTasks.RowCount = 0) and
      not FData.BoldSystem.BoldDirty, Format('%d projects, %d tasks', [grdProjects.RowCount, grdTasks.RowCount]));
    InsertThrough(navProjects);
    Check('Drop Database: the counter starts again', Column(grdProjects, 'Name') = 'Project1',
      Column(grdProjects, 'Name'));

    // A task row is open while Clear All Data deletes its project, and the
    // new task with it, and then the row is cancelled. The dataset reached
    // the destroyed task (access violation in TBoldObject.InternalDiscard).
    bdsProjects.First;
    Settle;
    navProjectTasks.BtnClick(TNavigateButton.nbInsert);
    bdsProjectTasks.FieldByName('Title').AsString := 'typed';
    ClearAllData;
    navProjectTasks.BtnClick(TNavigateButton.nbCancel);
    Settle;
    Check('a task row open while Clear All Data deletes it, then cancelled',
      (bdsProjectTasks.State = dsBrowse) and (grdProjects.RowCount = 0) and (grdProjectTasks.RowCount = 0),
      Format('state %d, %d projects, %d task rows', [Ord(bdsProjectTasks.State), grdProjects.RowCount, grdProjectTasks.RowCount]));
    // The same when the changes are discarded, as No does when the window
    // is closed with unsaved changes
    InsertThrough(navProjects);
    bdsProjects.First;
    Settle;
    navProjectTasks.BtnClick(TNavigateButton.nbInsert);
    bdsProjectTasks.FieldByName('Title').AsString := 'typed';
    FData.BoldSystem.Discard;
    navProjectTasks.BtnClick(TNavigateButton.nbCancel);
    Settle;
    Check('a task row open while the changes are discarded, then cancelled',
      (bdsProjectTasks.State = dsBrowse) and (grdProjects.RowCount = 0) and not FData.BoldSystem.BoldDirty,
      Format('state %d, %d projects', [Ord(bdsProjectTasks.State), grdProjects.RowCount]));

    // The report of an exception (#130)
    Report := '';
    try
      RaiseForTheSelfTest;
    except
      on E: Exception do
        Report := BoldExceptionReport(E);
    end;
    // With Stacker linked it names the routine that raised it; without a
    // provider it gives the address
    {$IFDEF BOLD_STACKER}
    Check('the report of an exception names the routine that raised it (Stacker)',
      Pos('RAISEFORTHESELFTEST', UpperCase(Report)) > 0, Report);
    {$ELSE}
    Check('the report of an exception gives where it was raised (no provider)',
      Pos('Raised at $', Report) > 0, Report);
    {$ENDIF}

    // An exception in the event loop goes to Bold's log with its call stack,
    // then to the program's own handler, here SelfTestException: the hook of
    // BoldExceptionLogFMX took that handler over when the event loop started
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
  Log.Add(IntToStr(Failures) + ' failed');
  SelfTestTrace(Log.Text);
  Log.Free;
  Result := Failures;
end;

end.
