{$INCLUDE bold.inc}
{-----------------------------------------------------------------------------
  OclWorkbenchForm

  A bench for writing OCL against a live Bold system and seeing what comes back.

  It exists to show three different ways Bold puts OCL to work, all of which
  share the same expression text:

    1. Direct evaluation. TBoldElement.EvaluateExpression takes a string and an
       empty TBoldIndirectElement and fills it in. This is the general path: the
       result can be a list, an object, a number, a string or nothing at all,
       and the workbench reports what it actually got.

    2. Handle-driven evaluation. Assigning the same string to
       TBoldListHandle.Expression makes a grid follow the result and keep
       following it. Edit an object and the grid re-evaluates on its own, which
       direct evaluation does not do.

    3. Editing the expression in Bold's own editor, TBoldOclPropEditForm from
       BoldOclPropEditor.pas. That dialog type-checks against the model as you
       type, offers member and operation help, and is the same dialog the IDE
       shows behind the ellipsis on any OCL property.

  A fourth use of the same editor is on the Tools menu: installing
  TBoldOclGraphicRTDebugger makes Bold pop that editor up whenever an
  expression fails at runtime, and retry with whatever you corrected it to.

  TOclExplorerForm from BoldOCLExplorer.pas is the fifth, but it draws its
  result grids with DevExpress, so it is only compiled into the DevEx build
  configuration. See the README.
-----------------------------------------------------------------------------}
unit OclWorkbenchForm;

interface

uses
  System.Actions,
  System.Classes,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Grids,
  Vcl.Menus,
  Vcl.StdCtrls,

  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldElements,
  BoldGrid,
  BoldHandles,
  BoldListBox,
  BoldListHandle,
  BoldOclVariables,
  BoldRootedHandles,
  BoldSubscription,
  BoldSystem,
  BoldSystemHandle,

  OclWorkbenchEngines;

type
  TfrmOclWorkbench = class(TForm)
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miSeed: TMenuItem;
    miSave: TMenuItem;
    miFileSep: TMenuItem;
    miExit: TMenuItem;
    miOpen: TMenuItem;
    miClose: TMenuItem;
    miOpenSep: TMenuItem;
    miClear: TMenuItem;
    miDatabase: TMenuItem;
    miEngineSep: TMenuItem;
    miCheckCurrent: TMenuItem;
    actCheckPreconditions: TAction;
    miTools: TMenuItem;
    miEditOcl: TMenuItem;
    miExplorer: TMenuItem;
    miToolsSep: TMenuItem;
    miRtDebugger: TMenuItem;
    Actions: TActionList;
    actOpenSystem: TAction;
    actCloseSystem: TAction;
    actEvaluate: TAction;
    actEditOcl: TAction;
    actExplorer: TAction;
    actSeed: TAction;
    actSave: TAction;
    actExit: TAction;
    actRtDebugger: TAction;
    actDescribeColumns: TAction;
    actClearData: TAction;

    pnlStatus: TPanel;
    lblStatus: TLabel;
    pnlDbState: TPanel;
    pnlSaveState: TPanel;

    pnlLeft: TPanel;
    pnlSamplesHeader: TPanel;
    lblSamples: TLabel;
    tvSamples: TTreeView;
    splContext: TSplitter;
    pnlContext: TPanel;
    pnlContextHeader: TPanel;
    lblContext: TLabel;
    lbPeople: TBoldListBox;

    splLeft: TSplitter;

    pnlMessages: TPanel;
    pnlMessagesHeader: TPanel;
    lblMessages: TLabel;
    memMessages: TMemo;

    splMessages: TSplitter;

    pnlExpression: TPanel;
    pnlExpressionHeader: TPanel;
    lblExpression: TLabel;
    pnlExpressionButtons: TPanel;
    btnEvaluate: TButton;
    btnEditOcl: TButton;
    cbEvaluateInPS: TCheckBox;
    pnlComment: TPanel;
    lblComment: TLabel;
    memExpression: TMemo;

    splExpression: TSplitter;

    pcResult: TPageControl;
    tsValue: TTabSheet;
    memValue: TMemo;
    tsLive: TTabSheet;
    pnlLiveHeader: TPanel;
    lblLive: TLabel;
    lblDerivedLegend: TLabel;
    btnDescribeColumns: TButton;
    grdResult: TBoldGrid;

    lhaResult: TBoldListHandle;
    lhaPeople: TBoldListHandle;
    OclVariables: TBoldOclVariables;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actClearDataExecute(Sender: TObject);
    procedure tvSamplesChange(Sender: TObject; Node: TTreeNode);
    procedure actEvaluateExecute(Sender: TObject);
    procedure actEditOclExecute(Sender: TObject);
    procedure actExplorerExecute(Sender: TObject);
    procedure actSeedExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actRtDebuggerExecute(Sender: TObject);
    procedure actDescribeColumnsExecute(Sender: TObject);
    procedure actOpenSystemExecute(Sender: TObject);
    procedure actCloseSystemExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actCheckPreconditionsExecute(Sender: TObject);
  private
    procedure BuildEngineMenu;
    procedure EngineItemClick(Sender: TObject);
    procedure UpdateEngineMenu;
    procedure ReportPreconditions(const AReport: TPreconditionReport);
    procedure SwitchToEngine(AEngine: TEngineId);
    procedure LoadSampleTree;
    function ActiveSystem: TBoldSystem;
    function ContextType: TBoldElementTypeInfo;
    function CurrentExpression: string;
    procedure EvaluateCurrentExpression;
    procedure DescribeElement(AElement: TBoldElement; ALines: TStrings);
    function CanEvaluateInPS(const AExpression: string): Boolean;
    procedure FollowAsLiveList(const AExpression: string; AIsList, AInPS: Boolean);
    function DataColumn(AIndex: Integer): TBoldGridColumn;
    procedure SetSpareDataColumns(AFirstUnused: Integer);
    procedure SetDataColumn(AIndex: Integer;
      const AExpression, ACaption: string; AWidth: Integer);
    procedure SetGenericColumns;
    procedure SetColumnsFromType(AType: TBoldElementTypeInfo);
    procedure Log(const AText: string);
    procedure UpdateStatus;
    procedure UpdateIndicators;
    function DatabaseFileMissing: Boolean;
    procedure HandleSystemOpened(Sender: TObject);
    procedure HandleSystemClosed(Sender: TObject);
  end;

var
  frmOclWorkbench: TfrmOclWorkbench;

implementation

uses
  System.SysUtils,
  System.UITypes,
  Vcl.Dialogs,
  Vcl.Graphics,

  BoldAttributes,
  BoldOcl,
  BoldOclPropEditor,
  BoldOCLGraphicRTDebug,
  BoldSystemRT,

  DemoDataModule,
  OclWorkbenchSamples,
  BoldOCLExplorer;

{$R *.dfm}

resourcestring
  sNoSystem = 'The Bold system is not open. Use File, Open system.';
  sEmptyExpression = 'Type an OCL expression, or pick one from the tree on the left.';

{ ---------------------------------------------------------------------------
  Small helpers
  --------------------------------------------------------------------------- }

{ A readable name for a type. List types are reported by their element type,
  because "a list of Person" is more use than the list class name. }
function TypeName(AType: TBoldElementTypeInfo): string;
begin
  if AType = nil then
    Exit('(unknown)');
  if AType is TBoldListTypeInfo then
  begin
    if TBoldListTypeInfo(AType).ListElementTypeInfo <> nil then
      Result := 'list of ' + TBoldListTypeInfo(AType).ListElementTypeInfo.ExpressionName
    else
      Result := 'list';
  end
  else
    Result := AType.ExpressionName;
end;

{ ---------------------------------------------------------------------------
  Form lifetime
  --------------------------------------------------------------------------- }

procedure TfrmOclWorkbench.FormCreate(Sender: TObject);
begin
  LoadSampleTree;
  BuildEngineMenu;

  dmDemo.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
  dmDemo.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;

  // The variable "current" resolves to whichever person is selected in the list
  // at the lower left. UseListElement = True is what makes it the current
  // element rather than the whole list. The same TBoldExternalVariableList is
  // handed to the evaluator and to the OCL editor, so the editor can type-check
  // expressions that mention it.
  OclVariables.AddVariable('current', lhaPeople, True);

  dmDemo.OpenSystem;
end;

{ Bold tracks every modified, created and deleted object in DirtyObjects, so the
  unsaved-changes question needs no bookkeeping of our own: the list is the
  answer. Cancel leaves the form open, which is why this belongs in
  OnCloseQuery rather than OnClose. }
procedure TfrmOclWorkbench.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  DirtyCount: Integer;
begin
  CanClose := True;

  if ActiveSystem <> nil then
  begin
    DirtyCount := ActiveSystem.DirtyObjects.Count;
    if DirtyCount > 0 then
      case MessageDlg(Format(
        '%d object(s) have been created or changed and are not yet in %s.' +
        sLineBreak + sLineBreak +
        'Save them before closing?',
        [DirtyCount, dmDemo.DatabaseName]),
        mtWarning, [mbYes, mbNo, mbCancel], 0) of
        mrYes:
          begin
            ActiveSystem.UpdateDatabase;
            Log('Saved on the way out.');
          end;
        mrNo:
          Log('Closed with ' + IntToStr(DirtyCount) + ' unsaved object(s).');
        mrCancel:
          CanClose := False;
      end;
  end;

  // Leaving the runtime debugger installed past the end of the form would leave
  // a dangling global, so take it down here.
  if CanClose then
    FreeAndNil(BoldOCLRTDebugger);
end;

{ The data module is created before this form and therefore destroyed after it,
  and its TBoldActivateSystemAction still holds the two callbacks installed in
  FormCreate. Its own destructor calls CloseSystem, which fires SystemClosed, so
  leaving the callbacks pointing at a freed form crashes on shutdown inside
  TBoldListHandle.SetExpression. Unhooking them here is the fix. }
procedure TfrmOclWorkbench.FormDestroy(Sender: TObject);
begin
  if Assigned(dmDemo) and Assigned(dmDemo.BoldActivateSystemAction1) then
  begin
    dmDemo.BoldActivateSystemAction1.OnSystemOpened := nil;
    dmDemo.BoldActivateSystemAction1.OnSystemClosed := nil;
  end;

  // Belt and braces: the runtime debugger is a global pointing at a form-owned
  // dialog, so it must not outlive the form even if FormCloseQuery was skipped.
  FreeAndNil(BoldOCLRTDebugger);
end;

procedure TfrmOclWorkbench.HandleSystemOpened(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;
  Log('System opened against ' + dmDemo.DatabaseName + '.');
  if SampleDataIsEmpty(ActiveSystem) then
    Log('No data yet. Use File, Create sample data to populate the model.');
  UpdateStatus;
end;

procedure TfrmOclWorkbench.HandleSystemClosed(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;
  lhaResult.Expression := '';
  Log('System closed.');
  UpdateStatus;
end;

function TfrmOclWorkbench.ActiveSystem: TBoldSystem;
begin
  if dmDemo.BoldSystemHandle1.Active then
    Result := dmDemo.BoldSystemHandle1.System
  else
    Result := nil;
end;

{ The context an expression is written against. Every sample here is rooted in
  the system itself, which is why they all start with a class name. The static
  type is available even when the system is closed, so the OCL editor can still
  check syntax before anything is connected. }
function TfrmOclWorkbench.ContextType: TBoldElementTypeInfo;
begin
  if dmDemo.BoldSystemHandle1.Active then
    Result := dmDemo.BoldSystemHandle1.BoldType
  else
    Result := dmDemo.BoldSystemHandle1.StaticBoldType;
end;

procedure TfrmOclWorkbench.UpdateStatus;
var
  Parts: string;
begin
  Parts := Format('%s via %s, %s', [EngineCaption(EngineFromIni(dmDemo.ConfigFile)),
    dmDemo.PersistenceTypeStr, dmDemo.DatabaseName]);
  if ActiveSystem = nil then
    lblStatus.Caption := Parts + '  |  closed'
  else
    lblStatus.Caption := Format('%s  |  %s  |  %d dirty',
      [Parts, SampleDataSummary(ActiveSystem), ActiveSystem.DirtyObjects.Count]);
  UpdateIndicators;
end;

{ True when the chosen engine keeps its data in a file and that file is not
  there. Only file engines can be answered cheaply: for a server, the question
  needs a connection, which is the Database menu's job and not something to do
  on every idle tick. }
function TfrmOclWorkbench.DatabaseFileMissing: Boolean;
var
  Name: string;
begin
  Result := False;
  if not (EngineFromIni(dmDemo.ConfigFile) in [egSQLite, egFirebird, egXML]) then
    Exit;
  Name := dmDemo.DatabaseName;
  Result := (Name <> '') and not FileExists(ExpandFileName(Name));
end;

{ The two questions worth answering at a glance: is there a database behind this
  engine, and is everything in it. Both are shown as coloured panels rather than
  as text in the status line, because a colour is read without being looked for
  and "3 dirty" in a sentence is not. }
procedure TfrmOclWorkbench.UpdateIndicators;
const
  cGreen = TColor($00D8F0D8);
  cAmber = TColor($00A8E4FF);
  cRed   = TColor($00C8C8FF);
  cGrey  = TColor($00E8E8E8);
var
  Dirty: Integer;
begin
  // Database. An open system is proof the database exists and has a schema,
  // because Bold could not have opened it otherwise.
  if dmDemo.BoldSystemHandle1.Active then
  begin
    pnlDbState.Color := cGreen;
    pnlDbState.Caption := Format('%s database ready',
      [EngineCaption(EngineFromIni(dmDemo.ConfigFile))]);
  end
  else if DatabaseFileMissing then
  begin
    pnlDbState.Color := cRed;
    pnlDbState.Caption := Format('No %s database yet',
      [EngineCaption(EngineFromIni(dmDemo.ConfigFile))]);
  end
  else
  begin
    pnlDbState.Color := cAmber;
    pnlDbState.Caption := Format('%s not open',
      [EngineCaption(EngineFromIni(dmDemo.ConfigFile))]);
  end;

  // Persistence. DirtyObjects holds everything created, changed or deleted and
  // not yet written, so it is the whole answer.
  if ActiveSystem = nil then
  begin
    pnlSaveState.Color := cGrey;
    pnlSaveState.Caption := 'Nothing loaded';
  end
  else
  begin
    Dirty := ActiveSystem.DirtyObjects.Count;
    if Dirty = 0 then
    begin
      pnlSaveState.Color := cGreen;
      pnlSaveState.Caption := 'All data saved';
    end
    else
    begin
      pnlSaveState.Color := cAmber;
      pnlSaveState.Caption := Format('%d unsaved change(s)', [Dirty]);
    end;
  end;
end;

procedure TfrmOclWorkbench.Log(const AText: string);
begin
  memMessages.Lines.Add(FormatDateTime('hh:nn:ss  ', Now) + AText);
end;

{ ---------------------------------------------------------------------------
  The sample catalogue
  --------------------------------------------------------------------------- }

procedure TfrmOclWorkbench.LoadSampleTree;
var
  i: Integer;
  Sample: TOclSample;
  CategoryNode, Node: TTreeNode;
  CurrentCategory: string;
begin
  tvSamples.Items.BeginUpdate;
  try
    tvSamples.Items.Clear;
    CategoryNode := nil;
    CurrentCategory := '';
    for i := 0 to OclSampleCount - 1 do
    begin
      Sample := OclSample(i);
      if Sample.Category <> CurrentCategory then
      begin
        CurrentCategory := Sample.Category;
        CategoryNode := tvSamples.Items.Add(nil, CurrentCategory);
      end;
      Node := tvSamples.Items.AddChild(CategoryNode, Sample.Expression);
      // Data holds the index biased by one, because a nil Data has to stay
      // distinguishable from sample number zero.
      Node.Data := Pointer(NativeInt(i) + 1);
    end;
  finally
    tvSamples.Items.EndUpdate;
  end;
end;

procedure TfrmOclWorkbench.tvSamplesChange(Sender: TObject; Node: TTreeNode);
var
  Index: NativeInt;
  Sample: TOclSample;
begin
  if (Node = nil) or (Node.Data = nil) then
  begin
    lblComment.Caption := '';
    Exit;
  end;
  Index := NativeInt(Node.Data) - 1;
  Sample := OclSample(Index);
  memExpression.Text := Sample.Expression;
  lblComment.Caption := Sample.Comment;
end;

function TfrmOclWorkbench.CurrentExpression: string;
begin
  Result := Trim(memExpression.Text);
end;

{ ---------------------------------------------------------------------------
  Path 1: direct evaluation
  --------------------------------------------------------------------------- }

procedure TfrmOclWorkbench.actEvaluateExecute(Sender: TObject);
begin
  EvaluateCurrentExpression;
end;

procedure TfrmOclWorkbench.EvaluateCurrentExpression;
var
  Expression: string;
  Indirect: TBoldIndirectElement;
  InPS: Boolean;
begin
  memValue.Clear;

  Expression := CurrentExpression;
  if Expression = '' then
  begin
    Log(sEmptyExpression);
    Exit;
  end;
  if ActiveSystem = nil then
  begin
    Log(sNoSystem);
    Exit;
  end;

  InPS := cbEvaluateInPS.Checked and CanEvaluateInPS(Expression);

  // TBoldIndirectElement is Bold's out-parameter for an evaluation. It may end
  // up owning the result (a computed list, say) or merely referencing it (an
  // attribute that belongs to an object), which is why it has to be freed and
  // the value must not be.
  Indirect := TBoldIndirectElement.Create;
  try
    try
      ActiveSystem.EvaluateExpression(Expression, Indirect,
        InPS, OclVariables.VariableList);

      DescribeElement(Indirect.Value, memValue.Lines);
      FollowAsLiveList(Expression,
        (Indirect.Value <> nil) and (Indirect.Value is TBoldList), InPS);

      if InPS then
        Log('Evaluated in the persistent storage: ' + Expression)
      else
        Log('Evaluated in memory: ' + Expression);
    except
      on E: Exception do
      begin
        // The message from Bold's OCL layer names the offending token and the
        // position, so it is worth showing verbatim rather than summarising.
        memValue.Lines.Add(E.ClassName);
        memValue.Lines.Add(E.Message);
        Log('Failed: ' + E.Message);
        FollowAsLiveList('', False, False);
      end;
    end;
  finally
    Indirect.Free;
  end;

  UpdateStatus;
end;

procedure TfrmOclWorkbench.DescribeElement(AElement: TBoldElement;
  ALines: TStrings);
var
  List: TBoldList;
  i: Integer;
  Item: TBoldElement;
begin
  if AElement = nil then
  begin
    ALines.Add('nil');
    ALines.Add('');
    ALines.Add('An expression can legitimately evaluate to nothing, for');
    ALines.Add('instance when it navigates an unset single link.');
    Exit;
  end;

  ALines.Add('OCL type     : ' + TypeName(AElement.BoldType));
  ALines.Add('Delphi class : ' + AElement.ClassName);

  if AElement is TBoldList then
  begin
    List := TBoldList(AElement);
    ALines.Add(Format('Elements     : %d', [List.Count]));
    ALines.Add('');
    for i := 0 to List.Count - 1 do
    begin
      Item := List[i];
      if Item = nil then
        ALines.Add(Format('%4d  nil', [i + 1]))
      else
        ALines.Add(Format('%4d  %s', [i + 1, Item.AsString]));
    end;
  end
  else
  begin
    ALines.Add('');
    ALines.Add('Value        : ' + AElement.AsString);
  end;
end;

{ Whether an expression can be evaluated in the persistent storage is not a
  question the type system can answer.

  Asking for something that is not an object list is one way to fail, since the
  translation ends in a query for object ids, and the type does catch that:
  "endDate - startDate" yields a Collection(Date) and can never come back from
  SQL. But an expression can be a perfectly good object list and still have no
  translation. allLoadedObjects is registered as an object list, yet "loaded" is
  an in-memory notion the database knows nothing about, so there is no SQL
  symbol for it and there never could be.

  So the expression is tried rather than inspected. One evaluation, caught here
  where it can be reported calmly, and the message is Bold's own, which names
  the real cause far better than a guess made from the type. The cost is one
  query that would have been run anyway had it succeeded. }
function TfrmOclWorkbench.CanEvaluateInPS(const AExpression: string): Boolean;
var
  Indirect: TBoldIndirectElement;
  Reason: string;
begin
  Reason := '';
  Indirect := TBoldIndirectElement.Create;
  try
    try
      ActiveSystem.EvaluateExpression(AExpression, Indirect, True,
        OclVariables.VariableList);
    except
      on E: Exception do
        Reason := E.Message;
    end;
  finally
    Indirect.Free;
  end;

  Result := Reason = '';
  if not Result then
    Log('Not evaluated in the persistent storage, falling back to memory: ' +
      Reason);
end;

{ ---------------------------------------------------------------------------
  Path 2: the same expression on a list handle
  --------------------------------------------------------------------------- }

{ Assigning the expression to a TBoldListHandle is a different proposition from
  evaluating it once. The handle subscribes to everything the expression touched,
  so the grid re-evaluates by itself when the underlying objects change. Only
  list-valued expressions can drive a list handle, hence the guard. }
procedure TfrmOclWorkbench.FollowAsLiveList(const AExpression: string;
  AIsList, AInPS: Boolean);
begin
  if not AIsList then
  begin
    lhaResult.Expression := '';
    SetSpareDataColumns(-1);
    lblLive.Caption :=
      'Not a list. A list handle can only follow a list-valued expression.';
    Exit;
  end;

  lhaResult.EvaluateInPS := AInPS;
  lhaResult.Expression := AExpression;
  SetGenericColumns;
  lblLive.Caption := Format(
    'Live: %d row(s). The grid re-evaluates on its own when the objects change.',
    [lhaResult.Count]);
end;

{ Columns[0] belongs to the grid, not to us. TBoldCustomGrid.EnsureOneFixedCol
  reserves it as the fixed indicator column, with an empty expression and its own
  renderer, and re-creates it the moment the collection is emptied. So calling
  Columns.Clear makes the grid rebuild underneath the rebuild, and the column
  collection ends up disagreeing with the parallel controller list that
  TBoldGridColumn.Destroy deletes from by index. The symptom is an EListError
  from deep inside TBoldControllerList.

  The cure is to stop fighting it: never empty the collection, and only ever own
  the columns from index 1 upward. }
{ Hands back the Nth data column, creating it if the grid does not have it yet.

  Columns are created on demand and never destroyed. Column 0 belongs to the
  grid: TBoldCustomGrid.EnsureOneFixedCol reserves it as the fixed indicator and
  re-creates it whenever the collection is emptied. Worse, TBoldGridColumn.Create
  appends its controller to a parallel list while Destroy removes it by the
  column's collection index, and those two get out of step, which surfaces as
  EListError from TBoldControllerList deep inside a Free.

  Re-pointing an existing column costs nothing and avoids the whole question. A
  column that is no longer wanted is blanked by SetSpareDataColumns rather than
  destroyed. }
function TfrmOclWorkbench.DataColumn(AIndex: Integer): TBoldGridColumn;
begin
  while grdResult.Columns.Count <= AIndex + 1 do
    grdResult.AddColumn;
  Result := grdResult.Columns[AIndex + 1];
end;

{ Blanks every data column from AFirstUnused onwards, so leftovers from a wider
  result do not show. }
procedure TfrmOclWorkbench.SetSpareDataColumns(AFirstUnused: Integer);
var
  i: Integer;
begin
  for i := AFirstUnused + 1 to grdResult.Columns.Count - 1 do
    with grdResult.Columns[i] do
    begin
      BoldProperties.Renderer := nil;
      BoldProperties.Expression := '';
      Title.Caption := '';
      Width := 0;
    end;
end;

{ Points one data column at an expression. }
procedure TfrmOclWorkbench.SetDataColumn(AIndex: Integer;
  const AExpression, ACaption: string; AWidth: Integer);
begin
  with DataColumn(AIndex) do
  begin
    BoldProperties.Renderer := nil;
    BoldProperties.Expression := AExpression;
    Title.Caption := ACaption;
    Title.Font.Style := Title.Font.Style - [fsItalic];
    Width := AWidth;
  end;
end;

{ Three columns that work for a list of anything: the object identity, the
  runtime class, and the default string rendering. An empty expression means
  "render the element itself", which is how the last column copes with lists of
  Integers or Strings that have no members at all. }
procedure TfrmOclWorkbench.SetGenericColumns;
var
  ElementType: TBoldElementTypeInfo;
  Used: Integer;
begin
  SetDataColumn(0, '', 'Value', 320);
  Used := 1;

  // ListElementType, not BoldType. A list handle is a cursor: its BoldType is
  // the type of the CURRENT ELEMENT, never a list type, so testing BoldType for
  // listness is always false and silently costs you every column below.
  ElementType := lhaResult.ListElementType;

  // boldId and oclType only mean something for objects. Asking for them on a
  // list of Integers would raise rather than return blank.
  if ElementType is TBoldClassTypeInfo then
  begin
    SetDataColumn(1, 'boldId', 'BoldId', 70);
    SetDataColumn(2, 'oclType', 'Class', 130);
    Used := 3;
  end;

  SetSpareDataColumns(Used - 1);
end;

{ Walks the model rather than the data: given the class the result holds, add a
  column per attribute. This is the same reflection the IDE property editors use
  to populate their member lists. }
procedure TfrmOclWorkbench.SetColumnsFromType(AType: TBoldElementTypeInfo);
var
  ClassType: TBoldClassTypeInfo;
  Member: TBoldMemberRTInfo;
  Column: TBoldGridColumn;
  i, Derived, Used: Integer;
begin
  if not (AType is TBoldClassTypeInfo) then
  begin
    Log('The result does not hold objects, so there are no members to expand.');
    Exit;
  end;

  ClassType := TBoldClassTypeInfo(AType);
  Derived := 0;
  Used := 0;
  for i := 0 to ClassType.AllMembersCount - 1 do
  begin
    Member := ClassType.AllMembers[i];
    // Single links render as the target object's AsString, which is useful.
    // Multi links would need a nested grid, so they are left out.
    if Member.IsMultiRole then
      Continue;

    Column := DataColumn(Used);
    Inc(Used);
    Column.BoldProperties.Renderer := nil;
    Column.BoldProperties.Expression := Member.ExpressionName;
    Column.Width := 110;
    Column.Title.Font.Style := Column.Title.Font.Style - [fsItalic];

    if Member.IsDerived then
    begin
      // UML writes a derived property with a leading slash, and Bold is a UML
      // tool, so the notation its own modelling language already defines beats
      // inventing one. Italic reinforces it for anyone who does not know the
      // convention.
      Inc(Derived);
      Column.Title.Caption := '/' + Member.ExpressionName;
      Column.Title.Font.Style := Column.Title.Font.Style + [fsItalic];
      // The derivation is the interesting part, and it does not fit in a
      // header, so it goes where there is room for it.
      if Member.DeriveExpression <> '' then
        Log(Format('  /%s is derived as: %s',
          [Member.ExpressionName, Member.DeriveExpression]))
      else
        Log(Format('  /%s is derived in Delphi code, not in OCL.',
          [Member.ExpressionName]));
    end
    else
      Column.Title.Caption := Member.ExpressionName;
  end;

  SetSpareDataColumns(Used - 1);

  Log(Format('Expanded %s into %d column(s), %d of them derived (marked with a ' +
    'leading slash, as UML writes them).',
    [ClassType.ExpressionName, Used, Derived]));
end;

procedure TfrmOclWorkbench.actDescribeColumnsExecute(Sender: TObject);
begin
  if lhaResult.Expression = '' then
  begin
    Log('Evaluate a list-valued expression first.');
    Exit;
  end;
  SetColumnsFromType(lhaResult.ListElementType);
end;

{ ---------------------------------------------------------------------------
  Path 3: Bold's own OCL editor
  --------------------------------------------------------------------------- }

{ TBoldOclPropEditForm is the dialog behind every OCL property in the IDE. It
  needs three things: the context type to check against, the expression to start
  from, and the variable list so that names like "current" are known to it. It
  checks the expression as you type and reports the result type, which is the
  part worth watching: it answers "would this parse" without touching any data. }
procedure TfrmOclWorkbench.actEditOclExecute(Sender: TObject);
var
  Editor: TBoldOclPropEditForm;
begin
  if ContextType = nil then
  begin
    Log('No model type information available yet.');
    Exit;
  end;

  Editor := TBoldOclPropEditForm.Create(Self);
  try
    Editor.Context := ContextType;
    Editor.OclExpr := CurrentExpression;
    Editor.Variables := OclVariables.VariableList;
    Editor.Caption := 'Edit OCL - checked against the DemoModel';

    if Editor.ShowModal = mrOK then
    begin
      memExpression.Text := Editor.OclExpr;
      Log('Expression accepted from the OCL editor.');
      EvaluateCurrentExpression;
    end
    else
      Log('OCL editor cancelled, expression left unchanged.');
  finally
    Editor.Free;
  end;
end;

{ ---------------------------------------------------------------------------
  Path 4: the same editor as a runtime repair dialog
  --------------------------------------------------------------------------- }

{ With TBoldOclGraphicRTDebugger installed in the global BoldOCLRTDebugger,
  every OCL failure anywhere in the application opens the editor with the broken
  expression loaded. Correct it and Bold retries with the corrected text, and
  remembers the correction for the rest of the session. Type something wrong
  into the expression memo and evaluate it to see this happen. }
procedure TfrmOclWorkbench.actRtDebuggerExecute(Sender: TObject);
begin
  actRtDebugger.Checked := not actRtDebugger.Checked;
  if actRtDebugger.Checked then
  begin
    BoldOCLRTDebugger := TBoldOclGraphicRTDebugger.Create;
    Log('Runtime OCL repair on. A failing expression now opens the editor.');
  end
  else
  begin
    FreeAndNil(BoldOCLRTDebugger);
    Log('Runtime OCL repair off. Failures are reported in this pane instead.');
  end;
end;

{ ---------------------------------------------------------------------------
  Path 5: the full OCL Explorer, DevExpress permitting
  --------------------------------------------------------------------------- }

procedure TfrmOclWorkbench.actExplorerExecute(Sender: TObject);
var
  Explorer: TOclExplorerForm;
begin
  if ActiveSystem = nil then
  begin
    Log(sNoSystem);
    Exit;
  end;
  // CreateWithSystem is the only supported way in: the form reads the system
  // from the handle before its own OnCreate runs.
  Explorer := TOclExplorerForm.CreateWithSystem(Self, dmDemo.BoldSystemHandle1);
  Explorer.Show;
  Log('OCL Explorer opened. It browses every class and drills down two levels.');
end;

{ ---------------------------------------------------------------------------
  Choosing the database engine
  --------------------------------------------------------------------------- }

{ The engine items are built from the enumeration rather than dropped in the
  designer, so adding an engine to OclWorkbenchEngines puts it on the menu with
  no further work. Tag carries the engine, which is what EngineItemClick reads
  back. }
procedure TfrmOclWorkbench.BuildEngineMenu;
var
  Engine: TEngineId;
  Item: TMenuItem;
  Position: Integer;
begin
  Position := 0;
  for Engine := Low(TEngineId) to High(TEngineId) do
  begin
    Item := TMenuItem.Create(Self);
    Item.Caption := EngineCaption(Engine);
    Item.Tag := Ord(Engine);
    Item.RadioItem := True;
    Item.GroupIndex := 1;
    Item.OnClick := EngineItemClick;
    miDatabase.Insert(Position, Item);
    Inc(Position);
  end;
  UpdateEngineMenu;
end;

{ Ticks whichever engine the .ini currently names. Reading it back from the file
  rather than remembering it means the menu still tells the truth if the .ini is
  edited by hand while the demo runs. }
procedure TfrmOclWorkbench.UpdateEngineMenu;
var
  Current: TEngineId;
  i: Integer;
  Item: TMenuItem;
begin
  Current := EngineFromIni(dmDemo.ConfigFile);

  // Every item is assigned, and the loop deliberately does not stop at the
  // match. VCL's TMenuItem.SetChecked does call TurnSiblingsOff for a radio
  // item, but only when the value actually changes, so an item that is already
  // checked clears nothing. Assigning all of them cannot leave two lit.
  for i := 0 to miDatabase.Count - 1 do
  begin
    Item := miDatabase.Items[i];
    // GroupIndex 1 is the engine group; the separator and the check item are 0.
    if Item.GroupIndex = 1 then
      Item.Checked := Item.Tag = Ord(Current);
  end;
  miDatabase.Caption := Format('&Database (%s)', [EngineCaption(Current)]);
end;

procedure TfrmOclWorkbench.EngineItemClick(Sender: TObject);
begin
  SwitchToEngine(TEngineId((Sender as TMenuItem).Tag));
end;

{ The full report goes to the Messages pane, which is monospaced and scrolls,
  and only the verdict goes in a dialog. A ten-line report in a message box is
  unreadable, and the detail is the part worth keeping. }
procedure TfrmOclWorkbench.ReportPreconditions(const AReport: TPreconditionReport);
var
  Line: string;
begin
  memMessages.Lines.Add('');
  for Line in AReport.AsText.Split([sLineBreak]) do
    memMessages.Lines.Add(Line);
end;

procedure TfrmOclWorkbench.SwitchToEngine(AEngine: TEngineId);
var
  Report: TPreconditionReport;
begin
  if EngineFromIni(dmDemo.ConfigFile) = AEngine then
  begin
    Log(EngineCaption(AEngine) + ' is already the selected engine.');
    UpdateEngineMenu;
    Exit;
  end;

  // Rungs 6 and 7 open a real connection, so this is not instant.
  Log('Checking preconditions for ' + EngineCaption(AEngine) + '...');
  Screen.Cursor := crHourGlass;
  try
    Report := CheckEngine(AEngine, dmDemo.ConfigFile);
  finally
    Screen.Cursor := crDefault;
  end;
  ReportPreconditions(Report);

  if Report.Failed then
  begin
    // Leave the .ini alone. Switching to an engine that cannot connect would
    // only trade a clear diagnosis for a broken application.
    UpdateEngineMenu;
    MessageDlg(
      'Cannot switch to ' + EngineCaption(AEngine) + '.' + sLineBreak +
      sLineBreak + Report.Summary + sLineBreak + sLineBreak +
      'The Messages pane lists every check and what to do about this one.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  if Report.Warned and (MessageDlg(
    EngineCaption(AEngine) + ' is usable, with warnings.' + sLineBreak +
    sLineBreak +
    'See the Messages pane for the details. Switch to it now?',
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
  begin
    UpdateEngineMenu;
    Exit;
  end;

  // ReloadConfiguration closes the system, which saves any pending changes and
  // empties the live grid through HandleSystemClosed.
  SetSpareDataColumns(-1);
  dmDemo.WriteEngineToIni(EnginePersistence(AEngine), EngineDbType(AEngine));
  dmDemo.ReloadConfiguration;
  Log('Switched to ' + EngineCaption(AEngine) + '.');

  dmDemo.OpenSystem;
  UpdateEngineMenu;
  UpdateStatus;
end;

procedure TfrmOclWorkbench.actCheckPreconditionsExecute(Sender: TObject);
var
  Current: TEngineId;
  Report: TPreconditionReport;
begin
  Current := EngineFromIni(dmDemo.ConfigFile);
  Log('Checking preconditions for ' + EngineCaption(Current) + '...');
  Screen.Cursor := crHourGlass;
  try
    Report := CheckEngine(Current, dmDemo.ConfigFile);
  finally
    Screen.Cursor := crDefault;
  end;
  ReportPreconditions(Report);

  if Report.Failed then
    MessageDlg(Report.Summary + sLineBreak + sLineBreak +
      'The Messages pane lists every check and what to do about this one.',
      mtWarning, [mbOK], 0)
  else
    MessageDlg(Report.Summary + sLineBreak + sLineBreak +
      'The Messages pane lists every check.', mtInformation, [mbOK], 0);
end;

{ ---------------------------------------------------------------------------
  Data and persistence
  --------------------------------------------------------------------------- }

{ Opening and closing the system has to be reachable from the menu, not only from
  FormCreate. Declining the "create the database?" prompt at startup leaves the
  system closed, and without this the application would be a dead shell. }
procedure TfrmOclWorkbench.actOpenSystemExecute(Sender: TObject);
begin
  if dmDemo.BoldSystemHandle1.Active then
  begin
    Log('The system is already open.');
    Exit;
  end;
  try
    dmDemo.OpenSystem;
    if not dmDemo.BoldSystemHandle1.Active then
      Log('The system was not opened. The database has to exist first.');
  except
    on E: Exception do
      Log('Could not open the system: ' + E.Message);
  end;
  UpdateStatus;
end;

procedure TfrmOclWorkbench.actCloseSystemExecute(Sender: TObject);
begin
  if not dmDemo.BoldSystemHandle1.Active then
  begin
    Log('The system is already closed.');
    Exit;
  end;
  // CloseSystem saves any dirty objects on the way out, so say so rather than
  // letting it happen silently.
  if ActiveSystem.DirtyObjects.Count > 0 then
    Log(Format('Saving %d dirty object(s) before closing.',
      [ActiveSystem.DirtyObjects.Count]));
  lhaResult.Expression := '';
  SetSpareDataColumns(-1);
  dmDemo.CloseSystem;
  UpdateStatus;
end;

{ Keeps the menu honest: every action that needs a live system is greyed out
  while there is none, so the error message is a fallback rather than the way
  the user finds out. }
procedure TfrmOclWorkbench.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  Open: Boolean;
begin
  Open := dmDemo.BoldSystemHandle1.Active;
  actOpenSystem.Enabled := not Open;
  actCloseSystem.Enabled := Open;
  actEvaluate.Enabled := Open;
  actSeed.Enabled := Open;
  actClearData.Enabled := Open;
  // ActiveSystem is nil unless Open, so this relies on short-circuit evaluation,
  // which is the Delphi default. DirtyObjects is a plain list, cheap enough to
  // read on every idle; anything needing an OCL evaluation does not belong in an
  // OnUpdate handler.
  actSave.Enabled := Open and (ActiveSystem.DirtyObjects.Count > 0);
  // Cheap enough for idle: an Active check plus one list count. The file
  // check inside only runs while the system is closed.
  UpdateIndicators;
  actExplorer.Enabled := Open;
  actDescribeColumns.Enabled := Open;
  // The OCL editor only needs model type information, which exists whether or
  // not the system is open, so it stays available.
  actEditOcl.Enabled := ContextType <> nil;
  Handled := False;
end;

procedure TfrmOclWorkbench.actSeedExecute(Sender: TObject);
begin
  if ActiveSystem = nil then
  begin
    Log(sNoSystem);
    Exit;
  end;
  if not SampleDataIsEmpty(ActiveSystem) then
    if MessageDlg('The model already holds data. Add another set on top?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

  CreateSampleData(ActiveSystem);
  Log(Format('Sample data created in memory: %d dirty object(s).',
    [ActiveSystem.DirtyObjects.Count]));
  UpdateStatus;

  // The model's classes are persistent, so these objects become rows as soon as
  // UpdateDatabase runs, and they are loaded again on the next run. Until then
  // they exist only in this process. Offering the save here keeps that boundary
  // visible instead of burying it.
  if MessageDlg('Sample data created in memory.' + sLineBreak + sLineBreak +
    'Write it to ' + dmDemo.DatabaseName + ' now? Until you do, it is lost ' +
    'when the application closes.', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ActiveSystem.UpdateDatabase;
    Log('Saved. The data will be there on the next run.');
    UpdateStatus;
  end;
end;

{ Deletes every object in the model and writes the deletions through.

  Two details make this less obvious than it looks. Deleting one end of an
  association also deletes the link objects hanging off it, so an object can
  already be gone by the time the loop reaches it: BoldObjectExists is the guard.
  And the list that allInstances returns is the live class extent, which shrinks
  underneath the loop, so the references are copied into a snapshot first. A
  deleted TBoldObject stays valid in memory until the transaction completes,
  which is what makes the snapshot safe to hold. }
procedure TfrmOclWorkbench.actClearDataExecute(Sender: TObject);
var
  Indirect: TBoldIndirectElement;
  All: TBoldObjectList;
  Snapshot: array of TBoldObject;
  i, Deleted: Integer;
begin
  if ActiveSystem = nil then
  begin
    Log(sNoSystem);
    Exit;
  end;

  if ActiveSystem.EvaluateExpressionAsBoolean(
    'BusinessClassesRoot.allInstances->isEmpty') then
  begin
    Log('The model is already empty, nothing to delete.');
    Exit;
  end;

  if MessageDlg(Format(
    'Delete all %s from %s?' + sLineBreak + sLineBreak +
    'This cannot be undone. You can rebuild the population afterwards with ' +
    'File, Create sample data.',
    [SampleDataSummary(ActiveSystem), dmDemo.DatabaseName]),
    mtWarning, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Stop the grid following a list whose objects are about to be deleted.
  lhaResult.Expression := '';
  SetSpareDataColumns(-1);

  Deleted := 0;
  ActiveSystem.StartTransaction;
  try
    Indirect := TBoldIndirectElement.Create;
    try
      ActiveSystem.EvaluateExpression('BusinessClassesRoot.allInstances', Indirect);
      All := Indirect.Value as TBoldObjectList;
      SetLength(Snapshot, All.Count);
      for i := 0 to All.Count - 1 do
        Snapshot[i] := All[i] as TBoldObject;
    finally
      Indirect.Free;
    end;

    for i := 0 to High(Snapshot) do
      if Snapshot[i].BoldObjectExists then
      begin
        Snapshot[i].Delete;
        Inc(Deleted);
      end;

    ActiveSystem.CommitTransaction;
  except
    on E: Exception do
    begin
      ActiveSystem.RollbackTransaction;
      Log('Clear failed, nothing was deleted: ' + E.Message);
      UpdateStatus;
      Exit;
    end;
  end;

  ActiveSystem.UpdateDatabase;
  Log(Format('Deleted %d object(s) and saved. The model is now empty.', [Deleted]));
  memValue.Clear;
  lblLive.Caption :=
    'Evaluate a list-valued expression to put a list handle behind this grid.';
  UpdateStatus;
end;

procedure TfrmOclWorkbench.actSaveExecute(Sender: TObject);
begin
  if ActiveSystem = nil then
  begin
    Log(sNoSystem);
    Exit;
  end;
  ActiveSystem.UpdateDatabase;
  Log('Saved.');
  UpdateStatus;
end;

procedure TfrmOclWorkbench.actExitExecute(Sender: TObject);
begin
  Close;
end;

end.
