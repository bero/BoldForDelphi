unit BoldOCLExplorer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Grids, ExtCtrls, ActnList, Menus, System.Actions,
  BoldListBox, BoldSubscription, BoldHandles, BoldRootedHandles,
  BoldAbstractListHandle, BoldCursorHandle, BoldListHandle, BoldEdit,
  BoldExpressionHandle, BoldOclVariables, BoldSystem, BoldHandleAction,
  BoldActions, BoldSystemHandle, BoldPlaceableListSubscriber, BoldControlPack,
  BoldElements, BoldStringControlPack, BoldDerivedHandle, BoldRawSQLHandle,
  BoldGrid;

type
  TOclExplorerForm = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    List2EditOCLButton: TButton;
    List2OCLMemo: TMemo;
    Panel4: TPanel;
    Panel5: TPanel;
    Label2: TLabel;
    List1Handle: TBoldListHandle;
    Panel8: TPanel;
    Splitter3: TSplitter;
    Panel9: TPanel;
    Panel10: TPanel;
    List1EditOCLButton: TButton;
    List1OCLMemo: TMemo;
    Panel11: TPanel;
    Panel12: TPanel;
    Label1: TLabel;
    List2Handle: TBoldListHandle;
    Splitter5: TSplitter;
    BoldVariableDefinition: TBoldOclVariables;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    ActionList1: TActionList;
    CloseApplicationAction: TAction;
    AboutAction: TAction;
    Close1: TMenuItem;
    ToolsMenu: TMenuItem;
    EditOCL1: TMenuItem;
    HelpMenu: TMenuItem;
    About1: TMenuItem;
    ShowDebuggerAction: TAction;
    EditList1OCL1: TMenuItem;
    EditList2OCL1: TMenuItem;
    EditList1Action: TAction;
    EditList2Action: TAction;
    ShowOCLSyntaxSummary: TAction;
    ShowOCLSummary1: TMenuItem;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    N1: TMenuItem;
    UpdateDB1: TMenuItem;
    cbEvaluateInPS2: TCheckBox;
    cbEvaluateInPS1: TCheckBox;
    LeftGrid: TBoldGrid;
    RightGrid: TBoldGrid;
    BoldPlaceableListSubscriber1: TBoldPlaceableListSubscriber;
    BoldPlaceableListSubscriber2: TBoldPlaceableListSubscriber;
    brObjects: TBoldAsStringRenderer;
    brIndex: TBoldAsStringRenderer;
    bfIsPersistent: TBoldAsStringRenderer;
    bfClassState: TBoldAsStringRenderer;
    brIds: TBoldAsStringRenderer;
    brIsAbstract: TBoldAsStringRenderer;
    brIsLinkClass: TBoldAsStringRenderer;
    procedure List2EditOCL(Sender: TObject);
    procedure List1EditOCL(Sender: TObject);
    procedure CloseApplicationActionExecute(Sender: TObject);
    procedure ShowDebuggerActionExecute(Sender: TObject);
    procedure BoldEditListActionPostExecute(Sender: TObject);
    procedure ShowOCLSyntaxSummaryExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbEvaluateInPS2Click(Sender: TObject);
    procedure cbEvaluateInPS1Click(Sender: TObject);
    procedure BoldPlaceableListSubscriber1AfterMakeUptoDate(
      Follower: TBoldFollower);
    procedure BoldPlaceableListSubscriber2AfterMakeUptoDate(
      Follower: TBoldFollower);
    function brObjectsGetAsString(aFollower: TBoldFollower): string;
    function brIndexGetAsString(aFollower: TBoldFollower): string;
    procedure brClassListSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    function bfIsPersistentGetAsString(aFollower: TBoldFollower): string;
    function bfClassStateGetAsString(aFollower: TBoldFollower): string;
    procedure bfClassStateSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    function brIdsGetAsString(AFollower: TBoldFollower): string;
    function brIsAbstractGetAsString(AFollower: TBoldFollower): string;
    function brIsLinkClassGetAsString(AFollower: TBoldFollower): string;
  private
    FBoldSystem: TBoldSystem;
    fSystemHandle: TBoldAbstractSystemHandle;
    { Private declarations }
    Procedure UpdateStatus;
    procedure SetColumnWidths;
    procedure ApplyEvaluateInPS(AHandle: TBoldListHandle; ABox: TCheckBox);
    procedure EditOCL(ListHandle: TBoldListHandle);
//    procedure ExportBoldListToStringList(BoldList: TBoldListHandle; StringList: TStrings);
    procedure SetBoldSystem(const Value: TBoldSystem);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    constructor CreateWithSystem(anOwner: TComponent; SystemHandle: TBoldSystemHandle);
    property BoldSystem: TBoldSystem read FBoldSystem write SetBoldSystem;
  end;

implementation

uses
//  BoldOclSymbolLister,
  BoldOclPropEditor,
  BoldSystemDebuggerForm,
  BoldObjectListControllers,
  BoldDefs, BoldSystemRT, BoldDBInterfaces;

{$R *.DFM}

procedure TOclExplorerForm.EditOCL(ListHandle: TBoldListHandle);
begin
  with TBoldOclPropEditForm.Create(Self) do
  try
    Context := ListHandle.RootHandle.BoldType;
    OclExpr := ListHandle.Expression;

    // Make sure that any variables we have added are visible to the OCL editor
    if assigned(ListHandle.Variables) then
      Variables := ListHandle.Variables.VariableList;

    if ShowModal = mrOK then
      ListHandle.Expression := OCLExpr;
  finally
    Free;
  end;
end;

procedure TOclExplorerForm.List2EditOCL(Sender: TObject);
begin
  EditOCL(List2Handle);
  UpdateStatus;
end;

procedure TOclExplorerForm.UpdateStatus;
var
  lTypeName: string;
begin
  if Assigned(List1Handle.BoldType) then
    lTypeName := List1Handle.BoldType.AsString
  else
    lTypeName := '';
  Label1.Caption := Format('%s (%d)', [lTypeName, List1Handle.count]);

  if Assigned(List2Handle.BoldType) then
    lTypeName := List2Handle.BoldType.AsString
  else
    lTypeName := '';
  if Assigned(List2Handle.BoldType) then  
    Label2.Caption := Format('%s (%d)', [List2Handle.BoldType.AsString, List2Handle.count])
  else
    Label2.Caption := '';
  List1OCLMemo.Text := AdjustLineBreaks(List1Handle.Expression);
//  ExportBoldListToStringList(List1Handle, List1ResultMemo.Lines);

  List2OCLMemo.Text := AdjustLineBreaks(List2Handle.Expression);
//  ExportBoldListToStringList(List2Handle, List2ResultMemo.Lines);
end;

procedure TOclExplorerForm.List1EditOCL(Sender: TObject);
begin
  EditOCL(List1Handle);
  UpdateStatus;
end;

(*procedure TOclExplorerForm.ExportBoldListToStringList(BoldList: TBoldListHandle;
    StringList: TStrings);
var Counter: Integer;
begin
try
  StringList.BeginUpdate;
  try
    StringList.Clear;
    StringList.Add('Type Information');
    StringList.Add('----------------');
    StringList.Add('List Type (AsString)    = ' + BoldList.ListType.AsString);
    StringList.Add('List Type (DelphiClass) = ' + BoldList.ListType.ClassName);
    if Assigned(BoldList.BoldType) then
    begin
      StringList.Add('Bold Type (AsString)    = ' + BoldList.BoldType.AsString);
      StringList.Add('Bold Type (DelphiClass) = ' + BoldList.BoldType.ClassName);
    end
    else
    begin
      StringList.Add('Bold Type (AsString)    = nil');
      StringList.Add('Bold Type (DelphiClass) = nil');
    end;

    if Assigned(BoldList.List) then
      StringList.Add('List Type (DelphiClass) = ' + BoldList.List.ClassName)
    else
      StringList.Add('List Type (DelphiClass) = nil');

    StringList.Add('');
    StringList.Add('Content Information');
    StringList.Add('-------------------');
    StringList.Add('Result Count = ' + IntToStr(BoldList.Count));
    if Assigned(BoldList.Value) then
    begin
      StringList.Add('Value (AsString)    = ' + BoldList.Value.AsString);
      StringList.Add('Value (DelphiClass) = ' + BoldList.Value.ClassName);
    end
    else
      StringList.Add('Value = nil');
{
    if assigned(BoldList.List) then
    begin
      StringList.Add('');
      StringList.Add('List Contents');
      StringList.Add('-------------------');
      for counter := 0 to  BoldList.List.Count -1 do
        StringList.Add(BoldList.List[counter].AsString);
    end;
}
  finally
    StringList.EndUpdate;
  end;
except
end;
end;
*)

procedure TOclExplorerForm.CloseApplicationActionExecute(Sender: TObject);
begin
  close;
end;

procedure TOclExplorerForm.ShowDebuggerActionExecute(Sender: TObject);
begin
  Assert(assigned(FBoldSystem));
  with TBoldSystemDebuggerFrm.CreateWithSystem(application,fSystemHandle.System) do
    show;
end;

procedure TOclExplorerForm.BoldEditListActionPostExecute(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TOclExplorerForm.ShowOCLSyntaxSummaryExecute(Sender: TObject);
begin
//  TOCLSyntaxForm.Create(self).Show;
end;

procedure TOclExplorerForm.SetBoldSystem(const Value: TBoldSystem);
begin
  FBoldSystem := Value;
end;

{ TBoldGridColumn.Width is declared "stored False" and its setter writes
  straight into the grid's ColWidths, which do not exist while the form is
  streaming. So the widths cannot live in the .dfm and are applied here, once
  the grid has its columns. }
procedure TOclExplorerForm.SetColumnWidths;

  procedure Widths(AGrid: TBoldGrid; const AWidths: array of Integer);
  var
    i: Integer;
  begin
    for i := 0 to High(AWidths) do
      if i < AGrid.Columns.Count then
        AGrid.Columns[i].Width := AWidths[i];
  end;

begin
  //        Index Class Objects Ids Persistent Abstract Link Loaded
  Widths(LeftGrid, [60, 298, 80, 80, 70, 70, 70, 70]);
  //         BoldId Class AsString
  Widths(RightGrid, [80, 140, 260]);
end;

procedure TOclExplorerForm.FormCreate(Sender: TObject);
begin
  if BoldSystem = nil then
    BoldSystem:= TBoldSystem.DefaultSystem;
  if FSystemHandle = nil then
    fSystemHandle:= TBoldAbstractSystemHandle.DefaultBoldSystemHandle;

  List1Handle.Expression  := BoldSystem.BoldSystemTypeInfo.RootClassTypeInfo.ExpressionName + '.allSubClasses';

  List1Handle.RootHandle:= fSystemHandle;
  BoldUpdateDBAction1.BoldSystemHandle := TBoldSystemHandle(fSystemHandle);
  SetColumnWidths;
  UpdateStatus;
end;

procedure TOclExplorerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TOclExplorerForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

constructor TOclExplorerForm.CreateWithSystem(anOwner: TComponent;
  SystemHandle: TBoldSystemHandle);
begin
  fSystemHandle:= SystemHandle;
  BoldSystem := SystemHandle.System;
  if anOwner = nil then
    inherited Create(Application)
  else
    inherited Create(anOwner);
end;

{ Whether an expression can be evaluated in the persistent storage is not a
  question the type system can answer. Asking for a non-object list is one way to
  fail, and the type does catch that, but an expression can be a perfectly good
  object list and still have no translation: allLoadedObjects is registered as an
  object list, yet "loaded" is an in-memory notion the database knows nothing
  about, so the translator has no SQL symbol for it.

  Both refusals surface while the handle re-derives on idle, and a list handle
  re-derives for ever, so the failure repeats until the flag is cleared. That is
  the behaviour worth preventing, and the only reliable way is to try the
  evaluation once, here, where the exception can be caught and the box unticked.
  It costs one query and reports Bold's own message, which names the real cause
  more precisely than any guess made from the type. }
procedure TOclExplorerForm.ApplyEvaluateInPS(AHandle: TBoldListHandle;
  ABox: TCheckBox);
var
  Indirect: TBoldIndirectElement;
  Root: TBoldElement;
  Reason: string;
begin
  if not ABox.Checked then
  begin
    AHandle.EvaluateInPS := False;
    Exit;
  end;

  if not Assigned(AHandle.RootHandle) or not Assigned(AHandle.RootHandle.Value) or
     (AHandle.Expression = '') then
  begin
    AHandle.EvaluateInPS := True;
    Exit;
  end;

  Root := AHandle.RootHandle.Value;
  Reason := '';
  Indirect := TBoldIndirectElement.Create;
  try
    try
      Root.EvaluateExpression(AHandle.Expression, Indirect, True,
        AHandle.VariableList);
    except
      on E: Exception do
        Reason := E.Message;
    end;
  finally
    Indirect.Free;
  end;

  if Reason = '' then
    AHandle.EvaluateInPS := True
  else
  begin
    ABox.Checked := False;
    AHandle.EvaluateInPS := False;
    ShowMessage('This expression cannot be evaluated in the persistent '
      + 'storage, so it will keep being evaluated in memory.'#13#10#13#10
      + Reason);
  end;
end;

procedure TOclExplorerForm.cbEvaluateInPS1Click(Sender: TObject);
begin
  ApplyEvaluateInPS(List1Handle, cbEvaluateInPS1);
end;

procedure TOclExplorerForm.cbEvaluateInPS2Click(Sender: TObject);
begin
  ApplyEvaluateInPS(List2Handle, cbEvaluateInPS2);
end;

procedure TOclExplorerForm.BoldPlaceableListSubscriber1AfterMakeUptoDate(
  Follower: TBoldFollower);

  function TypeName(AType: TBoldElementTypeInfo): string;
  begin
    if Assigned(AType) then
      Result := AType.AsString
    else
      Result := '';
  end;

var
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  Label1.Caption := Format('%s (%d)',
    [TypeName(List1Handle.BoldType), List1Handle.Count]);

  { The right pane lists the loaded objects of whichever class is selected on the
    left, so it only means anything while the left pane holds classes. That
    expression is editable, and the moment it yields something else this built
    expression is nonsense: a date turns into "1.4.2011.allLoadedObjects", which
    fails to parse. A list handle re-derives on every idle, so that failure then
    repeats for as long as the selection stands. }
  lClassTypeInfo := nil;
  if List1Handle.CurrentElement is TBoldClassTypeInfo then
    lClassTypeInfo := TBoldClassTypeInfo(List1Handle.CurrentElement);

  if not Assigned(List1Handle.CurrentElement) then
  begin
    // Nothing selected, so there is nothing for the right pane to be about.
    List2Handle.Expression := '';
    List2Handle.MutableListExpression := '';
    List2Handle.RootHandle := nil;
  end
  else if Assigned(lClassTypeInfo) then
  begin
    // The default pairing: pick a class, list its loaded objects. ExpressionName
    // rather than AsString, which is the same thing for a meta element since its
    // string representation IS the expression name, but it says why.
    List2Handle.RootHandle := fSystemHandle;
    List2Handle.Expression := lClassTypeInfo.ExpressionName + '.allLoadedObjects';
    List2Handle.MutableListExpression :=
      BoldSystem.BoldSystemTypeInfo.RootClassTypeInfo.ExpressionName + '.allInstances';
  end
  else
  begin
    // The left pane has been pointed at something that is not a class, so there
    // is no class name to build "X.allLoadedObjects" from. Leave whatever the
    // user wrote in the right pane alone rather than clearing it: the variable
    // "list" is bound to this handle's current element, so an expression such as
    // "list.ownedBuildings" keeps working and simply re-evaluates as the
    // selection moves. That pairing is the reason both panes are editable.
    List2Handle.RootHandle := fSystemHandle;
  end;

  Label2.Caption := Format('%s (%d)',
    [TypeName(List2Handle.ListElementType), List2Handle.Count]);
end;

procedure TOclExplorerForm.BoldPlaceableListSubscriber2AfterMakeUptoDate(
  Follower: TBoldFollower);
begin
//  Label2.Caption := Format('%s (%d)', [List2Handle.BoldType.AsString, List2Handle.count]);
end;

type TBoldObjectListAccess = Class(TBoldObjectList);

{ A VCL grid column carries text, so the boolean columns render as a tick or as
  nothing. That scans down a column as quickly as a check box did and costs no
  third party control. }
{ The computed columns only mean anything while the left pane holds a list of
  classes, which is what allSubClasses gives it. Nothing stops that expression
  being edited into something else, and then the element is not a class type info
  at all. A hard cast there raises while the grid is painting, painting is retried,
  and the dialog never stops. Answering with an empty cell is the honest result
  and it cannot loop. }
function AsClassTypeInfo(aFollower: TBoldFollower): TBoldClassTypeInfo;
begin
  if Assigned(aFollower) and (aFollower.Element is TBoldClassTypeInfo) then
    Result := TBoldClassTypeInfo(aFollower.Element)
  else
    Result := nil;
end;

function BoolMark(AValue: Boolean): string;
begin
  if AValue then
    Result := 'X'
  else
    Result := '';
end;

function TOclExplorerForm.brObjectsGetAsString(
  aFollower: TBoldFollower): string;
var
  lClassBoldObjectList: TBoldObjectList;
  lBoldClassListController: TBoldClassListController;
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  Result := '';
  lClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(lClassTypeInfo) then
  begin
    lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass(lClassTypeInfo.ObjectClass)];
    // The last hard cast in a paint path. A renderer that raises while the grid
    // is drawing raises again on the repaint, so the type is asked, not assumed.
    if TBoldObjectListAccess(lClassBoldObjectList).ObjectListController is TBoldClassListController then
    begin
      lBoldClassListController := TBoldClassListController(
        TBoldObjectListAccess(lClassBoldObjectList).ObjectListController);
      result := IntToStr(lBoldClassListController.LoadedObjectCount);
    end;
  end;
end;

procedure TOclExplorerForm.brClassListSubscribe(aFollower: TBoldFollower;
  Subscriber: TBoldSubscriber);
var
  lClassBoldObjectList: TBoldObjectList;
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  lClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(lClassTypeInfo) then
  begin
    lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass(lClassTypeInfo.ObjectClass)];
    lClassBoldObjectList.AddSmallSubscription(Subscriber, [beItemAdded, beItemDeleted, beObjectFetched, beObjectUnloaded], breReEvaluate);
  end;
end;

function TOclExplorerForm.brIdsGetAsString(AFollower: TBoldFollower): string;
var
  Query: IBoldQuery;
  sql: string;
  lClassBoldObjectList: TBoldObjectList;
  ClassTypeInfo: TBoldClassTypeInfo;
  dbType: integer;
  TableName: string;
const
  cAllInstancesIdCount = 'select count(bold_id) from %s where bold_type = %d';
begin
  Result := '';
  ClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(ClassTypeInfo) then
  begin
    if ClassTypeInfo.Persistent then
    begin
      with TBoldSystemHandle(fSystemHandle).PersistenceHandleDB.PersistenceControllerDefault.PersistenceMapper do
      begin
        TableName := RootClassObjectPersistenceMapper.MainTable.SQLName;
        dbType := BoldDbTypeForTopSortedIndex(ClassTypeInfo.TopSortedIndex);
      end;
      if dbType = 0 then
        exit;
      Query := TBoldSystemHandle(fSystemHandle).PersistenceHandleDB.DatabaseInterface.GetQuery;
      try
        Sql := Format(cAllInstancesIdCount, [TableName, dbType]);
        Query.AssignSQLText(SQL);
        Query.Open;
        result := IntToStr(Query.Fields[0].AsInteger);
      finally
        Query.Close;
        TBoldSystemHandle(fSystemHandle).PersistenceHandleDB.DatabaseInterface.ReleaseQuery(Query);
      end;
    end
    else
    begin
      lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass(ClassTypeInfo.ObjectClass)];
      result := IntToStr(lClassBoldObjectList.Count);
    end;
  end;
end;

function TOclExplorerForm.brIndexGetAsString(
  aFollower: TBoldFollower): string;
var
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  Result := '';
  lClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(lClassTypeInfo) then
    result := IntToStr(lClassTypeInfo.TopSortedIndex);
end;

function TOclExplorerForm.brIsAbstractGetAsString(
  AFollower: TBoldFollower): string;
var
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  Result := '';
  lClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(lClassTypeInfo) then
    Result := BoolMark(lClassTypeInfo.IsAbstract);
end;

function TOclExplorerForm.brIsLinkClassGetAsString(
  AFollower: TBoldFollower): string;
var
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  Result := '';
  lClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(lClassTypeInfo) then
    Result := BoolMark(lClassTypeInfo.IsLinkClass);
end;

function TOclExplorerForm.bfIsPersistentGetAsString(
  aFollower: TBoldFollower): string;
var
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  Result := '';
  lClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(lClassTypeInfo) then
    Result := BoolMark(lClassTypeInfo.Persistent);
end;

function TOclExplorerForm.bfClassStateGetAsString(
  aFollower: TBoldFollower): string;
var
  lClassBoldObjectList: TBoldObjectList;
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  Result := '';
  lClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(lClassTypeInfo) then
  begin
    lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass(lClassTypeInfo.ObjectClass)];
    result := BoolMark(lClassBoldObjectList.BoldPersistenceState = bvpsCurrent);
{    case lClassBoldObjectList.BoldPersistenceState of
      bvpsCurrent : result := 'Current';
      bvpsModified : result := 'Modified';
      bvpsInvalid : result := '';
      bvpsTransient : result := 'Transient';
    end;
}
  end;
end;

procedure TOclExplorerForm.bfClassStateSubscribe(aFollower: TBoldFollower;
  Subscriber: TBoldSubscriber);
var
  lClassBoldObjectList: TBoldObjectList;
  lClassTypeInfo: TBoldClassTypeInfo;
begin
  lClassTypeInfo := AsClassTypeInfo(aFollower);
  if Assigned(lClassTypeInfo) then
  begin
    lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass(lClassTypeInfo.ObjectClass)];
    lClassBoldObjectList.AddSmallSubscription(Subscriber, [beItemAdded, beItemDeleted, beObjectFetched, beClassListStateChanged, beObjectUnloaded], breReEvaluate);
  end;
end;

end.
