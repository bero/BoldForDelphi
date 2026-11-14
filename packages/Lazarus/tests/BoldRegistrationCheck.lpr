{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program BoldRegistrationCheck;
{ Checks what the Lazarus design-time package dclBold registers, without the
  IDE (#117): it catches the calls of BoldLazarusReg.Register, and for every
  registered component class creates an instance, writes it as the text of a
  form file and reads it back, as the form designer does when it saves and
  opens a form, and checks that it has its palette icon. It then does the same with a small data module: a TBoldModel
  holding the Invoicing example's model, a system handle, chained list
  handles, a persistence handle and a TBoldDataSet with fields, which refer
  to each other. Last, the property editor of Expression: what it is
  registered for, the check of expressions against that model, and its
  dialog, which is created (hence the LCL and a widgetset) but not shown.
  One line per check; exit code = number of failed checks. }

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces,
  SysUtils,
  Classes,
  TypInfo,
  Forms,
  PropEdits,
  BoldEnvironment,
  BoldHandles,
  BoldSystemHandle,
  BoldListHandle,
  BoldFilteredHandle,
  BoldModel,
  BoldPersistenceHandleFileXML,
  BoldDataSet,
  BoldElements,
  BoldExpressionHandle,
  BoldControlPack,
  BoldOclExpressionCheck,
  BoldOclExpressionPropEdit,
  BoldLazarusReg,
  BoldRegistrationChecks;

procedure CheckEveryComponentHasItsPaletteIcon;
var
  i: Integer;
  Size: string;
begin
  for i := 0 to High(Registered) do
  begin
    Size := PaletteIconSize(Registered[i].ComponentClass);
    // As in Delphi, where TBoldFilteredHandle has no bitmap and shows the default icon
    if Registered[i].ComponentClass = TBoldFilteredHandle then
      Check('TBoldFilteredHandle has no palette icon of its own', Pos('no resource', Size) = 1, Size)
    else
      Check(string(Registered[i].ComponentClass.ClassName) + ' has its palette icon', Size = '24 x 24', Size);
  end;
end;

type
  { A data module as a form designer would hold it: the components are
    owned by it and found by name }
  TCheckModule = class(TDataModule)
  end;

function InvoicingModelFileName: string;
begin
  // The program is in packages/Lazarus/tests/bin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' + PathDelim + '..' +
    PathDelim + '..' + PathDelim + 'examples' + PathDelim + 'Compound' + PathDelim + 'Invoicing' +
    PathDelim + 'Model' + PathDelim + 'Invoicing.bld');
end;

procedure BuildModule(AModule: TComponent);
var
  ModelText: TStringList;
  Model: TBoldModel;
  TypeInfoHandle: TBoldSystemTypeInfoHandle;
  Persistence: TBoldPersistenceHandleFileXML;
  SystemHandle: TBoldSystemHandle;
  Masters, Details: TBoldListHandle;
  DataSet: TBoldDataSet;
  Field: TBoldDataSetFieldDescription;
begin
  Model := TBoldModel.Create(AModule);
  Model.Name := 'BoldModel1';
  ModelText := TStringList.Create;
  try
    ModelText.LoadFromFile(InvoicingModelFileName);
    Model.SetFromModelAsString(ModelText);
  finally
    ModelText.Free;
  end;
  TypeInfoHandle := TBoldSystemTypeInfoHandle.Create(AModule);
  TypeInfoHandle.Name := 'BoldSystemTypeInfoHandle1';
  TypeInfoHandle.BoldModel := Model;
  Persistence := TBoldPersistenceHandleFileXML.Create(AModule);
  Persistence.Name := 'BoldPersistenceHandleFileXML1';
  Persistence.BoldModel := Model;
  Persistence.FileName := 'data.xml';
  SystemHandle := TBoldSystemHandle.Create(AModule);
  SystemHandle.Name := 'BoldSystemHandle1';
  SystemHandle.SystemTypeInfoHandle := TypeInfoHandle;
  SystemHandle.PersistenceHandle := Persistence;
  Masters := TBoldListHandle.Create(AModule);
  Masters.Name := 'blhMasters';
  Masters.StaticSystemHandle := SystemHandle;
  Masters.RootHandle := SystemHandle;
  Masters.Expression := 'Customer.allInstances';
  Details := TBoldListHandle.Create(AModule);
  Details.Name := 'blhDetails';
  Details.StaticSystemHandle := SystemHandle;
  Details.RootHandle := Masters;
  Details.Expression := 'invoices';
  DataSet := TBoldDataSet.Create(AModule);
  DataSet.Name := 'BoldDataSet1';
  DataSet.BoldHandle := Masters;
  Field := DataSet.FieldDescriptions.Add;
  Field.BoldProperties.Expression := 'name';
  Field.FieldName := 'Name';
  Field := DataSet.FieldDescriptions.Add;
  Field.BoldProperties.Expression := 'openBalance';
  Field.FieldName := 'Open balance';
end;

function AfterFirstLine(const AText: AnsiString): AnsiString;
begin
  Result := Copy(AText, Pos(AnsiString(#10), AText) + 1, MaxInt);
end;

procedure CheckModuleRoundTrips;
var
  Original, Reloaded, ReloadedAgain: TCheckModule;
  Written, WrittenAgain, WrittenThird: AnsiString;
  OriginalModel, ReloadedModel: TBoldModel;
  SystemHandle: TBoldSystemHandle;
  Masters, Details: TBoldListHandle;
  DataSet: TBoldDataSet;
begin
  Reloaded := nil;
  ReloadedAgain := nil;
  Original := TCheckModule.CreateNew(nil);
  try
    try
      Original.Name := 'CheckModule';
      BuildModule(Original);
      Written := ComponentToText(Original);
      Reloaded := TCheckModule.CreateNew(nil);
      TextToComponent(Written, Reloaded);
      WrittenAgain := ComponentToText(Reloaded);
      ReloadedAgain := TCheckModule.CreateNew(nil);
      TextToComponent(WrittenAgain, ReloadedAgain);
      WrittenThird := ComponentToText(ReloadedAgain);
      // Not Written = WrittenAgain: a TBoldModel that was read from a form
      // writes its classes in another order than one filled from a .bld file,
      // and adds the tagged value Bold.CRC (the same classes; checked below).
      // From then on the text is stable, so a form saved twice in the
      // designer does not change.
      // (compared after the first line, which has the module's name: with
      // the LCL linked in, a data module read while one of its name exists
      // is renamed, CheckModule_1)
      Check('the data module, once read, is written the same again',
        AfterFirstLine(WrittenAgain) = AfterFirstLine(WrittenThird),
        IntToStr(Length(WrittenAgain)) + ' characters, ' + IntToStr(Reloaded.ComponentCount) + ' components');

      OriginalModel := Original.FindComponent('BoldModel1') as TBoldModel;
      ReloadedModel := Reloaded.FindComponent('BoldModel1') as TBoldModel;
      Check('the model comes back with its classes',
        (OriginalModel.MoldModel.Classes.Count > 5) and
        (ReloadedModel.MoldModel.Classes.Count = OriginalModel.MoldModel.Classes.Count),
        IntToStr(ReloadedModel.MoldModel.Classes.Count) + ' classes');
      SystemHandle := Reloaded.FindComponent('BoldSystemHandle1') as TBoldSystemHandle;
      Masters := Reloaded.FindComponent('blhMasters') as TBoldListHandle;
      Details := Reloaded.FindComponent('blhDetails') as TBoldListHandle;
      DataSet := Reloaded.FindComponent('BoldDataSet1') as TBoldDataSet;
      Check('the system handle finds its type info handle and its model',
        Assigned(SystemHandle.SystemTypeInfoHandle) and (SystemHandle.SystemTypeInfoHandle.Owner = Reloaded) and
        (SystemHandle.SystemTypeInfoHandle.BoldModel = Reloaded.FindComponent('BoldModel1')),
        string(SystemHandle.SystemTypeInfoHandle.Name));
      Check('the system handle finds its persistence handle',
        SystemHandle.PersistenceHandle = Reloaded.FindComponent('BoldPersistenceHandleFileXML1'),
        (SystemHandle.PersistenceHandle as TBoldPersistenceHandleFileXML).FileName);
      Check('the master list handle is rooted on the system handle',
        (Masters.RootHandle = SystemHandle) and (Masters.StaticSystemHandle = SystemHandle), Masters.Expression);
      Check('the detail list handle is rooted on the master', Details.RootHandle = Masters, Details.Expression);
      Check('the dataset finds its list handle', DataSet.BoldHandle = Masters, string(DataSet.BoldHandle.Name));
      Check('the dataset keeps its fields',
        (DataSet.FieldDescriptions.Count = 2) and
        (DataSet.FieldDescriptions[0].FieldName = 'Name') and
        (DataSet.FieldDescriptions[0].BoldProperties.Expression = 'name') and
        (DataSet.FieldDescriptions[1].FieldName = 'Open balance'),
        IntToStr(DataSet.FieldDescriptions.Count) + ' fields');
    except
      on E: Exception do
        Check('the data module is written and read', False, string(E.ClassName) + ': ' + E.Message);
    end;
  finally
    ReloadedAgain.Free;
    Reloaded.Free;
    Original.Free;
  end;
end;

{ What the property editor of Expression asks: the type an expression of a
  component is evaluated in, and whether the expression is valid there. As in
  the IDE, the classes generated from the model are not linked in. }
procedure CheckOne(const AWhat: string; APersistent: TPersistent; const AExpression: string;
  AValid: Boolean; const AContextName, AResultOrError: string; AErrorPosition: Integer);
var
  Context: TBoldElementTypeInfo;
  Variables: TBoldExternalVariableList;
  R: TBoldOclCheckResult;
  Passed: Boolean;
  Actual: string;
begin
  if not BoldOclContextOf(APersistent, Context, Variables) then
  begin
    Check(AWhat, False, 'no expression context in ' + string(APersistent.ClassName));
    Exit;
  end;
  R := BoldCheckOclExpression(AExpression, Context, Variables);
  Actual := 'context ' + R.ContextName + ', ' + BoldOclCheckResultAsText(AExpression, R);
  Passed := R.Checked and (R.Valid = AValid) and (R.ContextName = AContextName);
  if AValid then
    Passed := Passed and (R.ResultTypeName = AResultOrError) and (R.ErrorPosition = -1)
  else
    Passed := Passed and (Pos(AResultOrError, R.ErrorMessage) > 0) and (R.ErrorPosition = AErrorPosition);
  Check(AWhat, Passed, Actual);
end;

procedure CheckEditorOf(APersistent: TPersistent; const AProperty: string; AIsOclEditor: Boolean);
var
  PropInfo: PPropInfo;
  EditorClass: TPropertyEditorClass;
  What: string;
begin
  What := string(APersistent.ClassName) + '.' + AProperty;
  PropInfo := GetPropInfo(APersistent, AProperty);
  if not Assigned(PropInfo) then
  begin
    Check(What + ' is a published property', False, 'not found');
    Exit;
  end;
  EditorClass := GetEditorClass(PropInfo, APersistent);
  if AIsOclEditor then
    Check(What + ' has the OCL expression editor', EditorClass = TBoldOclExpressionPropertyEditor,
      string(EditorClass.ClassName))
  else
    Check(What + ' keeps its own editor', EditorClass <> TBoldOclExpressionPropertyEditor,
      string(EditorClass.ClassName));
end;

type
  { The property editor with a user who types an expression and presses OK
    or Cancel, in place of the modal dialog }
  TEditorWithAUser = class(TBoldOclExpressionPropertyEditor)
  protected
    function ShowDialog(ADialog: TBoldOclExpressionDialog): Boolean; override;
  public
    Seen, Typed, StatusSeen: string;
    PressOK: Boolean;
  end;

function TEditorWithAUser.ShowDialog(ADialog: TBoldOclExpressionDialog): Boolean;
begin
  Seen := ADialog.Expression;
  ADialog.Expression := Typed;
  StatusSeen := ADialog.StatusText;
  Result := PressOK;
end;

procedure CheckTheEditorEdits(AHandle: TBoldListHandle);
var
  Hook: TPropertyEditorHook;
  Editor: TEditorWithAUser;
  Before: string;
begin
  Before := AHandle.Expression;
  Hook := TPropertyEditorHook.Create(nil);
  Editor := TEditorWithAUser.Create(Hook, 1);
  try
    Editor.SetPropEntry(0, AHandle, GetPropInfo(AHandle, 'Expression'));
    Check('the editor has the button that opens the dialog', paDialog in Editor.GetAttributes, 'paDialog');
    Editor.Typed := 'invoices->select(isPosted)';
    Editor.PressOK := False;
    Editor.Edit;
    Check('the dialog opens with the expression of the handle', Editor.Seen = Before, Editor.Seen);
    Check('Cancel leaves the expression as it was', AHandle.Expression = Before, AHandle.Expression);
    Editor.PressOK := True;
    Editor.Edit;
    Check('OK writes the expression to the handle',
      (AHandle.Expression = 'invoices->select(isPosted)') and (Editor.StatusSeen = 'Result type: Collection(Invoice)'),
      AHandle.Expression + ', ' + Editor.StatusSeen);
    AHandle.Expression := Before;
  finally
    Editor.Free;
    Hook.Free;
  end;
end;

{ The dialog as the property editor creates it, not shown }
procedure CheckTheDialog(AHandle: TBoldListHandle);
var
  Context: TBoldElementTypeInfo;
  Variables: TBoldExternalVariableList;
  Dialog: TBoldOclExpressionDialog;
begin
  BoldOclContextOf(AHandle, Context, Variables);
  Dialog := TBoldOclExpressionDialog.CreateDialog(nil, Context, Variables);
  try
    Check('the dialog names the context', Pos('Customer', Dialog.Caption) > 0, Dialog.Caption);
    Dialog.Expression := AHandle.Expression;
    Check('the dialog shows the result type of the expression',
      Dialog.LastCheck.Valid and (Dialog.StatusText = 'Result type: Collection(Invoice)'), Dialog.StatusText);
    Check('the dialog gives the expression back as it was set', Dialog.Expression = 'invoices', Dialog.Expression);
    Dialog.Expression := 'invoices.nosuchmember';
    Check('the dialog shows the error of an expression that is not valid',
      not Dialog.LastCheck.Valid and
      (Dialog.StatusText = 'Error at position 9: nosuchmember is not a member of Invoice'), Dialog.StatusText);
  finally
    Dialog.Free;
  end;
  Dialog := TBoldOclExpressionDialog.CreateDialog(nil, nil, nil);
  try
    Dialog.Expression := 'anything';
    Check('the dialog without a context says that nothing is checked',
      not Dialog.LastCheck.Checked and (Pos('No context', Dialog.StatusText) = 1), Dialog.StatusText);
  finally
    Dialog.Free;
  end;
end;

procedure CheckExpressionsAgainstTheModel;
var
  Module: TCheckModule;
  Masters, Details, Loose: TBoldListHandle;
  ExpressionHandle: TBoldExpressionHandle;
  DataSet: TBoldDataSet;
  Context: TBoldElementTypeInfo;
  Variables: TBoldExternalVariableList;
  R: TBoldOclCheckResult;
begin
  Module := TCheckModule.CreateNew(nil);
  try
    try
      Module.Name := 'CheckModule';
      BuildModule(Module);
      Masters := Module.FindComponent('blhMasters') as TBoldListHandle;
      Details := Module.FindComponent('blhDetails') as TBoldListHandle;
      DataSet := Module.FindComponent('BoldDataSet1') as TBoldDataSet;

      CheckOne('a list handle on the system handle: Customer.allInstances', Masters,
        'Customer.allInstances', True, 'Invoicing', 'Collection(Customer)', -1);
      CheckOne('a list handle on another: invoices', Details, 'invoices', True, 'Customer', 'Collection(Invoice)', -1);
      CheckOne('an empty expression is the context itself', Details, '', True, 'Customer', 'Customer', -1);
      CheckOne('a derived attribute', Details, 'openBalance', True, 'Customer', 'Currency', -1);
      CheckOne('a member that the class does not have', Details, 'invoices.nosuchmember', False,
        'Customer', 'nosuchmember is not a member of Invoice', 9);
      CheckOne('a syntax error', Details, 'invoices->select(isPosted', False,
        'Customer', 'Parenthesis are not in balance', 16);
      CheckOne('a field of a dataset: name', DataSet.FieldDescriptions[0].BoldProperties,
        'name', True, 'Customer', 'String', -1);
      CheckOne('a field of a dataset: an operation with the wrong argument',
        DataSet.FieldDescriptions[0].BoldProperties, 'name + 1', False, 'Customer', 'no common superclass', 5);

      Loose := TBoldListHandle.Create(Module);
      Loose.Name := 'blhLoose';
      Check('a handle with no root has an expression context to ask for',
        BoldOclContextOf(Loose, Context, Variables) and not Assigned(Context), 'nil');
      R := BoldCheckOclExpression('invoices', Context, Variables);
      Check('and its expression is not checked', not R.Checked and not R.Valid,
        BoldOclCheckResultAsText('invoices', R));
      Check('a component without expressions has no expression context',
        not BoldOclContextOf(Module.FindComponent('BoldModel1'), Context, Variables), 'TBoldModel');

      Writeln('The property editor of Expression');
      CheckEditorOf(Masters, 'Expression', True);
      CheckEditorOf(Masters, 'MutableListExpression', True);
      CheckEditorOf(DataSet.FieldDescriptions[0].BoldProperties, 'Expression', True);
      ExpressionHandle := TBoldExpressionHandle.Create(Module);
      ExpressionHandle.Name := 'behLoose';
      CheckEditorOf(ExpressionHandle, 'Expression', True);
      // Other strings keep the plain editor
      CheckEditorOf(DataSet.FieldDescriptions[0], 'FieldName', False);
      CheckEditorOf(Module.FindComponent('BoldPersistenceHandleFileXML1') as TPersistent, 'FileName', False);
      CheckTheDialog(Details);
      CheckTheEditorEdits(Details);
    except
      on E: Exception do
        Check('expressions are checked against the model', False, string(E.ClassName) + ': ' + E.Message);
    end;
  finally
    Module.Free;
  end;
end;

begin
  try
    // The dialog of the property editor is created, never shown
    Application.Initialize;
    RegisterComponentsProc := CatchRegisterComponents;
    BoldLazarusReg.Register;

    Writeln('Registration');
    Check('components registered', Length(Registered) = 19, IntToStr(Length(Registered)));
    Check('on the page Bold Handles', CountOnPage('Bold Handles') = 12, IntToStr(CountOnPage('Bold Handles')));
    Check('on the page Bold Persistence', CountOnPage('Bold Persistence') = 2, IntToStr(CountOnPage('Bold Persistence')));
    Check('on the page Bold Misc', CountOnPage('Bold Misc') = 5, IntToStr(CountOnPage('Bold Misc')));
    Check('TBoldDataSet is registered', IsRegistered(TBoldDataSet), 'TBoldDataSet');
    Check('Bold knows that it runs in the IDE', BoldInternalRunningInIDE, BoolToStr(BoldInternalRunningInIDE, True));

    Writeln('Every component, written as form text and read back');
    CheckEveryComponentRoundTrips;

    Writeln('Every component has its palette icon');
    CheckEveryComponentHasItsPaletteIcon;

    Writeln('A data module with handles, a persistence handle and a dataset');
    CheckModuleRoundTrips;

    Writeln('Expressions checked against the model');
    CheckExpressionsAgainstTheModel;

    if Failures = 0 then
      Writeln('All checks passed')
    else
      Writeln(Failures, ' check(s) FAILED');
    ExitCode := Failures;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 255;
    end;
  end;
end.
