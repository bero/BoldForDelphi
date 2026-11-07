
{ Global compiler directives }
{$include bold.inc}
unit BoldHandleReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  BoldUtils,
  ActnList,
  Actions,
  Classes,
  Controls,
  DesignIntf,
  DesignEditors,
  BoldElements,
  BoldSubscription,
  BoldDefs,
  BoldIDEConsts,
  BoldDefsDT,
  BoldOclVariables,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors,
  BoldHandlePropEditor,
  BoldPlaceableSubscriber,
  BoldPlaceableListSubscriber,
  BoldActions, BoldHandleAction, BoldListActions, BoldUndoActions,
  BoldSystem,
  BoldAbstractModel,
  BoldHandles,
  BoldDerivedHandle,
  BoldExpressionHandle,
  BoldRootedHandles,
  BoldCursorHandle,
  BoldListHandle,
  BoldVariableHandle,
  BoldSystemHandle,
  BoldSQLHandle,
  BoldRawSQLHandle,
  BoldFilteredHandle,
  BoldSortedHandle,
  BoldReferenceHandle,
//  BoldVariableDefinition,
  BoldOclRepository,
  BoldTypeNameSelector,
  BoldModelAwareComponentEditor,
  BoldUnloaderHandle;

{$R BoldHandleReg.res}

type
  // The Bold core links no GUI framework (#117), so a VCL application registers
  // the VCL environment by linking BoldEnvironmentVCL. Bold's VCL controls import
  // it themselves; for a form, frame or data module whose Bold components are all
  // non-visual (handles, the model, persistence, adapters) this adds the unit to
  // the uses clause when the IDE saves it, the way FireDAC adds its VCL wait unit.
  // Only VCL designers get it; an FMX or framework-neutral designer gets nothing.
  TBoldVCLEnvironmentSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure TBoldVCLEnvironmentSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  if Assigned(Designer) and Assigned(Designer.ActiveClassGroup) and
    Designer.ActiveClassGroup.InheritsFrom(TControl) then
    Proc('BoldEnvironmentVCL');
end;


procedure RegisterActionsInDelphi;
begin
  RegisterActions(BOLDACTIONGROUPNAME,
                  [
                   TBoldUpdateDBAction,
                   TBoldDiscardChangesAction,
                   TBoldActivateSystemAction,
                   TBoldFailureDetectionAction,
                   TBoldCreateDatabaseAction,
                   TBoldListHandleAddNewAction,
                   TBoldListHandleDeleteAction,
                   TBoldListHandleFirstAction,
                   TBoldListHandleLastAction,
                   TBoldListHandleNextAction,
                   TBoldListHandlePrevAction,
                   TBoldListHandleMoveUpAction,
                   TBoldListHandleMoveDownAction,
                   TBoldSetCheckPointAction,
                   TBoldUndoAction,
                   TBoldRedoAction,
                   TBoldClearUndoAction
                  ], nil);
end;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_HANDLES,
    [
    TBoldSystemHandle,
    TBoldSystemTypeInfoHandle,
    TBoldExpressionHandle,
    TBoldDerivedhandle,
    TBoldVariableHandle,
    TBoldListHandle,
    TBoldSQLHandle,
    TBoldRawSQLHandle,
    TBoldCursorHandle,
    TBoldFilteredHandle,
    TBoldReferenceHandle,
    TBoldOclVariables,
    TBoldUnloaderHandle
    ]);

  RegisterComponents(BOLDPAGENAME_MISC,
    [
    TBoldFilter,
    TBoldComparer,
    TBoldPlaceableSubscriber,
    TBoldPlaceableListSubscriber
    ]);

  RegisterComponents(BOLDPAGENAME_DEPRECATED,
    [
//      TBoldVariableDefinition
    ]);

end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(TBoldAbstractModel), TBoldSystemTypeInfoHandle, 'BoldModel', TBoldComponentPropertyIndicateMissing);  // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldSystemTypeInfoHandle), TBoldAbstractSystemHandle, 'SystemTypeInfoHandle', TBoldComponentPropertyIndicateMissing);   // do not localize

  RegisterPropertyEditor(TypeInfo(TBoldElementHandle), TBoldRootedHandle, 'RootHandle', TBoldRootedHandleRootHandlePropertyEditor);    // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldElementFilter), nil, '', TBoldElementFilterMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldElementSubscribe), nil, '', TBoldElementSubscribeMethodProperty);
  RegisterPropertyEditor(TypeInfo(TBoldElementCompare), nil, '', TBoldElementCompareMethodProperty);
  //TBoldPlaceableSubscriber
  RegisterPropertyEditor(TypeInfo(TBoldSubscribeToElementEvent), nil, '', TBoldElementSubscribeMethodProperty);

  RegisterComponentEditor(TBoldListHandle, TBoldOCLComponentEditor);
  RegisterComponentEditor(TBoldExpressionHandle, TBoldOCLComponentEditor);

  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldListHandle, 'Expression', TBoldOCLExpressionForOCLComponent);                // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldListHandle, 'MutableListExpression', TBoldOCLExpressionForOCLComponent);     // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldExpressionHandle, 'Expression', TBoldOCLExpressionForOCLComponent);          // do not localize

  RegisterPropertyEditor(TypeInfo(string), TBoldReferenceHandle, 'StaticValueTypeName', TBoldTypeNameSelectorPropertyForAllTypes);    // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldRootedHandle, 'RootTypeName', TBoldTypeNameSelectorPropertyForAllTypes);              // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldVariableHandle, 'ValueTypeName', TBoldTypeNameSelectorPropertyForVariableHandle);     // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldDerivedHandle, 'ValueTypeName', TBoldTypeNameSelectorPropertyForAllTypes);            // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldDerivedHandle, 'ValueTypeName', TBoldTypeNameSelectorPropertyForVariableHandle);      // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldSQLHandle, 'ClassExpressionName', TBoldTypeNameSelectorForSQLHandle);                 // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldRawSQLHandle, 'ClassExpressionName', TBoldTypeNameSelectorForSQLHandle);              // do not localize
  // Propeditor för listhandle.expression
  // Propeditor för Exprhandle.expression

  RegisterComponents(BOLDPAGENAME_MISC, [TBoldOclRepository]);
  RegisterPropertyEditor(TypeInfo(string), TBoldOclDefinition, 'Context', TBoldTypeNameSelectorForOclDefinition);  // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldOclDefinition, 'Expression', TBoldOclExpressionForOclDefinition);  // do not localize

  RegisterComponentEditor(TBoldOclVariables, TBoldOclVariablesEditor);
  RegisterComponentEditor(TBoldOCLRepository, TBoldOCLRepositoryEditor);
  RegisterComponentEditor(TBoldSystemHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldSystemTypeInfoHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldDerivedHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldVariableHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldSQLHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldRawSQLHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldCursorHandle, TBoldModelAwareComponentEditor);
  RegisterComponentEditor(TBoldReferenceHandle, TBoldModelAwareComponentEditor);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterEditors;
  RegisterActionsInDelphi;
  RegisterSelectionEditor(TBoldSubscribableComponent, TBoldVCLEnvironmentSelectionEditor);
end;

end.



