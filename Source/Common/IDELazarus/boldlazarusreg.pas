
{ Global compiler directives }
{$include bold.inc}
unit BoldLazarusReg;

{ Puts Bold's non-visual components on the Lazarus component palette (#117):
  the handles, the model, the persistence handles and TBoldDataSet. It is the
  Lazarus counterpart of the Register procedures of the Delphi design package
  dclBold (BoldHandleReg, BoldUMLModelHandleReg and the others), with the same
  palette pages, and is in the Lazarus design-time package dclBold
  (packages/Lazarus/dclBold.lpk).

  The palette icons are the bitmaps of the Delphi package (Images/Components)
  as PNG, in boldlazarusreg.res; images/make_res.sh makes that file.

  One property editor: Expression of the handles and of a TBoldDataSet field
  opens Delphi's OCL editor as an LCL dialog (BoldOclExpressionPropEdit).
  Two component editors: TBoldDataSet has the commands of Delphi's
  TBoldDataSetEditor (BoldDataSetComponentEditor), and TBoldModel loads a
  model from a .bld file (BoldModelComponentEditor). The other property and
  component editors of the Delphi package are written against Delphi's
  design-time API and are not ported. }

interface

procedure Register;

implementation

{$R boldlazarusreg.res}

uses
  Classes,
  PropEdits,
  ComponentEditors,
  BoldDefs,
  BoldControlPack,
  BoldOclExpressionPropEdit,
  BoldDataSetComponentEditor,
  BoldModelComponentEditor,
  BoldLazarusRequirements,
  BoldIDEConsts,
  BoldEnvironment,
  // Registers Bold's environment for the LCL: in the form designer a
  // component tells the IDE through it that the form has changed
  BoldEnvironmentLCL,
  BoldHandles,
  BoldSystemHandle,
  BoldExpressionHandle,
  BoldDerivedHandle,
  BoldVariableHandle,
  BoldListHandle,
  BoldCursorHandle,
  BoldFilteredHandle,
  BoldSortedHandle,
  BoldReferenceHandle,
  BoldOclVariables,
  BoldUnloaderHandle,
  BoldPlaceableSubscriber,
  BoldModel,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleFileXML,
  BoldExceptionHandlers,
  BoldDataSet;

const
  cHandles: array[0..11] of TComponentClass = (
    TBoldModel,
    TBoldSystemHandle,
    TBoldSystemTypeInfoHandle,
    TBoldExpressionHandle,
    TBoldDerivedHandle,
    TBoldVariableHandle,
    TBoldListHandle,
    TBoldCursorHandle,
    TBoldFilteredHandle,
    TBoldReferenceHandle,
    TBoldOclVariables,
    TBoldUnloaderHandle
    );
  cPersistence: array[0..1] of TComponentClass = (
    TBoldPersistenceHandleDB,
    TBoldPersistenceHandleFileXML
    );
  cMisc: array[0..4] of TComponentClass = (
    TBoldFilter,
    TBoldComparer,
    TBoldPlaceableSubscriber,
    TBoldExceptionHandler,
    TBoldDataSet
    );

procedure Register;
begin
  // What BoldEnvironmentIDE does in the Delphi design package
  BoldInternalRunningInIDE := True;

  RegisterComponents(BOLDPAGENAME_HANDLES, cHandles);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, cPersistence);
  RegisterComponents(BOLDPAGENAME_MISC, cMisc);

  // A project that gets one of them requires the package Bold, not dclBold
  RegisterComponentRequirements(cHandles, TBoldDclBoldNotRequired);
  RegisterComponentRequirements(cPersistence, TBoldDclBoldNotRequired);
  RegisterComponentRequirements(cMisc, TBoldDclBoldNotRequired);

  // TBoldExpression is string itself, so each property is named
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldListHandle, 'Expression', TBoldOclExpressionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldListHandle, 'MutableListExpression', TBoldOclExpressionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldExpressionHandle, 'Expression', TBoldOclExpressionPropertyEditor);
  // The BoldProperties of a TBoldDataSet field
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldFollowerController, 'Expression', TBoldOclExpressionPropertyEditor);

  // Fields Editor (the field descriptions), Create default fields, Clear all columns
  RegisterComponentEditor(TBoldAbstractDataSet, TBoldDataSetComponentEditor);
  // Load model from file
  RegisterComponentEditor(TBoldModel, TBoldModelComponentEditor);
end;

end.
