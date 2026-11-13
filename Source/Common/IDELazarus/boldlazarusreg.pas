
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

  Only registration so far: the property and component editors of the Delphi
  package are written against Delphi's design-time API and are not ported. }

interface

procedure Register;

implementation

{$R boldlazarusreg.res}

uses
  Classes,
  BoldIDEConsts,
  BoldEnvironment,
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

procedure Register;
begin
  // What BoldEnvironmentIDE does in the Delphi design package
  BoldInternalRunningInIDE := True;

  RegisterComponents(BOLDPAGENAME_HANDLES,
    [
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
    ]);

  RegisterComponents(BOLDPAGENAME_PERSISTENCE,
    [
    TBoldPersistenceHandleDB,
    TBoldPersistenceHandleFileXML
    ]);

  RegisterComponents(BOLDPAGENAME_MISC,
    [
    TBoldFilter,
    TBoldComparer,
    TBoldPlaceableSubscriber,
    TBoldExceptionHandler,
    TBoldDataSet
    ]);
end;

end.
