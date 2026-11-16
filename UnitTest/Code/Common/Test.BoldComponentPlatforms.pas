unit Test.BoldComponentPlatforms;

{ #117 step 8: the non-visual components that run off Windows.

  ComponentPlatformsAttribute decides for which target platforms the IDE offers
  a component on the palette (for the others it is greyed out); it does not
  affect compiling or running. The classes below carry pidLinux64 next to
  pidWin32 and pidWin64.

  The uses clause is half of the check: this unit is part of the Linux64 build
  of the test runner, so a class can only be listed here when its unit compiles
  for Linux64. The test is the other half: every listed class must carry the
  attribute with the three platforms.

  The VCL controls and the COM, SOAP, HTTP, propagator and DevExpress
  components keep pidWin32 or pidWin64: their units do not compile off Windows.
  macOS is not in the set: no macOS compiler has built these units yet. }

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  [Category('Common')]
  TTestBoldComponentPlatforms = class
  public
    [Test]
    procedure TestRuntimeComponentsAreOfferedForLinux64;
  end;

implementation

uses
  System.Classes,
  System.Rtti,
  System.SysUtils,
  BoldClassSubscriber,
  BoldConstraintValidator,
  BoldCursorHandle,
  BoldDatabaseAdapterFireDAC,
  BoldDerivedHandle,
  BoldExceptionHandlers,
  BoldExpressionHandle,
  BoldExternalObjectSpaceEventHandler,
  BoldExternalPersistenceHandleDataSet,
  BoldExternalPersistenceHandleEventDriven,
  BoldExternalPersistenceHandleSQL,
  BoldFilteredHandle,
  BoldFloatControlPack,
  BoldHandles,
  BoldIDCompressor,
  BoldListHandle,
  BoldManipulators,
  BoldMLRenderers,
  BoldModel,
  BoldNewObjectInterceptor,
  BoldNumericControlPack,
  BoldObjectUpgraderHandle,
  BoldOclRepository,
  BoldOclVariables,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleSystem,
  BoldPersistenceNotifier,
  BoldPlaceableListSubscriber,
  BoldPlaceableSubscriber,
  BoldRawSQLHandle,
  BoldReferenceHandle,
  BoldSortedHandle,
  BoldSQLHandle,
  BoldStringControlPack,
  BoldSubscription,
  BoldSystemHandle,
  BoldTemplateExpander,
  BoldTypeNameHandle,
  BoldUnloaderHandle,
  BoldVariableHandle;

const
  cRuntimeComponents: array[0..41] of TComponentClass = (
    TBoldAbstractPersistenceNotifier,
    TBoldAsFloatRenderer,
    TBoldAsIntegerRenderer,
    TBoldAsMLStringRenderer,
    TBoldAsStringRenderer,
    TBoldClassSubscriber,
    TBoldComparer,
    TBoldConstraintValidator,
    TBoldCursorHandle,
    TBoldDatabaseAdapterFireDAC,
    TBoldDerivedHandle,
    TBoldExceptionHandler,
    TBoldExpressionHandle,
    TBoldExternalObjectSpaceEventHandler,
    TBoldExternalPersistenceHandleDataSet,
    TBoldExternalPersistenceHandleEventDriven,
    TBoldExternalPersistenceHandleSQL,
    TBoldFilter,
    TBoldFilteredHandle,
    TBoldIDCompressor,
    TBoldListHandle,
    TBoldManipulator,
    TBoldModel,
    TBoldNewObjectInterceptor,
    TBoldObjectUpgraderHandle,
    TBoldOclRepository,
    TBoldOclVariables,
    TBoldPersistenceHandleDB,
    TBoldPersistenceHandleSystem,
    TBoldPersistenceNotifier,
    TBoldPlaceableSubscriber,
    TBoldRawSQLHandle,
    TBoldReferenceHandle,
    TBoldSQLHandle,
    TBoldSubscribableComponent,
    TBoldSystemHandle,
    TBoldSystemTypeInfoHandle,
    TBoldTemplateHolder,
    TBoldTypeNameHandle,
    TBoldUnloaderHandle,
    TBoldVariableHandle,
    TCustomBoldPlaceableListSubscriber);

procedure TTestBoldComponentPlatforms.TestRuntimeComponentsAreOfferedForLinux64;
const
  cExpected = pidWin32 or pidWin64 or pidLinux64;
var
  Context: TRttiContext;
  ComponentClass: TComponentClass;
  Attribute: TCustomAttribute;
  Platforms: TPlatformIds;
  Found: Boolean;
begin
  Context := TRttiContext.Create;
  try
    for ComponentClass in cRuntimeComponents do
    begin
      Found := False;
      Platforms := 0;
      for Attribute in Context.GetType(ComponentClass).GetAttributes do
        if Attribute is ComponentPlatformsAttribute then
        begin
          Found := True;
          Platforms := ComponentPlatformsAttribute(Attribute).Platforms;
        end;
      Assert.IsTrue(Found, ComponentClass.ClassName +
        ' must carry ComponentPlatformsAttribute');
      Assert.AreEqual(Integer(cExpected), Integer(Platforms and cExpected),
        ComponentClass.ClassName +
        ' must be offered for Win32, Win64 and Linux64');
    end;
  finally
    Context.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldComponentPlatforms);

end.
