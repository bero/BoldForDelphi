unit Test.BoldTaggedValueSupport;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestBoldTaggedValueSupport = class
  public
    // Add*ToStrings methods
    [Test] [Category('Quick')]
    procedure TestAddTableMappings;
    [Test] [Category('Quick')]
    procedure TestAddAttributeKinds;
    [Test] [Category('Quick')]
    procedure TestAddOptimisticLockingModes;
    [Test] [Category('Quick')]
    procedure TestAddDeleteActions;
    [Test] [Category('Quick')]
    procedure TestAddDelphiFunctionTypes;
    [Test] [Category('Quick')]
    procedure TestAddPropertyAccessKinds;
    [Test] [Category('Quick')]
    procedure TestAddEvolutionStates;
    [Test] [Category('Quick')]
    procedure TestAddNationalCharConversions;
    [Test] [Category('Quick')]
    procedure TestAddDefaultRegionModes;

    // Roundtrip: ToString <-> StringTo
    [Test] [Category('Quick')]
    procedure TestAttributeKindRoundtrip;
    [Test] [Category('Quick')]
    procedure TestDeleteActionRoundtrip;
    [Test] [Category('Quick')]
    procedure TestDelphiFunctionTypeRoundtrip;
    [Test] [Category('Quick')]
    procedure TestDelphiPropertyAccessKindRoundtrip;
    [Test] [Category('Quick')]
    procedure TestTableMappingRoundtrip;
    [Test] [Category('Quick')]
    procedure TestOptimisticLockingModeRoundtrip;
    [Test] [Category('Quick')]
    procedure TestEvolutionStateRoundtrip;
    [Test] [Category('Quick')]
    procedure TestNationalCharConversionRoundtrip;
    [Test] [Category('Quick')]
    procedure TestDefaultRegionModeRoundtrip;

    // StringTo with specific values
    [Test] [Category('Quick')]
    procedure TestStringToStorage;
    [Test] [Category('Quick')]
    procedure TestStringToBoolean;
    [Test] [Category('Quick')]
    procedure TestStringToBoolean_Invalid;

    // StringTo with defaults/fallbacks
    [Test] [Category('Quick')]
    procedure TestStringToAttributeKind_Default;
    [Test] [Category('Quick')]
    procedure TestStringToDeleteAction_Default;
    [Test] [Category('Quick')]
    procedure TestStringToDelphiFunctionType_AbstractVirtualLegacy;
    [Test] [Category('Quick')]
    procedure TestStringToDelphiFunctionType_Default;
    [Test] [Category('Quick')]
    procedure TestStringToDelphiPropertyAccessKind_Default;
    [Test] [Category('Quick')]
    procedure TestStringToTableMapping_Default;
    [Test] [Category('Quick')]
    procedure TestStringToOptimisticLockingMode_OldNames;
    [Test] [Category('Quick')]
    procedure TestStringToOptimisticLockingMode_Default;
    [Test] [Category('Quick')]
    procedure TestStringToEvolutionState_Default;
    [Test] [Category('Quick')]
    procedure TestStringToNationalCharConversion_All;
    [Test] [Category('Quick')]
    procedure TestStringToDefaultRegionMode_Default;
    [Test] [Category('Quick')]
    procedure TestStringToStorage_Default;
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldDefs,
  BoldDefaultTaggedValues,
  BoldTaggedValueSupport;

{ TTestBoldTaggedValueSupport }

// --- Add*ToStrings ---

procedure TTestBoldTaggedValueSupport.TestAddTableMappings;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddTableMappings(SL);
    Assert.AreEqual(4, SL.Count);
    Assert.AreEqual(TV_TABLEMAPPING_OWN, SL[0]);
    Assert.AreEqual(TV_TABLEMAPPING_PARENT, SL[1]);
    Assert.AreEqual(TV_TABLEMAPPING_CHILDREN, SL[2]);
    Assert.AreEqual(TV_TABLEMAPPING_IMPORTED, SL[3]);
  finally
    SL.Free;
  end;
end;

procedure TTestBoldTaggedValueSupport.TestAddAttributeKinds;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddAttributeKinds(SL);
    Assert.AreEqual(2, SL.Count);
    Assert.AreEqual(TV_ATTRIBUTEKIND_BOLD, SL[0]);
    Assert.AreEqual(TV_ATTRIBUTEKIND_DELPHI, SL[1]);
  finally
    SL.Free;
  end;
end;

procedure TTestBoldTaggedValueSupport.TestAddOptimisticLockingModes;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddOptimisticLockingModes(SL);
    Assert.AreEqual(5, SL.Count);
    Assert.AreEqual(DEFAULTNAME, SL[0]);
  finally
    SL.Free;
  end;
end;

procedure TTestBoldTaggedValueSupport.TestAddDeleteActions;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddDeleteActions(SL);
    Assert.AreEqual(4, SL.Count);
  finally
    SL.Free;
  end;
end;

procedure TTestBoldTaggedValueSupport.TestAddDelphiFunctionTypes;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddDelphiFunctionTypes(SL);
    Assert.AreEqual(5, SL.Count);
  finally
    SL.Free;
  end;
end;

procedure TTestBoldTaggedValueSupport.TestAddPropertyAccessKinds;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddPropertyAccessKinds(SL);
    Assert.AreEqual(4, SL.Count);
  finally
    SL.Free;
  end;
end;

procedure TTestBoldTaggedValueSupport.TestAddEvolutionStates;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddEvolutionStates(SL);
    Assert.AreEqual(3, SL.Count);
    Assert.AreEqual(TV_EVOLUTIONSTATE_NORMAL, SL[0]);
    Assert.AreEqual(TV_EVOLUTIONSTATE_TOBEREMOVED, SL[1]);
    Assert.AreEqual(TV_EVOLUTIONSTATE_REMOVED, SL[2]);
  finally
    SL.Free;
  end;
end;

procedure TTestBoldTaggedValueSupport.TestAddNationalCharConversions;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddNationalCharConversions(SL);
    Assert.AreEqual(3, SL.Count);
  finally
    SL.Free;
  end;
end;

procedure TTestBoldTaggedValueSupport.TestAddDefaultRegionModes;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    TBoldTaggedValueSupport.AddDefaultRegionModes(SL);
    Assert.AreEqual(5, SL.Count);
  finally
    SL.Free;
  end;
end;

// --- Roundtrip tests ---

procedure TTestBoldTaggedValueSupport.TestAttributeKindRoundtrip;
begin
  Assert.AreEqual(bastBold,
    TBoldTaggedValueSupport.StringToAttributeKind(
      TBoldTaggedValueSupport.AttributeKindToString(bastBold)));
  Assert.AreEqual(bastDelphi,
    TBoldTaggedValueSupport.StringToAttributeKind(
      TBoldTaggedValueSupport.AttributeKindToString(bastDelphi)));
end;

procedure TTestBoldTaggedValueSupport.TestDeleteActionRoundtrip;
var
  DA: TDeleteAction;
begin
  for DA := Low(TDeleteAction) to High(TDeleteAction) do
    Assert.AreEqual(Ord(DA),
      Ord(TBoldTaggedValueSupport.StringToDeleteAction(
        TBoldTaggedValueSupport.DeleteActionToString(DA))));
end;

procedure TTestBoldTaggedValueSupport.TestDelphiFunctionTypeRoundtrip;
var
  DFT: TDelphiFunctionType;
begin
  for DFT := Low(TDelphiFunctionType) to High(TDelphiFunctionType) do
    Assert.AreEqual(Ord(DFT),
      Ord(TBoldTaggedValueSupport.StringToDelphiFunctionType(
        TBoldTaggedValueSupport.DelphiFunctionTypeToString(DFT))));
end;

procedure TTestBoldTaggedValueSupport.TestDelphiPropertyAccessKindRoundtrip;
var
  PAK: TDelphiPropertyAccessKind;
begin
  for PAK := Low(TDelphiPropertyAccessKind) to High(TDelphiPropertyAccessKind) do
    Assert.AreEqual(Ord(PAK),
      Ord(TBoldTaggedValueSupport.StringToDelphiPropertyAccessKind(
        TBoldTaggedValueSupport.DelphiPropertyAccessKindToString(PAK))));
end;

procedure TTestBoldTaggedValueSupport.TestTableMappingRoundtrip;
var
  TM: TTableMapping;
begin
  for TM := Low(TTableMapping) to High(TTableMapping) do
    Assert.AreEqual(Ord(TM),
      Ord(TBoldTaggedValueSupport.StringToTableMapping(
        TBoldTaggedValueSupport.TableMappingToString(TM))));
end;

procedure TTestBoldTaggedValueSupport.TestOptimisticLockingModeRoundtrip;
var
  OLM: TBoldOptimisticLockingMode;
begin
  for OLM := Low(TBoldOptimisticLockingMode) to High(TBoldOptimisticLockingMode) do
    Assert.AreEqual(Ord(OLM),
      Ord(TBoldTaggedValueSupport.StringToOptimisticLockingMode(
        TBoldTaggedValueSupport.OptimisticLockingModeToString(OLM))));
end;

procedure TTestBoldTaggedValueSupport.TestEvolutionStateRoundtrip;
var
  ES: TBoldEvolutionState;
begin
  for ES := Low(TBoldEvolutionState) to High(TBoldEvolutionState) do
    Assert.AreEqual(Ord(ES),
      Ord(TBoldTaggedValueSupport.StringToEvolutionState(
        TBoldTaggedValueSupport.EvolutionStateToString(ES))));
end;

procedure TTestBoldTaggedValueSupport.TestNationalCharConversionRoundtrip;
var
  NCC: TBoldNationalCharConversion;
begin
  for NCC := Low(TBoldNationalCharConversion) to High(TBoldNationalCharConversion) do
    Assert.AreEqual(Ord(NCC),
      Ord(TBoldTaggedValueSupport.StringToNationalCharConversion(
        TBoldTaggedValueSupport.NationalCharConversionToString(NCC))));
end;

procedure TTestBoldTaggedValueSupport.TestDefaultRegionModeRoundtrip;
var
  DRM: TBoldAssociationEndDefaultRegionMode;
begin
  for DRM := Low(TBoldAssociationEndDefaultRegionMode) to High(TBoldAssociationEndDefaultRegionMode) do
    Assert.AreEqual(Ord(DRM),
      Ord(TBoldTaggedValueSupport.StringToDefaultRegionMode(
        TBoldTaggedValueSupport.DefaultRegionModeToString(DRM))));
end;

// --- StringTo with specific values ---

procedure TTestBoldTaggedValueSupport.TestStringToStorage;
begin
  Assert.AreEqual(Ord(bsInternal),
    Ord(TBoldTaggedValueSupport.StringToStorage(TV_STORAGE_INTERNAL)));
  Assert.AreEqual(Ord(bsPartiallyExternal),
    Ord(TBoldTaggedValueSupport.StringToStorage(TV_STORAGE_PARTIALLYEXTERNAL)));
  Assert.AreEqual(Ord(bsExternal),
    Ord(TBoldTaggedValueSupport.StringToStorage(TV_STORAGE_EXTERNAL)));
  Assert.AreEqual(Ord(bsExternalKey),
    Ord(TBoldTaggedValueSupport.StringToStorage(TV_STORAGE_EXTERNALKEY)));
end;

procedure TTestBoldTaggedValueSupport.TestStringToBoolean;
begin
  Assert.IsTrue(TBoldTaggedValueSupport.StringToBoolean(TV_TRUE));
  Assert.IsFalse(TBoldTaggedValueSupport.StringToBoolean(TV_FALSE));
end;

procedure TTestBoldTaggedValueSupport.TestStringToBoolean_Invalid;
begin
  Assert.WillRaise(
    procedure begin TBoldTaggedValueSupport.StringToBoolean('Maybe'); end,
    EBold);
end;

// --- Default/fallback values ---

procedure TTestBoldTaggedValueSupport.TestStringToAttributeKind_Default;
begin
  Assert.AreEqual(Ord(bastBold),
    Ord(TBoldTaggedValueSupport.StringToAttributeKind('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToDeleteAction_Default;
begin
  Assert.AreEqual(Ord(daDefault),
    Ord(TBoldTaggedValueSupport.StringToDeleteAction('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToDelphiFunctionType_AbstractVirtualLegacy;
begin
  // Legacy string 'AbstractVirtual' maps to dfAbstractVirtual
  Assert.AreEqual(Ord(dfAbstractVirtual),
    Ord(TBoldTaggedValueSupport.StringToDelphiFunctionType('AbstractVirtual')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToDelphiFunctionType_Default;
begin
  Assert.AreEqual(Ord(dfNormal),
    Ord(TBoldTaggedValueSupport.StringToDelphiFunctionType('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToDelphiPropertyAccessKind_Default;
begin
  Assert.AreEqual(Ord(pkNone),
    Ord(TBoldTaggedValueSupport.StringToDelphiPropertyAccessKind('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToTableMapping_Default;
begin
  Assert.AreEqual(Ord(tmOwn),
    Ord(TBoldTaggedValueSupport.StringToTableMapping('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToOptimisticLockingMode_OldNames;
begin
  // Old name 'Member' maps to bolmModifiedMembers
  Assert.AreEqual(Ord(bolmModifiedMembers),
    Ord(TBoldTaggedValueSupport.StringToOptimisticLockingMode(
      TV_OPTIMISTICLOCKING_MODIFIEDMEMBERS_OLDNAME)));
  // Old name 'Class' maps to bolmAllMembers
  Assert.AreEqual(Ord(bolmAllMembers),
    Ord(TBoldTaggedValueSupport.StringToOptimisticLockingMode(
      TV_OPTIMISTICLOCKING_ALLMEMBERS_OLDNAME)));
end;

procedure TTestBoldTaggedValueSupport.TestStringToOptimisticLockingMode_Default;
begin
  Assert.AreEqual(Ord(bolmDefault),
    Ord(TBoldTaggedValueSupport.StringToOptimisticLockingMode('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToEvolutionState_Default;
begin
  Assert.AreEqual(Ord(esNormal),
    Ord(TBoldTaggedValueSupport.StringToEvolutionState('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToNationalCharConversion_All;
begin
  Assert.AreEqual(Ord(nccDefault),
    Ord(TBoldTaggedValueSupport.StringToNationalCharConversion(DEFAULTNAME)));
  Assert.AreEqual(Ord(nccTrue),
    Ord(TBoldTaggedValueSupport.StringToNationalCharConversion(TV_TRUE)));
  Assert.AreEqual(Ord(nccFalse),
    Ord(TBoldTaggedValueSupport.StringToNationalCharConversion(TV_FALSE)));
  // Unknown falls back to nccDefault
  Assert.AreEqual(Ord(nccDefault),
    Ord(TBoldTaggedValueSupport.StringToNationalCharConversion('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToDefaultRegionMode_Default;
begin
  Assert.AreEqual(Ord(aedrmDefault),
    Ord(TBoldTaggedValueSupport.StringToDefaultRegionMode('UnknownValue')));
end;

procedure TTestBoldTaggedValueSupport.TestStringToStorage_Default;
begin
  Assert.AreEqual(Ord(bsInternal),
    Ord(TBoldTaggedValueSupport.StringToStorage('UnknownValue')));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldTaggedValueSupport);

end.
