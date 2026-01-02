unit Test.BoldUMLModelValidator;

interface

uses
  DUnitX.TestFramework,
  BoldUMLAbstractModelValidator,
  BoldUMLModelValidator,
  BoldModel,
  BoldTypeNameDictionary,
  Test.BoldAttributes; // For TjehodmBoldTest

type
  [TestFixture]
  [Category('UMLModel')]
  TTestBoldUMLModelValidator = class
  private
    FDataModule: TjehodmBoldTest;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // Enum and constant tests
    [Test]
    [Category('Quick')]
    procedure TestDefaultValidatorSourceLanguageIsDelphi;
    [Test]
    [Category('Quick')]
    procedure TestSourceLanguageEnumHasDelphiValue;
    [Test]
    [Category('Quick')]
    procedure TestSourceLanguageEnumHasNoneValue;
    [Test]
    [Category('Quick')]
    procedure TestSourceLanguageEnumOnlyHasTwoValues;

    // Integration tests - exercises IsValidDelphiIdentifier and other validation
    [Test]
    [Category('Quick')]
    procedure TestValidateWithDelphiLanguage;
    [Test]
    [Category('Quick')]
    procedure TestValidateWithNoLanguage;
    [Test]
    [Category('Quick')]
    procedure TestValidatorHighestSeverity;
  end;

implementation

uses
  TypInfo,
  SysUtils,
  BoldUMLModel,
  BoldUMLAttributes;

{ TTestBoldUMLModelValidator }

procedure TTestBoldUMLModelValidator.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldUMLModelValidator.TearDown;
begin
  FreeAndNil(FDataModule);
end;

procedure TTestBoldUMLModelValidator.TestDefaultValidatorSourceLanguageIsDelphi;
begin
  // Verify the default validator source language is Delphi (not C++)
  Assert.AreEqual(mvslDelphi, BoldDefaultValidatorSourceLanguage,
    'Default validator source language should be mvslDelphi');
end;

procedure TTestBoldUMLModelValidator.TestSourceLanguageEnumHasDelphiValue;
begin
  // Verify mvslDelphi exists in the enum
  Assert.AreEqual('mvslDelphi', GetEnumName(TypeInfo(TBoldModelValidatorSourceLanguage), Ord(mvslDelphi)),
    'Enum should have mvslDelphi value');
end;

procedure TTestBoldUMLModelValidator.TestSourceLanguageEnumHasNoneValue;
begin
  // Verify mvslNone exists in the enum
  Assert.AreEqual('mvslNone', GetEnumName(TypeInfo(TBoldModelValidatorSourceLanguage), Ord(mvslNone)),
    'Enum should have mvslNone value');
end;

procedure TTestBoldUMLModelValidator.TestSourceLanguageEnumOnlyHasTwoValues;
var
  TypeData: PTypeData;
begin
  // Verify the enum only has 2 values (mvslNone, mvslDelphi) - C++ support removed
  TypeData := GetTypeData(TypeInfo(TBoldModelValidatorSourceLanguage));
  Assert.AreEqual(1, TypeData.MaxValue,
    'TBoldModelValidatorSourceLanguage should only have 2 values (0=mvslNone, 1=mvslDelphi)');
end;

procedure TTestBoldUMLModelValidator.TestValidateWithDelphiLanguage;
var
  Validator: TBoldUMLModelValidator;
begin
  // Create validator with Delphi language - exercises IsValidDelphiIdentifier
  Validator := TBoldUMLModelValidator.Create(FDataModule.BoldModel1, nil, mvslDelphi);
  try
    Validator.Validate(FDataModule.BoldModel1.TypeNameDictionary);
    // If we get here without exception, validation ran successfully
    Assert.Pass('Validation completed with mvslDelphi language');
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldUMLModelValidator.TestValidateWithNoLanguage;
var
  Validator: TBoldUMLModelValidator;
begin
  // Create validator with no language - skips source code name validation
  Validator := TBoldUMLModelValidator.Create(FDataModule.BoldModel1, nil, mvslNone);
  try
    Validator.Validate(FDataModule.BoldModel1.TypeNameDictionary);
    Assert.Pass('Validation completed with mvslNone language');
  finally
    Validator.Free;
  end;
end;

procedure TTestBoldUMLModelValidator.TestValidatorHighestSeverity;
var
  Validator: TBoldUMLModelValidator;
  Severity: TSeverity;
begin
  // Test that HighestSeverity works after validation
  Validator := TBoldUMLModelValidator.Create(FDataModule.BoldModel1, nil, mvslDelphi);
  try
    Validator.Validate(FDataModule.BoldModel1.TypeNameDictionary);
    Severity := Validator.HighestSeverity;
    // Valid model should have no errors (sNone or sHint or sWarning)
    Assert.IsTrue(Severity < sError,
      'Valid test model should not have errors');
  finally
    Validator.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUMLModelValidator);

end.
