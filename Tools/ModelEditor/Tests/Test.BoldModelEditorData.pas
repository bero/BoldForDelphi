{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit Test.BoldModelEditorData;

{ Tests of the data module that the model editor's LCL and FMX clients share
  (#128): its datasets show the classes of the model and the attributes of
  the selected class, its commands change the model, and a change made
  through the values of a client's dialog is saved and generated. The same
  source for the Delphi (DUnitX) and the Free Pascal (FPCUnit) test programs.
  No user interface is involved: the data module and its datasets work
  without one. }

interface

uses
  BoldTestFramework,
  BoldModelEditorData;

type
  TTestBoldModelEditorData = class(TBoldTestCase)
  private
    FTempFolder: string;
    FData: TdmModelEditor;
    // What the client's question and save dialog answer
    FAnswer: TModelEditorSaveAnswer;
    FAsked: Integer;
    FChosenFile: string;
    function AskSave(const ADocumentName: string): TModelEditorSaveAnswer;
    function ChooseFile(var AFileName: string): Boolean;
    procedure OpenInvoicing;
    // Invoicing.bld saved to the temporary folder, opened from there
    procedure OpenACopyOfInvoicing;
    procedure SelectClassNamed(const AName: string);
    procedure DeleteTheSelectedClass;
    // For WillRaise: a class and an attribute given an empty name
    procedure ApplyAnEmptyClassName;
    procedure ApplyAnEmptyAttributeName;
    // For WillRaise: Customer given its subclass Sub, or no superclass
    procedure MakeCustomerASubclassOfSub;
    procedure TakeCustomersSuperclass;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOpenShowsTheClasses;
    procedure TestSelectingAClassShowsItsAttributes;
    procedure TestOpenIsNoChange;
    procedure TestAddClassSelectsTheNewClass;
    procedure TestDeleteClassRemovesItsRow;
    procedure TestTheRootClassIsNotDeleted;
    procedure TestAddAttributeToTheSelectedClass;
    procedure TestDeleteAttributeRemovesItsRow;
    // A change made through the values of a client's dialog is saved
    procedure TestAChangeInTheDialogValuesIsSaved;
    // The grids show and select; the clients edit in a dialog
    procedure TestTheClassesAndAttributesAreReadOnly;
    // The values a client's dialogs show, and what they give back
    procedure TestClassValues;
    procedure TestNewClassValuesHaveANameNoClassHas;
    procedure TestAddClassWithTheValuesOfADialog;
    procedure TestAClassNeedsAName;
    // The superclass is chosen in the class dialog: of every class but the
    // class itself and its subclasses, so no cycle can be chosen
    procedure TestSuperclassNamesLeaveOutTheClassAndItsSubclasses;
    procedure TestTheRootClassHasNoSuperclassToChoose;
    procedure TestAddClassUnderTheChosenSuperclass;
    procedure TestApplyClassValuesMovesTheClass;
    procedure TestASuperclassThatMakesACycleIsRefused;
    procedure TestAClassOfABoldifiedModelNeedsASuperclass;
    // A model that is not boldified, as a new one, allows no superclass
    procedure TestANewModelAllowsNoSuperclass;
    procedure TestAttributeValues;
    procedure TestNewAttributeValues;
    procedure TestAddAttributeWithTheValuesOfADialog;
    procedure TestAnAttributeNeedsAName;
    // The attribute types the attribute dialog offers
    procedure TestTypeNamesForTheAttributeDialog;
    // Generating flattens and boldifies the UML model and undoes it
    procedure TestGenerateIsNoChange;
    // After a change Bold makes its stored model again from the UML model,
    // which it boldifies for that
    procedure TestGenerateAfterSavingAChangeIsNoChange;
    procedure TestAnAddedClassIsGenerated;
    procedure TestOpenAnotherModel;
    procedure TestNewModelHasTheRootClassAndCanBeSaved;
    procedure TestTitleShowsTheFileAndAChange;
    procedure TestCloseModelWithoutChangesAsksNothing;
    procedure TestCloseModelCancelKeepsTheChanges;
    procedure TestCloseModelSavesTheChanges;
    procedure TestCloseModelDiscards;
    procedure TestSaveModelOfANewModelAsksForAFile;
    procedure TestWhatCanBeDone;
    // The type names of a form, which the clients' status bars name
    procedure TestTypeNamesOfAForm;
    // The consistency check: its findings are the rows of bdsViolations,
    // the most severe first, and a status line sums them up
    procedure TestValidateShowsTheFindings;
    procedure TestValidationSummaryIsEmptyUntilAModelIsChecked;
    // A finding leads to what it is about
    procedure TestSelectViolationElementSelectsTheClass;
    procedure TestSelectViolationElementSelectsTheAttribute;
    // The findings are of the model checked: a model opened after has none
    procedure TestOpenDropsTheFindings;
    // The findings are shown, not edited
    procedure TestTheFindingsAreReadOnly;
  end;

implementation

uses
  SysUtils,
  Classes,
  DB,
  BoldUMLModel,
  BoldUMLAttributes,
  ModelEditorTestSupport;

const
  cInvoicing = 'examples/Compound/Invoicing/Model/Invoicing.bld';
  cDemoModel = 'examples/Shared/DemoModel.bld';
  cTypeNamesForm = 'examples/Simple/ObjectSpace/CustomAttributes/MainForm.dfm';

{ The Name field of every row, sorted, as comma text }
function SortedNames(ADataSet: TDataSet): string;
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      Names.Add(ADataSet.FieldByName('Name').AsString);
      ADataSet.Next;
    end;
    Names.Sort;
    Result := Names.CommaText;
  finally
    Names.Free;
  end;
end;

function SortedText(const ACommaText: string): string;
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    Names.CommaText := ACommaText;
    Names.Sort;
    Result := Names.CommaText;
  finally
    Names.Free;
  end;
end;

function ClassNamed(AModel: TUMLModel; const AName: string): TUMLClass;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to AModel.Classes.Count - 1 do
    if AModel.Classes[i].Name = AName then
      Exit(AModel.Classes[i] as TUMLClass);
end;

{ TTestBoldModelEditorData }

procedure TTestBoldModelEditorData.SetUp;
begin
  inherited;
  FTempFolder := NewTempFolder('BoldModelEditorData');
  FData := TdmModelEditor.Create(nil);
  FData.OnAskSave := AskSave;
  FData.OnChooseSaveFile := ChooseFile;
  FAnswer := saCancel;
  FAsked := 0;
  FChosenFile := '';
end;

procedure TTestBoldModelEditorData.TearDown;
begin
  FreeAndNil(FData);
  DeleteFolder(FTempFolder);
  inherited;
end;

function TTestBoldModelEditorData.AskSave(const ADocumentName: string): TModelEditorSaveAnswer;
begin
  Inc(FAsked);
  Result := FAnswer;
end;

function TTestBoldModelEditorData.ChooseFile(var AFileName: string): Boolean;
begin
  AFileName := FChosenFile;
  Result := FChosenFile <> '';
end;

procedure TTestBoldModelEditorData.OpenInvoicing;
begin
  FData.Open(RepositoryFile(cInvoicing));
end;

procedure TTestBoldModelEditorData.OpenACopyOfInvoicing;
var
  CopyName: string;
begin
  CopyName := IncludeTrailingPathDelimiter(FTempFolder) + 'Invoicing.bld';
  OpenInvoicing;
  FData.SaveAs(CopyName);
  FData.Open(CopyName);
end;

procedure TTestBoldModelEditorData.SelectClassNamed(const AName: string);
var
  AClass: TUMLClass;
begin
  AClass := ClassNamed(FData.Document.UMLModel, AName);
  Assert.IsNotNull(AClass, 'the model has a class ' + AName);
  Assert.IsTrue(FData.SelectClass(AClass), 'the class ' + AName + ' is selected');
end;

procedure TTestBoldModelEditorData.DeleteTheSelectedClass;
begin
  FData.DeleteClass;
end;

procedure TTestBoldModelEditorData.ApplyAnEmptyClassName;
var
  Values: TModelEditorClassValues;
begin
  Values := FData.ClassValues(FData.CurrentClass);
  Values.Name := ' ';
  FData.ApplyClassValues(FData.CurrentClass, Values);
end;

procedure TTestBoldModelEditorData.MakeCustomerASubclassOfSub;
var
  Values: TModelEditorClassValues;
begin
  Values := FData.ClassValues(ClassNamed(FData.Document.UMLModel, 'Customer'));
  Values.Superclass := 'Sub';
  FData.ApplyClassValues(ClassNamed(FData.Document.UMLModel, 'Customer'), Values);
end;

procedure TTestBoldModelEditorData.TakeCustomersSuperclass;
var
  Values: TModelEditorClassValues;
begin
  Values := FData.ClassValues(ClassNamed(FData.Document.UMLModel, 'Customer'));
  Values.Superclass := '';
  FData.ApplyClassValues(ClassNamed(FData.Document.UMLModel, 'Customer'), Values);
end;

procedure TTestBoldModelEditorData.ApplyAnEmptyAttributeName;
var
  Values: TModelEditorAttributeValues;
begin
  Values := FData.AttributeValues(FData.CurrentAttribute);
  Values.Name := '';
  FData.ApplyAttributeValues(FData.CurrentAttribute, Values);
end;

procedure TTestBoldModelEditorData.TestOpenShowsTheClasses;
var
  Names: string;
begin
  OpenInvoicing;
  Assert.AreEqual(FData.Document.UMLModel.Classes.Count, FData.bdsClasses.RecordCount,
    'a row per class of the model');
  Names := SortedNames(FData.bdsClasses);
  Assert.Contains(Names, 'Customer', 'the classes');
  Assert.Contains(Names, 'Invoice', 'the classes');
  Assert.Contains(Names, 'InvoiceLine', 'the classes');
end;

procedure TTestBoldModelEditorData.TestSelectingAClassShowsItsAttributes;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  Assert.AreEqual(SortedText('Number,Name,Email,Phone,PaymentTermDays,IsActive,Notes,' +
    'InvoicedTotal,OpenBalance,OverdueCount'), SortedNames(FData.bdsAttributes),
    'the attributes of Customer');
  SelectClassNamed('Payment');
  Assert.AreEqual(SortedText('PaymentDate,Amount,Reference'), SortedNames(FData.bdsAttributes),
    'the attributes of Payment');
end;

procedure TTestBoldModelEditorData.TestOpenIsNoChange;
begin
  OpenInvoicing;
  SelectClassNamed('Invoice');
  SortedNames(FData.bdsAttributes);
  FData.Refresh;
  Assert.IsFalse(FData.Modified, 'opening and looking is no change');
end;

procedure TTestBoldModelEditorData.TestAddClassSelectsTheNewClass;
var
  Count: Integer;
  NewClass: TUMLClass;
begin
  OpenInvoicing;
  Count := FData.bdsClasses.RecordCount;
  NewClass := FData.AddClass;
  Assert.AreEqual('NewClass', NewClass.Name, 'the name of a new class');
  Assert.AreEqual(Count + 1, FData.bdsClasses.RecordCount, 'a row more');
  Assert.AreSame(NewClass, FData.CurrentClass, 'the new class is selected');
  Assert.AreEqual('NewClass', FData.bdsClasses.FieldByName('Name').AsString, 'the selected row');
  Assert.AreEqual('InvoicingRoot', FData.bdsClasses.FieldByName('Superclass').AsString,
    'in a boldified model a new class has the root class as its superclass');
  Assert.IsTrue(NewClass.persistent, 'a new class is persistent');
  Assert.AreEqual(0, FData.bdsAttributes.RecordCount, 'a new class has no attributes');
  Assert.IsTrue(FData.Modified, 'a new class is a change');
end;

procedure TTestBoldModelEditorData.TestDeleteClassRemovesItsRow;
var
  Count: Integer;
begin
  OpenInvoicing;
  Count := FData.bdsClasses.RecordCount;
  FData.AddClass;
  FData.DeleteClass;
  Assert.AreEqual(Count, FData.bdsClasses.RecordCount, 'the row is gone');
  Assert.IsNull(ClassNamed(FData.Document.UMLModel, 'NewClass'), 'the class is gone');
end;

procedure TTestBoldModelEditorData.TestTheRootClassIsNotDeleted;
begin
  OpenInvoicing;
  SelectClassNamed('InvoicingRoot');
  Assert.WillRaise(DeleteTheSelectedClass, EBoldModelEditor, 'deleting the root class');
  Assert.IsNotNull(ClassNamed(FData.Document.UMLModel, 'InvoicingRoot'), 'the root class is still there');
end;

procedure TTestBoldModelEditorData.TestAddAttributeToTheSelectedClass;
var
  Attribute: TUMLAttribute;
begin
  OpenInvoicing;
  SelectClassNamed('Payment');
  Attribute := FData.AddAttribute;
  Assert.AreSame(FData.CurrentClass, Attribute.owner, 'the attribute belongs to the selected class');
  Assert.AreEqual('NewAttribute', Attribute.Name, 'the name of a new attribute');
  Assert.AreEqual('String', Attribute.typeName, 'the type of a new attribute');
  Assert.AreEqual(4, FData.bdsAttributes.RecordCount, 'a row more');
  Assert.AreSame(Attribute, FData.CurrentAttribute, 'the new attribute is selected');
  Assert.AreEqual('NewAttribute', FData.bdsAttributes.FieldByName('Name').AsString, 'the selected row');
end;

procedure TTestBoldModelEditorData.TestDeleteAttributeRemovesItsRow;
begin
  OpenInvoicing;
  SelectClassNamed('Payment');
  FData.bdsAttributes.First;
  while not FData.bdsAttributes.Eof and (FData.bdsAttributes.FieldByName('Name').AsString <> 'Reference') do
    FData.bdsAttributes.Next;
  Assert.AreEqual('Reference', FData.CurrentAttribute.Name, 'the selected attribute');
  FData.DeleteAttribute;
  Assert.AreEqual(SortedText('PaymentDate,Amount'), SortedNames(FData.bdsAttributes), 'the attributes left');
end;

procedure TTestBoldModelEditorData.TestAChangeInTheDialogValuesIsSaved;
var
  Saved: string;
  ClassValues: TModelEditorClassValues;
  AttributeValues: TModelEditorAttributeValues;
begin
  Saved := IncludeTrailingPathDelimiter(FTempFolder) + 'Changed.bld';
  OpenInvoicing;
  SelectClassNamed('Customer');
  // As a client's class dialog gives a class its values back
  ClassValues := FData.ClassValues(FData.CurrentClass);
  ClassValues.Name := 'Client';
  FData.ApplyClassValues(FData.CurrentClass, ClassValues);
  Assert.IsTrue(FData.Modified, 'a change');
  FData.bdsAttributes.First;
  AttributeValues := FData.AttributeValues(FData.CurrentAttribute);
  AttributeValues.TypeName := 'Integer';
  FData.ApplyAttributeValues(FData.CurrentAttribute, AttributeValues);
  FData.SaveAs(Saved);
  Assert.IsFalse(FData.Modified, 'saved');
  Assert.AreEqual(Saved, FData.Document.FileName, 'the file name after SaveAs');

  FData.Open(Saved);
  Assert.IsNotNull(ClassNamed(FData.Document.UMLModel, 'Client'), 'read back, the class is called Client');
  Assert.IsNull(ClassNamed(FData.Document.UMLModel, 'Customer'), 'read back, there is no Customer');
  SelectClassNamed('Client');
  FData.bdsAttributes.First;
  Assert.AreEqual('Integer', FData.bdsAttributes.FieldByName('Type').AsString,
    'read back, the first attribute of Client has the new type');
end;

procedure TTestBoldModelEditorData.TestTheClassesAndAttributesAreReadOnly;
var
  i: Integer;
begin
  OpenInvoicing;
  for i := 0 to FData.bdsClasses.FieldCount - 1 do
    Assert.IsTrue(FData.bdsClasses.Fields[i].ReadOnly, 'the class column ' + FData.bdsClasses.Fields[i].FieldName);
  for i := 0 to FData.bdsAttributes.FieldCount - 1 do
    Assert.IsTrue(FData.bdsAttributes.Fields[i].ReadOnly, 'the attribute column ' + FData.bdsAttributes.Fields[i].FieldName);
end;

procedure TTestBoldModelEditorData.TestClassValues;
var
  Values: TModelEditorClassValues;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  Values := FData.ClassValues(FData.CurrentClass);
  Assert.AreEqual('Customer', Values.Name, 'the name');
  Assert.AreEqual(FData.bdsClasses.FieldByName('Superclass').AsString, Values.Superclass, 'the superclass');
  Assert.AreEqual(FData.CurrentClass.persistent, Values.Persistent, 'persistent');
  Assert.AreEqual(FData.CurrentClass.isAbstract, Values.Abstract, 'abstract');
  Values.Name := 'Client';
  Values.Abstract := not Values.Abstract;
  Values.Persistent := not Values.Persistent;
  FData.ApplyClassValues(FData.CurrentClass, Values);
  Assert.AreEqual('Client', FData.CurrentClass.name, 'the new name');
  Assert.AreEqual(Values.Abstract, FData.CurrentClass.isAbstract, 'abstract changed');
  Assert.AreEqual(Values.Persistent, FData.CurrentClass.persistent, 'persistent changed');
  Assert.AreEqual('InvoicingRoot', FData.CurrentClass.superclass.name, 'the same superclass');
end;

procedure TTestBoldModelEditorData.TestNewClassValuesHaveANameNoClassHas;
var
  Values: TModelEditorClassValues;
begin
  OpenInvoicing;
  Values := FData.NewClassValues;
  Assert.AreEqual('NewClass', Values.Name, 'the name of a new class');
  Assert.AreEqual('InvoicingRoot', Values.Superclass, 'the root class of the boldified model');
  Assert.IsTrue(Values.Persistent, 'persistent');
  Assert.IsFalse(Values.Abstract, 'not abstract');
  FData.AddClass(Values);
  Assert.AreEqual('NewClass_1', FData.NewClassValues.Name, 'a name no class has');
end;

procedure TTestBoldModelEditorData.TestAddClassWithTheValuesOfADialog;
var
  Values: TModelEditorClassValues;
  Count: Integer;
begin
  OpenInvoicing;
  Count := FData.bdsClasses.RecordCount;
  Values := FData.NewClassValues;
  Values.Name := 'Reminder';
  Values.Abstract := True;
  FData.AddClass(Values);
  Assert.AreEqual(Count + 1, FData.bdsClasses.RecordCount, 'a row more');
  Assert.AreEqual('Reminder', FData.CurrentClass.name, 'the new class is selected');
  Assert.IsTrue(FData.CurrentClass.isAbstract, 'abstract, as the dialog said');
  Assert.AreEqual('InvoicingRoot', FData.CurrentClass.superclass.name, 'under the root class');
  Assert.IsTrue(FData.Modified, 'a change');
end;

procedure TTestBoldModelEditorData.TestAClassNeedsAName;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  Assert.WillRaise(ApplyAnEmptyClassName, EBoldModelEditor, 'an empty name is refused');
  Assert.AreEqual('Customer', FData.CurrentClass.name, 'the name stays');
end;

{ Customer gets a subclass Sub, and Sub a subclass SubSub }
procedure AddSubclassesOfCustomer(AData: TdmModelEditor);
var
  Values: TModelEditorClassValues;
begin
  Values := AData.NewClassValues;
  Values.Name := 'Sub';
  Values.Superclass := 'Customer';
  AData.AddClass(Values);
  Values := AData.NewClassValues;
  Values.Name := 'SubSub';
  Values.Superclass := 'Sub';
  AData.AddClass(Values);
end;

procedure TTestBoldModelEditorData.TestSuperclassNamesLeaveOutTheClassAndItsSubclasses;
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    OpenInvoicing;
    AddSubclassesOfCustomer(FData);
    FData.GetSuperclassNames(ClassNamed(FData.Document.UMLModel, 'Customer'), Names);
    Assert.IsTrue(Names.IndexOf('Customer') < 0, 'not the class itself: ' + Names.CommaText);
    Assert.IsTrue(Names.IndexOf('Sub') < 0, 'nor its subclass: ' + Names.CommaText);
    Assert.IsTrue(Names.IndexOf('SubSub') < 0, 'nor the subclass of that: ' + Names.CommaText);
    Assert.IsTrue(Names.IndexOf('Invoice') >= 0, 'another class: ' + Names.CommaText);
    Assert.IsTrue(Names.IndexOf('InvoicingRoot') >= 0, 'the root class: ' + Names.CommaText);
    Assert.IsTrue(Names.IndexOf('') < 0, 'no "none" in a boldified model');
    FData.GetSuperclassNames(nil, Names);
    Assert.AreEqual(FData.Document.UMLModel.Classes.Count, Names.Count, 'every class for a new class');
  finally
    Names.Free;
  end;
end;

procedure TTestBoldModelEditorData.TestTheRootClassHasNoSuperclassToChoose;
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    OpenInvoicing;
    FData.GetSuperclassNames(ClassNamed(FData.Document.UMLModel, 'InvoicingRoot'), Names);
    Assert.AreEqual(0, Names.Count, 'none: ' + Names.CommaText);
  finally
    Names.Free;
  end;
end;

procedure TTestBoldModelEditorData.TestAddClassUnderTheChosenSuperclass;
begin
  OpenInvoicing;
  AddSubclassesOfCustomer(FData);
  Assert.AreEqual('SubSub', FData.CurrentClass.name, 'the last class added is selected');
  Assert.AreEqual('Sub', FData.CurrentClass.superclass.name, 'under the superclass chosen');
  Assert.AreEqual('Customer', ClassNamed(FData.Document.UMLModel, 'Sub').superclass.name, 'Sub under Customer');
end;

procedure TTestBoldModelEditorData.TestApplyClassValuesMovesTheClass;
var
  Values: TModelEditorClassValues;
begin
  OpenInvoicing;
  SelectClassNamed('Payment');
  Values := FData.ClassValues(FData.CurrentClass);
  Values.Superclass := 'Customer';
  FData.ApplyClassValues(FData.CurrentClass, Values);
  Assert.AreEqual('Customer', FData.CurrentClass.superclass.name, 'Payment is under Customer now');
  Assert.IsTrue(ClassNamed(FData.Document.UMLModel, 'Customer').subclasses.IndexOf(FData.CurrentClass) >= 0,
    'a subclass of Customer');
  Assert.IsTrue(FData.Modified, 'a change');
end;

procedure TTestBoldModelEditorData.TestASuperclassThatMakesACycleIsRefused;
begin
  OpenInvoicing;
  AddSubclassesOfCustomer(FData);
  Assert.WillRaise(MakeCustomerASubclassOfSub, EBoldModelEditor, 'a subclass is refused as the superclass');
  Assert.AreEqual('InvoicingRoot', ClassNamed(FData.Document.UMLModel, 'Customer').superclass.name,
    'the superclass stays');
end;

procedure TTestBoldModelEditorData.TestAClassOfABoldifiedModelNeedsASuperclass;
begin
  OpenInvoicing;
  Assert.WillRaise(TakeCustomersSuperclass, EBoldModelEditor, 'no superclass is refused');
  Assert.AreEqual('InvoicingRoot', ClassNamed(FData.Document.UMLModel, 'Customer').superclass.name,
    'the superclass stays');
end;

procedure TTestBoldModelEditorData.TestANewModelAllowsNoSuperclass;
var
  Names: TStringList;
  Values: TModelEditorClassValues;
begin
  Names := TStringList.Create;
  try
    FData.NewModel;
    Values := FData.NewClassValues;
    Assert.AreEqual('', Values.Superclass, 'a new class of a new model has none');
    FData.GetSuperclassNames(nil, Names);
    Assert.IsTrue(Names.IndexOf('') >= 0, '"none" is offered: ' + Names.CommaText);
    Assert.IsTrue(Names.Count > 1, 'and the classes: ' + Names.CommaText);
    FData.AddClass(Values);
    Assert.IsNull(FData.CurrentClass.superclass, 'added without a superclass');
    Values := FData.ClassValues(FData.CurrentClass);
    Values.Superclass := Names[1];
    FData.ApplyClassValues(FData.CurrentClass, Values);
    Assert.AreEqual(Names[1], FData.CurrentClass.superclass.name, 'given one');
    Values.Superclass := '';
    FData.ApplyClassValues(FData.CurrentClass, Values);
    Assert.IsNull(FData.CurrentClass.superclass, 'and none again');
  finally
    Names.Free;
  end;
end;

procedure TTestBoldModelEditorData.TestAttributeValues;
var
  Values: TModelEditorAttributeValues;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  FData.bdsAttributes.First;
  Values := FData.AttributeValues(FData.CurrentAttribute);
  Assert.AreEqual(FData.bdsAttributes.FieldByName('Name').AsString, Values.Name, 'the name');
  Assert.AreEqual(FData.bdsAttributes.FieldByName('Type').AsString, Values.TypeName, 'the type');
  Values.Name := 'Renamed';
  Values.TypeName := 'Integer';
  Values.Derived := not Values.Derived;
  Values.Persistent := not Values.Persistent;
  FData.ApplyAttributeValues(FData.CurrentAttribute, Values);
  Assert.AreEqual('Renamed', FData.CurrentAttribute.name, 'the new name');
  Assert.AreEqual('Integer', FData.CurrentAttribute.typeName, 'the new type');
  Assert.AreEqual(Values.Derived, FData.CurrentAttribute.derived, 'derived changed');
  Assert.AreEqual(Values.Persistent, FData.CurrentAttribute.persistent, 'persistent changed');
end;

procedure TTestBoldModelEditorData.TestNewAttributeValues;
var
  Values: TModelEditorAttributeValues;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  Values := FData.NewAttributeValues;
  Assert.AreEqual('NewAttribute', Values.Name, 'the name of a new attribute');
  Assert.AreEqual('String', Values.TypeName, 'the type');
  Assert.AreEqual(FData.CurrentClass.persistent, Values.Persistent, 'persistent as the class is');
  Assert.IsFalse(Values.Derived, 'not derived');
  FData.AddAttribute(Values);
  Assert.AreEqual('NewAttribute_1', FData.NewAttributeValues.Name, 'a name no attribute of the class has');
end;

procedure TTestBoldModelEditorData.TestAddAttributeWithTheValuesOfADialog;
var
  Values: TModelEditorAttributeValues;
  Count: Integer;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  Count := FData.bdsAttributes.RecordCount;
  Values := FData.NewAttributeValues;
  Values.Name := 'Phone';
  Values.TypeName := 'Integer';
  FData.AddAttribute(Values);
  Assert.AreEqual(Count + 1, FData.bdsAttributes.RecordCount, 'a row more');
  Assert.AreEqual('Phone', FData.CurrentAttribute.name, 'the new attribute is selected');
  Assert.AreEqual('Integer', FData.CurrentAttribute.typeName, 'of the type the dialog said');
  Assert.AreEqual('Customer', TUMLClass(FData.CurrentAttribute.owner).name, 'of the selected class');
end;

procedure TTestBoldModelEditorData.TestAnAttributeNeedsAName;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  FData.bdsAttributes.First;
  Assert.WillRaise(ApplyAnEmptyAttributeName, EBoldModelEditor, 'an empty name is refused');
  Assert.AreNotEqual('', FData.CurrentAttribute.name, 'the name stays');
end;

procedure TTestBoldModelEditorData.TestTypeNamesForTheAttributeDialog;
var
  Names: TStringList;
  i: Integer;
begin
  Names := TStringList.Create;
  try
    OpenInvoicing;
    FData.GetTypeNames(Names);
    Assert.IsTrue(Names.IndexOf('String') >= 0, 'String: ' + Names.CommaText);
    Assert.IsTrue(Names.IndexOf('Integer') >= 0, 'Integer: ' + Names.CommaText);
    Assert.IsTrue(Names.IndexOf('<Default>') < 0, 'not <Default>');
    Assert.IsTrue(Names.IndexOf('<Moment>') < 0, 'nor the other abstract ones');
    Assert.IsTrue(Names.IndexOf('Coordinate') < 0, 'Bold''s type names have no Coordinate');
    for i := 1 to Names.Count - 1 do
      Assert.IsTrue(AnsiCompareText(Names[i - 1], Names[i]) < 0, 'sorted: ' + Names[i - 1] + ', ' + Names[i]);
    FData.LoadTypeNames(RepositoryFile(cTypeNamesForm));
    FData.GetTypeNames(Names);
    Assert.IsTrue(Names.IndexOf('Coordinate') >= 0, 'the form''s type names have Coordinate: ' + Names.CommaText);
  finally
    Names.Free;
  end;
end;

procedure TTestBoldModelEditorData.TestGenerateIsNoChange;
begin
  OpenInvoicing;
  FData.GenerateCode(FTempFolder);
  Assert.IsTrue(FileExists(IncludeTrailingPathDelimiter(FTempFolder) + 'InvoicingClasses.pas'),
    'the unit is generated');
  Assert.IsFalse(FData.Modified, 'generating is no change');
end;

procedure TTestBoldModelEditorData.TestGenerateAfterSavingAChangeIsNoChange;
begin
  // A new model is not boldified (Invoicing.bld is): Bold boldifies it for
  // the generator and undoes that after
  FData.NewModel;
  FData.AddClass;
  FData.SaveAs(IncludeTrailingPathDelimiter(FTempFolder) + 'New.bld');
  Assert.IsFalse(FData.Modified, 'saved');
  FData.GenerateCode(FTempFolder);
  Assert.IsFalse(FData.Modified, 'generating is no change');
end;

procedure TTestBoldModelEditorData.TestAnAddedClassIsGenerated;
var
  NewClass: TUMLClass;
begin
  OpenInvoicing;
  NewClass := FData.AddClass;
  NewClass.Name := 'Reminder';
  FData.AddAttribute.Name := 'SentOn';
  FData.GenerateCode(FTempFolder);
  Assert.Contains(TextOf(IncludeTrailingPathDelimiter(FTempFolder) + 'InvoicingClasses_Interface.inc'),
    'TReminder = class', 'the new class in the generated code');
  Assert.Contains(TextOf(IncludeTrailingPathDelimiter(FTempFolder) + 'InvoicingClasses_Interface.inc'),
    'property SentOn', 'the new attribute in the generated code');
  Assert.IsTrue(FData.Modified, 'still changed after generating');
end;

procedure TTestBoldModelEditorData.TestOpenAnotherModel;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  FData.Open(RepositoryFile(cDemoModel));
  Assert.Contains(SortedNames(FData.bdsClasses), 'Project', 'the classes of the second model');
  Assert.IsNull(ClassNamed(FData.Document.UMLModel, 'Customer'), 'nothing of the first model');
  SelectClassNamed('Task');
  Assert.AreEqual(SortedText('Title,DueDate,Priority,IsCompleted'), SortedNames(FData.bdsAttributes),
    'the attributes of Task');
  Assert.IsFalse(FData.Modified, 'just opened');
end;

procedure TTestBoldModelEditorData.TestNewModelHasTheRootClassAndCanBeSaved;
var
  Saved: string;
begin
  Saved := IncludeTrailingPathDelimiter(FTempFolder) + 'New.bld';
  OpenInvoicing;
  FData.NewModel;
  // Bold's empty model, as the Delphi IDE makes it: New_Model with its root
  // class New_ModelRoot
  Assert.AreEqual(1, FData.bdsClasses.RecordCount, 'a new model has its root class only');
  Assert.AreEqual(FData.Document.UMLModel.Name + 'Root', SortedNames(FData.bdsClasses), 'the root class');
  Assert.AreEqual('', FData.Document.FileName, 'a new model has no file');
  FData.AddClass.Name := 'First';
  FData.AddAttribute.Name := 'Title';
  FData.SaveAs(Saved);
  FData.Open(Saved);
  SelectClassNamed('First');
  Assert.AreEqual('Title', SortedNames(FData.bdsAttributes), 'read back, the attribute of the class');
end;

procedure TTestBoldModelEditorData.TestTitleShowsTheFileAndAChange;
begin
  Assert.AreEqual('Untitled - ' + cModelEditorTitle, FData.Title, 'before a model is opened');
  OpenInvoicing;
  Assert.AreEqual('Invoicing.bld - ' + cModelEditorTitle, FData.Title, 'opened');
  FData.AddClass;
  Assert.AreEqual('Invoicing.bld* - ' + cModelEditorTitle, FData.Title, 'changed');
end;

procedure TTestBoldModelEditorData.TestCloseModelWithoutChangesAsksNothing;
begin
  OpenInvoicing;
  Assert.IsTrue(FData.CloseModel, 'a model without changes may be dropped');
  Assert.AreEqual(0, FAsked, 'nothing to ask');
end;

procedure TTestBoldModelEditorData.TestCloseModelCancelKeepsTheChanges;
begin
  OpenInvoicing;
  FData.AddClass;
  FAnswer := saCancel;
  Assert.IsFalse(FData.CloseModel, 'cancelled');
  Assert.AreEqual(1, FAsked, 'asked once');
  Assert.IsTrue(FData.Modified, 'the change is still there');
end;

procedure TTestBoldModelEditorData.TestCloseModelSavesTheChanges;
var
  FileName: string;
begin
  OpenACopyOfInvoicing;
  FileName := FData.Document.FileName;
  FData.AddClass;
  FAnswer := saSave;
  Assert.IsTrue(FData.CloseModel, 'saved, so the model may be dropped');
  Assert.IsFalse(FData.Modified, 'saved');
  Assert.Contains(TextOf(FileName), '"NewClass"', 'the file has the new class');
end;

procedure TTestBoldModelEditorData.TestCloseModelDiscards;
begin
  OpenACopyOfInvoicing;
  FData.AddClass;
  FAnswer := saDiscard;
  Assert.IsTrue(FData.CloseModel, 'discarded, so the model may be dropped');
  Assert.IsFalse(Pos('"NewClass"', TextOf(FData.Document.FileName)) > 0, 'the file is as it was');
end;

procedure TTestBoldModelEditorData.TestSaveModelOfANewModelAsksForAFile;
begin
  FData.NewModel;
  FData.AddClass;
  FChosenFile := '';
  Assert.IsFalse(FData.SaveModel, 'the save dialog was cancelled');
  Assert.IsTrue(FData.Modified, 'not saved');
  FChosenFile := IncludeTrailingPathDelimiter(FTempFolder) + 'Chosen.bld';
  Assert.IsTrue(FData.SaveModel, 'saved to the chosen file');
  Assert.AreEqual(FChosenFile, FData.Document.FileName, 'the file name after the save');
  Assert.IsTrue(FileExists(FChosenFile), 'the file is written');
  Assert.IsFalse(FData.Modified, 'saved');
end;

procedure TTestBoldModelEditorData.TestWhatCanBeDone;
begin
  OpenInvoicing;
  SelectClassNamed('InvoicingRoot');
  Assert.IsFalse(FData.CanDeleteClass, 'the root class of a boldified model');
  Assert.IsTrue(FData.CanAddAttribute, 'an attribute of the root class');
  Assert.IsFalse(FData.CanDeleteAttribute, 'the root class has no attributes');
  SelectClassNamed('Payment');
  Assert.IsTrue(FData.CanDeleteClass, 'another class');
  Assert.IsTrue(FData.CanDeleteAttribute, 'the selected attribute of Payment');
end;

procedure TTestBoldModelEditorData.TestTypeNamesOfAForm;
begin
  OpenInvoicing;
  Assert.AreEqual(sBoldTypeNames, FData.TypeNamesName, 'Bold''s type names at first');
  FData.LoadTypeNames(RepositoryFile(cTypeNamesForm));
  Assert.AreEqual(RepositoryFile(cTypeNamesForm), FData.Document.TypeNamesFile, 'the form of the document''s type names');
  Assert.AreEqual('MainForm.dfm', FData.TypeNamesName, 'the form''s name');
  Assert.IsFalse(FData.Modified, 'type names are no change of the model');
end;

{ Moves bdsViolations to the first finding whose description has AText }
function SelectViolationWith(AData: TdmModelEditor; const AText: string): Boolean;
begin
  Result := False;
  AData.bdsViolations.First;
  while not AData.bdsViolations.Eof do
  begin
    if Pos(AText, AData.bdsViolations.FieldByName('Description').AsString) > 0 then
      Exit(True);
    AData.bdsViolations.Next;
  end;
end;

{ The rows of bdsViolations, one per line, for a failure message }
function FindingsText(AData: TdmModelEditor): string;
begin
  Result := '';
  AData.bdsViolations.First;
  while not AData.bdsViolations.Eof do
  begin
    Result := Result + AData.bdsViolations.FieldByName('Severity').AsString + ': ' +
      AData.bdsViolations.FieldByName('Description').AsString + ' (' +
      AData.bdsViolations.FieldByName('Element').AsString + ')' + sLineBreak;
    AData.bdsViolations.Next;
  end;
end;

procedure TTestBoldModelEditorData.TestValidateShowsTheFindings;
var
  Violations: TViolationList;
begin
  OpenInvoicing;
  ClassNamed(FData.Document.UMLModel, 'Address').name := 'Customer';
  Assert.AreEqual(Ord(sError), Ord(FData.Validate), 'an error');
  Violations := FData.Document.UMLModel.Validator.Violation;
  Assert.IsTrue(Violations.Count > 0, 'findings');
  Assert.AreEqual(Violations.Count, FData.bdsViolations.RecordCount, 'a row per finding: ' + FindingsText(FData));
  FData.bdsViolations.First;
  Assert.AreEqual('Error', FData.bdsViolations.FieldByName('Severity').AsString, 'an error first: ' + FindingsText(FData));
  Assert.IsTrue(SelectViolationWith(FData, 'Duplicate class name'), 'the duplicate class name: ' + FindingsText(FData));
  Assert.AreEqual('Customer', FData.bdsViolations.FieldByName('Element').AsString, 'the element it is about');
  Assert.IsTrue(Pos('Consistency check: ', FData.ValidationSummary) = 1, FData.ValidationSummary);
  Assert.IsTrue(Pos(' errors', FData.ValidationSummary) > 0, 'the summary counts the errors: ' + FData.ValidationSummary);
end;

procedure TTestBoldModelEditorData.TestValidationSummaryIsEmptyUntilAModelIsChecked;
begin
  OpenInvoicing;
  Assert.AreEqual('', FData.ValidationSummary, 'not checked yet');
  FData.Validate;
  Assert.AreNotEqual('', FData.ValidationSummary, 'checked');
  FData.NewModel;
  Assert.AreEqual('', FData.ValidationSummary, 'a new model is not checked yet');
  FData.Validate;
  Assert.AreEqual('Consistency check: no findings', FData.ValidationSummary, 'nothing wrong with a new model');
end;

procedure TTestBoldModelEditorData.TestSelectViolationElementSelectsTheClass;
begin
  OpenInvoicing;
  ClassNamed(FData.Document.UMLModel, 'Address').name := 'Customer';
  FData.Validate;
  SelectClassNamed('Invoice');
  Assert.IsTrue(SelectViolationWith(FData, 'Duplicate class name'), 'the duplicate class name: ' + FindingsText(FData));
  Assert.IsTrue(FData.SelectViolationElement, 'the class is selected');
  Assert.AreEqual('Customer', FData.CurrentClass.name, 'a class named Customer');
  Assert.AreSame(FData.CurrentViolation.ModelElement, FData.CurrentClass, 'the class the finding is about');
end;

procedure TTestBoldModelEditorData.TestSelectViolationElementSelectsTheAttribute;
var
  Attribute: TUMLAttribute;
begin
  OpenInvoicing;
  SelectClassNamed('Customer');
  FData.bdsAttributes.Last;
  Attribute := FData.CurrentAttribute;
  Assert.IsNotNull(Attribute, 'an attribute of Customer');
  Attribute.name := 'begin';
  SelectClassNamed('Invoice');
  Assert.AreEqual(Ord(sError), Ord(FData.Validate), 'an error');
  Assert.IsTrue(SelectViolationWith(FData, 'is a reserved word'), 'the reserved word: ' + FindingsText(FData));
  Assert.IsTrue(FData.SelectViolationElement, 'the attribute is selected');
  Assert.AreEqual('Customer', FData.CurrentClass.name, 'in its class');
  Assert.AreSame(Attribute, FData.CurrentAttribute, 'the attribute the finding is about');
end;

procedure TTestBoldModelEditorData.TestOpenDropsTheFindings;
begin
  OpenInvoicing;
  ClassNamed(FData.Document.UMLModel, 'Address').name := 'Customer';
  FData.Validate;
  Assert.IsTrue(FData.bdsViolations.RecordCount > 0, 'findings');
  OpenInvoicing;
  Assert.AreEqual(0, FData.bdsViolations.RecordCount, 'none for the model opened after');
  Assert.IsNull(FData.CurrentViolation, 'none selected');
  Assert.IsFalse(FData.SelectViolationElement, 'nothing to select');
end;

procedure TTestBoldModelEditorData.TestTheFindingsAreReadOnly;
var
  i: Integer;
begin
  OpenInvoicing;
  for i := 0 to FData.bdsViolations.FieldCount - 1 do
    Assert.IsTrue(FData.bdsViolations.Fields[i].ReadOnly, FData.bdsViolations.Fields[i].FieldName + ' is read-only');
end;

initialization
  BoldRegisterTest(TTestBoldModelEditorData);

end.
