{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit Test.BoldModelEditorData;

{ Tests of the data module that the model editor's LCL and FMX clients share
  (#128): its datasets show the classes of the model and the attributes of
  the selected class, its commands change the model, and a change made
  through a dataset, as a grid makes it, is saved and generated. The same
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
    procedure TestAChangeInADataSetIsSaved;
    procedure TestTheSuperclassIsReadOnly;
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
  end;

implementation

uses
  SysUtils,
  Classes,
  DB,
  BoldUMLModel,
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

procedure TTestBoldModelEditorData.TestAChangeInADataSetIsSaved;
var
  Saved: string;
begin
  Saved := IncludeTrailingPathDelimiter(FTempFolder) + 'Changed.bld';
  OpenInvoicing;
  SelectClassNamed('Customer');
  // As a grid changes a cell: the row is edited, and posted when the grid
  // leaves it, or when the model is saved
  FData.bdsClasses.Edit;
  FData.bdsClasses.FieldByName('Name').AsString := 'Client';
  Assert.IsTrue(FData.Modified, 'a row that is edited is a change');
  FData.bdsAttributes.First;
  FData.bdsAttributes.Edit;
  FData.bdsAttributes.FieldByName('Type').AsString := 'Integer';
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

procedure TTestBoldModelEditorData.TestTheSuperclassIsReadOnly;
begin
  OpenInvoicing;
  Assert.IsTrue(FData.bdsClasses.FieldByName('Superclass').ReadOnly,
    'the superclass column shows the name of another class');
  Assert.IsFalse(FData.bdsClasses.FieldByName('Name').ReadOnly, 'the name of the class is edited');
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

initialization
  BoldRegisterTest(TTestBoldModelEditorData);

end.
