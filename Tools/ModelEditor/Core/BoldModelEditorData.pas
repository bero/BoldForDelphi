{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
{$INCLUDE bold.inc}
unit BoldModelEditorData;

{ The part of the standalone model editor (#128) that the LCL and the FMX
  client share: the model document, the Bold handles on its UML model, the
  datasets the clients' grids show, and the commands of the editor. A client
  adds only its controls: grids bound to the datasets (a TDataSource for the
  LCL's TDBGrid, LiveBindings for FMX), menus and buttons that call the
  commands, and the file dialogs.

  The data module (BoldModelEditorData.dfm) is read by both compilers, as
  TDataModule is declared in Classes on both. The unit imports no VCL, LCL or
  FMX unit, and nothing from Bold's VCL model editor (Source/UMLModel/Editor).

  The handles:
    rhModel       the UML model of the document
    lhClasses     its classes (classes)
    lhAttributes  the attributes of the class selected in lhClasses
                  (feature->filterOnType(UMLAttribute))
    lhViolations  the findings of the last consistency check, the most
                  severe first (validator.violation->orderdescending(severity),
                  as the IDE's validation form lists them)
  bdsClasses, bdsAttributes and bdsViolations are TBoldDataSets on the
  three list handles; bdsViolations is read-only.
  Selecting a row in bdsClasses moves lhClasses, and lhAttributes follows,
  as in the Master-Detail demo. The handles get the object space of the
  document's model in Connect, after each New and Open: each model has an
  object space of its own (TBoldModel.SystemHandle).

  Classes and attributes are added, changed and deleted by the commands
  below, not by the datasets, which are read-only: a client edits a class or
  an attribute in a dialog of its own, filled from ClassValues or
  NewClassValues (AttributeValues, NewAttributeValues) and given back to
  ApplyClassValues or AddClass (ApplyAttributeValues, AddAttribute). A new
  class needs its namespace, its Bold tagged values and, in a boldified
  model, the root class as its superclass, and a deleted one hands its
  subclasses to its superclass. }

interface

uses
  SysUtils,
  Classes,
  DB,
  BoldElements,
  BoldHandles,
  BoldReferenceHandle,
  BoldListHandle,
  BoldDataSet,
  BoldUMLAttributes,
  BoldUMLModel,
  BoldModelDocument;

type
  EBoldModelEditor = class(Exception);

  { The answer to "save the changes of <model>?" }
  TModelEditorSaveAnswer = (saSave, saDiscard, saCancel);
  TModelEditorAskSaveEvent = function(const ADocumentName: string): TModelEditorSaveAnswer of object;
  { A save dialog: True with the chosen file in AFileName, False when the
    user cancels }
  TModelEditorChooseFileEvent = function(var AFileName: string): Boolean of object;

  { What a client's class dialog shows and edits: the columns of bdsClasses.
    The superclass is shown, not changed. }
  TModelEditorClassValues = record
    Name: string;
    Superclass: string;
    Persistent: Boolean;
    Abstract: Boolean;
  end;

  { What a client's attribute dialog shows and edits: the columns of
    bdsAttributes }
  TModelEditorAttributeValues = record
    Name: string;
    TypeName: string;
    Persistent: Boolean;
    Derived: Boolean;
  end;

  TdmModelEditor = class(TDataModule)
    rhModel: TBoldReferenceHandle;
    lhClasses: TBoldListHandle;
    lhAttributes: TBoldListHandle;
    bdsClasses: TBoldDataSet;
    bdsAttributes: TBoldDataSet;
    lhViolations: TBoldListHandle;
    bdsViolations: TBoldDataSet;
  private
    FDocument: TBoldModelDocument;
    // A consistency check ran since the model was made or opened
    FValidated: Boolean;
    FOnAskSave: TModelEditorAskSaveEvent;
    FOnChooseSaveFile: TModelEditorChooseFileEvent;
    procedure Connect;
    procedure Disconnect;
    function SelectRow(ADataSet: TBoldDataSet; AElement: TBoldElement): Boolean;
    function IsRootClass(AClass: TUMLClass): Boolean;
    function RequiredClass: TUMLClass;
    procedure SetClassValues(AClass: TUMLClass; const AValues: TModelEditorClassValues);
    procedure SetAttributeValues(AAttribute: TUMLAttribute; const AValues: TModelEditorAttributeValues);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Bold's display queue: the datasets show changes of the model when it
      is drained. The clients' idle loops drain it too (BoldEnvironmentLCL,
      FMX's idle handling); a command that selects a new row calls it
      first. }
    procedure Refresh;
    { Ends the editing of a row in the grids: Post writes the values to the
      model, Cancel drops them }
    procedure PostEdits;
    procedure CancelEdits;

    { The document. New and Open drop the model before; the client asks
      about unsaved changes first (Modified). }
    procedure NewModel;
    procedure Open(const AFileName: string);
    procedure Save;
    procedure SaveAs(const AFileName: string);
    { Generates the business classes into AFolder; DefaultCodeFolder is the
      folder of the model file }
    procedure GenerateCode(const AFolder: string);
    function DefaultCodeFolder: string;
    { The type names code generation uses, from the TBoldTypeNameHandle of a
      form (TBoldModelDocument.LoadTypeNames); TypeNamesName is the form's
      file name, or sBoldTypeNames }
    procedure LoadTypeNames(const AFormFile: string);
    function TypeNamesName: string;

    { What the clients' New, Open and close do first: True when the model
      may be dropped, as it has no changes, or the user saved them
      (OnAskSave answers saSave, then SaveModel) or discards them (saDiscard).
      False when the user cancels. Without OnAskSave the changes are
      dropped. }
    function CloseModel: Boolean;
    { Save, or for a model with no file name yet SaveAs to the file that
      OnChooseSaveFile gives. False when the user cancels. }
    function SaveModel: Boolean;
    { SaveAs to the file that OnChooseSaveFile gives. False when the user
      cancels. }
    function SaveModelAs: Boolean;

    { The values of a class for a client's dialog }
    function ClassValues(AClass: TUMLClass): TModelEditorClassValues;
    { The values a new class gets: a name no class has (NewClass,
      NewClass_1 ...), persistent, the root class of a boldified model as
      its superclass }
    function NewClassValues: TModelEditorClassValues;
    { Gives a class the values of a client's dialog; a class needs a name }
    procedure ApplyClassValues(AClass: TUMLClass; const AValues: TModelEditorClassValues);
    { A new class with the values of a client's dialog, in the namespace of
      the selected class (the model when none is selected), selected in
      bdsClasses. Without values: NewClassValues. }
    function AddClass(const AValues: TModelEditorClassValues): TUMLClass; overload;
    function AddClass: TUMLClass; overload;
    { Deletes the selected class; its subclasses get its superclass }
    procedure DeleteClass;
    { The values of an attribute for a client's dialog }
    function AttributeValues(AAttribute: TUMLAttribute): TModelEditorAttributeValues;
    { The values a new attribute of the selected class gets: a name no
      feature of the class has (NewAttribute ...), the type String,
      persistent as the class is }
    function NewAttributeValues: TModelEditorAttributeValues;
    { Gives an attribute the values of a client's dialog; an attribute needs
      a name }
    procedure ApplyAttributeValues(AAttribute: TUMLAttribute; const AValues: TModelEditorAttributeValues);
    { A new attribute of the selected class with the values of a client's
      dialog, selected in bdsAttributes. Without values: NewAttributeValues. }
    function AddAttribute(const AValues: TModelEditorAttributeValues): TUMLAttribute; overload;
    function AddAttribute: TUMLAttribute; overload;
    procedure DeleteAttribute;
    { The attribute types a client's attribute dialog offers: the model
      names of the type names the document uses (Bold's own, or a form's),
      sorted, without the abstract ones in angle brackets (<Default>,
      <Numeric>, <Moment>, <ValueSet>). A type that is not among them can
      still be typed. }
    procedure GetTypeNames(AList: TStrings);
    function SelectClass(AClass: TUMLClass): Boolean;
    function SelectAttribute(AAttribute: TUMLAttribute): Boolean;
    function CurrentClass: TUMLClass;
    function CurrentAttribute: TUMLAttribute;
    { What the clients enable their commands by }
    function CanEditClass: Boolean;
    function CanEditAttribute: Boolean;
    function CanDeleteClass: Boolean;
    function CanAddAttribute: Boolean;
    function CanDeleteAttribute: Boolean;

    { The consistency check (TBoldModelDocument.Validate): its findings are
      the rows of bdsViolations; the highest severity is returned }
    function Validate: TSeverity;
    { The result of the last check for a status line: the number of errors,
      warnings and hints, or that it found nothing; empty before the first
      check of a model }
    function ValidationSummary: string;
    { The finding selected in bdsViolations; nil when none is }
    function CurrentViolation: TViolation;
    { Selects what the selected finding is about: a class in bdsClasses, an
      attribute in bdsClasses and bdsAttributes, the owner class of another
      feature, the class at an association end. False when the finding is
      about something else, as the model, or about an element that is gone }
    function SelectViolationElement: Boolean;

    { The file name of the model, or Untitled, with a * when modified }
    function DocumentName: string;
    { What a client's window title shows }
    function Title: string;
    function Modified: Boolean;
    property Document: TBoldModelDocument read FDocument;
    { Set by the client: its question and its save dialog }
    property OnAskSave: TModelEditorAskSaveEvent read FOnAskSave write FOnAskSave;
    property OnChooseSaveFile: TModelEditorChooseFileEvent read FOnChooseSaveFile write FOnChooseSaveFile;
  end;

const
  cModelEditorTitle = 'Bold Model Editor';

resourcestring
  sBoldTypeNames = 'Bold''s type names';

{ The model file named on the command line: the first parameter that is not
  an option (one starting with - or /, as --selftest); empty when none }
function ModelFileFromCommandLine: string;
{ The form named on the command line by --typenames=<form>, for
  LoadTypeNames; empty when none }
function TypeNamesFileFromCommandLine: string;

implementation

uses
  BoldSystem,
  BoldQueue,
  BoldTypeNameDictionary,
  BoldUMLModelSupport;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

resourcestring
  sUntitled = 'Untitled';
  sNoClassSelected = 'No class is selected';
  sClassNeedsAName = 'A class needs a name';
  sAttributeNeedsAName = 'An attribute needs a name';
  sTheRootClass = 'The root class %s of a boldified model cannot be deleted';
  sNoFindings = 'Consistency check: no findings';
  sFindings = 'Consistency check: %d errors, %d warnings, %d hints';

const
  cNewClassName = 'NewClass';
  cNewAttributeName = 'NewAttribute';
  cNewAttributeType = 'String';

{ ABase, or ABase_1, ABase_2 ... as TBoldUMLSupport.UniqueName numbers
  them: the first one that ATaken does not hold, compared without case }
function UniqueName(const ABase: string; ATaken: TStrings): string;
var
  i: Integer;
begin
  Result := ABase;
  i := 0;
  while ATaken.IndexOf(Result) >= 0 do
  begin
    Inc(i);
    Result := Format('%s_%d', [ABase, i]);
  end;
end;

function ModelFileFromCommandLine: string;
var
  i: Integer;
  Param: string;
begin
  Result := '';
  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);
    if (Param <> '') and (Param[1] <> '-') {$IFDEF MSWINDOWS}and (Param[1] <> '/'){$ENDIF} then
      Exit(ExpandFileName(Param));
  end;
end;

function TypeNamesFileFromCommandLine: string;
const
  cOption = '--typenames=';
var
  i: Integer;
  Param: string;
begin
  Result := '';
  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);
    if SameText(Copy(Param, 1, Length(cOption)), cOption) then
      Exit(ExpandFileName(Copy(Param, Length(cOption) + 1, MaxInt)));
  end;
end;

{ TdmModelEditor }

constructor TdmModelEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument := TBoldModelDocument.Create;
  Connect;
end;

destructor TdmModelEditor.Destroy;
begin
  // Each component before what it refers to: the datasets, the handles,
  // then the document with the model's object space
  if Assigned(FDocument) then
    Disconnect;
  FreeAndNil(bdsViolations);
  FreeAndNil(lhViolations);
  FreeAndNil(bdsAttributes);
  FreeAndNil(bdsClasses);
  FreeAndNil(lhAttributes);
  FreeAndNil(lhClasses);
  FreeAndNil(rhModel);
  FreeAndNil(FDocument);
  inherited;
end;

procedure TdmModelEditor.Connect;
var
  Model: TUMLModel;
  i: Integer;
begin
  Model := FDocument.UMLModel;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TBoldNonSystemHandle then
      TBoldNonSystemHandle(Components[i]).StaticSystemHandle := FDocument.Model.SystemHandle;
  rhModel.Value := Model;
  bdsClasses.Open;
  bdsAttributes.Open;
  bdsViolations.Open;
  // Shown, not edited: the clients edit classes and attributes in a dialog
  for i := 0 to bdsClasses.FieldCount - 1 do
    bdsClasses.Fields[i].ReadOnly := True;
  for i := 0 to bdsAttributes.FieldCount - 1 do
    bdsAttributes.Fields[i].ReadOnly := True;
  for i := 0 to bdsViolations.FieldCount - 1 do
    bdsViolations.Fields[i].ReadOnly := True;
  FValidated := False;
end;

procedure TdmModelEditor.Disconnect;
var
  i: Integer;
begin
  CancelEdits;
  bdsViolations.Close;
  bdsAttributes.Close;
  bdsClasses.Close;
  rhModel.Value := nil;
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TBoldNonSystemHandle then
      TBoldNonSystemHandle(Components[i]).StaticSystemHandle := nil;
end;

procedure TdmModelEditor.Refresh;
begin
  TBoldQueueable.DisplayAll;
end;

procedure TdmModelEditor.PostEdits;
begin
  if bdsAttributes.State in dsEditModes then
    bdsAttributes.Post;
  if bdsClasses.State in dsEditModes then
    bdsClasses.Post;
end;

procedure TdmModelEditor.CancelEdits;
begin
  if bdsAttributes.State in dsEditModes then
    bdsAttributes.Cancel;
  if bdsClasses.State in dsEditModes then
    bdsClasses.Cancel;
end;

procedure TdmModelEditor.NewModel;
begin
  Disconnect;
  FDocument.NewModel;
  Connect;
end;

procedure TdmModelEditor.Open(const AFileName: string);
begin
  Disconnect;
  try
    FDocument.Open(AFileName);
  finally
    // The model before is gone when the file could not be read; the
    // document has an empty one then
    Connect;
  end;
end;

procedure TdmModelEditor.Save;
begin
  PostEdits;
  FDocument.Save;
end;

procedure TdmModelEditor.SaveAs(const AFileName: string);
begin
  PostEdits;
  FDocument.SaveAs(AFileName);
end;

procedure TdmModelEditor.GenerateCode(const AFolder: string);
begin
  PostEdits;
  FDocument.GenerateCode(AFolder);
end;

function TdmModelEditor.DefaultCodeFolder: string;
begin
  if FDocument.FileName <> '' then
    Result := ExtractFilePath(FDocument.FileName)
  else
    Result := GetCurrentDir;
end;

procedure TdmModelEditor.LoadTypeNames(const AFormFile: string);
begin
  FDocument.LoadTypeNames(AFormFile);
end;

function TdmModelEditor.TypeNamesName: string;
begin
  if FDocument.TypeNamesFile <> '' then
    Result := ExtractFileName(FDocument.TypeNamesFile)
  else
    Result := sBoldTypeNames;
end;

function TdmModelEditor.CloseModel: Boolean;
begin
  Result := True;
  if not Modified or not Assigned(FOnAskSave) then
    Exit;
  case FOnAskSave(DocumentName) of
    saSave: Result := SaveModel;
    saDiscard: CancelEdits;
    saCancel: Result := False;
  end;
end;

function TdmModelEditor.SaveModel: Boolean;
begin
  if FDocument.FileName = '' then
    Exit(SaveModelAs);
  Save;
  Result := True;
end;

function TdmModelEditor.SaveModelAs: Boolean;
var
  FileName: string;
begin
  FileName := FDocument.FileName;
  Result := Assigned(FOnChooseSaveFile) and FOnChooseSaveFile(FileName) and (FileName <> '');
  if Result then
    SaveAs(FileName);
end;

function TdmModelEditor.SelectRow(ADataSet: TBoldDataSet; AElement: TBoldElement): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if not ADataSet.Active or not Assigned(AElement) then
    Exit;
  Refresh;
  if ADataSet.ActiveBoldElement = AElement then
    Exit(True);
  Index := ADataSet.BoldHandle.List.IndexOf(AElement);
  if Index < 0 then
    Exit;
  ADataSet.First;
  ADataSet.MoveBy(Index);
  Refresh;
  Result := ADataSet.ActiveBoldElement = AElement;
end;

function TdmModelEditor.SelectClass(AClass: TUMLClass): Boolean;
begin
  Result := SelectRow(bdsClasses, AClass);
end;

function TdmModelEditor.SelectAttribute(AAttribute: TUMLAttribute): Boolean;
begin
  Result := SelectRow(bdsAttributes, AAttribute);
end;

function TdmModelEditor.CurrentClass: TUMLClass;
begin
  if bdsClasses.Active and (bdsClasses.ActiveBoldObject is TUMLClass) then
    Result := TUMLClass(bdsClasses.ActiveBoldObject)
  else
    Result := nil;
end;

function TdmModelEditor.CurrentAttribute: TUMLAttribute;
begin
  if bdsAttributes.Active and (bdsAttributes.ActiveBoldObject is TUMLAttribute) then
    Result := TUMLAttribute(bdsAttributes.ActiveBoldObject)
  else
    Result := nil;
end;

function TdmModelEditor.RequiredClass: TUMLClass;
begin
  Result := CurrentClass;
  if not Assigned(Result) then
    raise EBoldModelEditor.Create(sNoClassSelected);
end;

function TdmModelEditor.ClassValues(AClass: TUMLClass): TModelEditorClassValues;
begin
  Result.Name := AClass.name;
  if Assigned(AClass.superclass) then
    Result.Superclass := AClass.superclass.name
  else
    Result.Superclass := '';
  Result.Persistent := AClass.persistent;
  Result.Abstract := AClass.isAbstract;
end;

function TdmModelEditor.NewClassValues: TModelEditorClassValues;
var
  Model: TUMLModel;
  Taken: TStringList;
  i: Integer;
begin
  Model := FDocument.UMLModel;
  Taken := TStringList.Create;
  try
    Taken.CaseSensitive := False;
    for i := 0 to Model.Classes.Count - 1 do
      Taken.Add(Model.Classes[i].name);
    Result.Name := UniqueName(cNewClassName, Taken);
  finally
    Taken.Free;
  end;
  if TBoldUMLBoldify.IsBoldified(Model) then
    Result.Superclass := TBoldUMLBoldify.GetRootClass(Model).name
  else
    Result.Superclass := '';
  Result.Persistent := True;
  Result.Abstract := False;
end;

procedure TdmModelEditor.SetClassValues(AClass: TUMLClass; const AValues: TModelEditorClassValues);
begin
  AClass.name := AValues.Name;
  AClass.persistent := AValues.Persistent;
  AClass.isAbstract := AValues.Abstract;
end;

procedure TdmModelEditor.ApplyClassValues(AClass: TUMLClass; const AValues: TModelEditorClassValues);
begin
  if Trim(AValues.Name) = '' then
    raise EBoldModelEditor.Create(sClassNeedsAName);
  SetClassValues(AClass, AValues);
  Refresh;
end;

function TdmModelEditor.AddClass(const AValues: TModelEditorClassValues): TUMLClass;
var
  Model: TUMLModel;
  Namespace: TUMLNamespace;
begin
  if Trim(AValues.Name) = '' then
    raise EBoldModelEditor.Create(sClassNeedsAName);
  PostEdits;
  Model := FDocument.UMLModel;
  Namespace := Model;
  if (CurrentClass <> nil) and Assigned(CurrentClass.namespace_) then
    Namespace := CurrentClass.namespace_;
  // As Bold's model editor makes a new class
  Result := TUMLClass.Create(Model.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(Result);
  Result.namespace_ := Namespace;
  if TBoldUMLBoldify.IsBoldified(Model) then
    Result.SetFirstParent(TBoldUMLBoldify.GetRootClass(Model));
  SetClassValues(Result, AValues);
  SelectClass(Result);
end;

function TdmModelEditor.AddClass: TUMLClass;
begin
  Result := AddClass(NewClassValues);
end;

function TdmModelEditor.IsRootClass(AClass: TUMLClass): Boolean;
var
  Model: TUMLModel;
begin
  Model := FDocument.UMLModel;
  Result := Assigned(AClass) and TBoldUMLBoldify.IsBoldified(Model) and
    (TBoldUMLBoldify.GetRootClass(Model) = AClass);
end;

function TdmModelEditor.CanEditClass: Boolean;
begin
  Result := CurrentClass <> nil;
end;

function TdmModelEditor.CanEditAttribute: Boolean;
begin
  Result := CurrentAttribute <> nil;
end;

function TdmModelEditor.CanDeleteClass: Boolean;
begin
  Result := (CurrentClass <> nil) and not IsRootClass(CurrentClass);
end;

function TdmModelEditor.CanAddAttribute: Boolean;
begin
  Result := CurrentClass <> nil;
end;

function TdmModelEditor.CanDeleteAttribute: Boolean;
begin
  Result := CurrentAttribute <> nil;
end;

procedure TdmModelEditor.DeleteClass;
var
  AClass: TUMLClass;
begin
  AClass := CurrentClass;
  if not Assigned(AClass) then
    raise EBoldModelEditor.Create(sNoClassSelected);
  if IsRootClass(AClass) then
    raise EBoldModelEditor.CreateFmt(sTheRootClass, [AClass.name]);
  CancelEdits;
  TBoldUMLSupport.RelinkSpecializations(AClass);
  AClass.Delete;
  Refresh;
end;

function TdmModelEditor.AttributeValues(AAttribute: TUMLAttribute): TModelEditorAttributeValues;
begin
  Result.Name := AAttribute.name;
  Result.TypeName := AAttribute.typeName;
  Result.Persistent := AAttribute.persistent;
  Result.Derived := AAttribute.derived;
end;

function TdmModelEditor.NewAttributeValues: TModelEditorAttributeValues;
var
  AClass: TUMLClass;
  Taken: TStringList;
  i: Integer;
begin
  AClass := RequiredClass;
  Taken := TStringList.Create;
  try
    Taken.CaseSensitive := False;
    for i := 0 to AClass.feature.Count - 1 do
      Taken.Add(AClass.feature[i].name);
    Result.Name := UniqueName(cNewAttributeName, Taken);
  finally
    Taken.Free;
  end;
  Result.TypeName := cNewAttributeType;
  Result.Persistent := AClass.persistent;
  Result.Derived := False;
end;

procedure TdmModelEditor.SetAttributeValues(AAttribute: TUMLAttribute; const AValues: TModelEditorAttributeValues);
begin
  AAttribute.name := AValues.Name;
  AAttribute.typeName := AValues.TypeName;
  AAttribute.persistent := AValues.Persistent;
  AAttribute.derived := AValues.Derived;
end;

procedure TdmModelEditor.ApplyAttributeValues(AAttribute: TUMLAttribute; const AValues: TModelEditorAttributeValues);
begin
  if Trim(AValues.Name) = '' then
    raise EBoldModelEditor.Create(sAttributeNeedsAName);
  SetAttributeValues(AAttribute, AValues);
  Refresh;
end;

function TdmModelEditor.AddAttribute(const AValues: TModelEditorAttributeValues): TUMLAttribute;
var
  AClass: TUMLClass;
begin
  if Trim(AValues.Name) = '' then
    raise EBoldModelEditor.Create(sAttributeNeedsAName);
  PostEdits;
  AClass := RequiredClass;
  // As Bold's model editor makes a new attribute
  Result := TUMLAttribute.Create(AClass.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(Result);
  Result.owner := AClass;
  SetAttributeValues(Result, AValues);
  SelectAttribute(Result);
end;

function TdmModelEditor.AddAttribute: TUMLAttribute;
begin
  Result := AddAttribute(NewAttributeValues);
end;

procedure TdmModelEditor.GetTypeNames(AList: TStrings);
var
  Dictionary: TBoldTypeNameDictionary;
  Names: TStringList;
  i: Integer;
begin
  Names := TStringList.Create;
  try
    Names.Sorted := True;
    Names.Duplicates := dupIgnore;
    Names.CaseSensitive := False;
    Dictionary := FDocument.Model.TypeNameDictionary;
    for i := 0 to Dictionary.Count - 1 do
      if (Dictionary.Mapping[i].ModelName <> '') and (Dictionary.Mapping[i].ModelName[1] <> '<') then
        Names.Add(Dictionary.Mapping[i].ModelName);
    AList.Assign(Names);
  finally
    Names.Free;
  end;
end;

procedure TdmModelEditor.DeleteAttribute;
var
  Attribute: TUMLAttribute;
begin
  Attribute := CurrentAttribute;
  if not Assigned(Attribute) then
    Exit;
  CancelEdits;
  Attribute.Delete;
  Refresh;
end;

function TdmModelEditor.Validate: TSeverity;
begin
  PostEdits;
  Result := FDocument.Validate;
  FValidated := True;
  Refresh;
end;

function TdmModelEditor.ValidationSummary: string;
var
  Violations: TViolationList;
  Counts: array[TSeverity] of Integer;
  Severity: TSeverity;
  i: Integer;
begin
  if not FValidated then
    Exit('');
  for Severity := Low(TSeverity) to High(TSeverity) do
    Counts[Severity] := 0;
  Violations := FDocument.UMLModel.Validator.Violation;
  for i := 0 to Violations.Count - 1 do
    Inc(Counts[Violations[i].Severity]);
  if Violations.Count = 0 then
    Result := sNoFindings
  else
    Result := Format(sFindings, [Counts[sError], Counts[sWarning], Counts[sHint]]);
end;

function TdmModelEditor.CurrentViolation: TViolation;
begin
  if bdsViolations.Active and (bdsViolations.ActiveBoldObject is TViolation) then
    Result := TViolation(bdsViolations.ActiveBoldObject)
  else
    Result := nil;
end;

function TdmModelEditor.SelectViolationElement: Boolean;
var
  Violation: TViolation;
  Element: TUMLModelElement;
  OwnerClass: TUMLClassifier;
begin
  Result := False;
  Violation := CurrentViolation;
  if not Assigned(Violation) then
    Exit;
  Element := Violation.ModelElement;
  if Element is TUMLClass then
    Exit(SelectClass(TUMLClass(Element)));
  if Element is TUMLAssociationEnd then
  begin
    OwnerClass := TUMLAssociationEnd(Element).type_;
    Exit((OwnerClass is TUMLClass) and SelectClass(TUMLClass(OwnerClass)));
  end;
  if Element is TUMLFeature then
  begin
    OwnerClass := TUMLFeature(Element).owner;
    if not ((OwnerClass is TUMLClass) and SelectClass(TUMLClass(OwnerClass))) then
      Exit;
    if Element is TUMLAttribute then
      Exit(SelectAttribute(TUMLAttribute(Element)));
    Result := True;
  end;
end;

function TdmModelEditor.Modified: Boolean;
begin
  Result := FDocument.Modified or
    (bdsClasses.State in dsEditModes) or (bdsAttributes.State in dsEditModes);
end;

function TdmModelEditor.DocumentName: string;
begin
  if FDocument.FileName <> '' then
    Result := ExtractFileName(FDocument.FileName)
  else
    Result := sUntitled;
  if Modified then
    Result := Result + '*';
end;

function TdmModelEditor.Title: string;
begin
  Result := DocumentName + ' - ' + cModelEditorTitle;
end;

end.
