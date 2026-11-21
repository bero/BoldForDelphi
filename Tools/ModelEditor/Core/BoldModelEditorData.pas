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
  bdsClasses and bdsAttributes are TBoldDataSets on the two list handles.
  Selecting a row in bdsClasses moves lhClasses, and lhAttributes follows,
  as in the Master-Detail demo. The handles get the object space of the
  document's model in Connect, after each New and Open: each model has an
  object space of its own (TBoldModel.SystemHandle).

  Classes and attributes are added and deleted by the commands below, not by
  the datasets: a new class needs its namespace, its Bold tagged values and,
  in a boldified model, the root class as its superclass, and a deleted one
  hands its subclasses to its superclass. The two lists are derived, so a
  grid's own insert does nothing. }

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
  BoldUMLModel,
  BoldLogInterfaces,
  BoldModelDocument;

type
  EBoldModelEditor = class(Exception);

  { The answer to "save the changes of <model>?" }
  TModelEditorSaveAnswer = (saSave, saDiscard, saCancel);
  TModelEditorAskSaveEvent = function(const ADocumentName: string): TModelEditorSaveAnswer of object;
  { A save dialog: True with the chosen file in AFileName, False when the
    user cancels }
  TModelEditorChooseFileEvent = function(var AFileName: string): Boolean of object;

  TdmModelEditor = class(TDataModule)
    rhModel: TBoldReferenceHandle;
    lhClasses: TBoldListHandle;
    lhAttributes: TBoldListHandle;
    bdsClasses: TBoldDataSet;
    bdsAttributes: TBoldDataSet;
  private
    FDocument: TBoldModelDocument;
    FOnAskSave: TModelEditorAskSaveEvent;
    FOnChooseSaveFile: TModelEditorChooseFileEvent;
    FLogSink: IBoldLogSink;
    procedure Connect;
    procedure Disconnect;
    function SelectRow(ADataSet: TBoldDataSet; AElement: TBoldElement): Boolean;
    function IsRootClass(AClass: TUMLClass): Boolean;
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

    { A new class in the namespace of the selected class (the model when
      none is selected), selected in bdsClasses }
    function AddClass: TUMLClass;
    { Deletes the selected class; its subclasses get its superclass }
    procedure DeleteClass;
    { A new attribute of the selected class, selected in bdsAttributes }
    function AddAttribute: TUMLAttribute;
    procedure DeleteAttribute;
    function SelectClass(AClass: TUMLClass): Boolean;
    function SelectAttribute(AAttribute: TUMLAttribute): Boolean;
    function CurrentClass: TUMLClass;
    function CurrentAttribute: TUMLAttribute;
    { What the clients enable their commands by }
    function CanDeleteClass: Boolean;
    function CanAddAttribute: Boolean;
    function CanDeleteAttribute: Boolean;

    { The editor's log in AFileName: Bold's log (BoldLogManager), which gets
      the exceptions of LogException with their call stacks }
    procedure StartLogFile(const AFileName: string);
    { An exception that reached the client's exception handler, to the log
      with its call stack (BoldExceptionTrace, #130). Call it from the
      handler: the call stack is that of the exception being handled. }
    procedure LogException(E: Exception);

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

{ The model file named on the command line: the first parameter that is not
  an option (one starting with - or /, as --selftest); empty when none }
function ModelFileFromCommandLine: string;

implementation

uses
  BoldSystem,
  BoldQueue,
  BoldLogSinks,
  BoldExceptionTrace,
  BoldUMLModelSupport;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

resourcestring
  sUntitled = 'Untitled';
  sNoClassSelected = 'No class is selected';
  sTheRootClass = 'The root class %s of a boldified model cannot be deleted';

const
  cSuperclassField = 'Superclass';

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
  FreeAndNil(bdsAttributes);
  FreeAndNil(bdsClasses);
  FreeAndNil(lhAttributes);
  FreeAndNil(lhClasses);
  FreeAndNil(rhModel);
  FreeAndNil(FDocument);
  if Assigned(FLogSink) then
    BoldLogManager.UnregisterSink(FLogSink);
  FLogSink := nil;
  inherited;
end;

procedure TdmModelEditor.StartLogFile(const AFileName: string);
begin
  if Assigned(FLogSink) then
    BoldLogManager.UnregisterSink(FLogSink);
  FLogSink := TBoldFileLogSink.Create(AFileName);
  BoldLogManager.RegisterSink(FLogSink);
end;

procedure TdmModelEditor.LogException(E: Exception);
begin
  BoldLogException(E, cModelEditorTitle);
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
  // The name of another class: shown here, changed with the class
  bdsClasses.FieldByName(cSuperclassField).ReadOnly := True;
  bdsAttributes.Open;
end;

procedure TdmModelEditor.Disconnect;
var
  i: Integer;
begin
  CancelEdits;
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

function TdmModelEditor.AddClass: TUMLClass;
var
  Model: TUMLModel;
  Namespace: TUMLNamespace;
begin
  PostEdits;
  Model := FDocument.UMLModel;
  Namespace := Model;
  if (CurrentClass <> nil) and Assigned(CurrentClass.namespace_) then
    Namespace := CurrentClass.namespace_;
  // As Bold's model editor makes a new class
  Result := TUMLClass.Create(Model.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(Result);
  Result.namespace_ := Namespace;
  Result.name := TBoldUMLSupport.UniqueName(Result, 'NewClass');
  if TBoldUMLBoldify.IsBoldified(Model) then
    Result.SetFirstParent(TBoldUMLBoldify.GetRootClass(Model));
  Result.persistent := True;
  SelectClass(Result);
end;

function TdmModelEditor.IsRootClass(AClass: TUMLClass): Boolean;
var
  Model: TUMLModel;
begin
  Model := FDocument.UMLModel;
  Result := Assigned(AClass) and TBoldUMLBoldify.IsBoldified(Model) and
    (TBoldUMLBoldify.GetRootClass(Model) = AClass);
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

function TdmModelEditor.AddAttribute: TUMLAttribute;
var
  AClass: TUMLClass;
begin
  PostEdits;
  AClass := CurrentClass;
  if not Assigned(AClass) then
    raise EBoldModelEditor.Create(sNoClassSelected);
  // As Bold's model editor makes a new attribute
  Result := TUMLAttribute.Create(AClass.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(Result);
  Result.owner := AClass;
  Result.name := TBoldUMLSupport.UniqueName(Result, 'NewAttribute');
  Result.typeName := 'String';
  Result.persistent := AClass.persistent;
  SelectAttribute(Result);
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
