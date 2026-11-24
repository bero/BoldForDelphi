{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
{$INCLUDE bold.inc}
unit BoldModelDocument;

{ The part of the standalone model editor (#128) that has no user interface:
  a model read from a .bld file, written back to one, and the business
  classes generated from it, as the Delphi IDE's Save and Generate does.
  The LCL and the FMX clients show and edit Model.EnsuredUMLModel; this unit
  knows nothing of either framework.

  Open reads the file into a TBoldModel of its own (TMoldBLDRW, then the
  model's UML model from it on first use). Save converts the UML model back
  (TBoldModelConverter.UMLModelToMold) and writes it as the .bld link of the
  IDE does, with CRLF line breaks on every platform, so that a file saved on
  Linux and on Windows is the same. GenerateCode runs TBoldGenerator with
  the Delphi templates into a folder of the caller's choice.

  Modified follows every change of the UML model, through the broadcast of
  member events of its object space (TBoldUMLSupport.SubscribeToEntireModel),
  so a change made in a client's grid counts as one made in code.

  The type names (the model's attribute types to Delphi classes, units and
  mappers) are Bold's own, or those of the TBoldTypeNameHandle of a form
  (LoadTypeNames): in the IDE a project gives its TBoldModel such a handle
  for its own attribute types, and the .bld file does not hold them. }

interface

uses
  SysUtils,
  Classes,
  BoldSubscription,
  BoldMeta,
  BoldModel,
  BoldTypeNameHandle,
  BoldUMLModel;

type
  EBoldModelDocument = class(Exception);

  TBoldModelDocument = class
  private
    FModel: TBoldModel;
    FFileName: string;
    FModified: Boolean;
    // nil for Bold's own type names
    FTypeNameHandle: TBoldTypeNameHandle;
    FTypeNamesFile: string;
    FChangeSubscriber: TBoldPassthroughSubscriber;
    // Above zero while the document changes the model for itself
    FIgnoreChanges: Integer;
    function GetUMLModel: TUMLModel;
    procedure ReplaceModel(AMoldModel: TMoldModel);
    procedure ReceiveChange(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent);
  public
    constructor Create;
    destructor Destroy; override;
    { An empty model with no file name; the model before is dropped }
    procedure NewModel;
    { Reads a .bld file; the model read before is dropped }
    procedure Open(const AFileName: string);
    { Writes the model to the file it was read from, or to AFileName }
    procedure Save;
    procedure SaveAs(const AFileName: string);
    { The model as the text of a .bld file, CRLF line breaks }
    function AsText: string;
    { Writes the business classes of the model into AFolder: the unit
      named by the model's tagged value Bold.UnitName, its _Interface.inc,
      and an include file per class with method stubs, which is only
      written where it does not exist yet, so implemented methods are kept }
    procedure GenerateCode(const AFolder: string);
    { Reads the type names from the TBoldTypeNameHandle of a text form
      (.dfm, .lfm, .fmx), as the IDE takes them from the handle its
      TBoldModel names. Code generation and validation use them until
      another form is read; New and Open keep them. A form that has no such
      handle, or more than one, is refused with EBoldModelDocument, and the
      type names read before stay. }
    procedure LoadTypeNames(const AFormFile: string);
    { The form the type names are from; empty for Bold's own }
    property TypeNamesFile: string read FTypeNamesFile;
    property Model: TBoldModel read FModel;
    property UMLModel: TUMLModel read GetUMLModel;
    { Empty for a new model that was not saved yet }
    property FileName: string read FFileName;
    { The model was changed since it was opened, made or saved }
    property Modified: Boolean read FModified;
  end;

implementation

uses
  BoldBld,
  BoldUMLModelConverter,
  BoldUMLModelSupport,
  BoldGen,
  BoldGeneratorTemplatesDelphi; // registers the Delphi templates TBoldGenerator uses

const
  cBldLineBreak = #13#10;

resourcestring
  sNoFileName = 'The model has no file name yet; use SaveAs';
  sNoFolder = 'The folder for the generated code does not exist: %s';
  sNoTypeNameHandle = 'The form %s has no TBoldTypeNameHandle';
  sSeveralTypeNameHandles = 'The form %s has more than one TBoldTypeNameHandle: %s';

{ The name of the component that a line of a text form starts, as
  "object BoldTypeNameHandle1: TBoldTypeNameHandle" (also "inherited" and
  "inline"), when the component is a TBoldTypeNameHandle; empty otherwise }
function TypeNameHandleOfLine(const ALine: string): string;
const
  cKeywords: array[0..2] of string = ('object ', 'inherited ', 'inline ');
var
  Line: string;
  Colon, i: Integer;
begin
  Result := '';
  Line := Trim(ALine);
  Colon := Pos(':', Line);
  if (Colon = 0) or not SameText(Trim(Copy(Line, Colon + 1, MaxInt)), TBoldTypeNameHandle.ClassName) then
    Exit;
  for i := Low(cKeywords) to High(cKeywords) do
    if SameText(Copy(Line, 1, Length(cKeywords[i])), cKeywords[i]) then
      Exit(Trim(Copy(Line, Length(cKeywords[i]) + 1, Colon - Length(cKeywords[i]) - 1)));
end;

function IndentOf(const ALine: string): Integer;
begin
  Result := 0;
  while (Result < Length(ALine)) and (ALine[Result + 1] = ' ') do
    Inc(Result);
end;

{ The text of the one TBoldTypeNameHandle of a text form: its lines up to the
  "end" with the indent of its first line }
function TypeNameHandleText(const AFormFile: string): string;
var
  Lines, Handle, Names: TStringList;
  First, Indent, i: Integer;
begin
  Lines := TStringList.Create;
  Handle := TStringList.Create;
  Names := TStringList.Create;
  try
    Lines.LoadFromFile(AFormFile);
    First := -1;
    for i := 0 to Lines.Count - 1 do
      if TypeNameHandleOfLine(Lines[i]) <> '' then
      begin
        if First < 0 then
          First := i;
        Names.Add(TypeNameHandleOfLine(Lines[i]));
      end;
    if Names.Count = 0 then
      raise EBoldModelDocument.CreateFmt(sNoTypeNameHandle, [AFormFile]);
    if Names.Count > 1 then
      raise EBoldModelDocument.CreateFmt(sSeveralTypeNameHandles, [AFormFile, Names.CommaText]);
    Indent := IndentOf(Lines[First]);
    Handle.Add(Lines[First]);
    for i := First + 1 to Lines.Count - 1 do
    begin
      Handle.Add(Lines[i]);
      if (IndentOf(Lines[i]) = Indent) and SameText(Trim(Lines[i]), 'end') then
        Break;
    end;
    Result := Handle.Text;
  finally
    Names.Free;
    Handle.Free;
    Lines.Free;
  end;
end;

{ Reads the text of a TBoldTypeNameHandle into AHandle, as a form is read }
procedure ReadTypeNameHandle(const AText: string; AHandle: TBoldTypeNameHandle);
var
  Text: TBytesStream;
  Binary: TMemoryStream;
begin
  Text := TBytesStream.Create(TEncoding.UTF8.GetBytes(AText));
  Binary := TMemoryStream.Create;
  try
    ObjectTextToBinary(Text, Binary);
    Binary.Position := 0;
    Binary.ReadComponent(AHandle);
  finally
    Binary.Free;
    Text.Free;
  end;
end;

{ TBoldModelDocument }

constructor TBoldModelDocument.Create;
begin
  inherited Create;
  FChangeSubscriber := TBoldPassthroughSubscriber.Create(ReceiveChange);
  NewModel;
end;

destructor TBoldModelDocument.Destroy;
begin
  FreeAndNil(FChangeSubscriber);
  FreeAndNil(FModel);
  FreeAndNil(FTypeNameHandle);
  inherited;
end;

procedure TBoldModelDocument.ReplaceModel(AMoldModel: TMoldModel);
begin
  // A TBoldModel of its own for each model: the UML model of a TBoldModel is
  // made once, from the model it has then
  FChangeSubscriber.CancelAllSubscriptions;
  FreeAndNil(FModel);
  FModel := TBoldModel.Create(nil);
  FModel.TypeNameHandle := FTypeNameHandle;
  if Assigned(AMoldModel) then
    FModel.SetFromModel(AMoldModel);
  TBoldUMLSupport.SubscribeToEntireModel(FModel.EnsuredUMLModel, FChangeSubscriber);
  FModified := False;
end;

procedure TBoldModelDocument.ReceiveChange(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if FIgnoreChanges = 0 then
    FModified := True;
end;

function TBoldModelDocument.GetUMLModel: TUMLModel;
begin
  Result := FModel.EnsuredUMLModel;
end;

procedure TBoldModelDocument.NewModel;
begin
  ReplaceModel(nil);
  FFileName := '';
end;

procedure TBoldModelDocument.Open(const AFileName: string);
begin
  ReplaceModel(TMoldBLDRW.ModelFromFile(AFileName));
  FFileName := AFileName;
end;

function TBoldModelDocument.AsText: string;
var
  MoldModel: TMoldModel;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  MoldModel := TBoldModelConverter.UMLModelToMold(UMLModel);
  try
    TMoldBLDRW.ModelToStrings(MoldModel, Lines);
    Lines.LineBreak := cBldLineBreak;
    Result := Lines.Text;
  finally
    MoldModel.Free;
    Lines.Free;
  end;
end;

procedure TBoldModelDocument.Save;
begin
  if FFileName = '' then
    raise EBoldModelDocument.Create(sNoFileName);
  SaveAs(FFileName);
end;

procedure TBoldModelDocument.SaveAs(const AFileName: string);
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(AsText);
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    Stream.Free;
  end;
  FFileName := AFileName;
  FModified := False;
end;

procedure TBoldModelDocument.GenerateCode(const AFolder: string);
var
  Generator: TBoldGenerator;
begin
  if not DirectoryExists(AFolder) then
    raise EBoldModelDocument.CreateFmt(sNoFolder, [AFolder]);
  // TBoldModel makes its stored model from the UML model when that changed:
  // it flattens and boldifies the UML model for that and undoes it after,
  // which is no change of the document
  Inc(FIgnoreChanges);
  try
    Generator := TBoldGenerator.Create(FModel.TypeNameDictionary);
    try
      Generator.BaseFilePath := IncludeTrailingPathDelimiter(AFolder);
      Generator.UseTypedLists := True;
      Generator.MoldModel := FModel.MoldModel;
      Generator.GenerateBusinessObjectCode;
      Generator.EnsureMethodImplementations;
    finally
      // The generator writes its files when it is freed
      Generator.Free;
    end;
  finally
    Dec(FIgnoreChanges);
  end;
end;

procedure TBoldModelDocument.LoadTypeNames(const AFormFile: string);
var
  Handle: TBoldTypeNameHandle;
begin
  // Read into a handle of its own, so that a form that cannot be read leaves
  // the type names as they were
  Handle := TBoldTypeNameHandle.Create(nil);
  try
    ReadTypeNameHandle(TypeNameHandleText(AFormFile), Handle);
  except
    Handle.Free;
    raise;
  end;
  FModel.TypeNameHandle := Handle;
  FTypeNameHandle.Free;
  FTypeNameHandle := Handle;
  FTypeNamesFile := AFormFile;
end;

end.
