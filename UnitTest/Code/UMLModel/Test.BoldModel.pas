unit Test.BoldModel;

{ How a TBoldModel streams into a form file. The designer writes every stored
  property when it saves a form or data module, so a model that only holds the
  empty default model, and Boldify settings at their defaults, must not be
  stored: otherwise saving a data module that loads its model at run time (such
  as the Invoicing example's dmInvoicing) adds an empty model to its form file. }

interface

uses
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('UMLModel')]
  {$ENDIF}
  TTestBoldModelStreaming = class(TBoldTestCase)
  published
    procedure TestEmptyModelIsNotStreamed;
    procedure TestModelWithContentIsStreamed;
    procedure TestModelSurvivesStreaming;
    procedure TestDefaultBoldifyIsNotStreamed;
    procedure TestChangedBoldifyIsStreamed;
    procedure TestChangedBoldifySurvivesStreaming;
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldMeta,
  BoldModel;

const
  cModelProperty = 'Model = (';
  cModelName = 'StreamedModel';

// The component as the text a form file stores it in.
function StreamAsText(Component: TComponent): string;
var
  Binary, Text: TMemoryStream;
  Bytes: AnsiString;
begin
  Binary := TMemoryStream.Create;
  Text := TMemoryStream.Create;
  try
    Binary.WriteComponent(Component);
    Binary.Position := 0;
    ObjectBinaryToText(Binary, Text);
    SetLength(Bytes, Text.Size);
    if Text.Size > 0 then
      Move(Text.Memory^, Bytes[1], Text.Size);
    Result := string(Bytes);
  finally
    Text.Free;
    Binary.Free;
  end;
end;

// Streams Source out and into Target, as saving and loading a form file does.
procedure CopyByStreaming(Source, Target: TComponent);
var
  Binary: TMemoryStream;
begin
  Binary := TMemoryStream.Create;
  try
    Binary.WriteComponent(Source);
    Binary.Position := 0;
    Binary.ReadComponent(Target);
  finally
    Binary.Free;
  end;
end;

procedure TTestBoldModelStreaming.TestEmptyModelIsNotStreamed;
var
  Model: TBoldModel;
  Text: string;
begin
  Model := TBoldModel.Create(nil);
  try
    Text := StreamAsText(Model);
    Assert.IsFalse(Pos(cModelProperty, Text) > 0, Text);
  finally
    Model.Free;
  end;
end;

procedure TTestBoldModelStreaming.TestModelWithContentIsStreamed;
var
  Model: TBoldModel;
  Text: string;
begin
  Model := TBoldModel.Create(nil);
  try
    Model.SetFromModel(TMoldModel.Create(nil, cModelName));
    Text := StreamAsText(Model);
    Assert.IsTrue(Pos(cModelProperty, Text) > 0, Text);
    Assert.IsTrue(Pos(cModelName, Text) > 0, Text);
  finally
    Model.Free;
  end;
end;

procedure TTestBoldModelStreaming.TestModelSurvivesStreaming;
var
  Source, Target: TBoldModel;
begin
  Source := TBoldModel.Create(nil);
  Target := TBoldModel.Create(nil);
  try
    Source.SetFromModel(TMoldModel.Create(nil, cModelName));
    CopyByStreaming(Source, Target);
    Assert.AreEqual(cModelName, Target.MoldModel.Name);
  finally
    Target.Free;
    Source.Free;
  end;
end;

procedure TTestBoldModelStreaming.TestDefaultBoldifyIsNotStreamed;
var
  Model: TBoldModel;
  Text: string;
begin
  Model := TBoldModel.Create(nil);
  try
    Text := StreamAsText(Model);
    Assert.IsFalse(Pos('Boldify.', Text) > 0, Text);
  finally
    Model.Free;
  end;
end;

procedure TTestBoldModelStreaming.TestChangedBoldifyIsStreamed;
var
  Model: TBoldModel;
  Text: string;
begin
  Model := TBoldModel.Create(nil);
  try
    Model.Boldify.DefaultNavigableMultiplicity := '1';
    Model.Boldify.DefaultNonNavigableMultiplicity := '1..*';
    Model.Boldify.EnforceDefaultUMLCase := True;
    Text := StreamAsText(Model);
    Assert.IsTrue(Pos('Boldify.DefaultNavigableMultiplicity', Text) > 0, Text);
    Assert.IsTrue(Pos('Boldify.DefaultNonNavigableMultiplicity', Text) > 0, Text);
    Assert.IsTrue(Pos('Boldify.EnforceDefaultUMLCase', Text) > 0, Text);
  finally
    Model.Free;
  end;
end;

procedure TTestBoldModelStreaming.TestChangedBoldifySurvivesStreaming;
var
  Source, Target: TBoldModel;
begin
  Source := TBoldModel.Create(nil);
  Target := TBoldModel.Create(nil);
  try
    Source.Boldify.DefaultNavigableMultiplicity := '1';
    Source.Boldify.DefaultNonNavigableMultiplicity := '1..*';
    Source.Boldify.EnforceDefaultUMLCase := True;
    CopyByStreaming(Source, Target);
    Assert.AreEqual('1', Target.Boldify.DefaultNavigableMultiplicity);
    Assert.AreEqual('1..*', Target.Boldify.DefaultNonNavigableMultiplicity);
    Assert.IsTrue(Target.Boldify.EnforceDefaultUMLCase);
  finally
    Target.Free;
    Source.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldModelStreaming);

end.
