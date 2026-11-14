
{ Global compiler directives }
{$include bold.inc}
unit BoldModelComponentEditor;

{ The component editor of TBoldModel in the Lazarus form designer (#117): one
  command in the menu of the component, also the double click:

    Load model from file...   replaces the model with the one of a .bld file

  Delphi's component editor of TBoldModel opens the model editor, which is not
  ported; a model is made there, or in another tool, and saved as a .bld file.
  With the model in the form, the OCL editor checks the expressions of the
  handles and TBoldDataSet can make its default fields.

  The file is read as the model editor of Delphi imports it (BoldUMLBldLink):
  into the UML model of the component, which is what the form file holds.

  In the Lazarus design-time package dclBold (packages/Lazarus/dclBold.lpk). }

interface

uses
  Classes,
  ComponentEditors,
  BoldModel;

const
  bmoVerbLoadFromFile = 0;

type
  { TBoldModelComponentEditor }
  TBoldModelComponentEditor = class(TComponentEditor)
  private
    function GetBoldModel: TBoldModel;
  protected
    // Asks for the file; virtual so that a check can stand in for the dialog
    function AskForFileName(var AFileName: string): Boolean; virtual;
  public
    function GetVerbCount: Integer; override;
    // AnsiString: the string type of the LCL
    function GetVerb(Index: Integer): AnsiString; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure LoadFromFile;
    property BoldModel: TBoldModel read GetBoldModel;
  end;

{ Replaces the model of ABoldModel with the one in the .bld file. When the
  file cannot be read as a model, an exception says so and the model stays as
  it was. }
procedure BoldLoadModelFromBldFile(ABoldModel: TBoldModel; const AFileName: string);

implementation

uses
  SysUtils,
  Dialogs,
  BoldDefs,
  BoldMeta,
  BoldBld,
  BoldQueue,
  BoldSystem,
  BoldUMLModel,
  BoldUMLModelConverter;

procedure BoldLoadModelFromBldFile(ABoldModel: TBoldModel; const AFileName: string);
var
  MoldModel: TMoldModel;
  UMLModel: TUMLModel;
  BoldSystem: TBoldSystem;
  UndoWasActive: Boolean;
begin
  MoldModel := nil;
  try
    // Read first: a file that is no model leaves the component untouched
    try
      MoldModel := TMoldBLDRW.ModelFromFile(AFileName);
    except
      on E: Exception do
        raise EBoldImport.CreateFmt('%s is not read as a Bold model: %s', [AFileName, E.Message]);
    end;
    if not Assigned(MoldModel) or (MoldModel.Classes.Count = 0) then
      raise EBoldImport.CreateFmt('%s is not read as a Bold model: it has no classes', [AFileName]);

    ABoldModel.EnsuredUMLModel;
    // Through the property, which notes that the UML model is the current one
    UMLModel := ABoldModel.UMLModel;
    BoldSystem := UMLModel.BoldSystem;
    UndoWasActive := BoldSystem.UndoHandlerInterface.Enabled;
    BoldSystem.UndoHandlerInterface.Enabled := False;
    TBoldQueueable.DeActivateDisplayQueue;
    try
      BoldSystem.StartTransaction;
      try
        UMLModel.Clear;
        TBoldModelConverter.MoldModelToUML(MoldModel, UMLModel);
        BoldSystem.CommitTransaction;
      except
        BoldSystem.RollbackTransaction;
        raise;
      end;
    finally
      BoldSystem.UndoHandlerInterface.Enabled := UndoWasActive;
      TBoldQueueable.ActivateDisplayQueue;
    end;
  finally
    MoldModel.Free;
  end;
end;

{ TBoldModelComponentEditor }

function TBoldModelComponentEditor.GetBoldModel: TBoldModel;
begin
  if GetComponent is TBoldModel then
    Result := TBoldModel(GetComponent)
  else
    Result := nil;
end;

function TBoldModelComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TBoldModelComponentEditor.GetVerb(Index: Integer): AnsiString;
begin
  if Index = bmoVerbLoadFromFile then
    Result := 'Load model from file...'
  else
    Result := '';
end;

procedure TBoldModelComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index = bmoVerbLoadFromFile then
    LoadFromFile;
end;

function TBoldModelComponentEditor.AskForFileName(var AFileName: string): Boolean;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Dialog.Title := 'Load model from file';
    Dialog.DefaultExt := 'bld';
    Dialog.Filter := 'Bold model (*.bld)|*.bld|All files|*';
    Dialog.Options := Dialog.Options + [ofFileMustExist, ofPathMustExist];
    Result := Dialog.Execute;
    if Result then
      AFileName := string(Dialog.FileName);
  finally
    Dialog.Free;
  end;
end;

procedure TBoldModelComponentEditor.LoadFromFile;
var
  FileName: string;
begin
  FileName := '';
  if Assigned(BoldModel) and AskForFileName(FileName) then
  begin
    BoldLoadModelFromBldFile(BoldModel, FileName);
    if Assigned(Designer) then
      Modified;
  end;
end;

end.
