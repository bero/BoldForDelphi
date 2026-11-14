
{ Global compiler directives }
{$include bold.inc}
unit BoldDataSetComponentEditor;

{ The component editor of TBoldDataSet in the Lazarus form designer (#117):
  the menu of the component, and what a double click on it does. It is the
  Lazarus counterpart of Delphi's TBoldDataSetEditor
  (BoldDataSetPropertyEditors), with its three commands:

    Fields Editor...        the field descriptions, in the collection editor
                            (also the double click)
    Create default fields   one field description for each attribute of the
                            class the handle lists
    Clear all columns       no field descriptions

  A fourth command is the one every dataset has in Lazarus, its own fields
  editor for persistent TField components.

  In the Lazarus design-time package dclBold (packages/Lazarus/dclBold.lpk). }

interface

uses
  Classes,
  ComponentEditors,
  FieldsEditor,
  BoldDataSet;

const
  // The commands, in the order of the menu
  bdsVerbFieldDescriptions = 0;
  bdsVerbCreateDefaultFields = 1;
  bdsVerbClearFields = 2;
  bdsVerbPersistentFields = 3;

type
  { TBoldDataSetComponentEditor }
  TBoldDataSetComponentEditor = class(TFieldsComponentEditor)
  private
    function GetDataSet: TBoldAbstractDataSet;
    procedure FieldDescriptionsChanged;
    procedure DeselectFieldDescriptions;
  protected
    // Opens the collection editor; virtual so that a check can stand in for it
    procedure ShowFieldDescriptions(ACollection: TCollection; AOwner: TPersistent;
      const APropertyName: string); virtual;
    procedure EditFieldDescriptions;
    procedure CreateDefaultFields;
    procedure ClearFields;
  public
    function GetVerbCount: Integer; override;
    // AnsiString: the string type of the LCL
    function GetVerb(Index: Integer): AnsiString; override;
    procedure ExecuteVerb(Index: Integer); override;
    property DataSet: TBoldAbstractDataSet read GetDataSet;
  end;

implementation

uses
  PropEdits;

const
  cFieldDescriptionsProperty = 'FieldDescriptions';

type
  TBoldExposedAbstractDataSet = class(TBoldAbstractDataSet);

{ TBoldDataSetComponentEditor }

function TBoldDataSetComponentEditor.GetDataSet: TBoldAbstractDataSet;
begin
  if GetComponent is TBoldAbstractDataSet then
    Result := TBoldAbstractDataSet(GetComponent)
  else
    Result := nil;
end;

function TBoldDataSetComponentEditor.GetVerbCount: Integer;
begin
  Result := 3 + inherited GetVerbCount;
end;

function TBoldDataSetComponentEditor.GetVerb(Index: Integer): AnsiString;
begin
  case Index of
    bdsVerbFieldDescriptions: Result := 'Fields Editor...';
    bdsVerbCreateDefaultFields: Result := 'Create default fields';
    bdsVerbClearFields: Result := 'Clear all columns';
  else
    Result := inherited GetVerb(Index - bdsVerbPersistentFields);
  end;
end;

procedure TBoldDataSetComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    bdsVerbFieldDescriptions: EditFieldDescriptions;
    bdsVerbCreateDefaultFields: CreateDefaultFields;
    bdsVerbClearFields: ClearFields;
  else
    inherited ExecuteVerb(Index - bdsVerbPersistentFields);
  end;
end;

procedure TBoldDataSetComponentEditor.ShowFieldDescriptions(ACollection: TCollection;
  AOwner: TPersistent; const APropertyName: string);
begin
  TCollectionPropertyEditor.ShowCollectionEditor(ACollection, AOwner, APropertyName);
end;

procedure TBoldDataSetComponentEditor.EditFieldDescriptions;
begin
  if Assigned(DataSet) then
    ShowFieldDescriptions(DataSet.FieldDescriptions, DataSet, cFieldDescriptionsProperty);
end;

{ The Object Inspector may show a field description that is about to be
  freed: the dataset is selected in its place }
procedure TBoldDataSetComponentEditor.DeselectFieldDescriptions;
begin
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.SelectOnlyThis(DataSet);
end;

{ Marks the form as changed and has the Object Inspector and an open
  collection editor read the field descriptions again }
procedure TBoldDataSetComponentEditor.FieldDescriptionsChanged;
begin
  if Assigned(Designer) then
    Modified;
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RefreshPropertyValues;
end;

procedure TBoldDataSetComponentEditor.CreateDefaultFields;
begin
  // As in Delphi: without a handle there is no class to take the fields
  // from, and the field descriptions stay as they are
  if Assigned(DataSet) and Assigned(TBoldExposedAbstractDataSet(DataSet).BoldHandle) then
  begin
    DeselectFieldDescriptions;
    DataSet.CreateDefaultFields;
    FieldDescriptionsChanged;
  end;
end;

procedure TBoldDataSetComponentEditor.ClearFields;
begin
  if Assigned(DataSet) then
  begin
    DeselectFieldDescriptions;
    TBoldExposedAbstractDataSet(DataSet).DeleteAllFields;
    FieldDescriptionsChanged;
  end;
end;

end.
