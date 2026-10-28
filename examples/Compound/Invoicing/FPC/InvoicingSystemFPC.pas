{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit InvoicingSystemFPC;
{ The Bold infrastructure of the Invoicing example for Free Pascal: the FPC
  counterpart of ../Data/dmInvoicing.pas. dmInvoicing stores in SQLite through
  FireDAC, which Free Pascal does not have; this unit stores in an XML file
  through Bold's file persistence (TBoldPersistenceHandleFileXML). The calls
  match dmInvoicing, so the FPC runner and the FPCUnit tests read like their
  Delphi twins: LoadModelFromFile, OpenSystem, Save, CloseSystem. }

interface

uses
  SysUtils,
  Classes,
  BoldDefs,
  BoldHandles,
  BoldModel,
  BoldSystem,
  BoldSystemHandle,
  BoldPersistenceHandleFileXML;

type
  TInvoicingSystem = class
  private
    FModel: TBoldModel;
    FTypeInfoHandle: TBoldSystemTypeInfoHandle;
    FPersistenceHandle: TBoldPersistenceHandleFileXML;
    FSystemHandle: TBoldSystemHandle;
    FModelLoaded: Boolean;
    function GetBoldSystem: TBoldSystem;
    function GetIsOpen: Boolean;
    function GetDataFile: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadModelFromFile(const AFileName: string);
    { ADataFile is the XML file the object space is read from and saved to.
      A file that does not exist yet is an empty database. }
    procedure OpenSystem(const ADataFile: string);
    procedure CloseSystem(ASaveChanges: Boolean);
    procedure Save;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property IsOpen: Boolean read GetIsOpen;
    property ModelLoaded: Boolean read FModelLoaded;
    property DataFile: string read GetDataFile;
  end;

implementation

resourcestring
  sModelNotLoaded = 'Load the model with LoadModelFromFile before opening the system';
  sSystemAlreadyOpen = 'The system is already open on %s';
  sModelFileNotFound = 'Model file not found: %s';

constructor TInvoicingSystem.Create;
begin
  inherited Create;
  FModel := TBoldModel.Create(nil);
  FTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  FTypeInfoHandle.BoldModel := FModel;
  FPersistenceHandle := TBoldPersistenceHandleFileXML.Create(nil);
  FPersistenceHandle.BoldModel := FModel;
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FTypeInfoHandle;
  FSystemHandle.PersistenceHandle := FPersistenceHandle;
end;

destructor TInvoicingSystem.Destroy;
begin
  CloseSystem(False);
  // Reverse creation order, so nothing is destroyed while still referenced.
  FSystemHandle.PersistenceHandle := nil;
  FreeAndNil(FSystemHandle);
  FreeAndNil(FPersistenceHandle);
  FreeAndNil(FTypeInfoHandle);
  FreeAndNil(FModel);
  inherited;
end;

function TInvoicingSystem.GetBoldSystem: TBoldSystem;
begin
  Result := FSystemHandle.System;
end;

function TInvoicingSystem.GetIsOpen: Boolean;
begin
  Result := FSystemHandle.Active;
end;

function TInvoicingSystem.GetDataFile: string;
begin
  Result := FPersistenceHandle.FileName;
end;

procedure TInvoicingSystem.LoadModelFromFile(const AFileName: string);
var
  Strings: TStringList;
begin
  if not FileExists(AFileName) then
    raise EBold.CreateFmt(sModelFileNotFound, [AFileName]);
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(AFileName);
    FModel.SetFromModelAsString(Strings);
    FModelLoaded := True;
  finally
    Strings.Free;
  end;
end;

procedure TInvoicingSystem.OpenSystem(const ADataFile: string);
begin
  if not FModelLoaded then
    raise EBold.Create(sModelNotLoaded);
  if IsOpen then
    raise EBold.CreateFmt(sSystemAlreadyOpen, [DataFile]);
  FPersistenceHandle.FileName := ADataFile;
  FSystemHandle.Active := True;
end;

procedure TInvoicingSystem.CloseSystem(ASaveChanges: Boolean);
begin
  if not IsOpen then
    Exit;
  if BoldSystem.BoldDirty then
  begin
    if ASaveChanges then
      BoldSystem.UpdateDatabase
    else
      BoldSystem.Discard;
  end;
  FSystemHandle.Active := False;
end;

procedure TInvoicingSystem.Save;
begin
  BoldSystem.UpdateDatabase;
end;

end.
