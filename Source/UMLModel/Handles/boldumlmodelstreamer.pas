
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelStreamer;

interface

uses
  BoldSystem,
  BoldMeta;

type
  TUMLModelStreamer = class
  public
    class procedure FillSystemFromString(BoldSystem: TBoldSystem; const UMLModelAsString: string; MoldModel: TMoldModel);
    class function SystemAsString(BoldSystem: TBoldSystem; MoldModel: TMoldModel): string;
  end;


implementation

uses
  BoldXMLStreaming,
  BoldDefaultXMLStreaming,
  BoldDomainElement,
  BoldGuard,
  BoldDefs,
  BoldValueSpaceInterfaces,
  BoldID;

{ TUMLModelStreamer }

class procedure TUMLModelStreamer.FillSystemFromString(BoldSystem: TBoldSystem;
  const UMLModelAsString: string; MoldModel: TMoldModel);
var
  anXMLDoc: TBoldXMLDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(anXMLDoc, aMgr, aNode);
  anXMLDoc := TBoldXMLDocument.Create;
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);

  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToOverWrite := [bvpsInvalid, bvpsModified, bvpsTransient, bvpsCurrent];

  anXMLDoc.LoadXML(UMLModelAsString);

  aNode := aMgr.GetRootNode(anXMLDoc, 'ValueSpace'); // do not localize
  aMgr.ReadValueSpace(BoldSystem.AsIBoldvalueSpace[bdepPMIn], aNode);
end;

class function TUMLModelStreamer.SystemAsString(BoldSystem: TBoldSystem; MoldModel: TMoldModel): string;
var
  anXMLDoc: TBoldXMLDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  anIdList: TBoldObjectIdList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(aNode, aMgr, anXMLDoc, anIDList);
  anXMLDoc := TBoldXMLDocument.Create;
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);
  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsTransient, bvpsCurrent];
  aNode := aMgr.NewRootNode(anXMLDoc, 'ValueSpace'); // do not localize
  anIdList := TBoldObjectIdList.Create;

  BoldSystem.AsIBoldValueSpace[bdepPMOut].AllObjectIds(anIdList, True);
  aMgr.WriteValueSpace(BoldSystem.AsIBoldValueSpace[bdepPMOut], anIdList, nil, aNode);
  Result := anXMLDoc.RootXML;
end;

end.
