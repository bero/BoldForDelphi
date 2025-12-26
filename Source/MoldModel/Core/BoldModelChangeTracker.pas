{$INCLUDE bold.inc}
unit BoldModelChangeTracker;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  BoldMeta;

type
  TBoldAttributeSnapshot = record
    ClassName: string;
    AttributeName: string;
    TypeName: string;
    IsPersistent: Boolean;
    IsDerived: Boolean;
  end;

  TBoldClassSnapshot = record
    ClassName: string;
    SuperClassName: string;
    IsPersistent: Boolean;
    TableName: string;
  end;

  TBoldRoleSnapshot = record
    AssociationName: string;
    RoleName: string;
    ClassName: string;
    Multiplicity: string;
    IsNavigable: Boolean;
    IsPersistent: Boolean;
    IsOrdered: Boolean;
  end;

  TBoldModelSnapshot = class
  private
    FClasses: TList<TBoldClassSnapshot>;
    FAttributes: TList<TBoldAttributeSnapshot>;
    FRoles: TList<TBoldRoleSnapshot>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure CaptureFrom(MoldModel: TMoldModel);
    property Classes: TList<TBoldClassSnapshot> read FClasses;
    property Attributes: TList<TBoldAttributeSnapshot> read FAttributes;
    property Roles: TList<TBoldRoleSnapshot> read FRoles;
  end;

  TBoldModelChangeTracker = class
  private
    FBeforeSnapshot: TBoldModelSnapshot;
    FAfterSnapshot: TBoldModelSnapshot;
    FSchemaChangeReasons: TStringList;
    function FindClass(Snapshot: TBoldModelSnapshot; const ClassName: string): Integer;
    function FindAttribute(Snapshot: TBoldModelSnapshot; const ClassName, AttrName: string): Integer;
    function FindRole(Snapshot: TBoldModelSnapshot; const AssocName, RoleName: string): Integer;
    procedure DetectClassChanges;
    procedure DetectAttributeChanges;
    procedure DetectRoleChanges;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CaptureBeforeState(MoldModel: TMoldModel);
    procedure CaptureAfterState(MoldModel: TMoldModel);
    function RequiresSqlEvolution: Boolean;
    property SchemaChangeReasons: TStringList read FSchemaChangeReasons;
  end;

implementation

{ TBoldModelSnapshot }

constructor TBoldModelSnapshot.Create;
begin
  inherited;
  FClasses := TList<TBoldClassSnapshot>.Create;
  FAttributes := TList<TBoldAttributeSnapshot>.Create;
  FRoles := TList<TBoldRoleSnapshot>.Create;
end;

destructor TBoldModelSnapshot.Destroy;
begin
  FClasses.Free;
  FAttributes.Free;
  FRoles.Free;
  inherited;
end;

procedure TBoldModelSnapshot.Clear;
begin
  FClasses.Clear;
  FAttributes.Clear;
  FRoles.Clear;
end;

procedure TBoldModelSnapshot.CaptureFrom(MoldModel: TMoldModel);
var
  i, j: Integer;
  MoldClass: TMoldClass;
  MoldAttr: TMoldAttribute;
  MoldAssoc: TMoldAssociation;
  MoldRole: TMoldRole;
  ClassSnap: TBoldClassSnapshot;
  AttrSnap: TBoldAttributeSnapshot;
  RoleSnap: TBoldRoleSnapshot;
begin
  Clear;

  // Capture classes and their attributes
  for i := 0 to MoldModel.Classes.Count - 1 do
  begin
    MoldClass := MoldModel.Classes[i];

    // Capture class info
    ClassSnap.ClassName := MoldClass.ExpandedExpressionName;
    if Assigned(MoldClass.SuperClass) then
      ClassSnap.SuperClassName := MoldClass.SuperClass.ExpandedExpressionName
    else
      ClassSnap.SuperClassName := '';
    ClassSnap.IsPersistent := MoldClass.EffectivePersistent;
    ClassSnap.TableName := MoldClass.TableName;
    FClasses.Add(ClassSnap);

    // Capture attributes
    for j := 0 to MoldClass.Attributes.Count - 1 do
    begin
      MoldAttr := MoldClass.Attributes[j];
      AttrSnap.ClassName := MoldClass.ExpandedExpressionName;
      AttrSnap.AttributeName := MoldAttr.ExpandedExpressionName;
      AttrSnap.TypeName := MoldAttr.BoldType;
      AttrSnap.IsPersistent := MoldAttr.EffectivePersistent;
      AttrSnap.IsDerived := MoldAttr.Derived;
      FAttributes.Add(AttrSnap);
    end;
  end;

  // Capture roles
  for i := 0 to MoldModel.Associations.Count - 1 do
  begin
    MoldAssoc := MoldModel.Associations[i];
    for j := 0 to MoldAssoc.Roles.Count - 1 do
    begin
      MoldRole := MoldAssoc.Roles[j];
      RoleSnap.AssociationName := MoldAssoc.ExpandedExpressionName;
      RoleSnap.RoleName := MoldRole.ExpandedExpressionName;
      if Assigned(MoldRole.MoldClass) then
        RoleSnap.ClassName := MoldRole.MoldClass.ExpandedExpressionName
      else
        RoleSnap.ClassName := '';
      RoleSnap.Multiplicity := MoldRole.Multiplicity;
      RoleSnap.IsNavigable := MoldRole.Navigable;
      RoleSnap.IsPersistent := MoldRole.EffectivePersistent;
      RoleSnap.IsOrdered := MoldRole.Ordered;
      FRoles.Add(RoleSnap);
    end;
  end;
end;

{ TBoldModelChangeTracker }

constructor TBoldModelChangeTracker.Create;
begin
  inherited;
  FBeforeSnapshot := TBoldModelSnapshot.Create;
  FAfterSnapshot := TBoldModelSnapshot.Create;
  FSchemaChangeReasons := TStringList.Create;
end;

destructor TBoldModelChangeTracker.Destroy;
begin
  FBeforeSnapshot.Free;
  FAfterSnapshot.Free;
  FSchemaChangeReasons.Free;
  inherited;
end;

procedure TBoldModelChangeTracker.CaptureBeforeState(MoldModel: TMoldModel);
begin
  FBeforeSnapshot.CaptureFrom(MoldModel);
  FSchemaChangeReasons.Clear;
end;

procedure TBoldModelChangeTracker.CaptureAfterState(MoldModel: TMoldModel);
begin
  FAfterSnapshot.CaptureFrom(MoldModel);
end;

function TBoldModelChangeTracker.FindClass(Snapshot: TBoldModelSnapshot; const ClassName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Snapshot.Classes.Count - 1 do
    if SameText(Snapshot.Classes[i].ClassName, ClassName) then
      Exit(i);
  Result := -1;
end;

function TBoldModelChangeTracker.FindAttribute(Snapshot: TBoldModelSnapshot; const ClassName, AttrName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Snapshot.Attributes.Count - 1 do
    if SameText(Snapshot.Attributes[i].ClassName, ClassName) and
       SameText(Snapshot.Attributes[i].AttributeName, AttrName) then
      Exit(i);
  Result := -1;
end;

function TBoldModelChangeTracker.FindRole(Snapshot: TBoldModelSnapshot; const AssocName, RoleName: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Snapshot.Roles.Count - 1 do
    if SameText(Snapshot.Roles[i].AssociationName, AssocName) and
       SameText(Snapshot.Roles[i].RoleName, RoleName) then
      Exit(i);
  Result := -1;
end;

procedure TBoldModelChangeTracker.DetectClassChanges;
var
  i, j: Integer;
  BeforeClass, AfterClass: TBoldClassSnapshot;
begin
  // Check for added or modified classes
  for i := 0 to FAfterSnapshot.Classes.Count - 1 do
  begin
    AfterClass := FAfterSnapshot.Classes[i];
    j := FindClass(FBeforeSnapshot, AfterClass.ClassName);

    if j = -1 then
    begin
      // New class
      if AfterClass.IsPersistent then
        FSchemaChangeReasons.Add(Format('Added persistent class: %s', [AfterClass.ClassName]));
    end
    else
    begin
      BeforeClass := FBeforeSnapshot.Classes[j];

      // Check persistence change
      if BeforeClass.IsPersistent <> AfterClass.IsPersistent then
        FSchemaChangeReasons.Add(Format('Changed persistence of class: %s', [AfterClass.ClassName]));

      // Check table name change
      if AfterClass.IsPersistent and (not SameText(BeforeClass.TableName, AfterClass.TableName)) then
        FSchemaChangeReasons.Add(Format('Changed table name of class: %s', [AfterClass.ClassName]));

      // Check superclass change (affects table structure)
      if AfterClass.IsPersistent and (not SameText(BeforeClass.SuperClassName, AfterClass.SuperClassName)) then
        FSchemaChangeReasons.Add(Format('Changed superclass of class: %s', [AfterClass.ClassName]));
    end;
  end;

  // Check for removed classes
  for i := 0 to FBeforeSnapshot.Classes.Count - 1 do
  begin
    BeforeClass := FBeforeSnapshot.Classes[i];
    if BeforeClass.IsPersistent then
    begin
      j := FindClass(FAfterSnapshot, BeforeClass.ClassName);
      if j = -1 then
        FSchemaChangeReasons.Add(Format('Removed persistent class: %s', [BeforeClass.ClassName]));
    end;
  end;
end;

procedure TBoldModelChangeTracker.DetectAttributeChanges;
var
  i, j: Integer;
  BeforeAttr, AfterAttr: TBoldAttributeSnapshot;
begin
  // Check for added or modified attributes
  for i := 0 to FAfterSnapshot.Attributes.Count - 1 do
  begin
    AfterAttr := FAfterSnapshot.Attributes[i];
    j := FindAttribute(FBeforeSnapshot, AfterAttr.ClassName, AfterAttr.AttributeName);

    if j = -1 then
    begin
      // New attribute
      if AfterAttr.IsPersistent and not AfterAttr.IsDerived then
        FSchemaChangeReasons.Add(Format('Added persistent attribute: %s.%s',
          [AfterAttr.ClassName, AfterAttr.AttributeName]));
    end
    else
    begin
      BeforeAttr := FBeforeSnapshot.Attributes[j];

      // Check if became persistent
      if (not BeforeAttr.IsPersistent or BeforeAttr.IsDerived) and
         (AfterAttr.IsPersistent and not AfterAttr.IsDerived) then
        FSchemaChangeReasons.Add(Format('Attribute became persistent: %s.%s',
          [AfterAttr.ClassName, AfterAttr.AttributeName]));

      // Check if became non-persistent
      if (BeforeAttr.IsPersistent and not BeforeAttr.IsDerived) and
         (not AfterAttr.IsPersistent or AfterAttr.IsDerived) then
        FSchemaChangeReasons.Add(Format('Attribute became non-persistent: %s.%s',
          [AfterAttr.ClassName, AfterAttr.AttributeName]));

      // Check type change
      if (AfterAttr.IsPersistent and not AfterAttr.IsDerived) and
         (not SameText(BeforeAttr.TypeName, AfterAttr.TypeName)) then
        FSchemaChangeReasons.Add(Format('Changed type of attribute: %s.%s (%s -> %s)',
          [AfterAttr.ClassName, AfterAttr.AttributeName, BeforeAttr.TypeName, AfterAttr.TypeName]));
    end;
  end;

  // Check for removed attributes
  for i := 0 to FBeforeSnapshot.Attributes.Count - 1 do
  begin
    BeforeAttr := FBeforeSnapshot.Attributes[i];
    if BeforeAttr.IsPersistent and not BeforeAttr.IsDerived then
    begin
      j := FindAttribute(FAfterSnapshot, BeforeAttr.ClassName, BeforeAttr.AttributeName);
      if j = -1 then
        FSchemaChangeReasons.Add(Format('Removed persistent attribute: %s.%s',
          [BeforeAttr.ClassName, BeforeAttr.AttributeName]));
    end;
  end;
end;

procedure TBoldModelChangeTracker.DetectRoleChanges;
var
  i, j: Integer;
  BeforeRole, AfterRole: TBoldRoleSnapshot;
begin
  // Check for added or modified roles
  for i := 0 to FAfterSnapshot.Roles.Count - 1 do
  begin
    AfterRole := FAfterSnapshot.Roles[i];
    j := FindRole(FBeforeSnapshot, AfterRole.AssociationName, AfterRole.RoleName);

    if j = -1 then
    begin
      // New role
      if AfterRole.IsPersistent then
        FSchemaChangeReasons.Add(Format('Added persistent role: %s.%s',
          [AfterRole.AssociationName, AfterRole.RoleName]));
    end
    else
    begin
      BeforeRole := FBeforeSnapshot.Roles[j];

      // Check persistence change
      if BeforeRole.IsPersistent <> AfterRole.IsPersistent then
        FSchemaChangeReasons.Add(Format('Changed persistence of role: %s.%s',
          [AfterRole.AssociationName, AfterRole.RoleName]));

      // Check multiplicity change (affects storage)
      if AfterRole.IsPersistent and (BeforeRole.Multiplicity <> AfterRole.Multiplicity) then
        FSchemaChangeReasons.Add(Format('Changed multiplicity of role: %s.%s (%s -> %s)',
          [AfterRole.AssociationName, AfterRole.RoleName, BeforeRole.Multiplicity, AfterRole.Multiplicity]));

      // Check ordered change (may affect storage)
      if AfterRole.IsPersistent and (BeforeRole.IsOrdered <> AfterRole.IsOrdered) then
        FSchemaChangeReasons.Add(Format('Changed ordering of role: %s.%s',
          [AfterRole.AssociationName, AfterRole.RoleName]));
    end;
  end;

  // Check for removed roles
  for i := 0 to FBeforeSnapshot.Roles.Count - 1 do
  begin
    BeforeRole := FBeforeSnapshot.Roles[i];
    if BeforeRole.IsPersistent then
    begin
      j := FindRole(FAfterSnapshot, BeforeRole.AssociationName, BeforeRole.RoleName);
      if j = -1 then
        FSchemaChangeReasons.Add(Format('Removed persistent role: %s.%s',
          [BeforeRole.AssociationName, BeforeRole.RoleName]));
    end;
  end;
end;

function TBoldModelChangeTracker.RequiresSqlEvolution: Boolean;
begin
  FSchemaChangeReasons.Clear;
  DetectClassChanges;
  DetectAttributeChanges;
  DetectRoleChanges;
  Result := FSchemaChangeReasons.Count > 0;
end;

end.
