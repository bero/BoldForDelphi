# TBoldClassTypeInfo

`TBoldClassTypeInfo` contains runtime type information for a single class in the Bold model. It provides metadata about the class's members, associations, methods, constraints, and position in the inheritance hierarchy.

## Class Hierarchy

```mermaid
classDiagram
    TBoldElementTypeInfo <|-- TBoldElementTypeInfoWithConstraint
    TBoldElementTypeInfoWithConstraint <|-- TBoldClassTypeInfo
    TBoldClassTypeInfo <|-- TBoldNilTypeInfo
    TBoldClassTypeInfo o-- TBoldMemberRTInfo : AllMembers
    TBoldClassTypeInfo o-- TBoldRoleRTInfo : AllRoles

    TBoldClassTypeInfo : +AllMembers
    TBoldClassTypeInfo : +SuperClassTypeInfo
    TBoldClassTypeInfo : +ObjectClass
    TBoldClassTypeInfo : +BoldIsA()

    click TBoldSystemTypeInfo href "../TBoldSystemTypeInfo/" "TBoldSystemTypeInfo"
    click TBoldMemberRTInfo href "../TBoldMemberRTInfo/" "TBoldMemberRTInfo"
```

## Class Definition

```pascal
TBoldClassTypeInfo = class(TBoldElementTypeInfoWithConstraint)
public
  // Member access
  property AllMembers: TBoldMemberRTInfoList;   // attributes + roles (including inherited)
  property AllRoles: TBoldRoleRTInfoList;        // roles only
  property Methods: TBoldMethodRTInfoList;
  property AllMembersCount: Integer;
  property AllRolesCount: Integer;
  property AttributeCount: Integer;
  property SinglelinkCount: Integer;
  property MultilinkCount: Integer;
  property DerivedMemberCount: Integer;
  property FirstOwnMemberIndex: Integer;  // inherited members come before this
  property MemberIndexByExpressionName[const name: string]: Integer;
  property MemberRTInfoByExpressionName[const name: string]: TBoldMemberRTInfo;
  property MemberRTInfoByModelName[const name: string]: TBoldMemberRTInfo;

  // Hierarchy
  property SuperClassTypeInfo: TBoldClassTypeInfo;  // nil for root
  property SubClasssesBoldClassTypeInfoList: TBoldClassTypeInfoList;
  property TopSortedIndex: Integer;
  property SystemTypeInfo: TBoldSystemTypeInfo;
  function BoldIsA(C2: TBoldElementTypeInfo): Boolean;
  function ConformsTo(CompareElement: TBoldElementTypeInfo): Boolean;
  function LeastCommonSuperClass(Other: TBoldClassTypeInfo): TBoldClassTypeInfo;

  // Class metadata
  property ObjectClass: TClass;         // Delphi class
  property Persistent: Boolean;
  property IsAbstract: Boolean;
  property IsLinkClass: Boolean;        // association class?
  property HasSubclasses: Boolean;
  property Stereotype: string;
  property DefaultStringRepresentation: string;
  property QualifiedName: string;
  property OptimisticLocking: TBoldOptimisticLockingMode;
  property Versioned: Boolean;
  property ToBeRemoved: Boolean;        // schema evolution

  // Factory
  function CreateElement: TBoldElement;
end;
```

## Working with TBoldClassTypeInfo

### Iterating Members

```pascal
var
  ClassInfo: TBoldClassTypeInfo;
  MemberInfo: TBoldMemberRTInfo;
  i: Integer;
begin
  ClassInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['Customer'];

  for i := 0 to ClassInfo.AllMembersCount - 1 do
  begin
    MemberInfo := ClassInfo.AllMembers[i];
    Log(Format('%s: attr=%s, role=%s, persistent=%s, derived=%s', [
      MemberInfo.ExpressionName,
      BoolToStr(MemberInfo.IsAttribute, True),
      BoolToStr(MemberInfo.IsRole, True),
      BoolToStr(MemberInfo.Persistent, True),
      BoolToStr(MemberInfo.IsDerived, True)]));
  end;
end;
```

### Checking Inheritance

```pascal
var
  CustomerInfo, PersonInfo: TBoldClassTypeInfo;
begin
  CustomerInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['Customer'];
  PersonInfo := SystemTypeInfo.ClassTypeInfoByExpressionName['Person'];

  // Check if Customer is a subclass of Person
  if CustomerInfo.BoldIsA(PersonInfo) then
    Log('Customer inherits from Person');

  // Navigate the hierarchy
  Log('Parent: ' + CustomerInfo.SuperClassTypeInfo.ExpressionName);

  // Find common superclass
  var Common := CustomerInfo.LeastCommonSuperClass(SupplierInfo);
end;
```

### Own vs Inherited Members

```pascal
// Members before FirstOwnMemberIndex are inherited
for i := ClassInfo.FirstOwnMemberIndex to ClassInfo.AllMembersCount - 1 do
begin
  // These are this class's own members
  MemberInfo := ClassInfo.AllMembers[i];
  Log('Own member: ' + MemberInfo.ExpressionName);
end;
```

### TBoldNilTypeInfo

`TBoldNilTypeInfo` is a special subclass representing the `nil` type. It conforms to all classes (like `null` in Java/C#), which is essential for OCL type checking.

## See Also

- [TBoldSystemTypeInfo](TBoldSystemTypeInfo.md) - System-wide type info
- [TBoldMemberRTInfo](TBoldMemberRTInfo.md) - Member metadata
- [TBoldSystem](TBoldSystem.md) - Object Space
