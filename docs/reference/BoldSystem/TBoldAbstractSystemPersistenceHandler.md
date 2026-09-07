# TBoldAbstractSystemPersistenceHandler

Bold-internal

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractSystemPersistenceHandler = class(TBoldSystemExtension)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. [TBoldSystemExtension](TBoldSystemExtension.md)
6. TBoldAbstractSystemPersistenceHandler
7. **Direct subclasses**
8. `TBoldSystemPersistenceHandler`

## Description

Bold-internal

## Properties

| Name | Summary | Notes |
|---|---|---|
| [OnPreUpdate](#onpreupdate) |  |  |
| [TimeStampOfLatestUpdate](#timestampoflatestupdate) |  | read-only |

### OnPreUpdate

```delphi
property OnPreUpdate: TNotifyEvent;
```

### TimeStampOfLatestUpdate

```delphi
property TimeStampOfLatestUpdate: TBoldTimeStampType;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [EndFetchForAll](#endfetchforall) |  |  |
| [EndUpdateForAll](#endupdateforall) |  |  |
| [EnsureEnclosure](#ensureenclosure) |  | abstract |
| [FetchClass](#fetchclass) |  | abstract |
| [FetchLinksWithObjects](#fetchlinkswithobjects) |  | abstract |
| [FetchList](#fetchlist) |  | abstract |
| [FetchMember](#fetchmember) |  | abstract |
| [FetchObjectById](#fetchobjectbyid) |  | abstract |
| [GetAllInClassWithSQL](#getallinclasswithsql) |  | abstract |
| [GetAllWithCondition](#getallwithcondition) |  | abstract |
| [GetTimeForTimestamp](#gettimefortimestamp) |  | abstract |
| [GetTimestampForTime](#gettimestampfortime) |  | abstract |
| [GetTimeStampOfLatestUpdate](#gettimestampoflatestupdate) |  | protected, abstract |
| [StartUpdateForAll](#startupdateforall) |  |  |
| [UpdateDatabaseWithList](#updatedatabasewithlist) | Internal use | abstract |

### EndFetchForAll

```delphi
procedure EndFetchForAll(ObjectList: TBoldObjectList; MemberIdList: TBoldMemberIdList);
```

### EndUpdateForAll

```delphi
procedure EndUpdateForAll(ObjectList: TBoldObjectList; Translationlist: TBoldIdTranslationlist);
```

### EnsureEnclosure

```delphi
function EnsureEnclosure(ObjectList: TBoldObjectList; ValidateOnly: Boolean): Boolean; virtual; abstract;
```

### FetchClass

```delphi
procedure FetchClass(ClassList: TBoldObjectList; Time: TBoldTimestampType); virtual; abstract;
```

### FetchLinksWithObjects

```delphi
procedure FetchLinksWithObjects(ObjectList: TBoldObjectList; const LinkName: string); virtual; abstract;
```

### FetchList

```delphi
procedure FetchList(FetchList: TBoldObjectList); virtual; abstract;
```

### FetchMember

```delphi
procedure FetchMember(Member: TBoldMember); virtual; abstract;
```

### FetchObjectById

```delphi
procedure FetchObjectById(BoldObjectId: TBoldObjectId); virtual; abstract;
```

### GetAllInClassWithSQL

```delphi
procedure GetAllInClassWithSQL(aList: TBoldObjectList; AClass: TBoldObjectClass; WhereClause, OrderByClause: String; Params: TParams; JoinInheritedTables: Boolean; MaxAnswers: integer; Offset: integer); virtual; abstract;
```

### GetAllWithCondition

```delphi
procedure GetAllWithCondition(aList: TBoldObjectList; Condition: TBoldCondition); virtual; abstract;
```

### GetTimeForTimestamp

```delphi
function GetTimeForTimestamp(Timestamp: TBoldTimestampType): TDateTime; virtual; abstract;
```

### GetTimestampForTime

```delphi
function GetTimestampForTime(ClockTime: TDateTime): TBoldTimestampType; virtual; abstract;
```

### GetTimeStampOfLatestUpdate

```delphi
function GetTimeStampOfLatestUpdate: TBoldTimeStampType; virtual; abstract;
```

### StartUpdateForAll

```delphi
function StartUpdateForAll(ObjectList: TBoldObjectList): Boolean;
```

### UpdateDatabaseWithList

```delphi
procedure UpdateDatabaseWithList(ObjectList: TBoldObjectList); virtual; abstract;
```

Abstract declaration of UpdateDatabaseWithList.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
