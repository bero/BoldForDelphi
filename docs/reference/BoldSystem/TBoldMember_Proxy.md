# TBoldMember_Proxy

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldMember_Proxy = class(TBoldDomainElement_Proxy, IBoldStreamable, IBoldValue)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldRefCountedObject`
5. [TBoldDomainElement_Proxy](../BoldDomainElement/TBoldDomainElement_Proxy.md)
6. TBoldMember_Proxy
7. **Direct subclasses**
8. [TBoldAttribute_Proxy](TBoldAttribute_Proxy.md)
9. `TBoldClassList_Proxy`
10. `TBoldObjectList_Proxy`

## Properties

| Name | Summary | Notes |
|---|---|---|
| [ProxedController](#proxedcontroller) |  | protected, read-only |
| [ProxedMember](#proxedmember) |  | protected, read-only |

### ProxedController

```delphi
property ProxedController: TBoldAbstractController;
```

### ProxedMember

```delphi
property ProxedMember: TBoldMember;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AssignContent](#assigncontent) |  | protected |
| [AssignContentValue](#assigncontentvalue) |  | protected, abstract |
| [GetBoldPersistenceState](#getboldpersistencestate) |  | protected |
| [GetContentName](#getcontentname) |  | protected |
| [GetStreamName](#getstreamname) |  | protected |
| [SetBoldPersistenceState](#setboldpersistencestate) |  | protected |

### AssignContent

```delphi
procedure AssignContent(Source: IBoldValue);
```

### AssignContentValue

```delphi
procedure AssignContentValue(Source: IBoldValue); virtual; abstract;
```

### GetBoldPersistenceState

```delphi
function GetBoldPersistenceState: TBoldValuePersistenceState;
```

### GetContentName

```delphi
function GetContentName: String;
```

### GetStreamName

```delphi
function GetStreamName: String;
```

### SetBoldPersistenceState

```delphi
procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
