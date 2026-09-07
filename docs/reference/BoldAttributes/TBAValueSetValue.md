# TBAValueSetValue

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAValueSetValue = class(TBoldElement)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. TBAValueSetValue
7. **Direct subclasses**
8. `TBAMLValueSetValue`

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsInteger](#asinteger) |  |  |
| [StringRepresentationCount](#stringrepresentationcount) |  | read-only |

### AsInteger

```delphi
property AsInteger: Integer;
```

### StringRepresentationCount

```delphi
property StringRepresentationCount: Integer;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AddString](#addstring) |  | protected |
| [DefaultSubscribe](#defaultsubscribe) |  | override |
| [Destroy](#destroy) |  | override |
| [GetAsInteger](#getasinteger) |  | protected |
| [GetAsList](#getaslist) |  | override |
| [GetBoldType](#getboldtype) |  | protected, override |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [GetStringRepresentationCount](#getstringrepresentationcount) |  | protected, virtual |
| [InternalCreate](#internalcreate) |  |  |
| [SetAsInteger](#setasinteger) |  | protected |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |

### AddString

```delphi
procedure AddString(Value: string);
```

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

### GetAsInteger

```delphi
function GetAsInteger: Integer;
```

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); override; See also Ancestor Method
```

### GetBoldType

```delphi
function GetBoldType: TBoldElementTypeInfo; override; See also Ancestor Method
```

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### GetStringRepresentationCount

```delphi
function GetStringRepresentationCount: Integer; virtual;
```

### InternalCreate

```delphi
constructor InternalCreate(StringValues: Integer; List: TBAValueSetValueList);
```

### SetAsInteger

```delphi
procedure SetAsInteger(Value: Integer);
```

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
