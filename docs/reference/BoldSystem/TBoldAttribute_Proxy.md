# TBoldAttribute_Proxy

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAttribute_Proxy = class(TBoldMember_Proxy, IBoldNullableValue)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldRefCountedObject`
5. [TBoldDomainElement_Proxy](../BoldDomainElement/TBoldDomainElement_Proxy.md)
6. [TBoldMember_Proxy](TBoldMember_Proxy.md)
7. TBoldAttribute_Proxy

## Properties

| Name | Summary | Notes |
|---|---|---|
| [ProxedAttribute](#proxedattribute) |  | protected, read-only |

### ProxedAttribute

```delphi
property ProxedAttribute: TBoldAttribute;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [GetContentIsNull](#getcontentisnull) |  | protected |
| [SetContentToNull](#setcontenttonull) |  | protected |

### AssignContentValue

```delphi
procedure AssignContentValue(Source: IBoldValue); override; See also Ancestor Method
```

### GetContentIsNull

```delphi
function GetContentIsNull: Boolean;
```

### SetContentToNull

```delphi
procedure SetContentToNull;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
