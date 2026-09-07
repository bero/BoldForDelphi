# TBoldDomainElement_Proxy

**Unit**: [BoldDomainElement](index.md)

## Declaration

```delphi
TBoldDomainElement_Proxy = class(TBoldRefCountedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldRefCountedObject`
5. TBoldDomainElement_Proxy
6. **Direct subclasses**
7. [TBoldMember_Proxy](../BoldSystem/TBoldMember_Proxy.md)

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Mode](#mode) |  | read-only |
| [ProxedElement](#proxedelement) |  | read-only |

### Mode

```delphi
property Mode: TBoldDomainElementProxyMode;
```

### ProxedElement

```delphi
property ProxedElement: TBoldDomainElement;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) |  |  |
| [UnsupportedMode](#unsupportedmode) |  | protected |

### Create

```delphi
constructor Create(ProxedElement: TBoldDomainElement; Mode: TBoldDomainElementProxyMode);
```

### UnsupportedMode

```delphi
procedure UnsupportedMode(Mode: TBoldDomainElementProxyMode; Func: string);
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
