# TBoldAbstractPessimisticLockHandler

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractPessimisticLockHandler = class(TBoldSystemExtension)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. [TBoldSystemExtension](TBoldSystemExtension.md)
6. TBoldAbstractPessimisticLockHandler
7. **Direct subclasses**
8. `TBoldPessimisticLockHandler`

## Description

The pessimistic lock handler defines methods for requesting and releasing locks for domain elements.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [EnsureLocks](#ensurelocks) |  | abstract |
| [LockElement](#lockelement) |  | abstract |
| [ReleaseUnneededRegions](#releaseunneededregions) |  | abstract |

### EnsureLocks

```delphi
function EnsureLocks: Boolean; virtual; abstract;
```

This method is called automatically before update to check with the Lock Manager server that the client still holds the locks it should. There is normally no need to call it directly.

**See Also**

- Related Topics

### LockElement

```delphi
function LockElement(Element: TBoldDomainElement): Boolean; virtual; abstract;
```

Requests the required locks for the element. If the call returns `true`, the element is then allowed to be modified. If the call returns `false`, the required locks could not be obtained.

**See Also**

- Related Topics

### ReleaseUnneededRegions

```delphi
procedure ReleaseUnneededRegions; virtual; abstract;
```

Releases the locks that the client holds, that it does not need. Locks for elements that are dirty, i.e. where the client holds a modified state, are kept. This method also releases the so-called exclusive database lock.

**See Also**

- Related Topics

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
