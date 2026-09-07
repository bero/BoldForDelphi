# TBoldDomainElementCollection

**Unit**: [BoldDomainElement](index.md)

## Declaration

```delphi
TBoldDomainElementCollection = class(TBoldUnorderedIndexableList)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. `TBoldUnOrderedIndexableList`
6. TBoldDomainElementCollection

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Add](#add) |  |  |
| [create](#create) |  |  |
| [CreateTraverser](#createtraverser) |  |  |
| [Includes](#includes) |  |  |
| [TraverserClass](#traverserclass) |  | protected, override |

### Add

```delphi
procedure Add(item: TBoldDomainElement);
```

### create

```delphi
constructor create;
```

### CreateTraverser

```delphi
function CreateTraverser: TBoldDomainElementCollectionTraverser;
```

### Includes

```delphi
function Includes(item: TBoldDomainElement): Boolean;
```

### TraverserClass

```delphi
function TraverserClass: TBoldIndexableListTraverserClass; override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
