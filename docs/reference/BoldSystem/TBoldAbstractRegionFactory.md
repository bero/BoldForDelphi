# TBoldAbstractRegionFactory

Provides the interface definition to a region factory.

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractRegionFactory = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldAbstractRegionFactory
4. **Direct subclasses**
5. `TBoldRegionFactory`

## Description

Provides the interface definition to a region factory. This class is internal to the implementation of pessimistic locking.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [System](#system) | Bold system that the region factory is connected to. | protected, read-only |

### System

```delphi
property System: TBoldSystem;
```

Bold system that the region factory is connected to.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [GetRegionsForElement](#getregionsforelement) | The regions that the element is part of. | abstract |

### GetRegionsForElement

```delphi
procedure GetRegionsForElement(Element: TBoldDomainElement; ResultList: TList); virtual; abstract;
```

The regions that the element is part of.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
