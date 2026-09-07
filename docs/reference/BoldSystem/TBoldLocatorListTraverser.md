# TBoldLocatorListTraverser

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldLocatorListTraverser = class(TBoldIndexableListTraverser)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldIndexableListTraverser`
4. TBoldLocatorListTraverser

## Description

Objects of this class can be used to traverse the locatorlist of a `TBoldSystem`.

Get hold of a traverser by calling `aBoldSystem.Locators.CreateTraverser`. This object must be freed when you are done with it. You can have multiple traversers traversing the same list at the same time.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [locator](#locator) |  | read-only |

### locator

```delphi
property locator: TBoldObjectLocator;
```

While traversing a `LocatorList` the `Locator`-property will point to the current locator. Call the `Next`-operation to move on in the list.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
