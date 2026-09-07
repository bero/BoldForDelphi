# TBoldSystemLocatorList

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldSystemLocatorList = class(TBoldUnOrderedIndexableList)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. `TBoldUnOrderedIndexableList`
6. TBoldSystemLocatorList

## Description

This list is used by the system to keep track of all locators/objects in memory. If you want to traverse the objects or the locators of a system you must get hold of a traverser using the [CreateTraverser](TBoldSystemLocatorList.md#createtraverser)-method, see the example.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [LocatorByID](#locatorbyid) |  | read-only |
| [ObjectByID](#objectbyid) |  | read-only |

### LocatorByID

```delphi
property LocatorByID[ObjectID:TBoldObjectId]: TBoldObjectLocator;
```

If the locator with this ObjectId is in the list, then it will be returned, otherwise nil.

### ObjectByID

```delphi
property ObjectByID[ObjectID:TBoldObjectId]: TBoldObject;
```

Finds a BoldObject by hashing on its id.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) |  |  |
| [CreateTraverser](#createtraverser) |  |  |
| [TraverserClass](#traverserclass) |  | protected, override |
| [UpdateID](#updateid) |  |  |

### Create

```delphi
constructor Create;
```

You should never have to create a SystemLocatorList yourself. Instead, use a [TBoldObjectList](TBoldObjectList.md).

### CreateTraverser

```delphi
function CreateTraverser: TBoldLocatorListTraverser;
```

This function returns a traverser that you can use to loop over all the objects or locators in a system. Remember to free the traverser when you are done.

### TraverserClass

```delphi
function TraverserClass: TBoldIndexableListTraverserClass; override; See also Ancestor Method
```

### UpdateID

```delphi
procedure UpdateID(Locator: TBoldObjectLocator; NewObjectID: TBoldObjectId; AllowInternal: Boolean = false);
```

There is normally no need to call this method. it is called by the system during certain database operations.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
