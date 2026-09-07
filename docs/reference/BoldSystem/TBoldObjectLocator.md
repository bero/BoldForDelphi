# TBoldObjectLocator

The purpose of a TBoldObjectLocator is to provide a way to refer to objects without having them loaded in memory.

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldObjectLocator = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldObjectLocator

## Description

Its two main features are the [BoldObject](TBoldObjectLocator.md#boldobject) and [BoldObjectID](TBoldObjectLocator.md#boldobjectid) properties. Locators are always owned by a [Locators](TBoldSystem.md#locators)

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsString](#asstring) | The textual representation of the locator. | read-only |
| [BoldObject](#boldobject) | The Bold object that the locator is a locator for. | read-only |
| [BoldObjectID](#boldobjectid) | Id of object | read-only |
| [BoldSystem](#boldsystem) | TBoldSystem that the locator belongs to | read-only |
| [EmbeddedSingleLinks](#embeddedsinglelinks) | Get/Set embedded single links when object is not loaded |  |
| [EnsuredBoldObject](#ensuredboldobject) | The Bold object that the locator is a locator for. | read-only |
| [ObjectIsPersistent](#objectispersistent) | If the object is persistent | read-only |

### AsString

```delphi
property AsString: string;
```

The textual representation of the locator is the same as the textual representation of its object id.

### BoldObject

```delphi
property BoldObject: TBoldObject;
```

This property contains the Bold object that the locator is a locator for. If the Bold object is not loaded into memory, the BoldObject property is nil.

### BoldObjectID

```delphi
property BoldObjectID: TBoldObjectId;
```

This is the ID of the object which the locator refers to. The BoldObjectId is owned by the locator.

### BoldSystem

```delphi
property BoldSystem: TBoldSystem;
```

Each locator belongs to a TBoldSystem. All locators bolonging to a system can be found in the Locators property of the system.

### EmbeddedSingleLinks

```delphi
property EmbeddedSingleLinks[EmbeddedIndex:integer]: TBoldObjectLocator;
```

The EmbeddedSingleLinks property makes it possible to store the values of all embedded single links in the Locator when the object is not loaded. Assigning to this property will automatically create storage for the embedded singllinks. It is freed by EmbeddedSingleLinksToObject

### EnsuredBoldObject

```delphi
property EnsuredBoldObject: TBoldObject;
```

This is the same as the [BoldObject](TBoldObjectLocator.md#boldobject) property if the Bold object is loaded in memory. If it is not loaded, the EnsuredBoldObject property loads the Bold object and returns it, whereas the BoldObject property returns nil.

### ObjectIsPersistent

```delphi
property ObjectIsPersistent: Boolean;
```

If the object is persistent

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AtTime](#attime) |  |  |
| [Destroy](#destroy) | Destroy object locator | override |
| [DiscardBoldObject](#discardboldobject) | Discard BoldObject if it is in memory. |  |
| [EnsureBoldObject](#ensureboldobject) | Ensure that BoldObject is assigned |  |
| [Hash](#hash) | The locator has a hash value, allowing it to be efficiently indexed in a list. |  |
| [UnloadBoldObject](#unloadboldobject) | Unloads the locator's Bold object from memory. |  |

### AtTime

```delphi
function AtTime(Time: TBoldTimeStampType): TBoldObjectLocator;
```

Returns the locator as it looked at the time point identified by the Time time stamp.

| **Note** |
|---|

| This feature is only available in the Object Versioning Extension to Bold for Delphi. |
|---|

### Destroy

```delphi
destructor Destroy; override;
```

Destroys locator, freeing BoldObjectId.

| **Note** |
|---|

| This destructor should not be called directly, since `BoldSystem.Locators` owns it's objects. Call `BoldSystem.Locators.Remove(locator)` instead. |
|---|

### DiscardBoldObject

```delphi
procedure DiscardBoldObject;
```

Discard BoldObject if it is in memory.

### EnsureBoldObject

```delphi
procedure EnsureBoldObject;
```

The method will fetch the object corresponding to BoldObjectId into memory if is is not already there.

### Hash

```delphi
function Hash: Cardinal;
```

The locator has a hash value, allowing it to be efficiently indexed in a list.

### UnloadBoldObject

```delphi
procedure UnloadBoldObject;
```

Unloads the locator's Bold object from memory. UnloadBoldObject will raise an exception if the Bold object is dirty.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
