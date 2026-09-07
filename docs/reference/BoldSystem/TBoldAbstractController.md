# TBoldAbstractController

Bold-internal class that implements the behaviour of single- and multilinks

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractController = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldAbstractController
4. **Direct subclasses**
5. [TBoldAbstractObjectReferenceController](TBoldAbstractObjectReferenceController.md)
6. [TBoldListController](TBoldListController.md)

## Description

This is a Bold-internal class who's subclasses implement the behaviour of single- and multilinks.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [OwningMember](#owningmember) | The member the controller is controlling | read-only |
| [StreamName](#streamname) |  | read-only |

### OwningMember

```delphi
property OwningMember: TBoldMember;
```

The member the controller is controlling

### StreamName

```delphi
property StreamName: String;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AssertedLocatorForID](#assertedlocatorforid) |  | protected |
| [AssertIntegrity](#assertintegrity) | Assert integrity of links. | virtual |
| [Changed](#changed) | Call Changed on owning member. | protected |
| [DbFetchClassForMember](#dbfetchclassformember) |  | protected |
| [DbFetchOwningMember](#dbfetchowningmember) | Call DbFetch on owning member. | protected |
| [EndModify](#endmodify) | Call EndModify on owning member. | protected |
| [GetControllerForMember](#getcontrollerformember) | Get controller for member. | protected |
| [GetOwningMember](#getowningmember) | Getter for property OwningMember | protected, abstract |
| [GetStreamName](#getstreamname) | Get stream name | protected, abstract |
| [LinkTo](#linkto) | Add Locator from referred locators in owning member. | virtual |
| [LocatorForID](#locatorforid) | Get ensured locator for id. | protected |
| [NewValueInOptimisticLocking](#newvalueinoptimisticlocking) | Get new value for member in optimistic locking area | protected |
| [PreChange](#prechange) | Call PreChange on owning member. | protected |
| [ProxyInterface](#proxyinterface) | Bold-internal equivalent of QueryInterface | virtual |
| [StartModify](#startmodify) | Call StartModify on owning member. | protected |
| [Unlink](#unlink) | Remove Locator from referred locators in owning member. | virtual |

### AssertedLocatorForID

```delphi
function AssertedLocatorForID(ObjectId: TBoldObjectId): TBoldObjectLocator;
```

### AssertIntegrity

```delphi
function AssertIntegrity: Boolean; virtual;
```

It the case where [OwningMember](TBoldAbstractController.md#owningmember) is an AssociationEnd, this method will assert it's integrity, i.e. check that the links are bidirectional, and that the link objects are correct. The method will return `true` if everything is OK, otherwise it will raise an exception. It only returns a value in order to be useable in an `Assert()`.

### Changed

```delphi
procedure Changed(Event: TBoldEvent; const Args: array of const);
```

This method just calls [OwningMember](TBoldAbstractController.md#owningmember).[Changed](TBoldMember.md#changed). It allows the subclasses to call the method, without making it public on `TBoldMember`.

### DbFetchClassForMember

```delphi
procedure DbFetchClassForMember(Timestamp: TBoldTimestampType);
```

### DbFetchOwningMember

```delphi
procedure DbFetchOwningMember;
```

This method just calls [OwningMember](TBoldAbstractController.md#owningmember).DbFetch. It allows the subclasses to call the method, without making it public on `TBoldMember`.

### EndModify

```delphi
procedure EndModify;
```

This method just calls [OwningMember](TBoldAbstractController.md#owningmember).DbFetch. It allows the subclasses to call the method, without making it public on `TBoldMember`.

### GetControllerForMember

```delphi
class function GetControllerForMember(Member: TBoldMember): TBoldAbstractController;
```

This method gets the `Controller` for member.

### GetOwningMember

```delphi
function GetOwningMember: TBoldMember; virtual; abstract;
```

This is the Get-method for [OwningMember](TBoldAbstractController.md#owningmember). It is overridden in the concrete subclasses.

### GetStreamName

```delphi
function GetStreamName: string; virtual; abstract;
```

Get stream name

### LinkTo

```delphi
procedure LinkTo(NewLocator: TBoldObjectLocator; updateOrderNo: Boolean; Mode: TBoldLinkUnlinkMode); virtual;
```

>This method will add `Locator` to the locators referred to by [OwningMember](TBoldAbstractController.md#owningmember). For a controller for a [TBoldObjectList](TBoldObjectList.md) this means adding the locator from the list, for a [TBoldObjectReference](TBoldObjectReference.md) this means setting the reference to `Locator`. The meaning of `Mode` is explained in TBoldLinkUnlinkMode.

### LocatorForID

```delphi
function LocatorForID(ObjectId: TBoldObjectId): TBoldObjectLocator;
```

Use this method to get a ensured locator for `ObjectId` in the [TBoldSystem](TBoldSystem.md) [OwningMember](TBoldAbstractController.md#owningmember) is part of.

### NewValueInOptimisticLocking

```delphi
function NewValueInOptimisticLocking: IBoldValue;
```

If the optimistic locking area doesn't already contain a value for [OwningMember](TBoldAbstractController.md#owningmember) then a value is created and returned. If a value already exists, the function returns `**nil**`

### PreChange

```delphi
procedure PreChange;
```

This method just calls [OwningMember](TBoldAbstractController.md#owningmember).[PreChange](TBoldMember.md#prechange). It allows the subclasses to call the method, without making it public on `TBoldMember`.

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; virtual;
```

Bold-internal equivalent of `QueryInterface`

### StartModify

```delphi
function StartModify: Boolean;
```

This method just calls [OwningMember](TBoldAbstractController.md#owningmember).[StartModify](TBoldMember.md#startmodify). It allows the subclasses to call the method, without making it public on `TBoldMember`.

### Unlink

```delphi
procedure Unlink(OldLocator: TBoldObjectLocator; Mode: TBoldLinkUnlinkMode); virtual;
```

This method will remove `Locator` from the locators referred to by [OwningMember](TBoldAbstractController.md#owningmember). For a controller for a [TBoldObjectList](TBoldObjectList.md) this means removing the locator from the list, for a [TBoldObjectReference](TBoldObjectReference.md) this means setting the reference to `**nil**`. The meaning of `Mode` is explained in TBoldLinkUnlinkMode.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
