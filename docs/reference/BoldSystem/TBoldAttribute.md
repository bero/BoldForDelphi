# TBoldAttribute

Superclass to all attribute classes in Bold

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAttribute = class(TBoldMember)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. [TBoldMember](TBoldMember.md)
8. TBoldAttribute
9. **Direct subclasses**
10. [TBABlob](../BoldAttributes/TBABlob.md)
11. [TBAMoment](../BoldAttributes/TBAMoment.md)
12. [TBANumeric](../BoldAttributes/TBANumeric.md)
13. [TBAString](../BoldAttributes/TBAString.md)
14. [TBAValueSet](../BoldAttributes/TBAValueSet.md)

## Description

Attributes are used for two different purposes.

First of all they are used to be attributes of business objects (subclasses of [TBoldObject](TBoldObject.md)). In this role they can be persistent, or transient, they can also be derived (calculated). When used as the attribute of a business object, the property [BoldAttributeRTInfo](TBoldAttribute.md#boldattributertinfo) will always be assigned, and indicate the role of the attribute in the model.

`TBoldAttribute` can also be created independent of a business object. This happens frequently when OCL expressions are evaluated, and the result is a calculated value (concatenation of two strings for example). It is also allowed to call the constructor of a concrete subclass of `TBoldAttribute`. Often however when attributes are used stand-alone, it is more convenient to use the `TBoldVariableHandle`, or the [TBoldMemberFactory](TBoldMemberFactory.md) to create the attribute-object.

Every subclass of `TBoldAttribute` has its own internal data, and usually it has an access-property to read and write the contents of the attribute object in the native format. Most attribute types also implement the [AsString](../BoldElements/TBoldElement.md#asstring) property to allow reading and writing the string representation of the attribute.

To subclass `TBoldAttribute`, the easiest way is to use the Attribute-wizard found in the Bold menu in the Delphi/C++Builder IDE.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldAttributeRTInfo](#boldattributertinfo) |  | read-only |
| [ContentIsNull](#contentisnull) | Bold-internal check for NULL | protected, read-only |
| [IsNull](#isnull) | If the attribute has the special 'empty' value NULL. | read-only |

### BoldAttributeRTInfo

```delphi
property BoldAttributeRTInfo: TBoldAttributeRTInfo;
```

This property is the same as [BoldMemberRTInfo](TBoldMember.md#boldmemberrtinfo), but downcasted to `TBoldAttributeRTInfo`.

### ContentIsNull

```delphi
property ContentIsNull: Boolean;
```

Bold-internal variant of [IsNull](TBoldAttribute.md#isnull)

### IsNull

```delphi
property IsNull: Boolean;
```

Note that Null is a special value and that it in most cases is an error to access the basic representation of the attribute, e.g. `AsInteger`, if `IsNull` is `True`.

An attribute can be set to `Null` with [SetToNull](TBoldAttribute.md#settonull). An attribute is set to a non-Null value by setting the value.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) | Overridden from Assign | override |
| [AssignContentValue](#assigncontentvalue) | Assigns the internal value | protected, abstract |
| [AssignValue](#assignvalue) | Assigns the internal state | protected, abstract |
| [CanSetToNull](#cansettonull) |  |  |
| [CompareToAs](#comparetoas) | Overriddden from CompareToAs | override |
| [DefaultSubscribe](#defaultsubscribe) | Subscribes to default events on the attribute. | override |
| [DoSetInitialValue](#dosetinitialvalue) | Bold-internal | protected, override |
| [EitherIsNull](#eitherisnull) | True if IsNull is true for either of the arguments. | protected |
| [EnsureNotNull](#ensurenotnull) | Called by subclasses to guard against NULL values. | protected |
| [FormatFailure](#formatfailure) | Convenience function for subclasses for setting the failure reason. | protected |
| [GetContentIsNull](#getcontentisnull) | Get-method for ContentIsNull property | protected |
| [GetStreamName](#getstreamname) | Overrides TBoldMember.GetStreamName | protected, override |
| [IsEqualToValue](#isequaltovalue) | Overrides TBoldMember.IsEqualToValue. | override |
| [NullBiggest](#nullbiggest) | Internal convenience method for comparing NULL values. | protected |
| [NullSmallest](#nullsmallest) | Internal convenience method for comparing NULL values. | protected |
| [ProxyInterface](#proxyinterface) | Overrides TBoldDomainElement.ProxyInterface. | override |
| [RecycleValue](#recyclevalue) | Bold-internal |  |
| [SetContentToNull](#setcontenttonull) | Sets the internal data value to NULL. | protected |
| [SetEmptyValue](#setemptyvalue) | Set the attribute to an 'empty' value | virtual |
| [SetToNonNull](#settononnull) | Set the attribute to a non-null value. | protected |
| [SetToNull](#settonull) | Sets the attribute to the special value Null. | virtual |
| [SubscribeToStringRepresentation](#subscribetostringrepresentation) | Overrides TBoldElement.SubscribeToStringRepresentation | override |

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

Overridden from [Assign](../BoldElements/TBoldElement.md#assign)

### AssignContentValue

```delphi
procedure AssignContentValue(Source: IBoldValue); virtual; abstract;
```

This is an internal assigning method. It sets the internal data value of the attribute only.

### AssignValue

```delphi
procedure AssignValue(Source: IBoldValue); virtual; abstract;
```

This is an internal assigning method. It sets the internal data value of the attribute and the persistence state from the `source` value.

### CanSetToNull

```delphi
function CanSetToNull(Subscriber: TBoldSubscriber): Boolean;
```

Returns `True` if the member may be set to `Null`. A programmer can disallow reading a member by subscribing to the bqMaySetToNull query. If a `Subscriber` is passed to the function, subscriptions will be placed to notify when the result of `MaySetToNull` may have changed. The mode information for the attribute may indicate that the attribute is not allowed to be set to null.

### CompareToAs

```delphi
function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

Overriddden from [CompareToAs](../BoldElements/TBoldElement.md#comparetoas)

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

Subscribes to default events on the attribute. This will give a subscription on `beValueChanged`.

**See Also**

- Subscriptions

### DoSetInitialValue

```delphi
procedure DoSetInitialValue; override; See also Ancestor Method
```

Bold-internal

### EitherIsNull

```delphi
class function EitherIsNull(Attribute1, Attribute2: TBoldAttribute): Boolean;
```

`True` if [IsNull](TBoldAttribute.md#isnull) is `true` for either of the arguments.

### EnsureNotNull

```delphi
procedure EnsureNotNull;
```

Called by subclasses to guard against `NULL` values. If the attribute is `NULL` the exception [EBoldAccessNullValue](EBoldAccessNullValue.md) is raised.

### FormatFailure

```delphi
procedure FormatFailure(const value, ExpectedDataType: String);
```

Convenience function for subclasses for setting the failure reason. See SetBoldLastFailureReason.

### GetContentIsNull

```delphi
function GetContentIsNull: Boolean;
```

Get-method for [ContentIsNull](TBoldAttribute.md#contentisnull) property

### GetStreamName

```delphi
function GetStreamName: string; override; See also Ancestor Method
```

Overrides [GetStreamName](TBoldMember.md#getstreamname)

### IsEqualToValue

```delphi
function IsEqualToValue(Value: IBoldValue): Boolean; override; See also Ancestor Method
```

Overrides [IsEqualToValue](TBoldMember.md#isequaltovalue).

### NullBiggest

```delphi
function NullBiggest(BoldElement: TBoldElement): Integer;
```

Internal convenience method for comparing NULL values. NULL is considered bigger than any other value.

### NullSmallest

```delphi
function NullSmallest(BoldElement: TBoldElement): Integer;
```

Internal convenience method for comparing NULL values. NULL is considered smaller than any other value.

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

Overrides [ProxyInterface](../BoldDomainElement/TBoldDomainElement.md#proxyinterface).

### RecycleValue

```delphi
procedure RecycleValue;
```

Bold-internal

### SetContentToNull

```delphi
procedure SetContentToNull;
```

Sets the internal data value to NULL. Used by subclasses and persistence mechanisms. To set an attribute to NULL normally, use [SetToNull](TBoldAttribute.md#settonull).

### SetEmptyValue

```delphi
procedure SetEmptyValue; virtual;
```

All attributes (descendents of [TBoldAttribute](TBoldAttribute.md)), such as [TBAString](../BoldAttributes/TBAString.md), should implement this method and set the value of the attribute to an 'empty' value, such as an empty string or zero. This is to support the assign operation with no source argument.

### SetToNonNull

```delphi
procedure SetToNonNull;
```

This method should only be called by descendents of [TBoldAttribute](TBoldAttribute.md), such as [TBAString](../BoldAttributes/TBAString.md), in the implementation of the Set-method(s) when the attribute is set to a non-NULL value.

### SetToNull

```delphi
procedure SetToNull; virtual;
```

Sets the attribute to the special value Null. This requires that `AllowNull` for the attribute is `True` in the model. See also [CanSetToNull](TBoldAttribute.md#cansettonull).

**See Also**

- Related Topics

### SubscribeToStringRepresentation

```delphi
procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

This is equivalent to calling [DefaultSubscribe](TBoldAttribute.md#defaultsubscribe) unless overridden by a concrete subclass.

**See Also**

- Subscriptions

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
