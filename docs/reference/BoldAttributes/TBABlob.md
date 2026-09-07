# TBABlob

Attribute type for Binary Large Objects (BLOB)

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBABlob = class(TBoldAttribute)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. [TBoldMember](../BoldSystem/TBoldMember.md)
8. [TBoldAttribute](../BoldSystem/TBoldAttribute.md)
9. TBABlob
10. **Direct subclasses**
11. [TBABlobImageBMP](TBABlobImageBMP.md)
12. [TBABlobImageJPEG](TBABlobImageJPEG.md)
13. [TBATypedBlob](TBATypedBlob.md)

## Description

This attribute type can be used to store binary data of any kind (images, formatted text, sound...). Normally they will be stored as binary data in a persistent storage.

The data value of the blob can be retrieved using either the [AsString](../BoldElements/TBoldElement.md#asstring)-property, or using the method [CreateBlobStream](TBABlob.md#createblobstream).

## Properties

| Name | Summary | Notes |
|---|---|---|
| [ContentType](#contenttype) |  |  |

### ContentType

```delphi
property ContentType: string;
```

This property will always be empty of the object is a `TBABlob`, but subclasses can use this to describe the type of the blob data.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) |  | override |
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [AssignValue](#assignvalue) |  | override |
| [CanSetValue](#cansetvalue) |  |  |
| [CompareToAs](#comparetoas) |  | override |
| [CreateBlobStream](#createblobstream) |  |  |
| [FreeContent](#freecontent) | Releases the content from the attribute. | protected, override |
| [GetAsBlob](#getasblob) |  | protected |
| [GetAsVariant](#getasvariant) |  | override |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [IsEqualAs](#isequalas) | Compares attribute content. | override |
| [MaySetValue](#maysetvalue) | Override to affect the result of CanSetValue . | protected, virtual |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetAsBlob](#setasblob) |  | protected |
| [SetAsVariant](#setasvariant) |  | override |
| [SetEmptyValue](#setemptyvalue) | SetEmptyValue will set the content to empty string. | override |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |
| [SetToNull](#settonull) |  | override |

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

Copies data from another instance of `TBABlob`. The source is a blob with a `ContentType` but the target is a `TBABlob` (and not a subclass), the blobdata will be copied, but content type will be lost (no exception will occur).

### AssignContentValue

```delphi
procedure AssignContentValue(Source: IBoldValue); override; See also Ancestor Method
```

### AssignValue

```delphi
procedure AssignValue(Source: IBoldValue); override; See also Ancestor Method
```

### CanSetValue

```delphi
function CanSetValue(NewValue: string; Subscriber: TBoldSubscriber): Boolean;
```

Use this function to test if it is allowed to set the attribute to a specific value.

If a subscriber is sent to the function, this subscriber will be notified if the conditions have changed (so that an illegal value is now legal). It is OK to send a nilpointer instead.

### CompareToAs

```delphi
function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

### CreateBlobStream

```delphi
function CreateBlobStream(Mode: TBoldBlobStreamMode): TBoldBlobStream;
```

This will create a subclass of `TStream` that will read and write from the blob attribute. The mode-parameter indicates if the stream is readable, writeable or both.

### FreeContent

```delphi
procedure FreeContent; override; See also Ancestor Method
```

`FreeContent` resets the content of the blob attribute.

### GetAsBlob

```delphi
function GetAsBlob: String;
```

### GetAsVariant

```delphi
function GetAsVariant: Variant; override; See also Ancestor Method
```

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### IsEqualAs

```delphi
function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override; See also Ancestor Method
```

`IsEqualAs` will compare the data content of the attribute with the content in the BoldElement submitted as parameter.

If the parameter is not a `TBABlob` the inherited method will be invoked.

Comparison will yield `true` if both the Blob and the submitted blob are null or if their data content is equal.

### MaySetValue

```delphi
function MaySetValue(NewValue: String; Subscriber: TBoldSubscriber): Boolean; virtual;
```

A subclass of `TBABlob` may override this method to prohibit setting the attribute to a specific value. If the result value is `false`, the setting will be aborted. If it is `true`, there may still be other reasons why the changing of the value is not allowed.

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

### SetAsBlob

```delphi
procedure SetAsBlob(NewValue: String);
```

### SetAsVariant

```delphi
procedure SetAsVariant(const Value: Variant); override; See also Ancestor Method
```

### SetEmptyValue

```delphi
procedure SetEmptyValue; override; See also Ancestor Method
```

`SetEmptyValue` will set the `AsString` property of the `TBABlob` to en empty string.

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

### SetToNull

```delphi
procedure SetToNull; override; See also Ancestor Method
```

Set the value of the attribute to Null. If Null is not an allowed value for the attribute, an exception will be raised. As an optimization, the data part of the attribute will be disposed.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
