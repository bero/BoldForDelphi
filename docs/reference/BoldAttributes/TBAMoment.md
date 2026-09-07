# TBAMoment

Abstract superclass for date and time attributes

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAMoment = class(TBoldAttribute)
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
9. TBAMoment
10. **Direct subclasses**
11. [TBADate](TBADate.md)
12. [TBADateTime](TBADateTime.md)
13. [TBATime](TBATime.md)

## Description

This is an abstract superclass of the three different classes that uses `TDateTime` internally to store their data ([TBADateTime](TBADateTime.md), [TBATime](TBATime.md), [TBADate](TBADate.md)). It has no public data-properties.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsDate](#asdate) |  | protected |
| [AsDateTime](#asdatetime) |  | protected |
| [AsTime](#astime) |  | protected |
| [Days](#days) | The day part of the date | protected, read-only |
| [Hours](#hours) | The hour part of the time | protected, read-only |
| [Minutes](#minutes) | The minutes part of the time | protected, read-only |
| [Months](#months) | The month part of the date | protected, read-only |
| [Seconds](#seconds) | The seconds part of the time | protected, read-only |
| [Years](#years) | The year part of the date | protected, read-only |

### AsDate

```delphi
property AsDate: TDateTime;
```

### AsDateTime

```delphi
property AsDateTime: TDateTime;
```

### AsTime

```delphi
property AsTime: TDateTime;
```

### Days

```delphi
property Days: Word;
```

`Days` contains the day part of the date value. For a date 1970-03-17, `Days` will return 17.

**See Also**

- [Years](TBAMoment.md#years)
- [Months](TBAMoment.md#months)

### Hours

```delphi
property Hours: Word;
```

`Hours` contains the hours part of the time value. For a time 10:15:16, `Hours` will return 10.

**See Also**

- [Minutes](TBAMoment.md#minutes)
- [Seconds](TBAMoment.md#seconds)

### Minutes

```delphi
property Minutes: Word;
```

`Minutes` contains the minutes part of the time value. For a time 10:15:16, `Minutes` will return 15.

**See Also**

- [Seconds](TBAMoment.md#seconds)
- [Hours](TBAMoment.md#hours)

### Months

```delphi
property Months: Word;
```

`Months` contains the month part of the date value. For a date 1970-03-17, `Months` will return 3.

**See Also**

- [Days](TBAMoment.md#days)
- [Months](TBAMoment.md#months)

### Seconds

```delphi
property Seconds: Word;
```

`Seconds` contains the seconds part of the time value. For a time 10:15:16, `Seconds` will return 16.

**See Also**

- [Minutes](TBAMoment.md#minutes)
- [Hours](TBAMoment.md#hours)

### Years

```delphi
property Years: Word;
```

`Years` contains the year part of the date value. For a date 1970-03-17, `Years` will return 1970.

**See Also**

- [Days](TBAMoment.md#days)
- [Months](TBAMoment.md#months)

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) |  | override |
| [CanSetValue](#cansetvalue) |  |  |
| [CompareToAs](#comparetoas) |  | override |
| [GetAsDate](#getasdate) |  | protected |
| [GetAsDateTime](#getasdatetime) |  | protected |
| [GetAsTime](#getastime) |  | protected |
| [GetAsVariant](#getasvariant) |  | override |
| [MaySetValue](#maysetvalue) | Override to affect the result of CanSetValue . | protected, virtual |
| [SetAsDate](#setasdate) |  | protected |
| [SetAsDateTime](#setasdatetime) |  | protected |
| [SetAsTime](#setastime) |  | protected |
| [SetAsVariant](#setasvariant) |  | override |
| [SetEmptyValue](#setemptyvalue) |  | override |

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

This copies the data from another `TBAMoment`.

### CanSetValue

```delphi
function CanSetValue(NewValue: TDateTime; Subscriber: TBoldSubscriber): Boolean;
```

Use this function to test if it is allowed to set the attribute to a specific value.

If a subscriber is sent to the function, this subscriber will be notified if the conditions have changed (so that an illegal value is now legal). It is OK to send a nilpointer instead.

### CompareToAs

```delphi
function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

This will compare the internal value to the value of the `BoldElement` using the `CompType` as follows:

- ctDate: Compares dates only
- ctTime: Compares times only
- cdDefault: Compares dates and times

The resultcodes used are:

- `self.[AsDate|AsTime|AsDateTime] < TBAMoment(BoldElement).[AsDate|AsTime|AsDateTime]` returns -1
- `self.[AsDate|AsTime|AsDateTime] = TBAMoment(BoldElement).[AsDate|AsTime|AsDateTime]` returns 0
- `self.[AsDate|AsTime|AsDateTime] > TBAMoment(BoldElement).[AsDate|AsTime|AsDateTime]` returns 1

### GetAsDate

```delphi
function GetAsDate: TDateTime;
```

### GetAsDateTime

```delphi
function GetAsDateTime: TDateTime;
```

### GetAsTime

```delphi
function GetAsTime: TDateTime;
```

### GetAsVariant

```delphi
function GetAsVariant: Variant; override; See also Ancestor Method
```

### MaySetValue

```delphi
function MaySetValue(NewValue: TDateTime; Subscriber: TBoldSubscriber): Boolean; virtual;
```

A subclass of `TBAMoment` may override this method to prohibit setting the attribute to a specific value. If the result value is `false`, the setting will be aborted. If it is `true`, there may still be other reasons why the changing of the value is not allowed.

### SetAsDate

```delphi
procedure SetAsDate(Value: TDateTime);
```

### SetAsDateTime

```delphi
procedure SetAsDateTime(Value: TDateTime);
```

### SetAsTime

```delphi
procedure SetAsTime(Value: TDateTime);
```

### SetAsVariant

```delphi
procedure SetAsVariant(const Value: Variant); override; See also Ancestor Method
```

### SetEmptyValue

```delphi
procedure SetEmptyValue; override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
