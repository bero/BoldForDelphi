# TBAValueSetValueList

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAValueSetValueList = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBAValueSetValueList
4. **Direct subclasses**
5. `TBADerivedValueSetValueList`

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Count](#count) |  | read-only |
| [ValueSetValues](#valuesetvalues) |  | read-only |

### Count

```delphi
property Count: integer;
```

### ValueSetValues

```delphi
property ValueSetValues[index:integer]: TBAValueSetValue;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Add](#add) |  |  |
| [AddValue](#addvalue) |  |  |
| [Create](#create) |  |  |
| [Destroy](#destroy) |  | override |
| [FindByInteger](#findbyinteger) |  |  |
| [FindByString](#findbystring) |  |  |
| [FindByText](#findbytext) |  |  |
| [GetFirstValue](#getfirstvalue) |  |  |
| [ToStrings](#tostrings) |  |  |
| [ToStringsWithNil](#tostringswithnil) |  |  |

### Add

```delphi
procedure Add(intvalue: Integer; arrString: array of string);
```

### AddValue

```delphi
procedure AddValue(Value: TBAValueSetValue);
```

### Create

```delphi
constructor Create;
```

### Destroy

```delphi
destructor Destroy; override;
```

### FindByInteger

```delphi
function FindByInteger(Value: Integer): TBAValueSetValue;
```

### FindByString

```delphi
function FindByString(Representation: TBoldRepresentation; Value: string): TBAValueSetValue;
```

### FindByText

```delphi
function FindByText(Representation: TBoldRepresentation; Value: string): TBAValueSetValue;
```

### GetFirstValue

```delphi
function GetFirstValue: TBAValueSetValue;
```

### ToStrings

```delphi
procedure ToStrings(Representation: TBoldRepresentation; theStrings: TStrings);
```

### ToStringsWithNil

```delphi
procedure ToStringsWithNil(Representation: TBoldRepresentation; theStrings: TStrings; nilstring: string);
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
