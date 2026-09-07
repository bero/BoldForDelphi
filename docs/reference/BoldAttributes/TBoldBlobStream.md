# TBoldBlobStream

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBoldBlobStream = class(TStream)
```

## Hierarchy

1. TStream
2. TBoldBlobStream

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Clear](#clear) |  |  |
| [Create](#create) |  |  |
| [LoadFromFile](#loadfromfile) |  |  |
| [LoadFromStream](#loadfromstream) |  |  |
| [Read](#read) |  | override |
| [SaveToFile](#savetofile) |  |  |
| [SaveToStream](#savetostream) |  |  |
| [Seek](#seek) |  | override |
| [SetSize](#setsize) |  | override |
| [Truncate](#truncate) |  |  |
| [Write](#write) |  | override |

### Clear

```delphi
procedure Clear;
```

### Create

```delphi
constructor Create(BlobAttr: TBABlob; Mode: TBoldBlobStreamMode);
```

### LoadFromFile

```delphi
procedure LoadFromFile(const FileName: string);
```

### LoadFromStream

```delphi
procedure LoadFromStream(Stream: TStream);
```

### Read

```delphi
function Read(var Buffer; Count: integer): integer; override;
```

### SaveToFile

```delphi
procedure SaveToFile(const FileName: string);
```

### SaveToStream

```delphi
procedure SaveToStream(Stream: TStream);
```

### Seek

```delphi
function Seek(Offset: integer; Origin: Word): integer; override;
```

### SetSize

```delphi
procedure SetSize(NewSize: integer); override;
```

### Truncate

```delphi
procedure Truncate;
```

### Write

```delphi
function Write(const Buffer; Count: integer): integer; override;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
