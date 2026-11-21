# TBABlob

`TBABlob` (unit `BoldAttributes`) stores binary data through a `TBoldBlobStream`. The model type Blob maps to it. Use it for files, images, documents and other binary content.

## Class Hierarchy

```mermaid
classDiagram
    class TBoldAttribute {
        <<abstract>>
    }
    TBoldAttribute <|-- TBABlob
    TBABlob <|-- TBATypedBlob
    TBABlob <|-- TBABlobImageJPEG
    TBABlob <|-- TBABlobImageBMP

    class TBABlob {
        +AsStream TBoldBlobStream
        +BlobSize Int64
        +ContentType string
        +LoadFromFile()
        +SaveToFile()
    }

    click TBoldAttribute href "../TBoldAttribute/" "TBoldAttribute documentation"
```

## Class Definition

```pascal
TBABlob = class(TBoldAttribute)
public
  destructor Destroy; override;
  procedure SetToNull; override;
  procedure Assign(Source: TBoldElement); override;
  procedure SetEmptyValue; override;
  procedure LoadFromStream(Stream: TStream);
  procedure LoadFromFile(const aFileName: string; aMode: Word = fmShareDenyNone);
  procedure SaveToStream(Stream: TStream);
  procedure SaveToFile(const FileName: string);
  function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
  property AsStream: TBoldBlobStream read GetAsStream;
  property BlobSize: Int64 read GetBlobSize;
  property ContentType: string index brShort read GetStringRepresentation write SetStringRepresentation;
end;

TBATypedBlob = class(TBABlob)
public
  // Stores ContentType with the data
  function CanSetContentType(Value: string; Subscriber: TBoldSubscriber): Boolean;
end;
```

## TBoldBlobStream

The blob's data is accessed through `TBoldBlobStream`, which extends `TStream`:

```pascal
TBoldBlobStream = class(TStream)
public
  procedure Clear;
  procedure Truncate;
  procedure LoadFromStream(Stream: TStream);
  procedure LoadFromFile(const aFileName: string; aMode: Word = fmShareDenyNone);
  procedure SaveToStream(Stream: TStream);
  procedure SaveToFile(const FileName: string);
  function IsDataSame(AData: Pointer; ASize: Int64): Boolean;
end;
```

## Working with TBABlob

### Loading and Saving Files

```pascal
// Load a file into a blob attribute
Document.M_Attachment.LoadFromFile('annual.pdf');

// Save blob to file
Document.M_Attachment.SaveToFile('export.pdf');
```

### Working with Streams

```pascal
// Load from stream
var MemStream: TMemoryStream;
MemStream := TMemoryStream.Create;
try
  MemStream.LoadFromFile('image.jpg');
  Product.M_Photo.LoadFromStream(MemStream);
finally
  MemStream.Free;
end;

// Read via stream
Product.M_Photo.AsStream.SaveToFile('output.jpg');
```

### Checking Size and Content

```pascal
// Check if blob has data
if Product.M_Photo.IsNull then
  ShowMessage('No photo');

// Check size
ShowMessage(Format('Size: %d bytes', [Product.M_Photo.BlobSize]));
```

### Typed Blobs

`ContentType` is declared on `TBABlob`, but only `TBATypedBlob` (model type TypedBlob) stores it. A plain `TBABlob` returns an empty content type and ignores an assignment. The image blobs have a fixed content type and raise when another one is assigned.

```pascal
// Attachment is a TypedBlob attribute
Document.M_Attachment.ContentType := 'application/pdf';
```

## Blob Subtypes

| Type | Model Type | Purpose | Content Type |
|------|------------|---------|--------------|
| `TBABlob` | Blob | Generic binary data | Empty |
| `TBATypedBlob` | TypedBlob | Binary with MIME type | Stored with the data |
| `TBABlobImageJPEG` | BlobImageJPEG | JPEG images | `image/jpeg` |
| `TBABlobImageBMP` | BlobImageBMP | BMP images | `image/bitmap` |

## See Also

- [TBABlob reference](../reference/BoldAttributes/TBABlob.md) - Generated API reference
- [TBoldAttribute](TBoldAttribute.md) - Base class
- [TBAString](TBAString.md) - For text data, use string attributes
