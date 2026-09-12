# Bold Image

A Bold `TypedBlob` attribute carries two things: the bytes, and a MIME content
type describing them. `TBoldImage` uses the second to decide how to decode the
first, picking a registered view adapter at display time rather than at design
time.

This demo puts every part of that mechanism on one form: the image itself, the
content type as an editable string, load and save, and clipboard transfer.

## Running it

There is no `.dproj` here, only the legacy `BE_BoldImage.dpr`. Open it in Delphi
and let the IDE create a project file. The form uses the IBX persistence chain
(`TBoldDatabaseAdapterIB`, `TBoldIBDatabaseAction`, `TIBDatabase`), whose units
live under `Source\Deprecated\Persistance\IBX` and are in no current Bold
package, so they have to be put on the unit search path by hand.

`IBDatabase1` ships with `user_name=sysdba` and `password=masterkey` but **no
`DatabaseName`**, so fill that in before running. Then press **Create DB**
(`BoldIBDatabaseAction1`) and **Open system** (`BoldActivateSystemAction1`), and
add an image with the navigator.

## The model

`BoldModel1` carries the whole model inline, generated into
`ImageDemoClasses.pas`. One class matters:

| Class | Attribute | Type |
| --- | --- | --- |
| `ImageClass` | `Image` | `TypedBlob`, AllowNULL |
| | `Description` | `String` |

## The control pack

Everything on the form follows one list handle, `blhImages`, a
`TBoldListHandle` rooted on `BoldSystemHandle1` with

```
Expression = 'ImageClass.allInstances'
```

| Control | Class | BoldHandle | Expression |
| --- | --- | --- | --- |
| `BoldImage` | `TBoldImage` | `blhImages` | `image` |
| `btxtImageAsString` | `TBoldEdit` | `blhImages` | `image` |
| `bmemDescription` | `TBoldMemo` | `blhImages` | `description` |
| `BoldNavigator1` | `TBoldNavigator` | `blhImages` | |

The interesting one is `btxtImageAsString`, which points at the same `image`
attribute as the picture but adds `BoldProperties.Representation = 2`. That is
`brShort`, and `TBATypedBlob.GetStringRepresentation` answers `brShort` with the
content type. So the edit box shows `image/jpeg`, not the bytes, despite its
name.

## Content type picks the adapter

`TBoldImage` has no idea what a JPEG is. It asks each registered adapter whether
it can read the blob's content type:

| Adapter | Unit | Accepts |
| --- | --- | --- |
| `TBoldViewBitmapAdapter` | `BoldImageBitmap` | empty string, `image/bitmap`, `image/bmp` |
| `TBoldViewJPEGAdapter` | `BoldImageJPEG` | `image/jpeg` |

Both register themselves in their unit's initialization section, and `fMain`
imports both, which is the whole registration story.

`ContentTypeOnPaste` is the same question in reverse. It is `image/jpeg` in the
DFM, and the **ContentTypeOnPaste** edit box writes straight to the property, so
you can switch it to `image/bitmap` and paste the same clipboard content into a
different adapter. **View Clipboard Formats** opens `fViewClipboardFmt`, plain
VCL code that lists the format ids Windows currently offers.

## The other two forms

`fViewAutoSize` (`TfrmImageViewer`), opened by **View**, has no list handle. Its
`TBoldImage` follows `behImage`, a bare `TBoldReferenceHandle` that `fMain`
fills from code:

```pascal
behimage.Value := blhImages.CurrentBoldObject.BoldMemberByExpressionName['image'];
```

A control can therefore follow a member directly, with no expression at all. The
toolbar then exercises `StretchMode`: **S** sets `bsmStretchToScale` with
`AutoSize` on, **A** sets `bsmStretchProportional` with `Align = alClient`, and
the combo drives `Scale`.

`fViewStretch` is compiled into the program but never created, and its
`TBoldImage` has no `BoldHandle`. It is dead weight.
