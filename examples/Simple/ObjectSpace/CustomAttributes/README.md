# Custom Attribute Types

Bold's attribute types are not a closed set. A model attribute type is a row in
a type-name dictionary naming a Delphi class, a persistence mapper, an accessor
and a native type, and you can add rows of your own. This demo adds five
attribute types and two persistence mappers, and puts one of each on a single
`TestClass`.

The shipped `Readme.txt` says the samples exist mainly to show the mechanics of
writing attribute classes and registering them. That is a fair description.

## Running it

There is no `.dproj`. Open `BE_CustomAttributes.dpr` in the IDE and let Delphi
create one. The two `.dpk` files are Delphi 6 and Delphi 7 design-time packages
for the same units; the `.dpr` compiles the units directly and does not need
them.

It needs a database: InterBase through `TBoldDatabaseAdapterIB`, `sysdba` /
`masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it first. Then
**Create DB** and **Open system**.

`MainForm.dfm` is a binary form file that the repository's
`*.dfm text eol=crlf` rule inflates on checkout (16440 bytes against 16290 in
the object store); restore it with `git show HEAD:<path> > <path>` before
opening it.

## The five attribute types

| Model type | Delphi class | Derived from | What it adds |
| --- | --- | --- | --- |
| `Name` | `TBAName` | `TBAString` | nothing at all, only a distinct model type |
| `ShortString` | `TBAShortString` | `TBAString` | `ValidateString` rejects more than 25 characters |
| `SwedishSocSec` | `TBASwedishSocSec` | `TBAString` | `ValidateString` and `ValidateCharacter` for a Swedish personal number |
| `BudgetStatus` | `TBABudgetStatus` | `TBAValueSet` | an enum with three values, each in three string forms |
| `Coordinate` | `TBACoordinate` | custom member | `x`, `y`, `z`, a computed length, four extra representations |

`TBAName` is worth a look precisely because it is empty. It exists so the model
can say "this is a name" and so a later change of behaviour has somewhere to
go.

`TBABudgetStatus.GetValues` registers `Preliminary / Prel / P`,
`Current / Curr / C` and `Old / Old / O`, which is what lets `BoldComboBox1`
offer the values without a lookup list of its own.

## Registration: the TypeNameHandle

Nothing is registered in application code. `BoldTypeNameHandle1` on the form
holds the dictionary and `BoldModel1.TypeNameHandle` points at it. The five
added rows read:

| ModelName | DelphiName | ContentsName | MapperName | UnitName |
| --- | --- | --- | --- | --- |
| `ShortString` | `TBAShortString` | String | `TBoldPMString25` | `BAShortString` |
| `Coordinate` | `TBACoordinate` | `Coordinate` | `TBACoordinatePMapper` | `BACoordinate` |
| `SwedishSocSec` | `TBASwedishSocSec` | String | `TBoldPMString` | `BASwedishSocSec` |
| `BudgetStatus` | `TBABudgetStatus` | Integer | `TBoldPMInteger` | `BABudgetStatus` |
| `Name` | `TBAName` | String | `TBoldPMString` | `BAName` |

Each unit also calls `BoldMemberTypes.AddMemberTypeDescriptor` in its
`initialization` and removes it again in `finalization`.

The model root carries `Bold.InterfaceUses=BoldPMString25, BACoordinatePMapper`
so the generated unit pulls the mappers in.

## The two persistence mappers

`TBoldPMString25` is the small case: a `TBoldPMString` that overrides
`GetColumnTypeAsSQL` to emit `CHAR(25)` and `GetColumnSize` to return 25. One
attribute type, one column, a different column definition.

`TBACoordinatePMapper` is the interesting one, because a coordinate is not one
column. It is a `TBoldMemberDefaultMapper` with `GetColumnCount = 3` and three
`integer` columns, and `ValueToParam` / `ValueFromField` switch on
`ColumnIndex` to move `x`, `y` and `z` in and out. Storage shape and model
shape do not have to match.

## Representations as read-only views

`TBACoordinate` defines four representations beyond the default:

```pascal
brXCoordinate      = 2;
brYCoordinate      = 3;
brZCoordinate      = 4;
brCoordinateLength = 5;
```

Five `TBoldEdit`s bind the same expression, `coordinate`, and differ only in
`BoldProperties.Representation`:

| Control | Representation | Shows |
| --- | --- | --- |
| `BoldEdit4` | 2 | `x` |
| `BoldEdit5` | 3 | `y` |
| `BoldEdit6` | 4 | `z` |
| `BoldEdit7` | default | `x: %d y: %d z: %d` |
| `BoldEdit8` | 5 | `Length: %4.2f` |

None of them sets `ReadOnly`. The combined view and the length are read-only
anyway, because `ValidateString` and `ValidateCharacter` return `False` for
`brDefault` and `brCoordinateLength`. Editability is a property of the
representation, not of the control.

## Why the status bar is there

`Action1.OnUpdate` reads `GetBoldLastFailureReason` and puts its text in
`StatusBar1`. That is how a refused keystroke explains itself: type a 26th
character into the ShortString field and `TBAShortString.ValidateString` sets a
`TBoldFailureReason` reading *String too long*, which appears down there.

## Files

| File | What it holds |
| --- | --- |
| `BAName.pas` | the empty subclass |
| `BAShortString.pas` | length validation |
| `BASwedishSocSec.pas` | personal-number validation |
| `BABudgetStatus.pas` | a `TBAValueSet` enum |
| `BACoordinate.pas` | the multi-value attribute and its representations |
| `BACoordinateInterface.pas` | `ICoordinate`, the interface the mapper talks to |
| `BACoordinatePMapper.pas` | three-column persistence mapper |
| `BoldPMString25.pas` | `CHAR(25)` persistence mapper |
| `MainForm.pas` / `.dfm` | the TypeNameHandle, the model and the editors |
| `SampAttrPackD6/D7.dpk` | design-time packages for Delphi 6 and 7 |
