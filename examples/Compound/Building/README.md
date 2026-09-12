# Buildings and Owners

The kitchen-sink demo for the Bold-aware VCL. One small model, one crowded
form, and nearly every runtime feature Bold offers on top of a database:
grids and list boxes, custom renderers, filters and comparers, auto forms, a
form saver, undo and redo, two constraint validators, and the full set of
database actions.

It exists to be read rather than admired. Almost every control on the form
demonstrates exactly one thing.

## Running it

Open `BldOwn.dpr`. There is no `.dproj`, so the IDE will create one.

This demo needs a database and there is no `.ini` file; the connection settings
live in `Datamod.dfm`. As shipped, `BoldPersistenceHandleDB1.DatabaseAdapter`
points at `BoldDatabaseAdapterPostgres`, whose `Connection` is
`FDConnectionPostgres`:

```
Password=masterkey
Database=Buildings
User_Name=postgres
DriverID=PG
```

A second FireDAC pair (`BoldDatabaseAdapterSQLServer` / `FDConnectionSQLServer`,
`DriverID=MSSQL`, `Database=Buildings`) is present but not wired to the
persistence handle. Point the handle at it to switch engines.

Then, in the running application: **Create DB**, **Open system**, and start
adding people and buildings from the popup menus or the navigators. New objects
get random names and values from `CompleteCreate`, so you get something to look
at immediately. **Update DB** writes, **Discard changes** throws away.

## The model

| Class | Members |
| --- | --- |
| `Building` | `Address: String`, `ZipCode: Integer` |
| `Residential_Building` (: `Building`) | `TotalRent: Currency`, `Capacity: Integer` |
| `Person` | `FirstName`, `LastName`, `Assets: Currency`, `IsMarried: Boolean`, and a derived `name` |
| `Ownership` | link class, no attributes of its own |

`Person.name` is derived in the model, not in code:

```
firstName + ' ' + lastname
```

| Association | Shape |
| --- | --- |
| `Person.ownedBuildings` / `Building.owners` | many to many, through the link class `Ownership` |
| `Person.home` / `Residential_Building.residents` | 0..1 to many |

`TPerson.CompleteCreate` picks a name from two five-element arrays and gives the
person 5000 to 15000 in assets; `TBuilding.CompleteCreate` picks a landmark
name; `TResidential_Building.CompleteCreate` builds a street address, a rent and
a capacity. All three are model methods, so they run for objects created from
the navigator as well as from code.

## The renderers

A renderer intercepts how a value is presented and, optionally, how it is
written back. Each of these shows a different reason to write one.

| Renderer | Where | What it does |
| --- | --- | --- |
| `NegativeRedRenderer` | data module | `OnSetFont`: red when `Assets < 0`, blue otherwise |
| `IsRichRenderer` | data module | `OnGetAsCheckBoxState`: a three-state box, checked when `Assets > 10000`; `OnMayModify` returns False so it is read only |
| `bsrAddress` | form | aqua and bold when the address contains `Bold`, red for `Rose`, green for `Select` |
| `HighRentRenderer` | form | silver background and red text when `totalRent >= 1500` |
| `bsrResidentsTotalAssets` | form | sums `Assets` over all residents |
| `bsrRentPerResident` | form | rent divided by head count, and writable |

`bsrRentPerResident` is the one worth reading in full. It implements
`OnGetAsString`, `OnSetAsString`, `OnMayModify`, `OnSubscribe`,
`OnValidateCharacter`, `OnValidateString`, `OnHoldsChangedValue` and
`OnReleaseChangedValue`. It renders `***` when the rent is null, `No Residents`
when there is nobody to divide by, and when you type a new per-head figure it
multiplies back up:

```pascal
TotalRent := StrToCurr(v) * Residents.Count;
```

`OnHoldsChangedValue` registers the follower's subscriber as a modified value
holder on `M_TotalRent`, which is what lets a half-typed value survive until it
is applied or cancelled.

Every renderer that computes something has to say what it depends on.
`bsrResidentsTotalAssetsSubscribe` subscribes to `residents` with resubscribe
True and then to `assets` on each resident with resubscribe False; that pairing
is the general pattern for "the list may change, and so may each element".

## Filter and comparer

The two check boxes under the person grid swap a filter and a comparer onto the
same list handle at runtime:

```pascal
blhAllPerson.BoldFilter := datamodule1.IsRichFilter;      // Assets > 10000
blhAllPerson.BoldComparer := datamodule1.NameComparer;    // last name, then first
```

Both have `OnSubscribe` handlers, so the list re-filters and re-sorts when the
underlying values change rather than only when it is rebuilt.

The radio group at the bottom rewrites an expression instead, switching
`blbBuildingOwners.BoldRowProperties.Expression` between `firstName`,
`lastName` and `firstName + ' ' + lastName`.

## Two constraint validators, two modes

`TBoldConstraintValidator` evaluates OCL constraints and reports violations.
The demo has two, deliberately using different validation modes.

`BoldConstraintValidatorOnModify`, `ValidationMode = vmOnModify`, context
`Residential_Building`:

```
residents->size <= capacity
```

with the error message built in OCL too:
`'Building capacity limit is ' + capacity.asString`.

`BoldConstraintValidatorOnUpdate`, `ValidationMode = vmPreUpdate`, context
`Person`:

```
(ownedBuildings->filterOnType(Residential_Building)->notEmpty) implies (home <> nil)
```

The split is the point. The capacity rule can be checked the moment somebody
moves in. The homelessness rule cannot, because a half-finished edit will
legitimately violate it; it only has to hold at the moment the changes are
written, so it runs pre-update.

The grids have `BoldShowConstraints` set, so a violated constraint is visible in
the row rather than only raised at save time.

## Undo and redo

Bold's undo handler is switched on when the system opens:

```pascal
datamodule1.BoldSystemHandle1.System.UndoHandlerInterface.Enabled := true;
```

Two list boxes show `UndoList` and `RedoList` with the block name and creation
time. Double-clicking an entry calls `UndoBlock` or `RedoBlock` **by name**, not
by index, which is how Bold's named undo blocks are meant to be used. A
`TBoldPlaceableSubscriber` keeps the lists current by subscribing to
`beUndoChanged`, `beUndoBlock`, `beRedoBlock` and `beUndoSetCheckpoint`, all
requesting `beUndoChanged`. **Set checkpoint** closes the current block.

## Auto forms

`AutoFormProviderRegistry.FormForElement(BoldObject).Show` opens a form for any
object, and `TBoldPlaceableAFP` in the data module supplies it: `OnGetFormClass`
returns `TPersonAutoForm` for a `TPerson`, and `OnRetrieveHandle` hands back
that form's `brhPerson` reference handle and points its `BoldFormSaver1` at the
system handle.

`TPersonAutoForm` itself demonstrates the form saver: OK, Apply and Cancel
actions over a `TBoldFormSaver`, so edits in that window are a unit that can be
abandoned.

**Show in own window** on any of the popup menus is the entry point.

## The model editor at runtime

```pascal
BoldUMLModelEdit.UMLModelEditor.ShowEditFormForBoldModel(DataModule1.BoldModel1);
```

opens the IDE's model editor against the running application's model. The same
menu also carries **Evolve DB**, **Validate DB structure**, **Validate DB data**
and **Generate schema**, so the schema half of the design-time tooling is
reachable here too.

## Logging

Four toggles under the Log menu turn on Bold's own logs: OCL evaluation, SQL,
persistence-mapper calls, and object-space-sync traffic, plus
`BoldLogFormAction1` to show the log window. Turn on the SQL log and then open
the system to watch what the mapper actually issues.

## Files

| File | What it holds |
| --- | --- |
| `BldOwn.dpr` | data module, main form, and the person auto form |
| `Datamod.pas` / `.dfm` | the model, the system and persistence handles, both FireDAC adapters, the two validators, the shared renderers, the filter and the comparer |
| `Mainform.pas` / `.dfm` | everything else: the grids, the renderers written on the form, undo, logging, the DB actions |
| `PersonAutoFormUnit.pas` / `.dfm` | the auto form, with a `TBoldFormSaver` |
| `Building.bld`, `Building.mdl` | exported model and the original Rose model; the authoritative copy is in `Datamod.dfm` |
| `BuildingClasses.pas`, `BuildingClasses_Interface.inc` | generated code |
| `Building.inc`, `Person.inc`, `ResidentialBuilding.inc` | the hand-written method bodies |
| `BldOwn.mpb` | a ModelMaker project bundle, from the original authoring environment |

`BoldUMLRoseLink1.FileName` still points at `D:\bold\BfD\examples\...`, a path
from the machine this was built on. It matters only if you try to round-trip the
model through Rose.
