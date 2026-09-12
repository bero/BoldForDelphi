# External Persistence

A Bold model whose objects are read out of a database Bold did not create and
does not own. The external source here is the classic Borland **DBDEMOS Paradox
tables**, reached through the BDE, and the mapping between them and the model is
written by hand as event handlers.

The feature exists for the case where a legacy schema cannot be changed but you
want Bold objects, OCL, derived attributes and data-aware controls on top of it.
Crucially, it is *partial*: one object can have some members in the legacy source
and others in a Bold database, and neither side knows about the other.

Its twin, `ExternalPersistence2`, does the same job declaratively. See **How this
differs from ExternalPersistence2** at the end.

## The projects

One project, in this folder.

| File | What it holds |
| --- | --- |
| `BE_ExternalPersistence.dpr` | creates the form, then the data module |
| `dMain.pas` | the whole demo. The external handle, the BDE components, the Bold database, and about 400 lines of mapping code |
| `fMainForm.pas` | customers, their contacts, their orders and the items on an order. Three buttons wired to Bold actions |
| `BusinessClasses*.pas` / `.inc` | generated classes |
| `BusinessClasses_PersistenceInterfaces.pas` | generated `IPersistentXxx` interfaces, one per class. This is the file that makes the mapping code readable |
| `EmployeeModel.mdl` | the Rational Rose model the Bold model was imported from |
| `ExternalColorer.ebs` | a Rose Basic script, see the notes |

## Running it

1. You need the BDE installed with a `STANDARD` driver, and the Paradox demo
   tables. `Database1` is configured inline rather than through an alias:

   ```
   DriverName = 'STANDARD'
   PATH=c:\program files\common files\borland shared\data
   DEFAULT DRIVER=PARADOX
   ENABLE BCD=FALSE
   ```

   The tables it opens are `customer.db`, `orders.db`, `employee.db`,
   `items.db`, `parts.db` and `vendors.db`.
2. Run the exe and press **Create DB**. That is a `TBoldIBDatabaseAction`
   logging in as `SYSDBA` / `masterkey`; it creates `ExPe.gdb` next to the
   executable and puts the Bold schema in it.
3. Press **Open system**. `OnActivate` opens `Database1` and all six tables, and
   the grid fills with customers read out of Paradox.
4. Closing the window runs **Update DB**, so edits are written back.

## The main idea

### The chain

```
BoldSystemHandle1
  -> BoldExternalDataPersistenceHandle (TBoldExternalPersistenceHandleEventDriven)
       -> BoldPersistenceHandleDB1 -> BoldDatabaseAdapterIB1 -> IBDatabase1 (ExPe.gdb)
```

The external handle is first in line. Whatever it claims it handles goes to the
BDE tables; everything else falls through to `NextPersistenceHandle`, the
ordinary Bold database.

### The model is the contract

Which member goes where is not in the code. It is a tagged value, and the model
editor shows it as a stereotype letter:

| Tag | Stereotype | Meaning |
| --- | --- | --- |
| `Bold.Storage=PartiallyExternal` | on the class | some of this class lives outside |
| `Bold.Storage=External` | `E` | this member comes from the external source |
| `Bold.Storage=ExternalKey` | `EK` | this member *is* the external primary key |
| none | `I` | ordinary Bold member, stored in `ExPe.gdb` |

Six classes are `PartiallyExternal`: `Customer`, `Order`, `Employee`, `Item`,
`Part` and `Vendor`. Two are not tagged at all and live wholly in `ExPe.gdb`:
`Contact`, and the association class `bribesisBribedBy`, which links an
`Employee` to a `Vendor`. So the demo also shows a Bold-owned association
between two external classes.

`Customer` is the sharpest illustration. `Name` maps to `CUSTOMER.Company`,
`CustomerID` is the external key `CustNo`, and `IsMarried` is stereotype `I`.
One object, two databases, no code aware of the seam.

### The Config collection

`TBoldExternalPersistenceHandleEventDriven.Config` holds one item per class, and
each item implements only the events that class needs:

| Class | Events supplied | Effect |
| --- | --- | --- |
| `Customer` | `GetKeyList`, `Exists`, `ReadObject`, `ReadMember`, `CreateObject`, `UpdateObject`, `DeleteObject` | full read and write, and `Customer.allInstances` works |
| `Order` | `ReadObject`, `ReadMember`, `CreateObject`, `UpdateObject`, `DeleteObject` | read and write, but no key list, so orders are only reachable by navigation |
| `Employee`, `Part`, `Vendor` | `ReadObject`, `ReadMember` | read only |
| `Item` | `ReadObject`, `GetKeyFromObject`, `AssignKeyToObject`, `GetInternalSQLForKeys` | compound key, see below |

`GetKeyList` is what makes `allInstances` possible, and only `Customer` has one.
That is a real design point rather than an omission: a legacy table with a
million rows should not be enumerable from OCL.

### Compound external keys

An `Item` is identified by `OrderNo` *and* `ItemNo`, so its external key cannot
be an integer. `ItemGetKeyFromObject` builds a `TBoldStringId` holding
`'orderno,itemno'` and `ItemAssignKeyToObject` takes it apart again. The third
handler is the one worth reading:

```pascal
result := result + format('(OrderNo = %s AND ItemNo = %s)', [KeyList[0], KeyList[1]]);
```

`OnGetInternalSQLForKeys` turns a list of those string keys into a single SQL
`WHERE` clause so the *internal* handle can find the matching rows for the same
objects in one query instead of one per object.

### Turning foreign keys into Bold links

Two helper routines carry all the navigation, and they are the calls to
remember:

- `ReadMultiLink` collects the primary keys of the rows whose foreign key
  matches, then calls `BoldExternalDataPersistenceHandle.SetMultiLink`.
- `ReadSingleLink` reads one foreign key field, or nil when it is null, then
  calls `SetSingleLink`.

Everything else in `dMain.pas` is `Table.Locate` followed by field-by-field
assignment through the generated `IPersistentXxx` interface.

### Derived members across the seam

Three model members are derived and computed in memory, which means they read
across both stores without either store knowing:

| Member | Derivation |
| --- | --- |
| `Item.totalCost` | `qty*part.listprice*((100-discount)/100)` |
| `Order.totalCost` | `items.totalcost->sum` |
| `Order.parts` (role) | `items.part` |

### Noticing changes made behind Bold's back

`ApplicationEvents1Idle` polls `HasNewObjects` and `HasDeletedObjects` on the
external handle and logs the counts into the memo. Rows that appear or vanish in
the legacy source are surfaced as Bold objects appearing and disappearing.

## How this differs from ExternalPersistence2

Same model, same form, same feature; a different way of specifying the mapping.

| | `ExternalPersistence` | `ExternalPersistence2` |
| --- | --- | --- |
| External source | Paradox tables via the BDE | InterBase `MASTSQL.GDB` |
| Handle | `TBoldExternalPersistenceHandleEventDriven` | `TBoldExternalPersistenceHandleSQL` |
| Mapping lives in | Delphi event handlers | model tagged values |
| Lines of code in `dMain.pas` | about 400 | none |

## Notes

- The BDE is not shipped with modern Delphi, is 32-bit only, and the Paradox
  demo data is no longer installed by anything. This demo cannot be revived
  without replacing the `TDatabase` and `TTable` components.
  `ExternalPersistence2` is the easier of the pair to get running.
- `VendorReadObject` writes `Preferred := 0` when the Paradox `Preferred` flag
  is **True** and `1` when it is False. `Preferred` is Boolean in the model and
  integer at the persistence-value level, where 0 is false, so the flag arrives
  inverted.
- `OnBeforeUpdates` and `OnAfterUpdates` are wired up but their
  `StartTransaction` and `Commit` calls are commented out, so external writes are
  not transactional.
- `ExternalColorer.ebs` is a Rational Rose Basic script. It walks the diagram,
  reads each element's `Bold` / `Storage` property, and paints external elements
  grey with a red font. Useful only with Rose installed, and byte-identical to
  the copy in `ExternalPersistence2`.
- The Bold side uses `TBoldDatabaseAdapterIB` and IBX, from
  `Source/Deprecated/Persistance/IBX`.
- There is no `.dproj`, only the `.dpr`.
