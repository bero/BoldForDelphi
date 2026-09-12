# External Persistence 2

The same demo as `ExternalPersistence`, with the mapping code deleted.

`dMain.pas` here has no method bodies at all. A single
`TBoldExternalPersistenceHandleSQL` reads the legacy database directly, and
everything the previous demo wrote by hand (which table, which column, which
foreign key) is declared as tagged values in the model instead.

Read `ExternalPersistence` first for what partial external persistence is and
why it exists. This file covers what changes when the handle can speak SQL for
itself.

## The projects

One project, in this folder.

| File | What it holds |
| --- | --- |
| `BE_ExternalPersistence2.dpr` | creates the form, then the data module |
| `dMain.pas` | component declarations only. No implementation |
| `fMainForm.pas` | identical in behaviour to the one in `ExternalPersistence` |
| `BusinessClasses*.pas` / `.inc` | generated classes |
| `BusinessClasses_PersistenceInterfaces.pas` | generated, and in this demo nothing calls it |
| `ExternalColorer.ebs` | Rose diagram colouring script, byte-identical to the one next door |

There is no `.mdl` here; the Rose model lives in the `ExternalPersistence`
folder.

## Running it

1. The external source is `MASTSQL.GDB`, the InterBase version of the same
   Borland demo data, expected at:

   ```
   C:\Program Files\Common Files\Borland Shared\Data\MASTSQL.GDB
   ```

   Put it there or repoint `IBEmployee.DatabaseName`. It logs in as
   `sysdba` / `masterkey` and connects at SQL dialect 1.
2. Run the exe and press **Create DB**. That creates
   `BE_EXTERNALPERSISTENCE2.GDB` beside the executable and puts the Bold schema
   in it.
3. Press **Open system**.
4. Closing the window runs **Update DB**.

No BDE is involved, which makes this the more revivable of the two demos: it
needs an InterBase or Firebird server and the old demo database, nothing else
that has been withdrawn.

## The main idea

### Two databases, two adapters, one chain

```
bshMain
  -> bephEmployee (TBoldExternalPersistenceHandleSQL)
       DatabaseAdapter = dbaEmployee -> IBEmployee  (MASTSQL.GDB, dialect 1)
  -> bphBold (TBoldPersistenceHandleDB)
       DatabaseAdapter = dbaBold     -> IBBold      (BE_EXTERNALPERSISTENCE2.GDB, dialect 3)
```

`bephEmployee.NextPersistenceHandle` is `bphBold`. The external handle has its
own `TBoldDatabaseAdapterIB` pointing at the legacy database, which is the whole
mechanism: it issues its own SQL against a connection Bold did not create the
schema for.

### ClassesToHandle replaces the Config collection

Where the event-driven demo has a `Config` collection with an item and a set of
event handlers per class, this one has a string list:

```
ClassesToHandle.Strings = (
  'Customer'
  'Employee'
  'Item'
  'Order'
  'Part'
  'Vendor')
```

`Contact` and the association class `bribesisBribedBy` are absent, so they fall
through to `bphBold`, exactly as in the other demo.

### The model carries the full column mapping

Both demos tag members `Bold.Storage=External`, `ExternalKey` or nothing, and
both tag the six classes `Bold.Storage=PartiallyExternal`. The difference is in
what else the model has to say.

| | `ExternalPersistence` | `ExternalPersistence2` |
| --- | --- | --- |
| `Bold.TableName` | `Order`, `Part`, `Item` | those three and `Vendor` |
| `Bold.ColumnName` tags in all | four | thirteen |
| on attributes | `Company` and `CustNo`, both on `Customer` | those two and `EmpNo` on `Employee.EmployeeNo` |
| on association roles | `OrderNo_` and `PartNo`, on one end each of two associations | **both ends of all five** external associations |

That last row is the substance. In `ExternalPersistence2` every role of every
external association names its foreign key column:

| Association | One end | Other end |
| --- | --- | --- |
| `Customerorders` | `@CustNo` | `@CustNo` |
| `Vendorparts` | `VendorNo` | `@VendorNo` |
| `Orderitems` | `@OrderNo` | `@OrderNo` |
| `Partitems` | `PartNo` | `@PartNo` |
| `responsibleorders` | `EmpNo` | `@EmpNo` |

The `@` prefix is not part of the column name. `RemovePreAt` in
`BoldExternalPersistenceControllerSQL.pas` strips a leading `@` or `_` before
use, so `@CustNo` and `CustNo` name the same physical column; the prefix only
keeps the two ends distinguishable in the model.
`TAbstractRoleFetchObject.PrepareSQL` then builds the join itself:

```
SELECT <external key columns of the other end>
FROM   <other end's table>
WHERE  (<other end's column> = :<this end's column>)
```

which is the generated equivalent of `ReadMultiLink` and `ReadSingleLink` in the
event-driven demo. The controller asserts `sRoleHasNoColumnNames` when a role is
left untagged, which is the error you get if you copy this pattern and forget one
of the two ends.

### The table name matters now

`Vendor` gained `Bold.TableName=Vendors` here and did not need it next door. The
SQL controller derives the table name from the model (`_GetTableName`, defaulting
to the class name), where the event-driven demo simply names a `TTable`
component in code. The same reason explains why this model's mapping is complete
and the other one's is not.

### What stayed the same

The form, the list handles, the derived members (`Item.totalCost`,
`Order.totalCost`, the derived role `Order.parts`) and the idle handler that
reports `HasNewObjects` and `HasDeletedObjects` are unchanged. Only the
persistence declaration differs, which is the point the pair of demos is making.

## Notes

- `dMain.pas` still lists `BoldDatabaseAdapterBDE` and `BoldPersistenceHandleIB`
  in its uses clause although neither component is on the data module. Both live
  in `Source/Deprecated/Persistance`.
- The Bold side uses `TBoldDatabaseAdapterIB` and IBX, from
  `Source/Deprecated/Persistance/IBX`.
- The two `TIBDatabase` components run at different SQL dialects, 1 for the
  legacy database and 3 for the Bold one. That is correct for the vintage of
  `MASTSQL.GDB` and worth preserving if you repoint them.
- There is no `.dproj`, only the `.dpr`.
