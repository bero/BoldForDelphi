# Model Evolution

Two versions of the same application against the same database. Version 2
changes the model in three ways that a running system normally cannot survive,
and the demo shows the three Bold mechanisms that let it: the database schema is
evolved in place, existing objects are upgraded lazily as they are read, and the
old columns are kept written so that a version 1 client still works.

The difference between the two models *is* the demo, so it is spelled out in
full below.

## The projects

| Project | Folder | What it is |
| --- | --- | --- |
| `ModelEv_v1` | `Version1\` | the old application, model version 1 |
| `ModelEv_v2` | `Version2\` | the new application, model version 2, plus an **Evolve DB** button |
| `BatchUpgrader` | `Version2\` | a headless upgrader that walks the database in batches |

`Common\` holds the three units all of them share: `dSystem` (the system handle
and the Bold actions), `dSystemTypeInfo`, and `fBatchUpgraderGui`. Each version
has its own `dModel`, `dPersistence`, `ModelEvClasses`, `fStart` and `fMain`.

None of the three has a `.dproj`; open the `.dpr` files.

## Running it, in order

All three point at the same Interbase database, `..\ModelEvolution.gdb`
relative to the version folder, with `user_name=sysdba` / `password=masterkey`
through a `TBoldDatabaseAdapterIB`. That adapter now lives under
`Source\Deprecated\Persistance\IBX` and is not in the maintained package set, so
the search path needs it.

1. Run **ModelEv_v1**. The start dialog offers **Create schema**, **Open system**
   and **Cancel**. Create the schema, then open the system.
2. Enter data: persons with a `fullName`, products with a `cost`, orders with a
   `discount`, and order items joining orders to products. **Save and Reload**
   writes and reopens.
3. Close it and run **ModelEv_v2**. Its start dialog has a fourth button,
   **Evolve DB**. Press that first.
4. Then **Open system**. The persons, products and orders created under version
   1 are upgraded as they are fetched.
5. Optionally run **BatchUpgrader** to upgrade whatever has not been read yet.

## What differs between the two models

Both models are called `ModelEvDemo`, both have `Bold.UseModelVersion=True`, and
`Bold.ModelVersion` goes from `1` to `2`. That number is what the upgrader
compares against.

| | Version 1 | Version 2 |
| --- | --- | --- |
| `Person.fullName` | public `String` | still there, **visibility changed to private** |
| `Person.firstname`, `Person.lastName` | absent | added, public `String` |
| `Product.cost` | public `Currency` | still there, **private** |
| `Product.CostEuro` | absent | added, public `Currency` |
| `Order.discount` | public `Integer` | still there, **private** |
| `Discount` class | absent | added, with `percent: Integer` |
| `Order.theDiscount` / `Discount.order` | absent | added, association `theDiscountOrder`, 0..1 to 0..* |
| `Order.totalCost` derivation | `orderItem->collect(qty*product.cost)->sum*((100-discount)/100)` | `orderItem->collect(qty*product.costEuro)->sum*((100-theDiscount.percent)/100)` |
| methods | none | `Person.Upgrade`, `Product.Upgrade`, `Order.Upgrade`, `Person.PrepareUpdate`, `Product.PrepareUpdate`, `Discount.ReceiveEventFromOwned` |

So the three changes are, in the usual vocabulary:

- **split an attribute**: one `fullName` becomes `firstname` and `lastName`
- **change the unit of an attribute**: `cost` in crowns becomes `CostEuro`, at a
  fixed rate of 8
- **promote an attribute to a class**: the integer `Order.discount` becomes a
  link to a shared `Discount` object with a `percent`

## Mechanism one: the old members stay in the model

Nothing is deleted from version 2's model. `fullName`, `cost` and `discount` are
all still there; only their visibility changed from public (`2`) to private
(`0`). That is what makes the whole thing work: the columns keep existing, the
old values are still readable, the upgrade code can read them, and the
`PrepareUpdate` code can keep writing them. What the visibility change buys is
that the rest of the application cannot use them by accident.

## Mechanism two: evolve the schema

`Version2\fStart.pas` builds the evolution by hand rather than through an
action, which makes it readable:

```pascal
Evolutor := TBoldDataBaseEvolutor.Create(dmPersistence.BoldPersistenceHandleDB1);
form := TfrmBoldDbEvolutor.Create(self);
Evolutor.GenericScript := true;
Evolutor.CalculateScript;
Evolutor.GenerateScript(form.SQLScript, form.MappingInfoScript);
Evolutor.GenerateWarnings(form.Warnings);
if form.ShowModal = mrOK then
begin
  Evolutor.ExecuteScript;
  dmSystem.SystemHandle.Active := true;
  ModalResult := mrOK;
end;
```

The evolutor compares the model against the live database and produces three
things you get to inspect before anything runs: the SQL, the new mapping
information, and a list of warnings. Only pressing OK calls `ExecuteScript`.

For this model the script has to add columns for `firstname`, `lastName` and
`CostEuro`, and a table and link column for `Discount` and `theDiscountOrder`.
It does not have to drop anything, because nothing was removed.

## Mechanism three: upgrade objects one at a time

`Version2\dPersistence.dfm` configures a `TBoldObjectUpgraderHandle`:

```
Config = <
  item ExpressionName = 'Person'  UpgradeOlderThanVersion = 2  OnUpgradeObject = ... end
  item ExpressionName = 'Order'   UpgradeOlderThanVersion = 2  OnUpgradeObject = ... end
  item ExpressionName = 'Product' UpgradeOlderThanVersion = 2  OnUpgradeObject = ... end>
```

and the persistence handle points back at it through `UpgraderHandle`. Each
handler is one line, `(Obj as TPerson).Upgrade` and so on, and the three
`Upgrade` methods in `Version2\ModelEvClasses.inc` are the whole migration:

```pascal
procedure TPerson.Upgrade;
begin
  firstname := trim(copy(fullName, 1, pos(' ', fullName)-1));
  LastName := trim(copy(FullName, pos(' ', FullName)+1, maxint));
end;

procedure TProduct.Upgrade;
begin
  CostEuro := Cost/8;
end;

procedure TOrder.Upgrade;
begin
  if not assigned(theDiscount) then
    TheDiscount := TDiscount.Create(BoldSystem);
  TheDiscount.Percent := Discount;
end;
```

Because each object carries the model version it was last written with, this
runs exactly once per object, when that object is fetched. There is no migration
downtime and no single long transaction.

## The part that is easy to miss: writing the old values back

Upgrading forwards is only half of it. A version 1 client may still be running
against the same database, and it only knows about `fullName` and `cost`. So
version 2 keeps them current, in `PrepareUpdate`, which Bold calls just before
an object is written:

```pascal
procedure TPerson.PrepareUpdate;
begin
  inherited;
  fullName := firstname + ' ' + lastName;
end;

procedure TProduct.PrepareUpdate;
begin
  inherited;
  // the exchange rate between Swedish crowns (our old currency)
  // and Euro (the new currency)
  cost := CostEuro*8;
end;
```

The promoted attribute needs a different trick, because the value now lives on a
shared object rather than on the order. `TDiscount.ReceiveEventFromOwned` waits
for its own `percent` to finish changing and pushes the new value into every
order that points at it:

```pascal
if (Originator = M_percent) and (OriginalEvent = beCompleteModify) then
  for i := 0 to order.count-1 do
    Order[i].discount := percent;
```

That is a deliberate, temporary duplication: the old column is maintained until
the last version 1 client is gone.

## The batch upgrader

`BatchUpgrader.dpr` has no user interface of its own beyond
`Common\fBatchUpgraderGui`, and no system handle in the persistence sense; it
drives `TBoldBatchUpgrader` directly:

```pascal
fBatchUpgrader := TBoldBatchUpgrader.Create(
  dmPersistence.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper,
  dmPersistence.ObjectUpgrader.ObjectUpgrader);
```

Three track bars and a time picker set the four knobs that make it safe to run
against a live database:

| Control | Sets |
| --- | --- |
| batch size | `BatchUpgrader.BatchSize` |
| interval time | `BatchUpgrader.IntervalBetweenBatches` |
| execute time | `BatchUpgrader.MaxExecuteTime` |
| sleep time | the timer interval between runs |

Each timer tick opens the persistence handle, calls `UpgradeObjects`, reports
`UpgradedObjects` and `AutoUpgradedObjects`, closes the handle again and goes
back to sleep. The two counters are worth watching: objects it went looking for
versus objects that happened to be upgraded on the way.

## The two user interfaces

`Version1\fMain` shows persons (`fullName`), products (`description`, `cost`),
orders (`orderDate`, `totalCost`, `discount`) and order items, with **Add to
Order** and **Save and Reload**.

`Version2\fMain` is the same form with a fifth grid for `Discount.allInstances`,
`firstname` and `lastName` columns instead of `fullName`, `costEuro` instead of
`cost`, a `theDiscount.percent` column titled Discount, and a second button,
**Connect Current order to Discount**:

```pascal
(blhOrders.CurrentBoldObject as TOrder).theDiscount :=
  blhDiscounts.CurrentBoldObject as TDiscount;
```

Putting the two side by side is the quickest way to see what the upgrade
produced.

## Files

| File | What it holds |
| --- | --- |
| `Common\dSystem.pas` / `.dfm` | the system handle and the activate / update / create-database actions |
| `Common\dSystemTypeInfo.pas` / `.dfm` | the type info handle |
| `Common\fBatchUpgraderGui.pas` / `.dfm` | the batch upgrader window |
| `VersionN\dModel.dfm` | the model itself, in `Model.Model` |
| `VersionN\dPersistence.pas` / `.dfm` | the IB database, the adapter, the persistence handle and the upgrader handle |
| `Version2\ModelEvClasses.inc` | the three `Upgrade` methods, the two `PrepareUpdate` methods, and `TDiscount.ReceiveEventFromOwned` |
| `VersionN\ModelEv.mdl`, `ModelEvDemo.mpb` | the original Rose model and a ModelMaker bundle |

`Version1` has no `ModelEvClasses.inc`: version 1 has no method bodies at all.
