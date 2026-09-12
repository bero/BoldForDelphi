# Optimistic Locking

Two people read the same object, both change it, both save. Without a check the
second save silently overwrites the first. Bold's optimistic locking compares
what you read against what is in the database when you save, and fails the save
instead.

This demo runs both people inside one executable, side by side, so you can
cause the collision and then see what the failure gives you to work with.

## Running it

There is no `.dproj`. Open `BE_OptimisticLocking.dpr` in the IDE and let Delphi
create one.

It needs InterBase, `sysdba` / `masterkey`. Unlike the other demos here the
database name **is** filled in: both `IBDatabase1` and `IBDatabase2` point at
`OptimisticLocking.gdb`. Press **Create DB** once, then **Open system** in
each group box.

Then: change the same car's colour in App 1 and in App 2, press **Update
Database** in App 1, and press it in App 2.

## Two systems in one process

The form carries two of everything below the model:

| App 1 | App 2 |
| --- | --- |
| `BoldSystemHandle1` | `BoldSystemHandle2` |
| `BoldPersistenceHandleDB1` | `BoldPersistenceHandleDB2` |
| `BoldDatabaseAdapterIB1` | `BoldDatabaseAdapterIB2` |
| `IBDatabase1` | `IBDatabase2` |
| `blhAllPersons1`, `blhAllCars1` | `blhAllPersons2`, `blhallCars2` |
| `BoldActivateSystemAction1` | `BoldActivateSystemAction2` |

They share `BoldSystemTypeInfoHandle1` and therefore the model, but each has
its own object space and its own connection. Two `TBoldSystemHandle`s are what
two users are, as far as the persistence layer is concerned.

## Turning it on

One tagged value on the model root:

```
Bold.OptimisticLocking=Member
```

`Member` means the comparison is per member. Two users editing different
attributes of the same car do not collide; two users editing the same attribute
do. The alternative settings trade that precision for cheaper bookkeeping.

## Handling the failure

The save raises, and the exception names the objects that lost:

```pascal
bchFailedObjects1.List.Clear;
try
  BoldSystemHandle1.UpdateDataBase;
except
  on e: EBoldOperationFailedForObjectlist do
  begin
    bchFailedObjects1.List.AddList(e.ObjectList);
    raise;
  end;
end;
```

`EBoldOperationFailedForObjectlist.ObjectList` is the useful part. It is copied
into `bvhFailedObjects1`, a `TBoldVariableHandle` whose `ValueTypeName` is
`Collection(BusinessClassesRoot)`, surfaced through `bchFailedObjects1` and
listed in `BoldListBox1`. The `raise` is deliberate: the list is populated and
the user still gets the error.

**Discard** then calls `TBoldObject.Discard` on the selected failure, which
throws away the local change and lets the object re-read from the database.
Discarding one object rather than the whole system is what the per-object list
is for.

## The model

`Person` with `name` and `hasLicense`, `Car` with `color`, `maxSpeed` and
`model`, and an association between them. Small on purpose; the interesting
part is the two columns of identical controls.

## Files

| File | What it holds |
| --- | --- |
| `BE_OptimisticLocking.dpr` | creates `TForm1` and runs |
| `MainForm.pas` / `.dfm` | both system chains, both grids, the failure handling |
| `OptimisticLockingExampleClasses*.pas/.inc` | generated `Person` and `Car` |
