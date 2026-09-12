# Derived Handle

`TBoldDerivedHandle` is the escape hatch for a value OCL cannot express. You
write Delphi in `OnDeriveAndSubscribe` to produce the result, and in the same
method you declare what the result depends on by subscribing to it. Bold then
re-runs the handler whenever any of those things change, so the handle behaves
like any other handle downstream.

The question this demo answers is *who are the N richest people, counting only
those above an asset threshold*. Sorting and top-N are the awkward part.

## Running it

There is no `.dproj`. Open `BE_DerivedHandleDemo.dpr` in the IDE and let Delphi
create one.

It needs a database: InterBase through `TBoldDatabaseAdapterIB`, `sysdba` /
`masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it first. Then
**Create DB** and **Open system**. Ten people, Adam to Jacob with assets 10000
to 100000, are created on first open. Drag the track bar, spin the count, and
watch the lower grid change.

`Mainform.dfm` is a binary form file that the repository's
`*.dfm text eol=crlf` rule inflates on checkout (12254 bytes against 12205 in
the object store), so it will not stream until you restore it with
`git show HEAD:<path> > <path>`.

## Deriving

`bdhRichPersonsDeriveAndSubscribe` clones the `Person` class extent, sorts the
clone by assets descending with `RichSorter`, then walks it backwards removing
anyone who is too poor or too far down the list, and hands the survivors over:

```pascal
AllPersons := ClassByExpressionName['Person'] as TPersonList;
RichPersons := allPersons.Clone as TPersonList;
RichPersons.Sort(RichSorter);
for i := RichPersons.Count - 1 downto 0 do
begin
  RichPersons[i].M_Assets.DefaultSubscribe(Subscriber);
  if (RichPersons[i].assets < (bvhAssetBreakpoint.Value as TBACurrency).AsCurrency) or
     (i >= (bvhRichCount.Value as TBAInteger).asInteger) then
    RichPersons.RemoveByIndex(i);
end;
ResultElement.SetOwnedValue(RichPersons);
```

Walking backwards matters, because removing by index shifts everything after
it.

## Subscribing

The half that makes it a handle rather than a function is the four
subscriptions, and each one answers a different "what if":

| Subscription | Covers |
| --- | --- |
| `RichPersons[i].M_Assets.DefaultSubscribe(Subscriber)` | somebody's assets change |
| `AllPersons.DefaultSubscribe(Subscriber, breReSubscribe)` | a person is created or deleted |
| `bvhRichCount.Value.DefaultSubscribe(Subscriber)` | the count spinner moves |
| `bvhAssetBreakpoint.Value.DefaultSubscribe(Subscriber)` | the track bar moves |

The assets subscription is taken inside the loop, before the removal test, so
it covers people who are *not* in the result. It has to: somebody below the
line getting richer changes the answer.

Miss a subscription and nothing breaks loudly. The grid simply goes stale,
which is why this is the part to read carefully.

## The inputs

Two `TBoldVariableHandle`s hold the parameters as Bold values rather than as
Delphi fields, so they can be bound to controls and subscribed to:

| Handle | `ValueTypeName` | Initial value | Bound to |
| --- | --- | --- | --- |
| `bvhRichCount` | `Integer` | 3 | `edRichPersonCount`, with `UpDown1` |
| `bvhAssetBreakpoint` | `Currency` | 50000 | `edRichBreakpoint` and `btbRichBreakpoint` |

`btbRichBreakpoint` is a `TBoldTrackBar` with
`BoldProperties.ApplyPolicy = bapChange`, so it writes on every movement and
the derivation re-runs while you drag.

`bchRichPersons` is a `TBoldCursorHandle` rooted on the derived handle, and
`grdRichPersons` is bound to that, with `BoldAutoColumns` rather than declared
columns.

## A wrinkle worth knowing

`RichSorter` compares with `round(Person2.assets - Person1.assets)`. Two people
less than half a currency unit apart compare as equal.

## Files

| File | What it holds |
| --- | --- |
| `BE_DerivedHandleDemo.dpr` | creates `TForm1` and runs |
| `Mainform.pas` / `.dfm` | the derivation, the variable handles, the embedded model |
| `DerivedHandleExampleClasses*.pas/.inc` | generated `Person`, `Building`, `Ownership` |
