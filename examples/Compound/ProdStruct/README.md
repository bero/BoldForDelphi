# Product Structure

A bill of materials: assemblies contain products, and products can be
assemblies, so the structure nests to any depth. The cost of an assembly is the
sum of its parts' costs plus its own assembly cost, which means a derived value
that recurses.

The demo is built around two things that are awkward to show on a flat model:
writing a derivation in Delphi code and placing its subscriptions by hand, and
driving a `TBoldTreeView` whose node shape depends on the class of the object.

## Running it

Open `ProdStruct.dpr`. There is no `.dproj`.

This demo needs Interbase. `dm1.dfm` holds a `TIBDatabase` with
`DatabaseName = 'ProdStruct.gdb'`, `user_name=sysdba`, `password=masterkey`,
dialect 3, behind a `TBoldDatabaseAdapterIB`. Note that the IBX adapter units
now live under `Source\Deprecated\Persistance\IBX` and are not part of the
maintained package set, so this project will not compile against a stock current
build without putting that folder on the search path.

In the application: **Create DB**, then **Open system**. The first time the
system opens with no products,
`BoldActivateSystemAction1SystemOpened` seeds six objects:

| Object | Kind | Cost | Contains |
| --- | --- | --- | --- |
| BigBlade | Simple_Product | production 5 | |
| SmallBlade | Simple_Product | production 4 | |
| Handle | Simple_Product | production 2 | |
| BigKnife | Assembly | assembly 1 | BigBlade, Handle |
| SmallKnife | Assembly | assembly 1 | SmallBlade, Handle |
| KnifeKit | Assembly | assembly 1 | BigKnife, SmallKnife |

`Handle` is deliberately shared by both knives, and `KnifeKit` contains
assemblies rather than simple products, so the tree has both a diamond and two
levels of nesting.

## The model

| Class | Members |
| --- | --- |
| `Product` | `Name: String`, `Price: Currency`, `TotalCost: Currency` (derived, transient) |
| `Assembly` (: `Product`) | `AssemblyCost: Currency` |
| `Simple_Product` (: `Product`) | `ProductionCost: Currency` |
| `PartOfParts` | link class, no attributes |

One association, many to many and reflexive through the subclass:
`Assembly.parts` to `Product.partOf`, realised by the link class `PartOfParts`.
Because `Assembly` is itself a `Product`, an assembly can appear in another
assembly's `parts`.

## Derivation written in code

`TotalCost` is marked `Derived=True` with an empty `Bold.DerivationOCL`, so Bold
calls a generated hook instead, and the demo implements it three times, once per
class in `ProdStructClasses.inc`.

`TSimple_Product`:

```pascal
M_TotalCost.AsCurrency := productionCost;
SubscribeToExpression('productionCost', Subscriber, False);
```

`TAssembly`:

```pascal
m_AssemblyCost.DefaultSubscribe(Subscriber);
C := assemblyCost;
SubscribeToExpression('parts', Subscriber, True);
for I := 0 to parts.Count - 1 do
begin
  C := C + parts[I].totalCost;
  Parts[I].SubscribeToExpression('totalCost', Subscriber, False);
end;
M_TotalCost.AsCurrency := C;
```

`TProduct`'s own implementation is empty, because the base class has no common
rule.

Two details carry the lesson. The subscription on `parts` passes
`Resubscribe = True` so that the derivation runs again when the list changes and
can subscribe to the new members; the per-element subscriptions on `totalCost`
pass False, because a changed value only needs to invalidate, not resubscribe.
And the recursion is implicit: reading `parts[I].totalCost` derives that part's
cost, which may itself be an assembly.

The file says outright that the same thing in OCL would be one line:

```
assemblyCost + parts.totalCost->sum
```

and that the code version exists only to show how the hook works.

## The profit renderer

`dm1.pas` holds `BoldProfitAsStringRenderer`, a `TBoldAsStringRenderer` bound to
the "Profit" column of the product grid. It has no expression; it computes from
two attributes at once.

`OnGetAsString` renders `Round((Price / TotalCost - 1) * 100)` as a percentage,
or `n/a` when the cost is zero. `OnSubscribe` subscribes to both `price` and
`totalCost`. `OnMayModify` returns False when the cost is zero, which makes the
cell read only exactly when the value is meaningless. `OnSetAsString` strips
spaces and the per-cent sign and then works backwards, setting the price:

```pascal
Price := TotalCost * (StrToFloat(CleanedValue)/100+1);
```

That is a column you can type a margin into.

## The tree

`BoldTreeView1` is rooted on `hdlTreeRoot`, a `TBoldReferenceHandle`, and its
top-level controller expression is `self.OclType` with `InterpretAsList` set, so
which node description applies is decided by the object's class. There are four
descriptions:

| Description | Context | List parts | Text expression |
| --- | --- | --- | --- |
| `Assembly` | Assembly | `'PartOf'` and `'Parts'` | |
| `Simple_Product` | Simple_Product | `'PartOf'` | |
| `PartOf` | | element `partOf` | `'Part of ' + partOf->size.asString + if partOf->size=1 then ' assembly' else ' assemblies' endif` |
| `Parts` | | element `parts` | `parts->size.asString + if parts->size=1 then ' part' else ' parts' endif` |

The two grouping nodes also choose their icon by OCL, indexing into the image
list arithmetically:

```
2+partOf->size.min(3)
6+parts->size.min(3)
```

All four have `HideNodeWithNoChildren` set, so a simple product that is not part
of anything shows no empty branch.

The two buttons show the two ways to root a Bold tree, which are mutually
exclusive:

```pascal
hdlTreeRoot.Value := hdlAllProducts.CurrentElement;                              // Change Root
hdlTreeRoot.value := dmMain.BoldSystem.System.ClassByExpressionName['Product'];  // Show all
```

The second one is worth noting: the root is the *class*, and the tree then shows
its extent. `Assembly` and `Simple_Product` are subclasses of `Product`, so both
appear.

## Small things on the form worth finding

- **One navigator, three grids.** `NvgShared.BoldHandle` is reassigned in each
  grid's `OnEnter`, and a label says which list it is pointed at.
- **Backspace unlinks.** `lbAssemblyPartsKeyPress` removes the selected part
  from the current assembly's `Parts` on `#8`, without deleting the object.
- **The runtime column editor.** Double-clicking the assemblies grid opens
  `TfrmRTColEditor` from `BoldGridRTColEditor`, the same column editor the IDE
  offers, against a live grid.
- The product grid has a column that renders the class:
  `if oclIsTypeOf( Simple_Product ) then 'Simple' else 'Assembly' endif`.

## Files

| File | What it holds |
| --- | --- |
| `ProdStruct.dpr` | the data module and the main form |
| `dm1.pas` / `.dfm` | the model, the system and persistence handles, the IB adapter, and the profit renderer |
| `main.pas` / `.dfm` | the tree, the three grids, the seeding code |
| `ProdStructClasses.inc` | the three `_TotalCost_DeriveAndSubscribe` implementations |
| `ProdStruct.bld` | exported model; the authoritative copy is inside `dm1.dfm` |
| `*.bmp` | the tree view image list, exported as separate bitmaps |

Both `dm1.dfm` and `main.dfm` are binary form files, so they do not diff
usefully in git.
