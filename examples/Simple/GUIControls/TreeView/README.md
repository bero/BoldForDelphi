# Tree View

The same `TBoldTreeView` node-description mechanism as the SimpleTreeView demo,
pushed two steps further: a tree whose intermediate nodes are captions rather
than objects, and drag and drop that edits associations instead of moving rows.

The subject is a mountain bike built from parts, so every drag has an obvious
meaning: dropping a wheel on a bike fits it, dropping it on the unlink icon
takes it off again.

## Running it

There is no `.dproj`, only `BE_TreeViewDemo.dpr`. Open it in Delphi and let the
IDE create a project file. Persistence is IBX (`TBoldDatabaseAdapterIB`,
`TBoldIBDatabaseAction`, `TIBDatabase`), whose units live under
`Source\Deprecated\Persistance\IBX` and are in no current Bold package, so put
that folder on the unit search path.

`IBDatabase1` has credentials but **no `DatabaseName`** - set it, then press
**Create DB** and **Open system**. On open, `PopulateClass` reads `MTB.txt`,
`Frame.txt`, `Gear.txt`, `Brake.txt` and `Wheel.txt` from the working directory
and creates one object per line. Nothing links them, so every bike starts empty
and the trees start flat. Building bikes is the exercise.

## The model

| Class | Members |
| --- | --- |
| `MTB` | `Name`; `builtAround` to one `Frame`; `consistsOf` to many `Parts` |
| `Frame` | `Name`, `FrameSize`; `partOf` back to `MTB` |
| `Parts` | `Model`; `partOf` back to `MTB` |
| `Brake`, `Gear`, `Wheel` | subclasses of `Parts` |

`MTB_Comps` is the link class behind `consistsOf`. Both trees follow
`brhTreeRoot`, a `TBoldReferenceHandle`: **Show all MTB** sets it to the whole
`MTB` class extent, **Set Current as Root** to `CurrentTreeView.CurrentElement`,
so any node can become the root of the same tree.

## Basic tree: descriptions resolved by type

`btrvBasic` has five node descriptions - `MTB`, `Brake`, `Frame`, `Gear`,
`Wheel` - each with a `ContextTypeName` equal to its name. The `MTB` description
has two parts, `ElementExpression = 'builtAround'` with `InterpretAsList` False
and `ElementExpression = 'consistsOf'` with it True. Both carry
`ControllerExpression = 'self.ocltype'`, which tells the tree to pick whichever
description matches the child's class. That is why a `consistsOf` list of
brakes, gears and wheels renders three different node types with no branching
anywhere. `IconController.Expression = '-1'` throughout, so this tree has no
icons.

## Enhanced tree: descriptions resolved by name

`btrvEnhanced` starts from the same `MTB` element, but its `MTB` description
lists four parts with no element expression at all:

```
ControllerExpression = '''BuiltAroundFrame'''
ControllerExpression = '''ConsistsOfBrakes'''
ControllerExpression = '''ConsistsOfWheels'''
ControllerExpression = '''ConsistsOfGears'''
```

A `ControllerExpression` that evaluates to a string names a node description
directly. Those four descriptions also have `ContextTypeName = 'MTB'`, so they
run against the bike itself, and each supplies a constant caption plus a
filtered child list:

| Description | TextController | ElementExpression |
| --- | --- | --- |
| `BuiltAroundFrame` | `'Frame'` | `builtAround` |
| `ConsistsOfBrakes` | `'Brakes'` | `consistsOf->select(oclIsTypeOf(Brake)).oclAsType(Brake)` |
| `ConsistsOfWheels` | `'Wheels'` | `consistsOf->select(oclIsTypeOf(Wheel)).oclAsType(Wheel)` |
| `ConsistsOfGears` | `'Gears'` | `consistsOf->select(oclIsTypeOf(Gear)).oclAsType(Gear)` |

The `oclAsType` after the `select` gives the result a concrete type, so
`self.ocltype` on the children resolves to the `Brake`, `Wheel` or `Gear`
description. All four set `HideNodeWithNoChildren = True`, so a bike with no
brakes shows no Brakes group. `IconController.Expression` is a literal index
into `imlTreeNodeIcons`; the `16_*.bmp` files here are that list's source images.

A fifth named description, `IsPartOf`, has no `ContextTypeName` and follows
`partOf`, giving every part a child node listing the bikes it belongs to.

## Drag and drop edits the model

Only `btrvEnhanced` sets `BoldProperties.DragMode = bdgSelection`, which is what
puts the selection into `BoldGuiHandler.DraggedObjects`.

- `OnDragOver` accepts exactly one dragged object, provided the element under
  the cursor is a `TMTB` and the dragged object is a `TParts` or a `TFrame`.
- `OnDragDrop` then calls `TargetObject.ConsistsOf.Add(...)` for a part, or
  assigns `TargetObject.BuiltAround` for a frame.
- `OnStartDrag` records the selected node's *parent* element in `DragContext`,
  but only when it is a `TMTB`. `imgUnlink`, a plain `TImage`, uses that to
  remove the dropped object from that particular bike. Dragging from a grid,
  where there is no parent node, leaves `DragContext` nil and the unlink target
  refuses the drop.

The parts you drag in come from the four `TBoldGrid` controls down the
right-hand side, over `Frame.allInstances`, `Brake.allInstances`,
`Gear.allInstances` and `Wheel.allInstances`. None of them sets the VCL
`DragMode`; `TBoldCustomGrid` starts the drag itself when the mouse goes down on
the narrow row-indicator column, which is the unnamed first column in each
grid's `Columns` collection. `BoldListBox1`, over `blhComponents`
(`consistsOf` of the current bike), does set `DragMode = dmAutomatic` and is the
convenient place to drag a fitted part back off.
