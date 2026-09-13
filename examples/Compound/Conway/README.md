# Conway's Game of Life

The whole simulation lives in the model. Every cell on the board is a Bold
object, the visible board is a derived attribute, and typing into the memo runs
the derivation backwards to create and delete objects.

It is the clearest demonstration in this repository of three things that are
hard to show on a business model: OCL derivation chained across associations, a
qualified association used as a two-dimensional index, and reverse derivation.

## Running it

Open `Conway.dpr`. There is no `.dproj` in this folder, so the IDE will offer
to create one. Accept, but note that **the generated project will not compile as
it stands**. The IDE writes a project file with an empty unit search path, and
the first Bold unit then fails:

```
[dcc32 Fatal Error] fMain.pas(15): F2613 Unit 'BoldSubscription' not found.
```

The unit is present, at `Source\Common\Subscription\BoldSubscription.pas`. The
compiler simply has nowhere to look. Add Bold's source folders to the project's
unit search path, using the relative form `..\..\..\Source\...` so the project
stays portable. The quickest source of a correct list is the search path in
`examples\Simple\Tools\OclWorkbench\OclWorkbench.dproj`, which sits one level
deeper, so replace its leading `..\..\..\..\` with `..\..\..\`.

With that done the project builds clean, at roughly 229,000 lines. The one hint
that remains, an unused private `ActiveCount`, comes from the generated
`ConwayClasses_Interface.inc` and is not worth chasing.

No database is involved. `BoldSystemHandle1` has no `PersistenceHandle` and
`AutoActivate` is True, so the system opens empty in memory when the form is
created. Nothing is ever saved.

The demo reads nothing at startup and needs no particular working directory.
Press **Help** for a summary of the controls and the rules. `Instructions.txt`
is left in the folder but nothing loads it any more.

Then:

1. **Load pattern** reads a starting board from a text file and **Save
   pattern** writes the current one back out, so anything you draw can be kept.
   `Initial.txt` and `zip2.lif.txt` both ship here. You can also type `*`
   characters into the memo directly, where a space is a dead cell.
2. **Click a square in the memo** to toggle it between live and dead. Clicking
   just past the end of a row extends that row, which is how you draw on an
   empty board.
3. **One generation** advances the board once, **Start ticking** runs it on a
   timer.
4. **Help** explains the controls and states the rules.
5. The two track bars change the timer interval and the memo font size. Both are
   bound to model attributes, not to the controls. **The timer interval starts
   at zero**, so move that track bar before pressing Start ticking or the timer
   is enabled but can never fire.

### Two board conventions

`Initial.txt` is this demo's own format: an asterisk is a live cell and
everything else is dead. `zip2.lif.txt` is Life 1.05, which marks a dead cell
with a full stop and starts header and comment lines with `#`. Loading
translates the second into the first, so either can be opened.

### What clicking actually does

Nothing in the click handler touches a cell object. It edits one character of
the board text and assigns it back to `Game.Board`, which is a reverse derived
attribute, so the assignment runs `_board_ReverseDerive` and that rebuilds the
cell objects from the text. Clicking is therefore the same operation as typing,
only aimed.

One consequence is worth knowing. The board is rendered cropped to the live
cells plus their neighbours, so its origin moves whenever the extent changes.
On a small or nearly empty board a click can land somewhere different from
where you expect, because the text shifted under the mouse after the previous
edit. On a populated board the extent is stable and it behaves as you would
want.

## The model

Two classes, both descending from `BusinessClassesRoot`.

| Class | Member | Kind |
| --- | --- | --- |
| `Game` | `TimerTime`, `Generations`, `FontSize`, `collecting` | persistent scalars, defaults 1000, 0, 8 |
| | `xMin`, `xMax`, `yMin`, `yMax` | transient, the current extent of the live board |
| | `xSize` | derived, `xMax - xMin + 1` |
| | `board` | Blob, derived **and** reverse derived, transient |
| `Cell` | `x`, `y`, `Active` | persistent scalars |
| | `ActiveCount` | derived, `if active then 1 else 0 endif` |
| | `neighbours` | derived, the sum of the eight neighbours' `activeCount` |
| | `Intermediate`, `NeighboursEnsured` | `Bold.AttributeKind=Delphi`, plain fields with no Bold semantics |

The associations are where the interesting part is.

| Association | Shape |
| --- | --- |
| `Game.cell` / `Cell.game` | 1 to 0..*, the whole population |
| `Game.coord[x, y]` | a **qualified** role to `Cell`, qualifiers `x` and `y`, both Integer |
| `Cell.cLeft`, `cRight`, `cUp`, `cDown` | single roles, each derived from the qualified role |
| `Cell.cUpLeft`, `cUpRight`, `cDownLeft`, `cDownRight` | single roles, each derived by composing two of the four above |

## The neighbour count is OCL, not code

`Cell.neighbours` is derived by this expression, stored in the model as
`Bold.DerivationOCL`:

```
cDown.activeCount + cUp.activeCount + cLeft.activeCount + cRight.activeCount +
cDownRight.activeCount + cDownLeft.activeCount + cUpRight.activeCount + cUpLeft.activeCount
```

Each of those eight roles is itself derived. The four orthogonal ones index the
qualified association:

```
game.coord[x-1, y]      game.coord[x+1, y]
game.coord[x, y-1]      game.coord[x, y+1]
```

and the four diagonals are composed rather than indexed again:

```
cDown.cLeft     cUp.cRight     cDown.cRight     cUp.cLeft
```

So a cell finds its neighbours by navigation, and Bold's deriver places the
subscriptions. When a neighbour's `active` flips, `activeCount` is invalidated,
which invalidates `neighbours`, which invalidates the board. No notification
code is written anywhere in this demo.

`Cell.SetupCell` is what puts a cell into the index: it sets `x` and `y` and then
calls `Game.m_Coord.Add(Self)`.

## The board is a derived blob, both ways

`Game.board` is a derived Blob with `Bold.ReverseDerive=True`, and the memo is
bound straight to it:

```
BoldMemo1.BoldProperties.Expression = 'board'
```

`TGame._board_DeriveAndSubscribe` builds the string. It subscribes to the
`cell` link with `breResubscribe` so that it is called again when cells are
created or destroyed, subscribes to `M_Active` on every cell, then paints a
`TStringList` of the current bounds and assigns `m_Board.AsString`.

`TGame._board_ReverseDerive` runs the other way when you edit the memo. It
clears every cell, reads the text back, and for each position either updates the
existing cell through the qualified role `Coord[x, y]` or creates one. That is
what makes typing `*` into the memo a legitimate way to seed the model.

## One tick, two passes, one transaction

`TGame.Tick` cannot update cells in place, because a cell's fate depends on its
neighbours' current state. It runs two passes inside a single transaction:

```pascal
BoldSystem.StartTransaction;
Generations := Generations + 1;
for i := 0 to CellCount do Cell[i].CalculateIntermediate;
for i := 0 to CellCount do Cell[i].UpdateActive;
BoldSystem.CommitTransaction;
```

`CalculateIntermediate` reads `neighbours` and writes the plain Delphi field
`Intermediate`; `UpdateActive` copies it into `Active` and calls
`EnsureNeighbours`.

## The universe has no edges

The board is not a fixed grid. `EnsureNeighbours` creates the eight neighbours
of any living cell that does not have them yet, so the population grows outwards
as the pattern spreads, and `Game.UpdateBounds` widens `xMin..yMax` as it goes.

That would grow without limit, so `Tick` ends with a crude rule: if more than 80
percent of the cells went inactive this generation, run `GarbageCollect`, which
deletes every cell where `AllowRemove` is true (not active and no active
neighbour) and then recomputes the bounds. `TCell.PrepareDelete` clears the
`NeighboursEnsured` flag on the survivors, so they will recreate their
neighbours if they need them again.

`Collecting` is set to True around that sweep and `TBoldQueueable.DisplayAll` is
called, purely so the bound check box flickers and you can see it happen.

## Driving plain VCL properties from the model

Two `TBoldPropertiesController` components push model values into properties of
non-Bold components:

| Controller | Expression | Drives |
| --- | --- | --- |
| `bpcTimerInterval` | `timerTime` | `Timer1.Interval` |
| `bpcFontSize` | `fontSize` | `BoldMemo1.Font.Size` |

Both use `ApplyPolicy = bapChange`, and the track bars write back to the same
attributes. `Timer1.Interval` is 0 in the form file; it gets its value from the
model as soon as the system opens.

The two labels at the left are ordinary `TBoldLabel`s, one on `generations` and
one on:

```
cell->select(active)->size
```

which is the live count of living cells.

## Files

| File | What it holds |
| --- | --- |
| `Conway.dpr` | creates only `TfrmMain` |
| `fMain.pas` / `.dfm` | the form, the handles, and the model itself in `BoldModel1.Model` |
| `ConwayClasses_Interface.inc` | generated class declarations |
| `ConwayClasses.inc` | every method body: `Tick`, the two board derivations, `EnsureNeighbours`, `GarbageCollect` |
| `Instructions.txt` | shown in a message box at startup |
| `Initial.txt`, `zip2.lif.txt` | patterns to paste into the memo |

There is no separate data module and no `.bld` file. The model is stored only
inside `fMain.dfm`, as the `Model` property of `BoldModel1`.
