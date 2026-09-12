# OCL Workbench

A place to type an OCL expression, run it against a live Bold system, and see
exactly what came back: the OCL type, the Delphi class, and every element.

It exists because Bold ships two OCL user interfaces that no sample project ever
used. This demo puts both to work and shows what each is for.

## Running it

1. Build with the **Debug** configuration. No DevExpress, no UniDAC, nothing
   beyond a stock Delphi with FireDAC.
2. Run `OclWorkbench.exe`. It offers to create `OclWorkbench.db`, a SQLite file
   next to the executable. Answer yes. If you answer no, everything that needs a
   live system greys out until you choose **File, Open system**.
3. Choose **File, Create sample data**, then say yes when it offers to save.
4. Pick an expression from the tree on the left and press **Evaluate**, or F9.

**File, Clear all data** deletes every object in the model and saves, after asking
first. Use it to get back to an empty system without deleting the database file,
then seed again. It is worth reading the handler: deleting one end of an
association also deletes the link objects hanging off it, so objects disappear
before the loop reaches them, and the list that `allInstances` returns is the live
class extent that shrinks underneath you. The code snapshots the references first
and checks `BoldObjectExists` before each delete.

**File, Close system** saves any pending changes and disconnects, which is the
quickest way to see the difference between a snapshot result and a live one: with
the system closed the Result tab keeps what it last printed, while the Live list
tab empties.

The sample data is written to the database once you save it, so step 3 is a
one-off. Anything you create or change afterwards is held in memory until you
save; closing the window with unsaved objects asks you first and names the
count.

## The two indicators in the status bar

Two questions come up constantly and are easy to get wrong, so they are colour
panels at the bottom right rather than numbers in a sentence.

**The database panel** answers whether the chosen engine has a database behind
it.

| Colour | Meaning |
| --- | --- |
| green | the engine's database is open, so it exists and carries a Bold schema |
| red | the engine is file based and that file is not there yet |
| amber | the system is closed, so the database was not checked |

An open system is proof on its own: Bold could not have opened it otherwise.
The red state is only ever reported for SQLite, Firebird and XML, because a file
check costs nothing. Answering the same question for a server needs a
connection, which belongs to the Database menu's precondition ladder, not to an
idle tick.

**The save panel** answers whether everything is written.

| Colour | Meaning |
| --- | --- |
| green | every object is saved |
| amber | the count of created, changed or deleted objects not yet written |
| grey | nothing is loaded |

Bold tracks all of that in `DirtyObjects`, so the panel needs no bookkeeping of
its own. It refreshes on idle, which means editing a cell in the grid turns it
amber immediately.

## What each part of Bold is doing

### Direct evaluation

`TBoldElement.EvaluateExpression` is the general entry point. Hand it a string
and an empty `TBoldIndirectElement` and it fills the latter in. The result can be
anything the model can express, which is why the **Result** tab reports the OCL
type and the Delphi class before it reports a value. Expressions ending in
`->size` come back as an Integer, `->isEmpty` as a Boolean, `allInstances` as a
list, and navigating an unset link comes back as nil.

Evaluation is a one-shot: the answer is a snapshot, and nothing updates it.

### The list handle

The **Live list** tab assigns the same expression to
`TBoldListHandle.Expression`. That is a different proposition. The handle
subscribes to everything the expression touched, so the grid re-evaluates by
itself when the underlying objects change. Only list-valued expressions can
drive a list handle, which is why the tab says so when the result is a scalar.

The grid's columns are built at runtime. **Columns from result type** walks
`TBoldClassTypeInfo.AllMembers` and adds one column per member, which is the
same model reflection the IDE property editors use to fill their member lists.
Multi-valued roles are left out, because a collection has no rendering in a
single cell.

A derived member gets a leading slash in its column header, which is how UML
writes one, and the header is italic as a second cue. The Messages pane then
prints the derivation itself, so pressing the button on `Person.allInstances`
reports that `/fullName` is derived as `firstName + ' ' + lastName`. A derived
member is computed on demand and has no column in the database, which is also
why filtering on one cannot be pushed into SQL.

### Evaluate in PS

The checkbox switches evaluation from memory into the persistent storage, where
Bold translates the expression into SQL. Category 13 in the sample tree is built
around it: two expressions that translate cleanly, and two that cannot. A
derived transient attribute has no column to filter on, and counting across a
link is beyond the translator, so both fall back to memory.

Whether an expression can be translated is not a question the type system can
answer, so the workbench does not ask it. It runs the expression once with PS
enabled and sees what happens. Being an object list is necessary but not
sufficient: `Person.allLoadedObjects` is registered as an object list and passes
any type check, yet "loaded" is an in-memory notion with no SQL symbol behind it.
On refusal the log shows Bold's own message, which names the real cause, and the
expression is then evaluated in memory.

One consequence worth knowing when you run this under the IDE debugger: a refusal
reaches the workbench as an exception that it catches. Delphi still notifies you
on the raise, so expect an exception notification on every expression that cannot
be translated, not just on the ones that end in an error. Press Continue. If that
becomes tiring, add `EBoldOCLError` and `ESSYacc` to the ignore list under Tools,
Options, Debugger, Embarcadero Debuggers, Language Exceptions.

### BoldOclPropEditor

`TBoldOclPropEditForm` is the dialog behind the ellipsis on every OCL property
in the IDE. **Tools, Edit in the Bold OCL editor**, or F2, opens it on whatever
is in the expression memo. It needs three things:

| What | Where it comes from here |
| --- | --- |
| `Context` | the system handle's type, so expressions start at a class name |
| `OclExpr` | the current text of the expression memo |
| `Variables` | the workbench's `TBoldOclVariables` list |

The dialog type-checks against the model as you type and reports the result
type. That is the part worth watching: it answers "would this parse, and what
would it yield" without touching a single object.

### The same editor as a repair dialog

**Tools, Repair failing OCL at runtime** installs a
`TBoldOclGraphicRTDebugger` into the global `BoldOCLRTDebugger`. From then on
every OCL failure anywhere in the application opens that same editor with the
broken expression loaded, and Bold retries with whatever you correct it to,
remembering the correction for the rest of the session. Turn it on, type
something misspelt, and evaluate.

This is how the OCL editor was meant to be used against a shipped application
whose expressions live in DFMs rather than in code.

### OCL variables

The list at the lower left is bound to a variable named `current` through
`TBoldOclVariables.AddVariable`, with `UseListElement` set, so it resolves to the
selected person rather than to the whole list. The same variable list goes to the
evaluator and to the OCL editor, so the editor knows the name too. Select
somebody and evaluate `current.ownedBuildings` or `current.assignedTasks`.

## Changing the database engine

The **Database** menu lists every engine the shared `.ini` has a section for:
SQLite, SQL Server, PostgreSQL, Firebird, MariaDB, Oracle and an XML file. The
menu title names the one in force, and the chosen one is ticked.

Picking one does not just change a setting. It first walks a ladder of
preconditions and refuses to switch if any rung fails, because switching to an
engine that cannot connect trades a clear diagnosis for a broken application.
The rungs:

1. the `.ini` exists and has a section for that engine
2. the keys that engine needs are filled in, and no password is still the `<PW>`
   placeholder the template ships with
3. FireDAC has a driver linked for it
4. the client library exists **and has the same bitness as this build**
5. the **server** is reachable and accepts those credentials
6. the **database** the demo wants opens
7. the Bold schema is present in it

Rungs 5 and 6 are separate on purpose. One connection attempt cannot tell a
refused login from a missing database, and SQL Server makes that worse by
reporting a missing database as `Login failed for user '<account>'`, which reads
exactly like a password problem. So the server is asked first, against a database
that always exists: `master` for SQL Server, `postgres` for PostgreSQL,
`information_schema` for MariaDB. Only then is the real database tried. Oracle has
no equivalent, being organised by schema rather than by database, so it keeps the
single attempt.

That split is what lets a missing database be a **warning** rather than a failure:
the switch goes ahead and the demo offers to create it.

The full report goes to the Messages pane and only the verdict appears in a
dialog, because a ten-line report in a message box is unreadable. A real one
looks like this:

```
  OK   FireDAC driver PG
         Registered
  FAIL Client library
         C:\Program Files\PostgreSQL\18\bin\libpq.dll is 64-bit,
         and this build is 32-bit
         -> Point VendorLib at the 32-bit client DLL, or rebuild the demo
            for Win64. A 32-bit process cannot load a 64-bit library,
            whatever the path says.
```

Rung 4 earns its place. Installing a 64-bit PostgreSQL puts a 64-bit `libpq.dll`
at exactly the path the `.ini` names, and a 32-bit demo cannot load it. The path
is right, the file is there, and nothing works. FireDAC does mention bitness,
but only after the connection fails and only as a hint.

**Tools, Check preconditions** runs the same ladder against the engine already
in force, which is the quickest way to find out why a working setup stopped
working.

Rungs 5 and 6 open a real connection, so a check takes a moment and the cursor
turns to an hourglass. The connection carries a five second login timeout so a
switched-off host cannot freeze the window.

Switching happens in the running process. `TDemoDataModule.ReloadConfiguration`,
added in `examples/Shared`, closes the Bold system, frees the FireDAC
connection, adapter and persistence handle, re-reads the `.ini` and builds them
again. No restart.

## The OCL Explorer

`Source\UMLModel\Editor\BoldOCLExplorer.pas` holds `TOclExplorerForm`, a two-pane
OCL browser: every class on the left with its instance counts, a drill-down list
on the right, an Edit OCL button on each, and an Evaluate in PS box on each.
**Tools, OCL Explorer** opens it, and it is part of the ordinary build.

It was not always. Until this demo existed the form was referenced by no project,
no package and no `.dproj` in the repository, and it needed both DevExpress and a
unit that survives only in `Source\Deprecated`. Two changes fixed that.

**A crash in the DevExpress grid bridge.** The nested `Prefetch` inside
`cxGridBoldSupportUnit` cast its list to `TBoldObjectList` with no check.
`TBoldTypeList` and `TBoldObjectList` are siblings under `TBoldList`, so for a grid
bound to a list of types, as the explorer's left pane is through `allSubClasses`,
the cast was invalid and reading `Locators[0]` dispatched through a foreign field.
The enclosing `PreFetchColumns` already branched on `IsObjectList` in four places;
only the nested routine missed it.

**The form no longer uses DevExpress at all.** The two `TcxGrid` views became
`TBoldGrid`, the seven computed columns moved from `TBoldAsVariantRenderer` to
`TBoldAsStringRenderer`, and the import of the deprecated `BoldVariableDefinition`
went away. The coupling turned out to be almost entirely declarative: the
implementation held exactly one line of DevExpress logic. The executable went from
26.7 MB to 10.6 MB, and 1.2 million compiled lines to 271 thousand.

What was lost is the DevExpress grid itself: sorting, filtering and grouping, and
the per-column editors. The boolean columns read as a tick or nothing rather than
as check boxes. What was gained is a form anyone can compile.

One wrinkle if you edit that form. `TBoldGridColumn.Width` is declared
`stored False`, and its setter writes straight into the grid's `ColWidths`, which
do not exist while the form is streaming. Widths therefore cannot live in the
`.dfm`, and `SetColumnWidths` applies them from `FormCreate` instead.

The **DevEx** build configuration is now redundant. It still exists and still
builds, but it defines a symbol nothing reads and produces the same executable as
Debug. It can be deleted.

## The model

Everything runs against the shared `DemoModel` in `examples\Shared`, the same
model the Master-Detail demo uses. It is worth knowing its shape, because the
sample expressions are built around it:

- `Person` with `firstName`, `lastName`, `assets`, `birthDate`, `isActive` and a
  derived `fullName`.
- `Building` with two subclasses, `CommercialBuilding` and
  `ResidentialBuilding`, which is what makes `oclIsKindOf` and `oclAsType`
  demonstrable.
- `Ownership`, an association class between `Person` and `Building` carrying
  `sharedValue` and `acquiredDate`. Navigating to it and navigating past it are
  different expressions, and both appear in the tree.
- `Project` and `Task`, where `Task.dueDate` is derived as `project.endDate + 7`
  and copes with a project that is not set.

The seeded population is deliberately uneven: one building has two part owners,
one has none at all, one project has no manager, and two people hold no tasks.
Several of the sample expressions are only interesting because of those gaps.

## Notes

- `TProject.startDate` is generated into the private section of its class, so
  `OclWorkbenchSamples` sets it through `BoldMemberByExpressionName` instead.
  OCL reads it either way, which is a fair illustration of the model being the
  authority rather than the generated Delphi.
- OCL indexes from 1. `->at(1)` is the first element and `->at(0)` is an error.
- The expression and result panes use Consolas on purpose. Everything else is
  Segoe UI at 9 point.
