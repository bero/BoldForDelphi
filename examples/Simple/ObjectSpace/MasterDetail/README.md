# Master-Detail

A detail list that follows the selected master row, with no code. Root one
`TBoldListHandle` on another and give it a role name as its expression; Bold
tracks the master's cursor and re-evaluates the role for whatever is current.

This is the demo to start with. Together with its FMX twin it is the only demo
under `ObjectSpace` with a `.dproj` and the only one that needs no InterBase:
it runs on SQLite through FireDAC out of the box. It is in
`examples\DelphiExamples.groupproj`. It is a VCL program, so it runs on
Windows.

The same demo in FireMonkey, through `TBoldDataSet` and LiveBindings, is in
[`FMX`](FMX/README.md) (#117). It also runs on Linux64.

## Running it

1. Build `MasterDetail.dproj` with the **Debug** configuration for Win64 (the
   project still has Win32, which compiles but is no longer tested).
2. Check `MasterDetail.ini`. It ships with `Persistence=FireDAC` and
   `Type=SQLite`, writing `BoldDemo.db` next to the executable, which needs
   nothing installed. To use another engine, change `Type` and fill in that
   engine's section; the keys are described in
   [Database configuration](../../../../docs/examples/index.md#database-configuration).
3. Run. The form opens the system on create and offers to create the database
   if it is not there.

The status strip along the bottom names the config file in force, the
persistence type, the database type, whether persistence is connected and
whether the Bold system is active. **Drop Database** closes the system,
discards anything dirty, and drops it, so you can start over.

## The handle chain

Three list handles; the system handle is `DemoDataModule.BoldSystemHandle1`:

| Handle | Root | Expression | Grid |
| --- | --- | --- | --- |
| `lhaProjects` | the system handle | `Project.allInstances` | `grdProjects`, top |
| `lhaProjectTasks` | **`lhaProjects`** | `tasks` | `grdTasks`, bottom left |
| `lhaTasks` | the system handle | `Task.allInstances` | `BoldGrid1`, bottom right |

The second handle is rooted on the first, not on the system, and that single
property is the whole master-detail mechanism. Select a different project and
the bottom left grid changes while the bottom right one does not.

Putting the filtered and unfiltered lists side by side is deliberate: the right
grid adds a `project.name` column, so you can confirm that the left one is
showing the same rows, filtered.

`BoldLabel1` in the header counts the master list with

```
'(' + Project.allinstances->size.asString + ')'
```

and `grdTasks` shows `dueDate`, which is derived in the shared model as

```
if project.isNull then nil else project.enddate + 7 endif.floatAsDateTime
```

so a task shows a due date only once it belongs to a project.

## The shared model

The classes come from [`examples/Shared`](../../../Shared), not from this
folder: `DemoClasses.pas` generated from `DemoModel.bld`, and `TDemoDataModule`
which reads the `.ini` and builds the FireDAC, UniDAC or XML persistence chain
(UniDAC only when the project defines `UNIDAC`). The OCL Workbench and Save and
Generate demos in `Tools` use the same model, so changes here are visible
there.

`TProject.CompleteCreate` and `TTask.CompleteCreate` name new objects
`Project1`, `Task1` and so on from a module-level counter. On its own that
counter would restart at 1 after every run and collide with what is already
saved, so `HandleSystemOpened` calls `TProject.InitializeCounter` and
`TTask.InitializeCounter`, which scan the existing objects for the highest
number already used and continue from there.

## Clearing

**Clear All Data** deletes every project:

```pascal
List := dmDemo.BoldSystemHandle1.System.EvaluateExpressionAsNewElement('Project.allInstances') as TBoldList;
try
  for i := List.Count - 1 downto 0 do
    (List[i] as TBoldObject).Delete;
finally
  List.Free;
end;
```

`EvaluateExpressionAsNewElement` returns a list you own and must free, which is
why it is wrapped in `try ... finally`. Deleting backwards matters for the same
reason as everywhere else: the list shrinks as you go.

Nothing is written until **Save Changes**, and closing the window with dirty
objects asks first.

## Files

| File | What it holds |
| --- | --- |
| `MasterDetail.dpr` | creates `TDemoDataModule`, then `TMainForm` |
| `DemoForm.pas` / `.dfm` | the three handles, three grids, status strip |
| `MasterDetail.ini` | persistence type and per-engine connection settings |

The model, the generated classes and the data module live in
[`examples/Shared`](../../../Shared). `BoldDemo.db` (or `BoldDemo.xml` with XML
persistence) is created next to the executable by the first run and is not in
the repository.
