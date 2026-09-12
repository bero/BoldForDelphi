# Master-Detail

A detail list that follows the selected master row, with no code. Root one
`TBoldListHandle` on another and give it a role name as its expression; Bold
tracks the master's cursor and re-evaluates the role for whatever is current.

This is the demo to start with. It is the only one in this folder with a real
`.dproj`, the only one in `examples\DelphiExamples.groupproj`, and the only one
that runs against something other than InterBase.

## Running it

1. Build `MasterDetail.dproj` with the **Debug** configuration.
2. Check `MasterDetail.ini`. It ships with `Persistence=FireDAC` and
   `Type=SQLite`, writing `BoldDemo.db` next to the executable, which needs
   nothing installed. Sections for SQL Server, PostgreSQL, Firebird, MySQL,
   MariaDB, Oracle and an XML file are there too.
3. Run. The form opens the system on create and offers to create the database
   if it is not there.

The status strip along the bottom names the config file in force, the
persistence type, the database type, whether persistence is connected and
whether the Bold system is active. **Drop Database** closes the system,
discards anything dirty, and drops it, so you can start over.

## The handle chain

Three list handles, all pointing at `DemoDataModule.BoldSystemHandle1`:

| Handle | Root | Expression | Grid |
| --- | --- | --- | --- |
| `lhaProjects` | the system handle | `Project.allInstances` | `grdProjects`, top |
| `lhaProjectTasks` | **`lhaProjects`** | `tasks` | `grdTasks`, bottom left |
| `lhaTasks` | the system handle | `Task.allInstances` | `BoldGrid1`, bottom right |

Only the root differs between the second and the third, and that single
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

The classes come from `examples\Shared`, not from this folder:
`DemoClasses.pas` generated from `DemoModel.bld`, and `TDemoDataModule` which
reads the `.ini` and builds the FireDAC, UniDAC or XML persistence chain. The
OCL Workbench demo uses the same model, so changes here are visible there.

`TProject.CompleteCreate` and `TTask.CompleteCreate` name new objects
`Project1`, `Task1` and so on from a module-level counter. A counter in a
variable would restart at 1 after every run and collide with what is already
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
| `BoldDemo.db`, `BoldDemo.xml` | sample data left behind by earlier runs |

The model, the generated classes and the data module live in
`examples\Shared`.
