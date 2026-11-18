# Master-Detail - FMX

The Master-Detail demo of the folder above in FireMonkey (#117 step 10):
projects at the top, the tasks of the selected project at the bottom left, and
all tasks at the bottom right.

## How it works

The handle chain is the one of the VCL demo:

| Handle | Root | Expression | Grid |
| --- | --- | --- | --- |
| `lhaProjects` | the system handle | `Project.allInstances` | `grdProjects`, top |
| `lhaProjectTasks` | **`lhaProjects`** | `tasks` | `grdProjectTasks`, bottom left |
| `lhaTasks` | the system handle | `Task.allInstances` | `grdTasks`, bottom right |

Only the root differs between the second and the third, and that single
property is the whole master-detail mechanism: select another project and the
bottom left grid follows, with no code.

FMX has no Bold-aware controls, so the grids are reached the way the Invoicing
FMX client (`examples\Compound\Invoicing\FMX`) reaches them: each handle feeds
a `TBoldDataSet`, and LiveBindings binds the dataset to an FMX `TStringGrid`
(a `TBindSourceDB` and a `TLinkGridToDataSource`). A `TBindNavigator` on each
bind source takes the place of `TBoldNavigator`: its Insert button makes a new
object in the list of its handle, so a task inserted at the bottom left belongs
to the selected project at once. The grids are editable; the derived `dueDate`
(the end date of the project plus 7 days) is read-only.

The handles, datasets, bindings and controls are components in the form file,
`fMasterDetailMain.fmx`. The system handle is in the data module, which the
code creates, so the code gives the handles their system and the two lists of
all instances their root.

The project count next to **Projects** is the OCL expression of the VCL
demo's `TBoldLabel`, `'(' + Project.allInstances->size.asString + ')'`,
evaluated from FMX's idle loop, which also drains Bold's display queue.

## The data module

The VCL demo's data module (`examples\Shared\DemoDataModule.pas`) holds a VCL
action list and asks its questions with VCL dialogs, so this client has its
own: `dmMasterDetail.pas`, framework neutral, with SQLite through FireDAC. It
loads `examples\Shared\DemoModel.bld`, the model the shared `DemoClasses`
unit is generated from, so the classes and their code are the VCL demo's.
`MasterDetailFMX.db` next to the exe is created on the first run; the other
databases and the XML file of `MasterDetail.ini` are not offered here.

New objects are named `Project1`, `Task1` and so on by the model code, and
after the system opens the counters continue from the highest number in the
database, as in the VCL demo.

**Clear All Data** deletes every project (their tasks go with them), **Save
Changes** writes to the database, and **Drop Database** deletes the file and
opens an empty one. Closing the window with unsaved changes asks first.

## No VCL

The project defines `BOLD_NO_VCL`, so Bold's control packs, which
`TBoldDataSet` builds on, compile without their VCL parts and link no VCL
unit (see `BOLD_VCL` in `Source\Common\Include\bold.inc`).

## Running it

Build `MasterDetailFMX.dproj` for Win32 or Win64 and run it.

The project also has the Linux64 platform (Delphi 13 with FmxLinux). Its
deployment puts `DemoModel.bld` next to the program, where the client looks
for the model first, before `examples/Shared`; the Linux machine needs
`libsqlite3-dev`. The build needs the two FmxLinux workarounds described in
the Linux64 section of the Invoicing FMX client's README
(`examples/Compound/Invoicing/FMX`). Its self-test passes on Linux64; the
dialogs of Clear All Data, Drop Database and closing have not been tried by
hand there.

`MasterDetailFMX --selftest` shows no window: on a temporary database it
makes projects and tasks through the navigators, selects projects, edits
through the grids' datasets, saves, opens the system again, clears and drops,
checks what the grids show, writes the checks to
`MasterDetailFMX.selftest.txt` and exits with the number of failed checks.

## Files

| File | What it holds |
| --- | --- |
| `MasterDetailFMX.dpr` | creates the form; `--selftest` runs the self-test |
| `fMasterDetailMain.pas` / `.fmx` | the handles, datasets, bindings, grids and navigators |
| `dmMasterDetail.pas` / `.dfm` | the model, the system handle and SQLite persistence |
