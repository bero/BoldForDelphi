# Model editor

A standalone model editor for Bold (#128): a program that opens and saves
`.bld` model files, shows the model for editing and generates the business
classes, for Delphi and Lazarus projects alike, without the Delphi IDE.

Use it where the Delphi IDE is not at hand: with Lazarus or on Linux. Bold's
classic model editor (`Source/UMLModel/Editor`) is VCL and runs inside the
Delphi IDE, through the design package `dclBold`. So far the standalone editor
edits the classes and their attributes, and checks the model as Bold's model
editor does.

It follows the pattern of the Invoicing example: a part without user
interface, and two programs on top of it, an LCL client built with Lazarus and
an FMX client built with Delphi, both for Windows and Linux.

## Layout

| Folder | What it holds |
| --- | --- |
| `Core` | what both clients share: `BoldModelDocument.pas` (open a `.bld` file, save it, generate the business classes) and `BoldModelEditorData.pas` (the handles, the datasets and the commands of the editor) |
| `LCL` | the LCL client, `ModelEditorLCL.lpi`, with `build.sh` |
| `FMX` | the FMX client, `ModelEditorFMX.dproj` (Win64, Linux64) |
| `Tests` | the tests and the Delphi test program `ModelEditorTests.dpr` with `build.ps1` |
| `FPC` | the Free Pascal test program `ModelEditorTests.lpr` with `build.sh` |

## The model document

`TBoldModelDocument` holds a `TBoldModel`:

- `Open(FileName)` reads a `.bld` file and `NewModel` makes an empty one,
  Bold's `New_Model` with its root class. The model's UML model, `UMLModel`,
  is what a client shows and edits.
- `Save` and `SaveAs(FileName)` write the UML model back as the Delphi
  IDE's `.bld` link does, with CRLF line breaks on every platform, so a file
  saved on Linux and on Windows is the same.
- `GenerateCode(Folder)` runs Bold's code generator with the Delphi
  templates, as the IDE's Save and Generate does: the unit named by the
  model's tagged value `Bold.UnitName`, its `_Interface.inc`, and an include
  file with method stubs, which is written only where it does not exist yet.
- `LoadTypeNames(FormFile)` reads the type names, which map the model's
  attribute types to Delphi classes, units and persistence mappers, from the
  `TBoldTypeNameHandle` on a text form (`.dfm`, `.lfm`, `.fmx`), as the IDE
  takes them from the handle its `TBoldModel` names. The `.bld` file does not
  hold them. Until a form is read the document uses Bold's own type names,
  which give an attribute type of the project's own, such as the
  CustomAttributes example's `Coordinate`, as a string. New and Open keep
  the type names; a form with no such handle, or with more than one, is
  refused and the type names stay as they were.
- `Validate` checks the model with Bold's `TBoldUMLModelValidator` and the
  document's type names: as the IDE's Tools > Consistency Check does, with
  the database names of the model's mapper, and with the checks of the
  Delphi names that the IDE's Save and Generate adds. The findings are the
  violations of the UML model's validator (`UMLModel.Validator.Violation`),
  each with a severity, a description and the model element it is about; it
  returns the highest severity. A model read from a file the IDE saved is
  flattened and boldified, as the validator needs it, and the check changes
  nothing. A new model is flattened and boldified for the check and given
  back as Bold's `TBoldModel` does it; boldifying gives its root class the
  tagged value `persistence=persistent`, which stays and sets `Modified`.
- `Modified` follows every change of the UML model, made in code or in a
  client's grid, until the next save.

It builds on what Bold has: the `.bld` reader and writer (`BoldBld`), the
conversion between the stored model and the UML model
(`BoldUMLModelConverter`) and the code generator (`BoldGen`). The code it
generates from the Invoicing model is the code the Delphi IDE generated
(`examples/Compound/Invoicing/Model`), apart from the line with the date.

## What the clients share

`TdmModelEditor` (`Core/BoldModelEditorData.pas` with its `.dfm`) is a data
module that both clients create; Delphi and Free Pascal read the same `.dfm`.
It imports no VCL, LCL or FMX unit and nothing from Bold's VCL model editor
in `Source/UMLModel/Editor`. It holds:

- the document, and the Bold handles on its UML model: `rhModel` (the model),
  `lhClasses` (`classes`) and `lhAttributes`, rooted on `lhClasses`
  (`feature->filterOnType(UMLAttribute)`), so the attributes follow the
  selected class, as in the Master-Detail demo; `lhViolations` holds the
  findings of the last check, the most severe first
  (`validator.violation->orderdescending(severity)`, as the IDE's validation
  form lists them);
- `bdsClasses` (name, superclass, abstract, persistent) and `bdsAttributes`
  (name, type, persistent, derived), the `TBoldDataSet`s the clients' grids
  show. They are read-only: a client edits a class or an attribute in a
  dialog of its own. `bdsViolations` (severity, description, element) shows
  the findings and is read-only too;
- the commands: `NewModel`, `Open`, `Save`, `SaveAs`, `GenerateCode`,
  `LoadTypeNames` (with `TypeNamesName`, the form's file name for a status
  line), `AddClass`, `DeleteClass`, `AddAttribute`, `DeleteAttribute`, and what
  enables them (`CanEditClass`, `CanDeleteClass` ...); what the dialogs
  edit, `TModelEditorClassValues` (name, superclass, persistent, abstract)
  and `TModelEditorAttributeValues` (name,
  type, persistent, derived), with `ClassValues` and `NewClassValues` (a
  name no class has, NewClass, NewClass_1 ...) to fill a dialog and
  `ApplyClassValues` or `AddClass(Values)` to take its values, the same for
  attributes; `GetSuperclassNames`, the superclasses the class dialog offers
  a class: every class but the class itself and its subclasses at any
  depth, so no cycle can be chosen (every class for a new one), "none" only
  in a model that is not boldified, and nothing for the root class of a
  boldified model, which keeps no superclass; `ApplyClassValues` and
  `AddClass` refuse any other; and `GetTypeNames`, the types the attribute
  dialog offers (the model names of the document's type names, without the
  abstract ones in angle brackets); a class or an attribute needs a name;
  `Validate`, the check, with
  `ValidationSummary` (the numbers of errors, warnings and hints for a
  status line) and `SelectViolationElement`, which selects what the selected
  finding is about: its class, or its attribute in its class. A new class gets the Bold tagged
  values and, in a boldified model, the root class as its superclass, as in
  Bold's model editor; a deleted class hands its subclasses to its
  superclass; the root class of a boldified model is not deleted. Adding,
  changing and deleting go through these commands, not through the grids;
- the question about unsaved changes: `CloseModel`, which a client calls
  before New, Open and closing, asks through `OnAskSave` (save, discard or
  cancel), and `SaveModel` asks for a file through `OnChooseSaveFile` when
  the model has none yet. A client sets the two events to its own dialogs;
- the window title (`Title`: the file name, a `*` when modified); the unit
  also has `ModelFileFromCommandLine`, the model file named on the command
  line, and `TypeNamesFileFromCommandLine`, the form of `--typenames=<form>`.

A client adds only its controls: the grids bound to the datasets, a menu
item or button per command, and the dialogs: its class and attribute
dialogs, the question about unsaved changes and the file dialogs.

## The LCL client

`LCL/ModelEditorLCL.lpi`: the classes in a `TDBGrid` at the top, the
attributes of the selected class below, each on a `TDataSource` on the shared
dataset, and at the bottom the findings of Model > Check consistency, a
read-only grid whose label sums them up; a double click on a finding selects
what it is about. An action per command (`TActionList`) feeds the File and Model menus
and the buttons, with a hint each. The grids are read-only and select whole
rows; Add class and Edit class (or a double click on a class) open the class
dialog (`fModelEditorClass`), Add attribute and Edit attribute (or a double
click on an attribute) the attribute dialog (`fModelEditorAttribute`), whose
type box offers the type names and takes a typed one; OK is enabled while
the name is not empty. Cancel on Add adds nothing. The project requires the
Lazarus packages `Bold` and `BoldLCL` (`packages/Lazarus`); `Bold` holds the
code generator.

```sh
ModelEditorLCL [--typenames=form.dfm] [model.bld]
```

opens the model file when one is named, with the type names of the form when
one is named (File > Type names from form does the same; the status bar names
the form). `sh Tools/ModelEditor/LCL/build.sh` builds it with lazbuild and
runs `ModelEditorLCL --selftest` on the Invoicing model, with the type names
of the CustomAttributes example's form: it shows no window, checks that the
type names are the form's and that the grids show the classes
and the attributes of the selected class, that the grids are read-only, that
the commands add, edit (with the dialogs' answers given by the self-test, OK
or Cancel), delete, save and generate (into a temporary folder; the model
file is not changed), that the two dialogs, created but not shown, show the
values and give them back,
and that the check lists a class renamed to the name of another, whose
double click selects the class,
writes the checks to `bin/ModelEditorLCL.selftest.txt` and exits with the
number of failed checks. Without a display, `QT_QPA_PLATFORM=offscreen`
does for the Qt5 widgetset.

## The FMX client

`FMX/ModelEditorFMX.dproj`, for Win64 and Linux64 (Delphi 13 with FmxLinux;
Win32 still compiles but is not tested): the same three grids, `TStringGrid`s
bound to the shared datasets through LiveBindings (a `TBindSourceDB` and a
`TLinkGridToDataSource` each; the form gives the bind sources their datasets
when it makes the data module), read-only, and the same two dialogs
(`fModelEditorClass`, `fModelEditorAttribute`, the type in a `TComboEdit`),
opened by the commands or a double click on a cell (`OnCellDblClick`). An
action per command feeds the File and Model menus and the buttons, with a
hint each; the boolean columns show `True` and `False`. The project defines `BOLD_NO_VCL` and its unit search
path leaves out `Source\UMLModel\Editor`; `UnitTest\check_no_vcl.ps1` builds
it and checks that it links no VCL unit. As in the other FMX clients, the
dpr links the resource file with the manifest that the hints need on
Windows.

```text
ModelEditorFMX [--typenames=form.dfm] [model.bld]
ModelEditorFMX --selftest --typenames=examples\Simple\ObjectSpace\CustomAttributes\MainForm.dfm examples\Compound\Invoicing\Model\Invoicing.bld
```

The self-test is the LCL client's, on the grids: it selects a class with
`SelectRow`, as a click does (setting `Row` moves the grid but not the
dataset), and checks that the attribute grid follows, that the commands add,
delete, save and generate (into a temporary folder), that the title and the
status follow, and that the check finds a duplicate class name whose double
click selects the class; the checks go to `ModelEditorFMX.selftest.txt` next to the
program and the exit code is the number of failed checks. It found a bug in
`TBoldDataSet` (the handle followed every row while LiveBindings filled a grid,
#128), fixed with its tests in `UnitTest/Code/Handles/Test.BoldDataSet.pas`.
FMX programs need a desktop session: on Windows run the self-test there, not
over SSH.

## Exceptions and their call stacks

Both clients write an exception that reaches their exception handler to a log
file next to the program (`ModelEditorLCL.log`, `ModelEditorFMX.log`) with its
call stack, then show it; so does one that no handler takes. The programs name
`BoldExceptionLogLCL` and `BoldExceptionLogFMX` in their uses clauses, which
do that (#130; see `docs/concepts/logging.md`, also for where the log goes when
the program's folder cannot be written). The self-tests write the call stack
of an exception to their logs, check one, and check that an exception in the
event loop goes to Bold's log with its call stack and then to their own
handler.

- The LCL client is compiled with `-gl`, so Free Pascal's frames name routine,
  line and file.
- The FMX client takes [Stacker](https://codeberg.org/sivv/stacker) as its
  stack-trace provider, through `Tools/BoldStacker.targets`: from the
  environment variable `STACKER`, else from a folder `stacker` next to the
  repository. The project names the unit `Stacker` first in its uses clause
  (under `BOLD_STACKER`) and imports Stacker's build step, which embeds the
  symbols in the program. Windows gets routine, line and file from the map;
  Linux64 gets routine names from DWARF (the Linux64 build writes no map
  here). When Stacker is not there, the build stops with a message that gives
  the commands to clone it at the tested commit, or says to build without it:
  `STACKER=off`, and the log then gives the address an exception was raised
  at, which `ModelEditorFMX.map` resolves.

```bat
rem next to the repository, at the version it is tested with
git clone https://codeberg.org/sivv/stacker.git ..\stacker
git -C ..\stacker checkout 525dc8fe5d5ac36fea8e0e18c2da46188a517276
```

## Tests

The test units are the same source for Delphi (DUnitX) and Free Pascal
(FPCUnit), through `UnitTest/Code/Common/BoldTestFramework.pas`:
`Test.BoldModelDocument` (the document, its check among it) and
`Test.BoldModelEditorData` (the shared data module, with no user interface:
its datasets, its commands, the values of the class and attribute dialogs,
the question about unsaved changes, a change made through a dialog's values
saved and generated, the findings of the check and the selection of what
they are about). The models are the Invoicing example's and the Master-Detail
demos', found by walking up from the test program to the repository.

```sh
sh Tools/ModelEditor/FPC/build.sh                 # Free Pascal, Linux or Git Bash
```

```powershell
.\Tools\ModelEditor\Tests\build.ps1 -Platform Win64   # Delphi; DUnitX from $env:DUnitX
```

The Delphi test program defines `BOLD_NO_VCL` and has no `Vcl` unit scope,
so a VCL unit in the shared part fails its build.

Both CI jobs (Linux and Windows) run the Free Pascal tests and the LCL
client's self-test on every push and pull request to `develop`. The FMX
client is built and run with Delphi, which CI does not have.
