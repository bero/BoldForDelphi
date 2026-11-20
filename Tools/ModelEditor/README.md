# Model editor

A standalone model editor for Bold (#128): a program that opens and saves
`.bld` model files, shows the model for editing and generates the business
classes, for Delphi and Lazarus projects alike, without the Delphi IDE.

It follows the pattern of the Invoicing example: a part without user
interface, and two programs on top of it, an LCL client built with Lazarus and
an FMX client built with Delphi, both for Windows and Linux. The shared part
and the LCL client are here; the FMX client follows.

## Layout

| Folder | What it holds |
| --- | --- |
| `Core` | what both clients share: `BoldModelDocument.pas` (open a `.bld` file, save it, generate the business classes) and `BoldModelEditorData.pas` (the handles, the datasets and the commands of the editor) |
| `LCL` | the LCL client, `ModelEditorLCL.lpi`, with `build.sh` |
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
  selected class, as in the Master-Detail demo;
- `bdsClasses` (name, superclass, abstract, persistent) and `bdsAttributes`
  (name, type, persistent, derived), the `TBoldDataSet`s the clients' grids
  show and edit in place. The superclass column is read-only;
- the commands: `NewModel`, `Open`, `Save`, `SaveAs`, `GenerateCode`,
  `AddClass`, `DeleteClass`, `AddAttribute`, `DeleteAttribute`, and what
  enables them (`CanDeleteClass` ...). A new class gets the Bold tagged
  values and, in a boldified model, the root class as its superclass, as in
  Bold's model editor; a deleted class hands its subclasses to its
  superclass; the root class of a boldified model is not deleted. Adding
  and deleting go through these commands, not through the grids;
- the question about unsaved changes: `CloseModel`, which a client calls
  before New, Open and closing, asks through `OnAskSave` (save, discard or
  cancel), and `SaveModel` asks for a file through `OnChooseSaveFile` when
  the model has none yet. A client sets the two events to its own dialogs;
- the window title (`Title`: the file name, a `*` when modified) and the
  model file named on the command line (`ModelFileFromCommandLine`).

A client adds only its controls: the grids bound to the two datasets, a menu
item or button per command, and the dialogs.

## The LCL client

`LCL/ModelEditorLCL.lpi`: the classes in a `TDBGrid` at the top, the
attributes of the selected class below, each on a `TDataSource` on the shared
dataset. An action per command (`TActionList`) feeds the File and Model menus
and the buttons, with a hint each. The grids neither insert nor delete rows
themselves (`dgDisableInsert`, `dgDisableDelete`). The project requires the
Lazarus packages `Bold` and `BoldLCL` (`packages/Lazarus`); `Bold` holds the
code generator.

```sh
ModelEditorLCL [model.bld]
```

opens the model file when one is named. `sh Tools/ModelEditor/LCL/build.sh`
builds it with lazbuild and runs `ModelEditorLCL --selftest` on the
Invoicing model: it shows no window, checks that the grids show the classes
and the attributes of the selected class and that the commands add, delete,
save and generate (into a temporary folder; the model file is not changed),
writes the checks to `bin/ModelEditorLCL.selftest.txt` and exits with the
number of failed checks. Without a display, `QT_QPA_PLATFORM=offscreen`
does for the Qt5 widgetset.

## Tests

The test units are the same source for Delphi (DUnitX) and Free Pascal
(FPCUnit), through `UnitTest/Code/Common/BoldTestFramework.pas`:
`Test.BoldModelDocument` (the document) and `Test.BoldModelEditorData` (the
shared data module, with no user interface: its datasets, its commands, the
question about unsaved changes, a change made through a dataset saved and
generated). The models are the Invoicing example's and the Master-Detail
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
client's self-test on every push.
