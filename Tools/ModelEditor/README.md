# Model editor

A standalone model editor for Bold (#128): a program that opens and saves
`.bld` model files, shows the model for editing and generates the business
classes, for Delphi and Lazarus projects alike, without the Delphi IDE.

It follows the pattern of the Invoicing example: a part without user
interface, and two programs on top of it, an LCL client built with Lazarus and
an FMX client built with Delphi, both for Windows and Linux. The part without
user interface is here; the clients follow.

## Layout

| Folder | What it holds |
| --- | --- |
| `Core` | `BoldModelDocument.pas`: open a `.bld` file, save it, generate the business classes |
| `Tests` | the tests (`Test.BoldModelDocument.pas`) and the Delphi test program `ModelEditorTests.dpr` with `build.ps1` |
| `FPC` | the Free Pascal test program `ModelEditorTests.lpr` with `build.sh` |

## The part without user interface

`TBoldModelDocument` holds a `TBoldModel`:

- `Open(FileName)` reads a `.bld` file. The model's UML model,
  `UMLModel`, is what a client shows and edits.
- `Save` and `SaveAs(FileName)` write the UML model back as the Delphi
  IDE's `.bld` link does, with CRLF line breaks on every platform, so a file
  saved on Linux and on Windows is the same.
- `GenerateCode(Folder)` runs Bold's code generator with the Delphi
  templates, as the IDE's Save and Generate does: the unit named by the
  model's tagged value `Bold.UnitName`, its `_Interface.inc`, and an include
  file with method stubs, which is written only where it does not exist yet.

It builds on what Bold has: the `.bld` reader and writer (`BoldBld`), the
conversion between the stored model and the UML model
(`BoldUMLModelConverter`) and the code generator (`BoldGen`). The code it
generates from the Invoicing model is the code the Delphi IDE generated
(`examples/Compound/Invoicing/Model`), apart from the line with the date.

## Tests

The test unit is the same source for Delphi (DUnitX) and Free Pascal
(FPCUnit), through `UnitTest/Code/Common/BoldTestFramework.pas`. The models
are the Invoicing example's and the Master-Detail demos', found by walking up
from the test program to the repository.

```sh
sh Tools/ModelEditor/FPC/build.sh                 # Free Pascal, Linux or Git Bash
```

```powershell
.\Tools\ModelEditor\Tests\build.ps1 -Platform Win64   # Delphi; DUnitX from $env:DUnitX
```

Both CI jobs (Linux and Windows) run the Free Pascal tests on every push.
