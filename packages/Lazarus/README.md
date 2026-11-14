# Bold for Lazarus

Three runtime packages: `Bold.lpk`, the framework-neutral core, `BoldLCL.lpk`,
what an LCL application adds to it, and `BoldZeos.lpk`, the database adapter.
And two design-time packages: `dclBold.lpk`, which puts Bold's non-visual
components on the component palette, and `dclBoldZeos.lpk`, which adds the
Zeos database adapter to it.

`Bold.lpk` is the runtime package of Bold for Lazarus / Free Pascal (#117 step
17). It holds the framework-neutral units that build with Free Pascal: object
space, OCL, handles, persistence mappers, XML file persistence and
`TBoldDataSet`. It requires only the FCL: no LCL unit and no database library.

`BoldLCL.lpk` requires `Bold` and the LCL. It holds `BoldEnvironmentLCL`
(`Source/Common/EnvironmentLCL`), the twin of `BoldEnvironmentVCL`: name it in
one uses clause of an LCL application, and Bold's display queue is drained
from the LCL's idle handling, and `AskUser`, `QueryUser` and `InformUser` show
LCL dialogs. A console program or a service needs only `Bold`.

The queue is drained when the application is idle. Under the VCL Bold drains
it before every message, so also inside a loop that calls
`Application.ProcessMessages`; code that loops that way in an LCL application
calls `TBoldQueueable.DisplayAll` itself.

`BoldZeos.lpk` requires `Bold` and Zeos's package `zcomponent` (ZeosLib 8.0,
not part of this repository). It holds `TBoldDatabaseAdapterZeos`
(`Source/Persistence/Zeos`), which Bold uses for SQL databases under Free
Pascal, where Delphi uses FireDAC. Lazarus must know the Zeos packages first:
open `zcomponent.lpk` from Zeos's `packages/lazarus` folder once, or see
`examples/Compound/Invoicing/Zeos/build.sh` for the command line.

`dclBold.lpk` is installed in the IDE (Package, Open Package File, then Use,
Install; Lazarus rebuilds itself). It requires `Bold` and `BoldLCL` and
registers, on the same pages as the Delphi package of that name:

- **Bold Handles**: `TBoldModel`, `TBoldSystemHandle`,
  `TBoldSystemTypeInfoHandle`, `TBoldExpressionHandle`, `TBoldDerivedHandle`,
  `TBoldVariableHandle`, `TBoldListHandle`, `TBoldCursorHandle`,
  `TBoldFilteredHandle`, `TBoldReferenceHandle`, `TBoldOclVariables`,
  `TBoldUnloaderHandle`
- **Bold Persistence**: `TBoldPersistenceHandleDB`,
  `TBoldPersistenceHandleFileXML`
- **Bold Misc**: `TBoldFilter`, `TBoldComparer`, `TBoldPlaceableSubscriber`,
  `TBoldExceptionHandler`, `TBoldDataSet`

The components have the icons of the Delphi palette (`TBoldFilteredHandle`
has none there either and shows the default icon). The icons are PNG files in
`Source/Common/IDELazarus/images`; `make_res.sh` there makes them from the
bitmaps in `Images/Components` and packs them into `boldlazarusreg.res`, and
makes the icons of the OCL editor's lists from the image list of Delphi's
editor (`boldoclexpressionpropedit.res`).

`dclBoldZeos.lpk` is installed the same way, by those who use Zeos. It
requires `dclBold` and `BoldZeos` and puts `TBoldDatabaseAdapterZeos` on the
page **Bold Persistence**, so a form or data module can hold the whole
database setup: a `TZConnection` (from Zeos's own design-time package
`zcomponentdesign`), the adapter with its `Connection`, and a
`TBoldPersistenceHandleDB` with its `DatabaseAdapter`. Its icon is the one of
Bold's general database adapter. `tests/BoldZeosRegistrationCheck`
(`tests/build-zeos.sh`, needs `ZEOS`) checks the registration and such a data
module. A `TZConnection` read from a form writes four more of its format
settings than a new one, so a form with one changes once, the first time it
is saved again.

`dclBold` also has a property editor for OCL expressions: `Expression` and
`MutableListExpression` of a list handle, `Expression` of an expression
handle, and `Expression` under `BoldProperties` of a `TBoldDataSet` field. The
"..." button in the Object Inspector opens Delphi's OCL editor as an LCL
dialog. In the middle is the expression and, under it, the parser messages:
the type of the result, or the expression with a pointer at the error. At the
right is what can follow the expression, the members of its type (the classes,
for the system) and the operations, with the filter and the Derived,
Persistent and Transient boxes; a double click or Enter adds the line. At the
left are the OCL variables. The checking, the messages and the lists are one
unit that Delphi's form uses too, `BoldOclExpressionCheck`
(`Source/ObjectSpace/Ocl`, in `Bold.lpk`; #124), so the two editors show the
same. The dialog keeps what the user has done in that unit's
`TBoldOclEditorState`, as Delphi's FMX dialog `BoldOclExpressionDialogFMX`
does, so the LCL and the FMX dialog share the filter, the options and the
additions too, and their captions. Remove Last, Clear, Show Original Parser Errors
and Show Info are there; the font button and the list of recently used
expressions, which Delphi keeps in the Windows registry, are not. The model
must be in the form (a `TBoldModel` that the handles reach through their
system handle); a handle that is not connected to one can be edited, but is
not checked. The package requires `IDEIntf` for it, so it is built for a
widgetset (`lazbuild --ws=qt5`). On Debian and Ubuntu `lazbuild` needs the
package `lazarus-src` for that: `IDEIntf` uses `BuildIntf`, which those
distributions ship as source only.

A project never requires `dclBold`: it requires `Bold` (and `BoldLCL`), so an
application links no registration code. `tests/BoldRegistrationCheck` checks
without the IDE what the package registers, that every component, and a
data module of them that refer to each other, is written as form text and
read back, that every component has its icon, and the OCL editor against the
Invoicing model: the parser messages, the lists and their filter, adding a
line, Remove Last, and that an edited expression is written back.

## Using it

Open `Bold.lpk` in Lazarus 4 (FPC 3.2.2 or later) once, so the IDE knows where
it is, and add **Bold** to the required packages of a project (**BoldLCL**
for an LCL application; it brings Bold with it). A project in
this repository can name the file itself, as
`examples/Compound/Invoicing/LCL/InvoicingLCL.lpi` does, and then builds with
`lazbuild` on a machine that has never opened the package.

Bold's source needs the mode `delphiunicode`. The package compiles its own
units that way; a project that uses Bold's types in its own units should add
`-Mdelphiunicode` to its custom options too, or start those units with
`{$mode delphiunicode}`.

## What is in it

The units of `Bold` are the ones that the Free Pascal test suite
(`UnitTest/FPC`) and the Free Pascal examples compile, so they are built and
tested on each push. Four more are there for the palette and are built, and
written and read as components, but have no tests under Free Pascal yet:
`BoldDerivedHandle`, `BoldVariableHandle`, `BoldReferenceHandle` and
`BoldPlaceableSubscriber`. `Bold.pas`, `BoldLCL.pas`, `BoldZeos.pas`, `dclBold.pas` and
`dclBoldZeos.pas` are the packages' main units, which Lazarus writes.

`BoldZeos` is built and run on each push too, by the Invoicing runner in
`examples/Compound/Invoicing/Zeos`.

Not there yet:

- The other property and component editors of the Delphi design package
  (the model editor, the field editor of `TBoldDataSet`): they are written against Delphi's design-time API. Other
  properties are edited in the Object Inspector as text.
- Bold's visual controls.
