# Bold for Lazarus

Three runtime packages: `Bold.lpk`, the framework-neutral core, `BoldLCL.lpk`,
what an LCL application adds to it, and `BoldZeos.lpk`, the database adapter.

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

The units are the ones that the Free Pascal test suite (`UnitTest/FPC`) and the
Free Pascal examples compile, so every unit in the package is built and tested
on each push. `Bold.pas`, `BoldLCL.pas` and `BoldZeos.pas` are the packages'
main units, which Lazarus writes.

`BoldZeos` is built and run on each push too, by the Invoicing runner in
`examples/Compound/Invoicing/Zeos`.

Not there yet:

- Component registration: the packages are runtime only, so Bold's components
  are not on the Lazarus palette.
