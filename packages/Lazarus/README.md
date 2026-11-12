# Bold for Lazarus

`Bold.lpk` is the runtime package of Bold for Lazarus / Free Pascal (#117 step
17). It holds the framework-neutral units that build with Free Pascal: object
space, OCL, handles, persistence mappers, XML file persistence and
`TBoldDataSet`. It requires only the FCL: no LCL unit and no database library.

## Using it

Open `Bold.lpk` in Lazarus 4 (FPC 3.2.2 or later) once, so the IDE knows where
it is, and add **Bold** to the required packages of a project. A project in
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
on each push. `Bold.pas` is the package's main unit, which Lazarus writes.

Not in it yet:

- The Zeos database adapter (`Source/Persistence/Zeos`), which needs the Zeos
  packages; it will get a package of its own.
- Component registration: the package is runtime only, so Bold's components
  are not on the Lazarus palette.
