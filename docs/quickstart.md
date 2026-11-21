# Quick Start

Get a Bold program running in a few minutes, then choose how to continue.

## 1. Install Bold

In Delphi, install the design-time package with a package manager:

- [TMS Smart Setup](https://doc.tmssoftware.com/smartsetup/download): `tms install bero.boldfordelphi`
- [DPM](https://github.com/DelphiPackageManager/DPM/releases), per project: `dpm install bero.BoldForDelphi <yourproject>.dproj`

The manual route, and the Lazarus packages, are in [Installation](getting-started/installation.md). Either way,
clone the repository too, for the examples:

```shell
git clone https://github.com/bero/BoldForDelphi.git
```

## 2. Run an example

Pick the one for your toolchain. Each uses SQLite or an XML file, so no database server is needed.

=== "Delphi, VCL"

    Open `examples\Simple\ObjectSpace\MasterDetail\MasterDetail.dproj` and press **F9**. The form offers to create
    the database on the first run. Projects are at the top and the tasks of the selected project below them: the
    detail grid follows the master with no code. `MasterDetail.ini` selects the database; see
    [Database configuration](examples/index.md#database-configuration).

=== "Delphi, FireMonkey"

    Open `examples\Simple\ObjectSpace\MasterDetail\FMX\MasterDetailFMX.dproj` and run it, for Win64 or for Linux64
    with FmxLinux. It is the same demo, bound through `TBoldDataSet` and LiveBindings.

=== "Lazarus"

    Open `examples\Compound\Invoicing\LCL\InvoicingLCL.lpi` in Lazarus 4 and run it with
    **Run → Run without Debugging**. It shows customers, their invoices and the invoice lines in three grids over
    `TBoldDataSet`, and stores the data in an XML file. Its
    [README](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Invoicing/LCL) explains the
    exceptions you see on the first run in the debugger.

## 3. What you just saw

- The **model** (classes, attributes, associations) is in a `TBoldModel` component; these examples load it from a
  `.bld` file.
- The **object space** (`TBoldSystemHandle`) holds the objects. It fetches them from the database when they are
  first needed and writes the changed ones back on `UpdateDatabase`.
- **Handles** evaluate OCL expressions such as `Project.allInstances` or `tasks` against the object space. A list
  handle rooted on another list handle follows its current object: that is the whole master-detail mechanism.
- **Controls** bind to handles, and refresh by themselves when an object they show changes.

## 4. Continue

| Next | What you get |
|------|--------------|
| [First Application](getting-started/first-app.md) | Build a small application from an empty project, every component explained |
| [Tutorial](https://github.com/bero/BoldForDelphi/tree/develop/examples/Tutorial) | A five-step course that builds an invoicing application |
| [Examples](examples/index.md) | More than 40 example programs, one feature each |
| [Core Concepts](concepts/index.md) | How the object space, OCL, persistence and subscriptions work |
| [FAQ](faq.md) | Short answers to common tasks |
