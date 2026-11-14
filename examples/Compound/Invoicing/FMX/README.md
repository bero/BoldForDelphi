# Invoicing - FMX client

The Invoicing example in FireMonkey (#117 step 10): customers, the invoices of
the selected customer and the lines of the selected invoice, in three grids.

## How it works

Each grid shows a `TBoldDataSet` through LiveBindings: a `TBindSourceDB` on the
dataset and a `TLinkGridToDataSource` to an FMX `TStringGrid`. FMX has no
data-aware controls of its own, and LiveBindings is its standard way to bind a
`TDataSet`.

Each dataset sits on a `TBoldListHandle`. The invoice handle is rooted on the
customer handle with the expression `invoices`, the line handle on the invoice
handle with `lines`. Selecting a row moves the dataset, the dataset moves its
handle's current element, and the grids below follow with no code. The derived
attributes of the model, such as `openBalance`, `grossTotal` and `status`, are
ordinary columns, and a change made on the Bold side shows in the grids.

The data module is the one the console runner and the tests use
(`..\Data\dmInvoicing.pas`); it links no VCL unit. The model is
`..\Model\Invoicing.bld`, and on the first run `InvoicingFMX.db` next to the exe
is filled with the scenario from `..\Tests\InvoicingScenario.pas`.

## No VCL

The project defines `BOLD_NO_VCL` (Project Options, Delphi Compiler, Conditional
defines). Bold's control packs, which `TBoldDataSet` builds on, then compile
without their VCL parts and link no VCL unit (see `BOLD_VCL` in
`Source\Common\Include\bold.inc`). Without the VCL environment nothing
drains Bold's display queue by itself, so the form calls
`TBoldQueueable.DisplayAll` from `Application.OnIdle`.

## Running it

Build `InvoicingFMX.dproj` for Win32 or Win64 and run it. **Save changes**
writes to the database.

**Edit OCL** opens Bold's OCL editor as an FMX dialog
(`Source\ObjectSpace\UtilsGUI\FMX\BoldOclExpressionDialogFMX.pas`) on the
Invoicing model. It checks the expression as you type, shows the type of the
result or the error, and lists the members, operations and variables that
can follow it. After **OK** the client evaluates the expression and shows the
value. The dialog is the same as the Lazarus OCL property editor: both are
views over `TBoldOclEditorState` in `BoldOclExpressionCheck`.

`InvoicingFMX --selftest` shows no window: it fills a temporary database,
walks every customer and invoice, checks that the grids show the right rows,
drives the OCL editor through the steps of a user, writes the checks to
`InvoicingFMX.selftest.txt` and exits with the number of failed checks.

## Linux64

The project also has the Linux64 platform, tried with Delphi 13.2 and
FmxLinux 1.84 from GetIt (FMX on Linux needs FmxLinux). Run from the IDE
deploys through PAServer; the deployment holds `Invoicing.bld` next to the
program, where the client looks for the model first, before `..\Model`.

The Linux machine needs `libsqlite3-dev`: FireDAC loads `libsqlite3.so`
without a version number, and Ubuntu ships that name only in the dev package.

With Delphi 13.2 and FmxLinux 1.84 the build stops at one of two errors, both
reported to Embarcadero:

- `F1027 Unit not found: 'FMX.ScrollBox'` (RSS-6318). The Delphi folders
  `lib\linux64\release` and `lib\linux64\debug` hold an `FMX.ScrollBox.dcu`
  with no `.o`, which hides FmxLinux's unit while those folders come first on
  the library path. In Tools > Options > Language > Delphi > Library, for
  Linux 64-bit, move FmxLinux's `lib\Release` above
  `$(BDSLIB)\$(Platform)\release`.
- `F2051 Unit Fmx.Bind.Editors was compiled with a different version of
  Data.Bind.Components.IBindFillControlDefaultDisplay` (RSS-6317). FmxLinux's
  `Fmx.Bind.Editors` was built against a newer `Data.Bind.Components` than
  Delphi 13.2 has, so every FMX program with LiveBindings stops here on
  Linux64. Compile the unit from Delphi's own source against the FmxLinux
  units, in an empty folder in a RAD Studio Command Prompt:

  ```bat
  set FMXLINUX=%PUBLIC%\Documents\Embarcadero\Studio\37.0\CatalogRepository\FmxLinux\1.84\lib
  copy "%BDS%\source\databinding\components\Fmx.Bind.Editors.pas" .
  mkdir release debug
  dcclinux64 --no-config -Q -$O+ -$D- -$L- -$Y- -U"%FMXLINUX%\Release;%BDS%\lib\linux64\release" -NUrelease Fmx.Bind.Editors.pas
  dcclinux64 --no-config -Q -$O- -$D+ -$L+ -$Y+ -U"%FMXLINUX%\Debug;%BDS%\lib\linux64\debug;%BDS%\lib\linux64\release" -NUdebug Fmx.Bind.Editors.pas
  ```

  Keep a copy of `Fmx.Bind.Editors.dcu` and `Fmx.Bind.Editors.o` from
  FmxLinux's `lib\Release` and `lib\Debug`, then copy the new files from
  `release` and `debug` over them. Reinstalling FmxLinux brings the old units
  back. A program built with runtime packages still gets FmxLinux's
  `bindcompfmx` package, which this does not replace.

## Not yet

- The handles, datasets, grids and bindings are created in code
  (`fInvoicingMain.pas`), because `TBoldDataSet` is not on the component
  palette yet (#117 step 8).
- The grids are read-only here. `TBoldDataSet` can edit (Edit and Post write
  into the Bold objects), which a later version of the client can show.
