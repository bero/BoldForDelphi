# Invoicing - FMX client

The Invoicing example in FireMonkey (#117 step 10): customers, the invoices of
the selected customer and the lines of the selected invoice, in three grids.
The Lazarus client in `..\LCL` has the same form; its
[README](../LCL/README.md) describes what the two clients share. This one
covers what differs in FMX.

## How it works

Each grid shows a `TBoldDataSet` through LiveBindings: a `TBindSourceDB` on the
dataset and a `TLinkGridToDataSource` to an FMX `TStringGrid`. FMX has no
data-aware controls of its own, and LiveBindings is its standard way to bind a
`TDataSet`.

The datasets sit on chained `TBoldListHandle`s, as in the LCL client (see
[How it works](../LCL/README.md#how-it-works) there): selecting a row moves the
grids below with no code, and the derived attributes are ordinary columns.

All of it is in the form file, `fInvoicingMain.fmx`, as the form designer
writes it with the design package `dclBold` installed (`TBoldDataSet` is on
its page Bold Misc): the four list handles, the four datasets with their
fields, the bind sources, the bindings list with its links, and the labels,
grids, combo box and buttons are components on the form, wired through their
properties. The code in `fInvoicingMain.pas` opens the database, roots the two
lists of all customers on the data module's system handle, and opens the
datasets.

The data module is the one the console runner and the tests use
(`..\Data\dmInvoicing.pas`); it links no VCL unit. The model is
`..\Model\Invoicing.bld`, and on the first run `InvoicingFMX.db` next to the exe
is filled with the scenario from `..\Tests\InvoicingScenario.pas`.

## The customer of an invoice: a link in a combo box

Under the invoice grid a combo box shows the customer of the selected
invoice, and picking another customer moves the invoice to it. The two
datasets behind it, `bdsInvoices` with the link field `customer` and
`bdsCustomerChoice` with the fields `self` and `name`, are those of the LCL
client; see
[its combo box section](../LCL/README.md#the-customer-of-an-invoice-a-link-in-a-combo-box)
for the key they share and why the form follows a moved invoice. In FMX the
combo box is a plain `TComboBox`, bound by LiveBindings' lookup link:

| | |
|---|---|
| `lnkInvoiceCustomer`, a `TLinkFillControlToField` | `DataSource`/`FieldName` = the invoices' `Customer` field, `FillDataSource` = the choice dataset, `FillValueFieldName` = `self`, `FillDisplayFieldName` = `Name`, `Control` = the `TComboBox` |

The key field is not among the columns of the invoice grid's link. The form
follows a moved invoice in `InvoiceCustomerChange` and `FollowTheInvoice`, and
disables the combo box for a posted invoice and while the invoice grid is
empty.

## Editing the lines of an invoice in the grid

**New invoice**, **Post invoice**, **Delete invoice**, **Add line** and
**Delete line** work as in the LCL client; see
[Editing the lines of an invoice in the grid](../LCL/README.md#editing-the-lines-of-an-invoice-in-the-grid).
Two things differ in FMX. **Add line** is the way to start typing in an empty
lines grid: an FMX grid has no row to start in. And it is the grid link
(`TLinkGridToDataSource`) that writes a cell to the dataset field, which
`TBoldDataSet` then writes to the line. `UpdateInvoiceControls` makes the lines
grid read-only and disables the customer combo box when the selected invoice is
posted.

## The look of the LCL client

The form has the font size, the column widths and the selection of the LCL
client, so that the two look alike. The font size and the column widths are
properties in the form file (the grid links have their columns listed). The
selection is drawn by the form: FMX's default style shows the selected row
with a tinted control laid over the cells, in the style's accent colour; the
form makes that control invisible and draws the cells itself
(`GridDrawColumnBackground`, `GridDrawColumnCell`), a blue row with white
text. `ApplyTheLook` gives the column headers and the combo box's items the
font size, which are made while the program runs, and puts a grey rectangle
in place of the thumb of the grids' scroll bars, which the style draws as a
picture in its accent colour. The customer columns are narrow enough to fit
the grid, so it has no horizontal scroll bar.

## No VCL

The project defines `BOLD_NO_VCL` (Project Options, Delphi Compiler, Conditional
defines). Bold's control packs, which `TBoldDataSet` builds on, then compile
without their VCL parts and link no VCL unit (see `BOLD_VCL` in
`Source\Common\Include\bold.inc`). Without the VCL environment nothing
drains Bold's display queue by itself, so the form calls
`TBoldQueueable.DisplayAll` from `Application.OnIdle`.
`UnitTest\check_no_vcl.ps1` builds this project and fails when its map file
names a `Vcl.*` unit.

## Running it

Build `InvoicingFMX.dproj` for Win64 (Win32 still compiles) and run it, or for
Linux64 (see below). **Save changes** writes to the database.

**Edit OCL...** opens Bold's OCL editor as an FMX dialog
(`Source\ObjectSpace\UtilsGUI\FMX\boldoclexpressiondialogfmx.pas`) on the
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

## Exceptions and their call stacks

An exception that reaches the application's exception handler goes to
`InvoicingFMX.log` next to the program, with its call stack, and is then shown;
so does one that no handler takes. The program names `BoldExceptionLogFMX` in
its uses clause, which does that (#130; see `docs/concepts/logging.md`, also
for where the log goes when the program's folder cannot be written). The project
takes [Stacker](https://codeberg.org/sivv/stacker) as its stack-trace provider
(`Tools/BoldStacker.targets`): from the environment variable `STACKER`, else
from a folder `stacker` next to the repository. The frames then name the
routines: with line and file on Windows, from DWARF without lines on Linux64.
When Stacker is not there, the build stops with a message that gives the
commands to clone it, or says to build without it: `STACKER=off`, and the log
then gives the address the exception was raised at, which `InvoicingFMX.map`
resolves. The self-test checks the report of one exception either way.

## Not yet

- The form file was written to match what the components write, and checked
  by running it on Windows and Linux64; it has not been opened and saved in
  the Delphi form designer yet. The palette offers Bold's handles for the
  Windows platforms and Linux64 (#117 step 8); macOS is not among them yet
  (#127).
- The model and the system handle are in the data module, which the code
  creates, so the two top list handles get their root in code. In the LCL
  client they are components on the form too.
- Only the lines grid is edited; the customer and invoice grids are
  read-only.
- Typing in the lines grid has not been tried by hand in the FMX client;
  the self-test edits through the grid's dataset.
