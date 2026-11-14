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

The project also has the Linux64 platform (tried with Delphi 13). Run from
the IDE deploys through PAServer; the deployment holds `Invoicing.bld` next to
the program, where the client looks for the model first, before `..\Model`.

`InvoicingFMX --selftest` shows no window: it fills a temporary database,
walks every customer and invoice, checks that the grids show the right rows,
writes the checks to `InvoicingFMX.selftest.txt` and exits with the number of
failed checks.

## Not yet

- The handles, datasets, grids and bindings are created in code
  (`fInvoicingMain.pas`), because `TBoldDataSet` is not on the component
  palette yet (#117 step 8).
- The grids are read-only here. `TBoldDataSet` can edit (Edit and Post write
  into the Bold objects), which a later version of the client can show.
