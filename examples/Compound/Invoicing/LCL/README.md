# Invoicing - LCL client

The Invoicing example in Lazarus (#117 step 17): customers, the invoices of
the selected customer and the lines of the selected invoice, in three grids.
It is the Lazarus twin of the FMX client in `..\FMX`.

## How it works

Each grid is an LCL `TDBGrid` on a `TDataSource` on a `TBoldDataSet`. The LCL
has data-aware controls, so nothing more is needed to bind a `TDataSet`.

Each dataset sits on a `TBoldListHandle`. The invoice handle is rooted on the
customer handle with the expression `invoices`, the line handle on the invoice
handle with `lines`. Selecting a row moves the dataset, the dataset moves its
handle's current element, and the grids below follow with no code. The derived
attributes of the model, such as `openBalance`, `grossTotal` and `status`, are
ordinary columns, and a change made on the Bold side shows in the grids.

The Bold system is the one the Free Pascal runner and tests use
(`..\FPC\InvoicingSystemFPC.pas`): it stores in an XML file, so the client
needs no database library. The model is `..\Model\Invoicing.bld`, and on the
first run `InvoicingLCL.xml` next to the program is filled with the scenario
from `..\Tests\InvoicingScenario.pas`.

The form names `BoldEnvironmentLCL` in a uses clause, which registers Bold's
LCL environment: Bold's display queue is drained from the LCL's idle handling,
and Bold's questions to the user are LCL dialogs. Without it Bold runs on its
freestanding environment, where the application drains the queue itself.

## Building and running it

Open `InvoicingLCL.lpi` in Lazarus 4 (FPC 3.2.2 or later) and run it, or on
Linux:

```sh
./build.sh            # lazbuild, then the self-test
bin/InvoicingLCL      # the client
```

`build.sh` builds for the Qt5 widgetset; `LCL_WS=gtk2 ./build.sh` chooses
another. The project requires the packages `Bold` and `BoldLCL`
(`packages\Lazarus`), which lazbuild compiles first, and compiles its
own units in the mode `delphiunicode` (Project Options, Custom Options:
`-Mdelphiunicode`).

**Save changes** writes to the data file.

`InvoicingLCL --selftest` shows no window: it fills a temporary data file,
walks every customer and invoice, checks what the grids are bound to and that
the LCL environment drains Bold's display queue, writes
the checks to `InvoicingLCL.selftest.txt` and exits with the number of failed
checks. A `TDBGrid` counts only the rows it paints, and it paints nothing
without a window, so the row checks read the dataset behind each grid.

## Not yet

- The handles, datasets and grids are created in code (`fInvoicingMain.pas`),
  because the Lazarus packages are runtime only: Bold's components are not on
  the palette yet.
- The grids are read-only here. `TBoldDataSet` can edit (Edit and Post write
  into the Bold objects), which a later version of the client can show.
- Built and run on Linux only so far.
