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

All of it is in the form file, `fInvoicingMain.lfm`, as the form designer
writes it with the design-time package `dclBold` installed
(`packages\Lazarus`): a `TBoldModel`, the system, type info and persistence
handles, the three list handles, the three datasets with their fields, the
data sources and the grids are components on the form, wired through their
properties. The code in `fInvoicingMain.pas` names the data file, opens the
system and the datasets, and fills an empty database.

`BoldModel1` holds the model in the form file, as in a Delphi data module, so
the handles' OCL expressions are checked against it as soon as the form is
read. It is a copy of `..\Model\Invoicing.bld`, which the Delphi clients load;
the self-test compares the two. The data is stored in an XML file
(`TBoldPersistenceHandleFileXML`), so the client needs no database library,
and on the first run `InvoicingLCL.xml` next to the program is filled with the
scenario from `..\Tests\InvoicingScenario.pas`.

## The customer of an invoice: a link in a combo box

Under the invoice grid a combo box shows the customer of the selected
invoice, and picking another customer moves the invoice to it. It is the
LCL's `TDBLookupComboBox`; a single link needs nothing more than a
`TBoldDataSet` on each side:

| | |
|---|---|
| `bdsInvoices`, the dataset that is edited | a field with the expression `customer`, the link. It holds a key of the linked object, or null. |
| `bdsCustomerChoice`, the customers to choose from | a field with the expression `self`, named `self`: the same key. And `name`, to show. |
| `cmbInvoiceCustomer` | `DataSource`/`DataField` = the invoices' `Customer` field, `ListSource` = the choice dataset, `KeyField` = `self`, `ListField` = `Name` |

The key is a 64-bit integer that is only good for the running program, so it
is no grid column (the code hides the field). The choice dataset is one of
its own, on a list handle of all customers, so that the combo box does not
move the customer grid.

Here the link is also what the invoice grid is built on: it shows the
invoices of the selected customer. An invoice that gets another customer
leaves that list at once; the dataset ends the edit and keeps the change.
So that the invoice does not vanish from view, the form then follows it
(`cmbInvoiceCustomerSelect`, `FollowTheInvoice`): the customer grid moves to
the new customer, the invoice grid shows that customer's invoices with the
invoice selected, and the combo box shows the new customer, ready for
another pick. The derived columns of both customers have the change. It is
saved with **Save changes**, like every other.

The form disables the combo box for a posted invoice, which is frozen, and
while the invoice grid is empty
(`dsInvoicesDataChange`): a data-aware control that edits an empty dataset
inserts a record, which here would be a new invoice.

The combo box reads its list when the choice dataset opens and when its
number of rows changes; that is how the LCL's lookup controls work with any
dataset. A customer that is renamed while the form is open keeps its former
name in the list until then.

## Editing the lines of an invoice in the grid

**New invoice** makes a draft for the selected customer, through the model's
business method `Customer.NewInvoice`. The lines of a draft are edited in the
lines grid, a plain `TDBGrid` on a `TBoldDataSet`:

- typing in a cell changes the line; the derived columns of the line, the
  invoice and the customer follow. A field is written to the object when it
  is set, and **Esc** takes back what was typed in the row since it was
  entered, and only that;
- the down arrow on the last row adds a line: the dataset asks the list of
  its handle (`lines` of the selected invoice) for a new object, so the line
  belongs to the invoice at once. Esc there removes it again;
- **Ctrl+Del** deletes the line;
- the derived column `Amount` cannot be typed in: its field is read-only,
  because Bold says the value cannot be modified.

**Post invoice** calls `Invoice.Post`, which refuses an invoice without lines
and gives the invoice its number. A posted invoice is frozen. The model says
so in its business methods (`TInvoice.AddLine`), not for each attribute, so
the form follows the rule: `UpdateInvoiceControls` makes the lines grid
read-only and disables the customer combo box when the selected invoice is
posted. Only a draft gets lines, or another customer.

## The LCL environment

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

On the first run, filling the empty data file raises six `EBold` exceptions
on purpose: the scenario checks that the model refuses a payment on a draft
invoice, posting an empty invoice and the like, and catches each refusal.
Run from the Lazarus IDE, the debugger stops on each of them; choose
**Continue**, or start the program with **Run → Run without Debugging**.
Later runs open the saved data file and raise none.

**Save changes** writes to the data file.

**Edit OCL...** opens Bold's OCL editor (`BoldOclExpressionDialogLCL`, in the
package `BoldLCL`) on the Invoicing model: the classes, members and operations
that can follow the expression are listed and filtered as you type, and the
parser's answer is shown under it. After OK the expression is evaluated in
the running system and the client shows its value. The FMX client has the
same button, with the same editor as an FMX dialog.

`InvoicingLCL --selftest` shows no window: it fills a temporary data file,
walks every customer and invoice, checks what the grids are bound to and that
the LCL environment drains Bold's display queue, picks another customer for
an invoice in the combo box as a user does, makes a new invoice and edits its
lines through the grid's dataset, posts it, writes
the checks to `InvoicingLCL.selftest.txt` and exits with the number of failed
checks. A `TDBGrid` counts only the rows it paints, and it paints nothing
without a window, so the row checks read the dataset behind each grid.

## Not yet

- The form file was written to match what the components write, and checked
  by running it; it has not been opened and saved in the Lazarus form designer
  yet.
- Only the lines grid is edited; the customer and invoice grids are
  read-only.
- The customer and invoice grids show what the lines and the combo box
  change; they have no cells to type in.
- Built and run on Linux and, with Lazarus 4.8, on 64-bit Windows only so
  far.
