# Step 3: Invoices and Associations

In this step you learn how Bold models relationships between objects, and you build a master-detail UI: select a
customer to see its invoices, select an invoice to see its items.

Without Bold you would write SQL joins and refresh code. With Bold you use **handle chaining**: a handle takes
another handle as its root, and the handles form a hierarchy.

## What You Will Learn

- What an association is in Bold
- How associations work: the one end, the many end, and how Bold keeps both in step
- What handle chaining is and how it gives master-detail views
- How to create related objects, such as an invoice of a customer
- What derived attributes are and how Bold calculates them

## Associations: Modeling Relationships

The associations are the connections between the classes of the model from Step 1:

```
 ╔═══════════════════╗         ╔═════════════════════╗        ╔═════════════════════╗
 ║     Customer      ║         ║      Invoice        ║        ║    InvoiceItem      ║
 ╠═══════════════════╣         ╠═════════════════════╣        ╠═════════════════════╣
 ║ Name       String ║         ║ InvoiceNumber  Int  ║        ║ Description String  ║
 ║ Address    String ║         ║ InvoiceDate   Date  ║        ║ Quantity       Int  ║
 ║ City       String ║ 0..1  * ║ DueDate       Date  ║ 0..1 * ║ UnitPrice Currency  ║
 ║ Phone      String ╠═════════╣ IsPaid     Boolean  ╠════════╣ /LineTotal Currency ║
 ║ Email      String ║invoices ║ /TotalAmt  Currency ║ items  ║                     ║
 ╚═══════════════════╝         ╚═════════════════════╝        ╚═════════════════════╝
```

Each `═══` line is an **association**, and you can navigate it in both directions.

### The Customer Side

From a customer you reach the list of its invoices:

```pascal
var
  Customer: TCustomer;
  I: Integer;
begin
  Customer := lhaCustomers.CurrentBoldObject as TCustomer;
  ShowMessage('This customer has ' + IntToStr(Customer.Invoices.Count) + ' invoices');

  for I := 0 to Customer.Invoices.Count - 1 do
    ShowMessage(IntToStr(Customer.Invoices[I].InvoiceNumber));
end;
```

`Customer.Invoices` is a list, `TInvoiceList`. The code generator made it from the association.

### The Invoice Side

From an invoice you reach its customer:

```pascal
var
  Invoice: TInvoice;
begin
  Invoice := lhaInvoices.CurrentBoldObject as TInvoice;
  ShowMessage('This invoice belongs to: ' + Invoice.Customer.Name);
end;
```

`Invoice.Customer` is a single reference, also generated from the association.

### Both Ends Stay in Step

Bold keeps both ends of an association in step. Set one end:

```pascal
Invoice.Customer := Customer;
```

and the invoice is in the customer's list:

```pascal
Customer.Invoices.Includes(Invoice)   // True
```

You do not call `Customer.Invoices.Add(Invoice)`. It works the other way round too:

```pascal
Customer.Invoices.Add(Invoice);
// now Invoice.Customer = Customer
```

When you delete an invoice, it disappears from its customer's `Invoices`.

## Handle Chaining: Master-Detail Made Simple

### The Concept

Instead of one list handle, you build a chain:

```
┌───────────────────────────────────┐
│ BoldSystemHandle1 (root)          │
│ The object space                  │
└──────────┬────────────────────────┘
           │
┌──────────▼────────────────────────┐
│ lhaCustomers                      │
│ Expression: Customer.allInstances │
│ CurrentElement: Customer #5       │
└──────────┬────────────────────────┘
           │ (root)
┌──────────▼────────────────────────┐
│ lhaInvoices                       │
│ RootHandle: lhaCustomers          │
│ Expression: invoices              │ ← invoices of the current customer
│ CurrentElement: Invoice #23       │
└──────────┬────────────────────────┘
           │ (root)
┌──────────▼────────────────────────┐
│ lhaItems                          │
│ RootHandle: lhaInvoices           │
│ Expression: items                 │ ← items of the current invoice
│ CurrentElement: Item #1           │
└───────────────────────────────────┘
```

When you select another customer (`lhaCustomers.CurrentElement` changes), the handles further down evaluate again:

- `lhaInvoices` shows that customer's invoices.
- `lhaItems` shows the items of the selected invoice.

No code is needed.

### Setting Up the Chain

The finished form (`Step3Form.pas`) sets the handles in the Object Inspector:

| Handle | `RootHandle` | `Expression` |
|--------|--------------|--------------|
| `lhaCustomers` | `BoldSystemHandle1` of the data module | `Customer.allInstances` |
| `lhaInvoices` | `lhaCustomers` | `invoices` |
| `lhaItems` | `lhaInvoices` | `items` |

The same in code:

```pascal
lhaInvoices.RootHandle := lhaCustomers;  // the root is the customer handle, not the system
lhaInvoices.Expression := 'invoices';    // the invoices of the current customer
grdInvoices.BoldHandle := lhaInvoices;
```

**Key point**: when a handle's `RootHandle` is another list handle, Bold evaluates the expression on that handle's
current element.

- `invoices` on `lhaInvoices` means "the invoices of the current customer".
- Click Customer #5, and `lhaInvoices` shows the invoices of Customer #5.
- Click Customer #3, and it shows those of Customer #3.

There is no code in any `OnClick` handler.

## Creating Related Objects

When you add an invoice through the UI:

1. Click **Insert** in the navigator of the invoice grid.
2. Bold creates a new Invoice and adds it to the current customer's `invoices`.
3. Because both ends stay in step, the invoice's `Customer` is set.
4. The new invoice appears in the grid.

This works because the invoice grid's handle holds the list `invoices` of the current customer: Bold creates the new
object in that list.

## Derived Attributes (Calculated Fields)

In Step 1 you left out two attributes. Now you add them as derived attributes:

- **Invoice.TotalAmount**: the sum of the line totals of the items
- **InvoiceItem.LineTotal**: Quantity times UnitPrice

A derived attribute is read-only. Bold calculates it from an OCL expression.

### Adding Derived Attributes to the Model

In the model editor:

1. Select the InvoiceItem class, right-click it and choose **New Attribute**.
2. Name: `LineTotal`, Type: Currency.
3. Check **Derived** and clear **Persistent**: a derived attribute is calculated, not stored.
4. Type the expression in **Derivation OCL**: `quantity * unitPrice`

Do the same for the Invoice class:

1. Name: `TotalAmount`, Type: Currency.
2. Check **Derived** and clear **Persistent**.
3. **Derivation OCL**: `items.lineTotal->sum`

Then generate the code again (**Tools > Generate Code**).

The expression `items.lineTotal->sum` means:

- Start with the items of this invoice.
- Take the lineTotal of each item.
- Add the values up.

### How Derived Attributes Work

You read a derived attribute like any other:

```pascal
var
  Total: Currency;
begin
  Total := Invoice.TotalAmount;  // Bold evaluates items.lineTotal->sum for this invoice
end;
```

Bold evaluates the expression for that invoice and returns the value. When you change an item's quantity, the
invoice's `TotalAmount` follows: through subscriptions, Bold knows which values the result depends on.

### Showing Derived Attributes

A grid shows derived attributes as read-only columns:

```
Invoice grid columns:
  Invoice #   invoiceNumber (editable)
  Date        invoiceDate   (editable)
  Due Date    dueDate       (editable)
  Paid        isPaid        (editable)
  Total       totalAmount   (read-only, derived)
```

The totals change as you edit the quantities.

## Building the Step 3 Form

The form has three grids, one above the other:

```
┌─────────────────────────────────────────────────────┐
│ Customers                                           │
│ ┌─────────────────────────────────────────────────┐ │
│ │ Name       | City    | Phone     | Email        │ │
│ │ Acme Corp  | Boston  | ...       | ...          │ │
│ └─────────────────────────────────────────────────┘ │
│ [|<] [<] [>] [>|] [+] [-]                           │
├─────────────────────────────────────────────────────┤
│ Invoices for Customer                               │
│ ┌─────────────────────────────────────────────────┐ │
│ │ Invoice # | Date | Due Date | Paid | Total      │ │
│ │ 1         | ...  | ...      | [ ]  | 1500.00    │ │
│ └─────────────────────────────────────────────────┘ │
│ [|<] [<] [>] [>|] [+] [-]                           │
├─────────────────────────────────────────────────────┤
│ Invoice Line Items                                  │
│ ┌─────────────────────────────────────────────────┐ │
│ │ Description | Qty | Unit Price | Line Total     │ │
│ │ Widgets     | 100 | 15.00      | 1500.00        │ │
│ └─────────────────────────────────────────────────┘ │
│ [|<] [<] [>] [>|] [+] [-]                           │
├─────────────────────────────────────────────────────┤
│ [Save Changes] [Open/Close system]   status         │
└─────────────────────────────────────────────────────┘
```

### Steps to Build It

1. **Create three TBoldListHandle components**, as in the table under
   [Setting Up the Chain](#setting-up-the-chain).

2. **Create three TBoldGrid components** and connect them:

   ```
   grdCustomers.BoldHandle = lhaCustomers
   grdInvoices.BoldHandle  = lhaInvoices
   grdItems.BoldHandle     = lhaItems
   ```

3. **Create three TBoldNavigator components**:

   ```
   bnCustomers.BoldHandle = lhaCustomers
   bnInvoices.BoldHandle  = lhaInvoices
   bnItems.BoldHandle     = lhaItems
   ```

4. **Set up the grid columns**: right-click each grid, choose **Create Default Columns**, then remove the columns you
   do not want with **Edit Columns**.

5. **Add a Save Changes button** that calls `dmInvoice.BoldSystemHandle1.UpdateDatabase`, and the Open/Close
   button and `FormCreate` of Step 2.

### No Master-Detail Code

In traditional database programming you would:

1. Load the customers from the database.
2. Add an `OnClick` handler to the customer grid.
3. In it, query the database for that customer's invoices.
4. Refresh the invoice grid.
5. Add an `OnClick` handler to the invoice grid.
6. In it, query the invoice's items.
7. Refresh the item grid.

With Bold:

1. Create three handles.
2. Chain them.
3. Done: click, and everything follows.

The associations of the model become the relationships of the UI.

## Try It Yourself

### Exercise 1: Add Some Data

1. Run the application.
2. Click **Insert** in the Customers navigator and type a name, for example "Acme Corp".
3. Click **Insert** in the Invoices navigator. A new invoice appears.
4. Type an invoice number, for example 1.
5. Click **Insert** in the Items navigator.
6. Type a description, a quantity and a unit price.
7. Watch the invoice's Total follow.
8. Click **Save Changes**.

You now have a complete invoice with line items.

### Exercise 2: Move an Invoice to Another Customer

Add a button that moves the selected invoice to the first customer of the list (add `InvoiceClasses` to the form's
`uses` clause):

```pascal
procedure TfrmStep3.btnMoveInvoiceClick(Sender: TObject);
var
  Invoice: TInvoice;
begin
  Invoice := lhaInvoices.CurrentBoldObject as TInvoice;
  if Assigned(Invoice) then
    Invoice.Customer := lhaCustomers.ObjectList[0] as TCustomer;
end;
```

Click it: the invoice leaves the current customer's list. Select the first customer, and it is there. You changed
one end of the association, and Bold updated the other.

### Exercise 3: Remove an Item and Watch the Total

1. Select an invoice, then one of its items.
2. Click **Delete** in the Items navigator.
3. The item disappears from the invoice, and the invoice's Total follows.
4. Click **Save Changes**.

On a detail list such as `items`, the navigator removes the link between the invoice and the item: it asks whether
to unlink the item, and the item object itself stays in the database. Set the navigator's `BoldDeleteMode` to
`dmDelete` to delete the object instead.

### Exercise 4: Add Another Class

Create a Quotation class like Invoice, with its own attributes and an association to Customer. Add the handles and
grids for it. This practices handle chaining.

## Filtering with Handle Expressions

A handle's expression can do more than navigate:

```
Paid invoices only:        invoices->select(isPaid)
Unpaid and over 1000:      invoices->select((not isPaid) and (totalAmount > 1000))
Overdue:                   invoices->select(dueDate < today)
```

Try this expression on `lhaInvoices`:

```pascal
lhaInvoices.Expression := 'invoices->select(not isPaid)';
```

Now only unpaid invoices appear. When you mark an invoice as paid, it leaves the grid.

## Common Mistakes

### Wrong Root Handle

```pascal
lhaInvoices.RootHandle := dmInvoice.BoldSystemHandle1;  // WRONG
lhaInvoices.Expression := 'invoices';
```

This evaluates `invoices` on the object space, which has no member called `invoices`, so you get an error. Root the
handle on its parent handle:

```pascal
lhaInvoices.RootHandle := lhaCustomers;  // RIGHT
```

### Keeping Both Ends in Step by Hand

When you delete an invoice, Bold removes it from its customer's `Invoices`. You do not need code for that; the
association does it.

### Setting a Derived Attribute

A derived attribute is read-only. The generated property has no setter, so this does not compile:

```pascal
Invoice.TotalAmount := 5000;  // ERROR: TotalAmount is derived
```

Change the values it is calculated from (Quantity, UnitPrice).

## Next Step

You have a working invoice application with associations and master-detail views.

In Step 4 you learn OCL in depth and build dynamic queries and labels. Go to
[`Step4_OclExpressions/README.md`](../Step4_OclExpressions/README.md).

---

**Key Takeaways**:

- Associations model relationships between classes
- Bold keeps both ends of an association in step
- Handle chaining gives master-detail views without code
- Derived attributes are OCL expressions that Bold calculates
- Select a row, and the handles further down the chain evaluate again
- No SQL joins, no manual refresh, no event handlers
