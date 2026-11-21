# Step 2: Customer CRUD

In this step you build your first Bold UI, a form to create, read, update and delete customers. You will:

- Use **TBoldListHandle** to select objects from the object space
- Use **TBoldGrid** to show them in a table
- Use **TBoldNavigator** to create and delete objects
- Use **TBoldEdit** to view and edit a single attribute
- See how Bold keeps the controls in step with the objects

By the end, you have a working customer form with no SQL and little code.

## What You Will Learn

- What a handle is and why you need one
- How an OCL expression selects the objects to show
- How `TBoldGrid` and the other controls bind to objects
- How to save changes to the database
- How to handle dirty objects (unsaved changes)

## What is a Handle?

A **handle** is a window into the object space. A list handle evaluates an OCL expression and holds the resulting
list of objects:

```
Object space (holds the objects)
    |
    +-- TBoldListHandle (expression: Customer.allInstances)
    |
    +-- Holds: all Customer objects
```

Without a handle, you would write code like this:

```pascal
var
  Customers: TBoldObjectList;
  I: Integer;
begin
  Customers := dmInvoice.BoldSystemHandle1.System.ClassByExpressionName['Customer'];
  for I := 0 to Customers.Count - 1 do
    DisplayCustomer(Customers[I] as TCustomer);
end;
```

and then write more code to refresh the display whenever a customer changes. With a handle, you set the expression
and Bold does the rest:

```pascal
lhaCustomers.Expression := 'Customer.allInstances';
// Bold evaluates the expression, tracks the selected customer,
// and tells the controls when the list or a customer changes
```

Handles are the bridge between the object space and the controls on a form.

## Key Concepts: TBoldListHandle

### What It Does

```
TBoldListHandle
  RootHandle: BoldSystemHandle1          // where the expression starts: the object space
  Expression: 'Customer.allInstances'    // what to select
  CurrentElement                         // the selected object (like a cursor)
```

When the system is active:

1. Bold evaluates the expression on the root handle's value. `Customer.allInstances` loads all customers from the
   database the first time.
2. The handle holds the list of matching objects.
3. The handle tracks a **current element**, the selected row.
4. All controls connected to the handle stay in step with it.

### CurrentElement, Like a Database Cursor

A list handle points to one current object:

- When you click a row in the grid, the handle's `CurrentElement` changes.
- All other controls connected to the handle (`TBoldEdit`, `TBoldLabel`) show the new current object.

**Example**: `edtName` is a `TBoldEdit` connected to `lhaCustomers` with the expression `name`. It shows the name of
the current customer. Click another row, and `edtName` shows that customer's name. No code is needed.

## Building the Customer Form

The finished form (`Step2Form.pas`) looks like this:

```
┌──────────────────────────────────────────────┐
│ Step 2: Customer CRUD                        │
├──────────────────────────────────────────────┤
│ Grid of all customers                        │
│ (Name, City, Phone, Email)                   │
│ [|<] [<] [>] [>|] [+] [-]                    │
├──────────────────────────────────────────────┤
│ Name:    [______________]                    │
│ Address: [______________]                    │
│ City:    [______________]                    │
│ Phone:   [______________]                    │
│ Email:   [______________]                    │
├──────────────────────────────────────────────┤
│ [Save Changes] [Open/Close system]   status  │
└──────────────────────────────────────────────┘
```

Build it in your project from Step 1. `Step2.dproj` is the finished version to compare with.

First, add the data module's unit to the form's `uses` clause (**File > Use Unit**). Then the Object Inspector
offers the data module's components, such as `BoldSystemHandle1`.

### 1. Add a TBoldListHandle

Drop a `TBoldListHandle` (page **Bold Handles**) on the form and set:

- `Name`: `lhaCustomers`
- `RootHandle`: `BoldSystemHandle1` of the data module
- `Expression`: `Customer.allInstances`

This tells Bold: "give me all Customer objects". The finished form also sets `StaticSystemHandle` to
`BoldSystemHandle1`; it gives the handle the model's type information at design time.

### 2. Add a TBoldGrid

Drop a `TBoldGrid` (page **Bold Controls**) and set:

- `Name`: `grdCustomers`
- `BoldHandle`: `lhaCustomers`

Right-click the grid and choose **Create Default Columns**: Bold adds a column for each attribute of Customer. With
**Edit Columns** you can remove columns or change them; each column has an OCL expression
(`BoldProperties.Expression`, for example `name`) and a title (`Title.Caption`). The finished form shows Name,
City, Phone and Email.

### 3. Add a TBoldNavigator

Drop a `TBoldNavigator` (page **Bold Controls**) and set:

- `Name`: `bnCustomers`
- `BoldHandle`: `lhaCustomers`

The navigator has these buttons:

- **First**, **Prior**, **Next**, **Last**: move the current element
- **Insert**: creates a new Customer and adds it to the list
- **Delete**: deletes the selected customer, after asking (`DeleteQuestion`)

### 4. Add TBoldEdit Fields

Add a `TBoldEdit` (page **Bold Controls**) for each attribute you want to edit:

```
edtName: TBoldEdit
  BoldHandle: lhaCustomers
  BoldProperties.Expression: 'name'

edtCity: TBoldEdit
  BoldHandle: lhaCustomers
  BoldProperties.Expression: 'city'

edtEmail: TBoldEdit
  BoldHandle: lhaCustomers
  BoldProperties.Expression: 'email'
```

The finished form has one for each of Name, Address, City, Phone and Email. When you select a customer in the
grid, the edit fields show that customer. When you type in a field, the change goes to the object in memory.

### 5. Add the Buttons and Open the System

1. Add a **Save Changes** button. In its `OnClick`:

   ```pascal
   dmInvoice.BoldSystemHandle1.UpdateDatabase;
   ```

2. Add a button whose `Action` is `BoldActivateSystemAction1` of the data module, to open and close the system.
3. In the form's `OnCreate`, open the system as in Step 1:

   ```pascal
   procedure TfrmStep2.FormCreate(Sender: TObject);
   begin
     dmInvoice.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
     dmInvoice.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;
     dmInvoice.OpenSystem;
   end;
   ```

## How the Bold Controls Work

When you set the `BoldHandle` and the expression of a `TBoldEdit`:

1. **Initialization**: the control asks the handle for the current object.
2. **Display**: it evaluates its expression on that object and shows the value.
3. **Edit**: when you type, it writes to the object in the object space, not to the database.
4. **Sync**: when the handle's `CurrentElement` changes, all connected controls update.

This is data binding without data-binding code: no `OnChange` events, no manual refresh. Bold does it through its
subscription system.

### The Subscription System

Behind the scenes, Bold uses a **subscription pattern**:

- The `TBoldEdit` subscribes to the object and attribute it shows.
- When the attribute changes, the object notifies its subscribers.
- The `TBoldEdit` gets the notification and shows the new value.

So when code changes an object, every control that shows it follows:

```pascal
Customer.Name := 'New Name';   // the grid and edtName show 'New Name' without any extra code
```

## Understanding OCL Expressions

The handle uses the expression `Customer.allInstances`:

- **Customer**: the class (like a table in SQL)
- **allInstances**: an OCL operation that means "all objects of this class"

It is roughly the SQL `SELECT * FROM Customer`. But instead of SQL you use the **Object Constraint Language
(OCL)**, which is part of the UML standard. OCL works on objects, not tables:

```
Customer.allInstances          →  all Customer objects
invoices->size                 →  the number of invoices of this customer
invoices->select(not isPaid)   →  this customer's unpaid invoices
```

Step 4 covers OCL in depth.

## Try It Yourself

### Exercise 1: Add Customers Through the UI

1. Run the application.
2. Click **Insert** in the navigator. A new, empty row appears in the grid.
3. Type "Acme Corp" in the Name field.
4. Click another row or another field. The new customer is in memory.
5. Click **Save Changes**. The customer is now in the database.

Restart the application: the customer is still there. Bold loaded it from the database.

### Exercise 2: Edit in the Grid

1. Click a cell in the grid's Name column.
2. Type a new value.
3. The edit fields below show the new value.
4. Click **Save Changes**.

### Exercise 3: Delete a Customer

1. Select a customer.
2. Click **Delete** in the navigator and confirm.
3. The row disappears.
4. Click **Save Changes**. The customer is deleted from the database.

### Exercise 4: Discard Changes

Add a **Discard** button with this `OnClick`:

```pascal
dmInvoice.BoldSystemHandle1.System.Discard;
```

1. Change a customer's name. Do not save.
2. Click **Discard**.
3. The grid shows the saved name again.

`Discard` throws away all unsaved changes in the object space.

### Exercise 5: Try an Invalid Expression

Set the handle's expression to something invalid:

```
InvalidClass.allInstances
```

Bold reports an error when it evaluates the expression. Check an expression in the OCL editor of the Object
Inspector before you use it.

## The Form's Close Query

The form asks before it closes with unsaved changes:

```pascal
procedure TfrmStep2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if dmInvoice.BoldSystemHandle1.Active then
    if dmInvoice.BoldSystemHandle1.System.DirtyObjects.Count > 0 then
      case MessageDlg('There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: dmInvoice.BoldSystemHandle1.System.UpdateDatabase;
        mrNo: dmInvoice.BoldSystemHandle1.System.Discard;
        mrCancel: CanClose := False;
      end;
end;
```

- `DirtyObjects` lists the objects with unsaved changes.
- **Yes** saves them, **No** discards them, **Cancel** keeps the form open.

Every form of the tutorial does this: never lose unsaved work silently.

## Common Pitfalls

### Forgetting UpdateDatabase

Changes in memory do not go to the database by themselves:

```pascal
Customer.Name := 'New Name';
// The object space has the change, the database does not. You must call:
dmInvoice.BoldSystemHandle1.System.UpdateDatabase;
```

If you forget, the changes are lost when the application closes.

### Deleting from a List While Iterating

A deleted object leaves `Customer.allInstances`, so the list gets shorter while you loop over it. Loop backwards:

```pascal
// Right: iterate backwards
for I := lhaCustomers.ObjectList.Count - 1 downto 0 do
  lhaCustomers.ObjectList[I].Delete;

// Wrong: iterate forwards while deleting
for I := 0 to lhaCustomers.ObjectList.Count - 1 do
  lhaCustomers.ObjectList[I].Delete;  // skips items, then runs past the end
```

## Next Step

You have a working customer UI. Customers without invoices are not much use, though.

Go to [`Step3_Invoices/README.md`](../Step3_Invoices/README.md) to add invoices and master-detail views.

---

**Key Takeaways**:

- `TBoldListHandle` selects objects with an OCL expression
- `TBoldGrid` shows a list of objects; clicking a row changes the handle's `CurrentElement`
- `TBoldEdit` shows and edits one attribute of the current object
- All controls stay in step through subscriptions
- Changes stay in memory until you call `UpdateDatabase`
- Check `DirtyObjects` before closing, so no data is lost
