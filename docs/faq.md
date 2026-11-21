# Frequently Asked Questions

Short answers to common tasks. The examples use a `TCustomer` class with an `Orders` association and a `TOrder`
class with a `Customer` end; replace them with the classes of your own model. `BoldSystem` is the `TBoldSystem` of
your application, for example `BoldSystemHandle1.System`.

## Objects

### How do I create an object?

```pascal
// With the generated class
Customer := TCustomer.Create(BoldSystem);

// By the name of the model class, when the class is only known at run time
Customer := BoldSystem.CreateNewObjectByExpressionName('Customer') as TCustomer;
```

### How do I save changes to the database?

```pascal
BoldSystem.UpdateDatabase;          // or BoldSystemHandle1.UpdateDatabase
```

`UpdateDatabase` writes every new, changed and deleted object in one database transaction: either all of it is
saved or, on an error, none of it.

### How do I delete an object?

```pascal
Customer.Delete;              // deletes it in the object space
BoldSystem.UpdateDatabase;    // removes it from the database
```

### How do I check for unsaved changes?

```pascal
if BoldSystem.BoldDirty then
  ShowMessage(Format('%d unsaved objects', [BoldSystem.DirtyObjects.Count]));
```

### How do I throw away unsaved changes?

```pascal
BoldSystem.Discard;
```

## OCL queries

`EvaluateExpressionAsNewElement` returns a new element that the caller owns and must free. The other
`EvaluateExpressionAs...` functions return a plain value. See [OCL](concepts/ocl.md) for the language.

### How do I get all objects of a class?

```pascal
// The class extent: a list owned by the system, do not free it
Customers := BoldSystem.ClassByExpressionName['Customer'];

// The same through OCL: a new list, free it when done
Customers := BoldSystem.EvaluateExpressionAsNewElement('Customer.allInstances') as TBoldObjectList;
```

### How do I filter objects?

```pascal
Active := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances->select(active)') as TBoldObjectList;

WithOrders := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances->select(orders->notEmpty)') as TBoldObjectList;
```

### How do I sort a list?

```pascal
ByName := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances->orderBy(name)') as TBoldObjectList;

NewestFirst := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances->orderDescending(createdDate)') as TBoldObjectList;
```

### How do I calculate a sum or a count?

```pascal
Total := BoldSystem.EvaluateExpressionAsFloat('Order.allInstances.total->sum');
Count := Customer.EvaluateExpressionAsInteger('orders->size');
```

## Associations

### How do I link two objects?

```pascal
// One-to-many: either end
Customer.Orders.Add(Order);
Order.Customer := Customer;     // the same link

// Many-to-many: either end
Student.Courses.Add(Course);
Course.Students.Add(Student);   // the same link
```

Bold keeps both ends consistent: after either line of a pair, both ends show the link.

### How do I remove a link?

```pascal
Customer.Orders.Remove(Order);
Order.Customer := nil;          // the same
```

### How do I navigate associations?

```pascal
var
  Order: TOrder;
begin
  for Order in Customer.Orders do       // Delphi: generated lists have typed enumerators
    ProcessOrder(Order);

  Customer := Order.Customer;           // the single end
end;
```

Under Free Pascal the generated lists have no typed enumerator; use an index loop over `Customer.Orders[i]`.

## Null values

### How do I check whether an attribute is null?

Each attribute has an `M_` property that gives the attribute object itself:

```pascal
if Customer.M_Email.IsNull then
  ShowMessage('No email address');
```

### How do I set an attribute to null?

```pascal
Customer.M_Email.SetToNull;
```

## Transactions

### What are Bold transactions for?

`StartTransaction`, `CommitTransaction` and `RollbackTransaction` work on the object space, not on the database. A
rollback restores every object to the values it had when the transaction started, without a database round trip, so
a group of in-memory changes becomes all-or-nothing. Saving does not need a Bold transaction: `UpdateDatabase`
always writes in one database transaction.

[Object Space: Transactions](concepts/object-space.md#transactions) has the code pattern and how nested transactions
behave, and the
[Transaction example](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/Transaction)
shows a batch that is committed or rolled back as a whole.

## User interface

### How do I bind a grid to a list?

```pascal
BoldListHandle1.RootHandle := BoldSystemHandle1;
BoldListHandle1.Expression := 'Customer.allInstances';
BoldGrid1.BoldHandle := BoldListHandle1;
```

### How do I bind an edit to an attribute?

```pascal
BoldEdit1.BoldHandle := CustomerHandle;
BoldEdit1.BoldProperties.Expression := 'name';
```

The Bold-aware controls are VCL controls. In FireMonkey and in the LCL, bind a `TBoldDataSet` to the handle and use
LiveBindings or the LCL's data-aware controls.

### Why does my user interface not update?

1. Check that each handle's `RootHandle` chain ends at the system handle, and that the system is active.
2. Check the expression: an OCL error leaves the handle empty.
3. Check that the object is in the same object space as the handle.

## Performance

### How do I fetch related objects in fewer queries?

```pascal
// Fetch the orders of all customers in the list at once
BoldSystem.FetchLinksWithObjects(CustomerList, 'orders');

// Fetch all objects of a list that are not loaded yet
CustomerList.EnsureObjects;
```

### Why are my queries slow?

1. OCL is evaluated in memory by default, so `Customer.allInstances->select(...)` loads every customer first.
   Pass `EvaluateInPS = True` to `EvaluateExpressionAsNewElement` to translate the expression to SQL instead, or
   use `TBoldSQLHandle` for a hand-written `WHERE` clause.
2. Fetch links in batches, as above, instead of one object at a time.
3. Add database indexes on the columns you filter on.

### How do I reduce memory use?

`TBoldUnloaderHandle` (palette page Bold Handles) unloads objects that have not been used for a while. Unloaded
objects are fetched again when they are next needed.

## Errors

### "Member is read-only derived"

The code assigns a derived attribute. A derived attribute is computed from its derivation, so it cannot be set,
unless the model marks it reverse-derived and the class implements the reverse derivation (see the
[Reverse-Derived Attributes example](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/ReverseDerivedAttributes)).

### `UpdateDatabase` fails

The whole update is rolled back, and the objects stay dirty in the object space. Check the exception message for
the SQL error, typically a constraint violation, a missing table or column (the schema is older than the model), or
a lost connection.

### An OCL expression gives an error

1. Check that the class and member names match the names in the model.
2. Check the navigation path one step at a time, for example in the
   [OCL Workbench example](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/Tools/OclWorkbench)
   or with the OCL editor of a handle's `Expression` property.

## Model and schema changes

### How do I update the database schema after a model change?

In the IDE, **File → Save and Generate All** in the model editor offers to check the database for schema changes
and evolve it. In code, `TBoldDataBaseEvolutor` (unit `BoldDbEvolutor`) compares the model with the database while
the persistence handle is inactive:

```pascal
Evolutor := TBoldDataBaseEvolutor.Create(BoldPersistenceHandleDB1);
try
  Evolutor.CalculateScript;
  if Evolutor.HasSchemaChanges then
    Evolutor.ExecuteScript;     // or GenerateScript(DbScript, MappingScript) to review the SQL first
finally
  Evolutor.Free;
end;
```

The [Model Evolution example](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/ModelEvolution)
shows a model change that keeps the existing data.

### Can I use Bold with an existing database?

Yes, in two ways:

- Let Bold own a new database and import the old data into it. The tagged values `TableName` and `ColumnName` let
  the model choose the table and column names.
- Keep the external database and map it by hand with external persistence, as the
  [External Persistence example](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/ExternalPersistence)
  does.

## More help

- [Discord](https://discord.gg/C6frzsn)
- [GitHub issues](https://github.com/bero/BoldForDelphi/issues)
- [Bold for Delphi wiki](https://delphi.fandom.com/wiki/Bold_for_Delphi)
