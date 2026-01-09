# First Application

This guide walks you through creating a simple Bold application with a Customer-Order domain model.

## Step 1: Create the UML Model

Bold uses UML class diagrams to define your domain model. Create a new model with:

```
┌─────────────────┐        ┌─────────────────┐
│    Customer     │        │     Order       │
├─────────────────┤        ├─────────────────┤
│ - Name: String  │ 1    * │ - OrderDate:    │
│ - Email: String │────────│   DateTime      │
│                 │ orders │ - Total: Float  │
└─────────────────┘        └─────────────────┘
```

## Step 2: Generate Code

Bold generates a `BusinessClasses.pas` file from your model:

```pascal
type
  TCustomer = class(TBoldObject)
  private
    function GetName: string;
    procedure SetName(const Value: string);
    function GetOrders: TOrderList;
  public
    property Name: string read GetName write SetName;
    property Orders: TOrderList read GetOrders;
  end;
```

## Step 3: Set Up the Application

### Create a New VCL Application

1. File > New > VCL Forms Application
2. Add Bold packages to your project

### Add Required Components

Drop these components on your main form:

```pascal
// Data access
BoldSystemHandle1: TBoldSystemHandle;
BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;

// Database connection
FDConnection1: TFDConnection;
BoldDatabaseAdapterFireDAC1: TBoldDatabaseAdapterFireDAC;

// UI binding
BoldListHandle1: TBoldListHandle;
```

### Configure Connections

```pascal
// Link components
BoldSystemHandle1.SystemTypeInfoHandle := BoldSystemTypeInfoHandle1;
BoldSystemHandle1.PersistenceHandle := BoldPersistenceHandleDB1;
BoldDatabaseAdapterFireDAC1.Connection := FDConnection1;
BoldPersistenceHandleDB1.DatabaseAdapter := BoldDatabaseAdapterFireDAC1;
```

## Step 4: Work with Objects

### Create Objects

```pascal
var
  Customer: TCustomer;
begin
  // Start transaction
  BoldSystemHandle1.System.StartTransaction;
  try
    // Create new customer
    Customer := TCustomer.Create(BoldSystemHandle1.System);
    Customer.Name := 'Acme Corp';
    Customer.Email := 'contact@acme.com';

    // Commit transaction
    BoldSystemHandle1.System.CommitTransaction;
  except
    BoldSystemHandle1.System.RollbackTransaction;
    raise;
  end;
end;
```

### Query Objects with OCL

```pascal
var
  Customers: TBoldObjectList;
begin
  // Find all customers with orders
  Customers := BoldSystemHandle1.System.EvaluateExpressionAsNewElement(
    'Customer.allInstances->select(orders->notEmpty)',
    nil
  ) as TBoldObjectList;
end;
```

### Save to Database

```pascal
BoldSystemHandle1.UpdateDatabase;
```

## Step 5: Bind to UI

Use Bold-aware controls for automatic data binding:

```pascal
// BoldListHandle shows all customers
BoldListHandle1.RootHandle := BoldSystemHandle1;
BoldListHandle1.Expression := 'Customer.allInstances';

// BoldGrid displays the list
BoldGrid1.BoldHandle := BoldListHandle1;
```

## Complete Example

See the `examples/Simple/ObjectSpace/MasterDetail` project for a complete working example.
