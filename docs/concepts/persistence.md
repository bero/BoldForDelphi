# Persistence

Bold provides transparent **Object-Relational Mapping (ORM)** that automatically persists your domain objects to a relational database.

## Architecture
```mermaid
flowchart LR
    subgraph ObjectSpace["Object Space"]
        Objects[TBoldObject]
    end

    subgraph PMapper["Persistence Mapper"]
        ORM[Object-Relational Mapper]
        SQL[SQL Generator]
    end

    subgraph Database["Database"]
        Tables[(Tables)]
    end

    Objects --> ORM
    ORM --> SQL
    SQL --> Tables
```

## Mapping Rules

### Classes to Tables

| UML Element | Database Element |
|-------------|-----------------|
| Class | Table |
| Attribute | Column |
| Association (1:N) | Foreign Key |
| Association (N:M) | Link Table |
| Inheritance | Type discriminator column |

### Example Mapping
```mermaid
flowchart LR
    subgraph UML["UML Model"]
        Customer["Customer\n───────────\nName: String\nEmail: String"]
        Order["Order\n───────────\nOrderDate: Date\nTotal: Currency"]
        OrderItem["OrderItem\n───────────\nQuantity: Integer"]
    end

    subgraph DB["Database Schema"]
        T1["CUSTOMER"]
        T2["CUSTOMER_ORDER"]
        T3["ORDER_ITEM"]
    end

    Customer --> T1
    Order --> T2
    OrderItem --> T3
```
```mermaid
erDiagram
    CUSTOMER {
        INT BOLD_ID PK
        INT BOLD_TYPE
        VARCHAR NAME
        VARCHAR EMAIL
    }
    
    CUSTOMER_ORDER {
        INT BOLD_ID PK
        INT BOLD_TYPE
        INT CUSTOMER_ID FK
        DATE ORDERDATE
        DECIMAL TOTAL
    }
    
    ORDER_ITEM {
        INT BOLD_ID PK
        INT BOLD_TYPE
        INT ORDER_ID FK
        INT PRODUCT_ID FK
        INT QUANTITY
    }
    
    CUSTOMER ||--o{ CUSTOMER_ORDER : "places"
    CUSTOMER_ORDER ||--o{ ORDER_ITEM : "contains"
```

## Configuration

### Database Adapter Setup
```pascal
// FireDAC adapter (recommended)
BoldDatabaseAdapterFireDAC1.Connection := FDConnection1;
BoldPersistenceHandleDB1.DatabaseAdapter := BoldDatabaseAdapterFireDAC1;
```

### Supported Persistence Targets

| Target | Adapter |
|--------|---------|
| SQL Server | FireDAC, UniDAC |
| PostgreSQL | FireDAC, UniDAC |
| InterBase | FireDAC |
| Oracle | FireDAC, UniDAC |
| SQLite | FireDAC |
| XML | BoldPersistenceHandleFileXML |

## Operations

### Save Changes
```pascal
// Save all dirty objects to database
BoldSystemHandle1.UpdateDatabase;

// Or via system
BoldSystemHandle1.System.UpdateDatabase;
```

### Fetch Objects

Bold fetches objects lazily by default:
```pascal
// Objects loaded on first access
Customer := Customers[0];  // Fetches from DB if not loaded
Name := Customer.Name;     // Attribute already loaded
```

### Batch Fetching

For performance, prefetch related objects:
```pascal
// Fetch customers and their orders in one query
BoldSystemHandle1.System.FetchLinksWithObjects(
  CustomerList,
  'orders'
);
```

## Transactions

### Basic Transaction
```pascal
BoldSystem.StartTransaction;
try
  // Create, modify, delete objects
  Customer.Name := 'Updated';
  Order.Delete;

  BoldSystem.CommitTransaction;
  BoldSystem.UpdateDatabase;
except
  BoldSystem.RollbackTransaction;
  raise;
end;
```

### Nested Transactions

Bold supports nested transactions using savepoints. Each call to `StartTransaction` creates a new nesting level, and `CommitTransaction` or `RollbackTransaction` operates on that level.

```mermaid
flowchart TB
    subgraph Level0["Transaction Level 0"]
        Start0[StartTransaction]
        Change1[Modify Customer]

        subgraph Level1["Transaction Level 1 (Savepoint)"]
            Start1[StartTransaction]
            Change2[Create Order]

            subgraph Level2["Transaction Level 2 (Savepoint)"]
                Start2[StartTransaction]
                Change3[Add OrderItems]
                Rollback2[RollbackTransaction]
            end

            Commit1[CommitTransaction]
        end

        Commit0[CommitTransaction]
    end
```

```pascal
// Outer transaction
BoldSystem.StartTransaction;
try
  Customer.Name := 'Updated Name';

  // Nested transaction (savepoint)
  BoldSystem.StartTransaction;
  try
    Order := TOrder.Create(BoldSystem);
    Order.Customer := Customer;

    // Deeper nesting
    BoldSystem.StartTransaction;
    try
      OrderItem := TOrderItem.Create(BoldSystem);
      OrderItem.Order := Order;
      // Something goes wrong here
      raise Exception.Create('Validation failed');
      BoldSystem.CommitTransaction;
    except
      // Only rolls back OrderItem creation
      BoldSystem.RollbackTransaction;
      raise;
    end;

    BoldSystem.CommitTransaction;
  except
    // Rolls back Order creation
    BoldSystem.RollbackTransaction;
    raise;
  end;

  BoldSystem.CommitTransaction;
  BoldSystem.UpdateDatabase;
except
  // Rolls back everything including Customer change
  BoldSystem.RollbackTransaction;
  raise;
end;
```

**Key behaviors:**

| Operation | Effect |
|-----------|--------|
| `StartTransaction` | Creates savepoint at current nesting level |
| `CommitTransaction` | Confirms changes at current level, moves to parent level |
| `RollbackTransaction` | Reverts changes at current level, moves to parent level |
| `UpdateDatabase` | Persists all committed changes to database |

**Important:** `UpdateDatabase` should only be called after the outermost transaction commits. Nested transactions operate in memory until the final commit and database update.

## Schema Evolution

Bold can evolve your database schema when the model changes:
```pascal
// Use DbEvolutor to generate migration scripts
BoldDbEvolutor1.GenerateScript;
```

Changes handled:

- Add/remove classes (tables)
- Add/remove attributes (columns)
- Modify attribute types
- Add/remove associations

## Performance Tips

1. **Use batch fetching** for collections you'll iterate
2. **Avoid N+1 queries** by prefetching associations
3. **Use OCL efficiently** - filter in database when possible
4. **Index foreign keys** for faster association traversal