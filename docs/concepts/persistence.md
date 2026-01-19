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

Bold supports nested transactions with savepoints.

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