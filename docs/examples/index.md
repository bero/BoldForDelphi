# Examples

Bold for Delphi includes several example applications demonstrating different features.

## Location

Examples are in the `examples/` folder:

```
examples/
├── Simple/
│   ├── ObjectSpace/
│   │   └── MasterDetail/     # Basic CRUD operations
│   └── LogBridge/            # Logging integration
└── Compound/
    └── Building/             # Associations demo
```

## Simple Examples

### MasterDetail

**Location**: `examples/Simple/ObjectSpace/MasterDetail/`

Basic CRUD application demonstrating:

- Creating and editing objects
- List handles and grids
- Database persistence
- Navigator controls

```pascal
// Open the project
examples\Simple\ObjectSpace\MasterDetail\MasterDetail.dproj
```

### LogBridge

**Location**: `examples/Simple/LogBridge/`

Demonstrates integrating Bold with external logging frameworks.

## Compound Examples

### Building

**Location**: `examples/Compound/Building/`

More complex example showing:

- Multiple related classes
- Associations between objects
- Derived attributes
- OCL queries

## Online Example Documentation

Interactive example documentation is available in the repository at `Doc/html/index.html`.

Topics covered:

- MasterDetail - Basic CRUD
- Buildings & Owners - Associations
- Derived Attributes - Calculated values
- Constraints - Model validation
- Transactions - Transaction handling
- OCL Variables - Query parameters
- Renderers - Custom display
- TreeView - Hierarchical display

## PDF Tutorials

In-depth tutorials are available in PDF format in the `Doc/` folder:

| Tutorial | Description |
|----------|-------------|
| Starting Bfd - Part 1 | Introducing the Basics |
| Starting Bfd - Part 2 | Extending Models |
| Starting Bfd - Part 3 | OCL Queries |
| ad970808_UML11_OCL.pdf | Official OCL Language Reference |
| Creating custom Bold-aware components.pdf | Building Bold-aware controls |

Download from: [Doc folder on GitHub](https://github.com/bero/BoldForDelphi/tree/develop/Doc)

## Running Examples

1. Open the example project in Delphi
2. Build the project (Shift+F9)
3. Configure database connection in the INI file
4. Run (F9)

### Database Configuration

Most examples use an INI file for database configuration. Edit the INI to match your database:

```ini
[Database]
Persistence=FireDAC
Type=SQLite

[SQLite]
Database=example.db
```

See [First Application](../getting-started/first-app.md) for database configuration examples.
